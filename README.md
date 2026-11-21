# hilda

A coding agent in Haskell. It talks to any OpenAI-compatible chat-completions server or to OpenRouter, and runs headless or as a REPL.

## Why write an agent in Haskell?

An agent loop is mostly plumbing: JSON arrives from an untrusted model and drives effects on the machine. Haskell's type system makes that boundary explicit.

- **The backend is a function type.** `Complete = Request -> IO (Either Text Reply)` is the loop's only dependency on the network. `test/AgentSpec.hs` substitutes a scripted backend without a mocking library.

- **Permissions are a total function.** `authorize :: Mode -> Effect -> Verdict` maps each mode and tool effect to allow, confirm or deny. With `-Wall`, a new mode produces a warning until it is handled. A new effect is allowed in `yolo`, confirmed in `ask` and denied in `read-only`.

- **Effects are visible in signatures.** Edits (`applyEdit`), rendering (`renderEvent`), option resolution (`resolveProvider`) and prompt assembly (`assemble`) have no `IO` in their types. Their tests call them directly, with no temp files or servers.

- **One loop, two front ends.** Headless mode and the REPL run the same `runTurn`. They differ only in their `Hooks`: how events print and how `ask`-mode calls are confirmed.

- **Model output cannot crash the loop.** Malformed tool arguments, unknown tools and IO errors become `Either` values. The model receives them as tool results.

- **The runtime handles cancellation.** Green threads, `timeout` and asynchronous exceptions implement the bash timeout (it kills the process group) and Ctrl-C cancellation of a REPL turn. The shell runner is about 40 lines.

- **One native executable.** No interpreter or package tree at run time. The binary links only system libraries: libc, libm, libz and libgmp.

Costs:

- No official OpenAI or OpenRouter SDK exists for Haskell. `Hilda.Provider` is a hand-written HTTP client, so new API features need manual support.

- A clean build compiles about 100 dependencies. That takes minutes, not seconds.

- Building needs `libgmp-dev`. The binary needs libgmp at run time.

- Fewer contributors read Haskell than Python or TypeScript.

## Build

Requires GHC 9.10 and cabal (both via ghcup) and `libgmp-dev`.

```sh
make build     # cabal build all
make test      # cabal test
make install   # copies a stripped hilda to ~/.local/bin (override: BINDIR=...)
```

`vendor/isocline` is the isocline 1.1.0 line editor with a fix that lets it read Shift+Enter; `vendor/isocline.patch` lists the changes.

## Providers and models

| Provider | Selected when | Key | Base URL |
|-|-|-|-|
| `openrouter` | `OPENROUTER_API_KEY` is set, or `-P openrouter` | `OPENROUTER_API_KEY` (required) | `https://openrouter.ai/api/v1` |
| `openai` | otherwise, or `-P openai` | `OPENAI_API_KEY` (optional) | `OPENAI_BASE_URL`, else `https://api.openai.com/v1` |

`--base-url` and `--api-key-env VAR` override either. The `openai` provider covers local servers such as Ollama or llama.cpp; they usually need no key.

Pass `-m NAME` once per provider. hilda saves the last model used with each provider in `$XDG_STATE_HOME/hilda/models.json` (default `~/.local/state/hilda/`) and reuses it when `-m` is absent. A model is saved only after it answers, so a mistyped `-m` is not remembered. REPL `/model` changes are saved the same way.

## Usage

```sh
hilda -m vendor/model                           # REPL
hilda -m vendor/model -- "fix the failing test" # REPL that starts on this prompt
hilda -p "fix the failing test"                 # one prompt, answer on stdout
git diff | hilda -p -                           # prompt from stdin
hilda -p "list TODOs" --json                    # one JSON object
hilda -p "list TODOs" --stream-json             # one JSON line per event
hilda --append-system-file style.md             # REPL with extra instructions
hilda --acp                                     # ACP server for an editor or broker
```

Without `-p`, positional words start the REPL and enter it as the first line, joined by single spaces. The REPL treats it as typed: a leading `/` runs a command, `//` sends a prompt starting with `/`, and a blank prompt is ignored. Put `--` before a prompt that starts with `-`. Positional words cannot be combined with `-p`, `--json` or `--stream-json`. With `-c`, the prompt continues the resumed session.

Text mode prints the answer on stdout and tool activity on stderr, one line per tool call:

```
[bash] git status -> ~40
[edit] src/Hilda/Agent.hs -> error: old_string not found
```

The number after `->` estimates the tokens of the result the model receives, at four characters per token. Long calls are cut to 80 characters with `..`.

Token usage prints after each REPL turn, at REPL exit and after a headless run. OpenRouter also reports cost (`usage.cost`, in credits), which hilda sums per session and includes in `--json` output. OpenAI-compatible servers report no cost, so none is shown.

## Prompt caching

Each model call resends the whole conversation, so caching the repeated start is the main cost saving. OpenAI, DeepSeek, Gemini 2.5+, Grok and several others cache automatically. Anthropic models need a marker. On OpenRouter, hilda adds a top-level `cache_control` to requests for `anthropic/` models, and OpenRouter places it on the last cacheable block ([OpenRouter docs](https://openrouter.ai/docs/features/prompt-caching)). Anthropic's cache lasts 5 minutes by default.

Usage lines show cached tokens, e.g. `12000 in (9000 cached) / 300 out`. `--json` reports them as `cached_tokens`.

## Reasoning

`--reasoning EFFORT` asks the model to reason: `none`, `minimal`, `low`, `medium`, `high`, `xhigh` or `max`. hilda sends it as `reasoning.effort` to OpenRouter and as `reasoning_effort` to the `openai` provider. Each model supports a subset; others fail at the provider. Without the flag, the model's default applies, and Anthropic models do not reason.

`--keep-reasoning` sends each reply's `reasoning_details` back with later requests. OpenRouter advises this for tool calls with reasoning models ([OpenRouter docs](https://openrouter.ai/docs/use-cases/reasoning-tokens)); without it, the model restarts its reasoning after each tool call. The blocks count toward `--context-budget`. Elision drops them from replies to earlier prompts, never from the current tool loop.

It is off by default. In a test on Gemini 3.8 Flash, GPT-6 Luna Pro and Claude Haiku 4.5 with `--reasoning medium`, omitting the blocks never failed and answers were equally correct, while keeping them raised Gemini's cost by about 60%. Gemini's blocks are thought signatures bound to the upstream that issued them. OpenRouter can route one conversation to Google Vertex and then to Google AI Studio, which rejects Vertex signatures ("Corrupted thought signature"). hilda then drops all kept reasoning from the history, prints `[the provider rejected the kept reasoning; resending without it]` (`reasoning_dropped` in `--stream-json`), and resends once.

Color and markdown styling are on when the output is a terminal, and can be turned off separately:

| | Status and tool lines colored | Reply text styled as markdown |
|-|-|-|
| default | yes | yes |
| `--no-color` | no | yes |
| `--markdown=never` | yes | no |
| `--no-color --markdown=never`, `NO_COLOR` or `TERM=dumb` | no | no |

`--markdown=always` styles reply text even when piped (e.g. to `less -R`) or when `NO_COLOR` is set. The default, `auto`, styles only on a terminal.

Replies stream. The REPL prints text as it arrives. Markdown styling covers headings, bold, italic and code spans, and drops their markers. Code-block fences print dim; block content prints as written, as do links, tables and lists. Model text shows control characters escaped (`\x1b`) whenever it reaches a terminal or is styled, so a reply cannot send terminal escape sequences. An unstyled headless answer piped elsewhere is printed unchanged. On a terminal, a `[waiting Ns]` line counts up until the first text and is then erased. It reads `[thinking Ns]` while a reasoning model reasons; the reasoning itself is not printed. Tool-call arguments are not shown while they stream, so a long `write` shows only the waiting line. Headless text mode prints only the final answer, because narration and answer cannot be told apart until the reply ends. A server that ignores `stream` and returns plain JSON also works.

`--stream-json` prints `text_delta` and `reasoning_delta` lines as the reply streams, and `text`, `tool_call` and `tool_result` lines as they happen. Its last line is the `result` object that `--json` prints alone.

## Context budget

hilda keeps the history under `--context-budget` tokens, estimated at four characters per token. Before each model call, if the history is over budget, the oldest tool results and long tool-call arguments (such as file contents sent to `write`) are elided until it fits three quarters of the budget. Each elision changes an early message and invalidates the prompt cache from there on, so trimming in larger steps keeps it rare. Elided results are replaced with a stub such as `[elided to fit the context budget: 20692 characters; run the tool again if needed]`. Elided arguments become `[elided to fit the context budget: N characters]` inside otherwise valid JSON. hilda prints `[context: elided N old tool messages]` when this happens.

- The last assistant message and the results after it are never elided. Neither is user or assistant text, so a history made mostly of text can still exceed the budget.
- Elision is written into the history, so the start of the conversation stays the same between calls and provider prompt caching keeps working.
- Default on OpenRouter, or with `--context-window TOKENS`: half the model's context window, at most 100,000. The window is the smallest among the model's endpoints, looked up once and cached in `$XDG_STATE_HOME/hilda/contexts.json`. Elsewhere, or if the lookup fails, the default is 100,000. `/model` in the REPL looks up the new model's budget; an explicit `--context-budget` stays.
- With a local server, set the budget below the model's context window. Local models often have 8,000 to 32,000 tokens.
- Each tool result is limited to a quarter of the budget: 30,000 characters at the default, 8,000 at a budget of 8,000 tokens. Several recent results then fit even in a small budget.

The REPL footer and `/usage` show the context size: the prompt tokens of the last model call. `--json` output includes it as `context_tokens`.

`--max-cost USD` stops before the next model call once spending reaches the limit: per prompt in headless runs, per session in the REPL. It needs a provider that reports cost, such as OpenRouter; otherwise hilda warns that the limit has no effect. A single call can pass the limit, since cost is known only after it.

Exit codes: 0 finished, 1 error, 2 stopped by `--max-turns` (default 50), 3 stopped by `--max-cost`.

## Tools and modes

Tools:

- `read`: streams the file. Output stops at the line limit or the result limit and names the `offset` to continue from.
- `write`: writes through an exclusively created temporary file, then renames it into place.
- `edit`: exact, unique string replacement. Files over 10 MiB are refused.
- `bash`: `bash -c`, default timeout 120 s. The whole process group is killed on timeout.

Other tool output over the result limit (a quarter of `--context-budget`, at most 30,000 characters) keeps its head and tail.

**Warning:** `yolo`, the default, runs every command the model asks for with your user's permissions. A model can delete files, rewrite your dotfiles or send data over the network. Use `-M ask` or `-M read-only` with untrusted prompts or models, or run hilda as an unprivileged user or in a container.

| `-M`, `--mode` | read | write, edit, bash |
|-|-|-|
| `yolo` (default; `ask` under `--acp`) | run | run |
| `ask` | run | confirm on the terminal, showing every argument in full; refused without a terminal. With `--acp`, the client is asked |
| `read-only` | run | not offered to the model; refused if called |

## System prompt

The system prompt is assembled in this order:

1. hilda's default instructions, or the text of `--system TEXT` or
   `--system-file PATH`, which replace them.
2. `--append-system TEXT` or the text of `--append-system-file PATH`, if given.
3. `Working directory: <cwd>`, always.
4. AGENTS.md files.

Options:

- `AGENTS.md` files load from the git root down to the working directory, outermost first. Without a git root, only the working directory is read.

- `--agents PATH` (repeatable) loads the given files instead. `--no-agents` loads none.

`/system` in the REPL prints the assembled prompt.

## REPL commands

`/mode`, `/model`, `/tools`, `/system`, `/usage` (tokens and cost), `/clear`, `/quit`. Start a line with `//` to send a prompt that begins with `/`. Shift+Enter or Ctrl+J starts a new line in the prompt, and pasted text keeps its lines. Ctrl-C clears the entry at the prompt, cancels a running turn, and declines an `ask` confirmation. Ctrl-D exits.

## Sessions

The REPL saves its history after each turn and after `/clear`, one file per working directory, in `$XDG_STATE_HOME/hilda/sessions/` (mode 0700). `hilda -c` (`--continue`) resumes it. The system prompt is rebuilt from the current options and AGENTS.md files. Session cost starts at zero, so `--max-cost` counts from the resume. Saved transcripts contain file contents and command output. `--continue` works only in the REPL, not with `-p`.

Each `-p` run saves its history under a new session id, a 16-digit hex string, in the same directory. `--json` and `--stream-json` report it as `session_id` in the `result` object; text mode prints it in the usage line on stderr. `hilda --resume ID -p TEXT` sends TEXT as the next prompt after that history and saves it back under the same id. In the REPL, `--resume ID` resumes the session and saves each turn to it. hilda refuses a session saved in another working directory, since its history names files there. An unknown or invalid id exits with status 1; with `--json` the `result` object has `stop: "error"` and the reason in `error`. `-c` reads only the REPL's per-directory file, so `-p` runs never overwrite it. Two runs that resume the same id at once both succeed, and the later save wins.

`--no-session` saves no transcript: a `-p` run reports `session_id: null`, and the REPL keeps no history for `-c`. `--resume` and `-c` still read a saved session. Each run deletes the id sessions last saved more than `HILDA_SESSION_DAYS` days ago (default 30; 0 keeps all), so an id older than that no longer resumes. The REPL's per-directory files are kept. ACP sessions are id sessions too; each prompt saves the file again, so a session in use is not deleted.

```sh
id=$(hilda -p "read main.c" --json | jq -r .session_id)
hilda --resume "$id" -p "now fix the leak" --json | jq -r .result
```

## ACP

`hilda --acp` serves the [Agent Client Protocol](https://agentclientprotocol.com/protocol/), version 1, on stdin and stdout. An editor such as Zed, or the minos broker (`minosb -agent acp -- hilda --acp`), runs it as a subprocess. stdout carries only ACP messages, one JSON-RPC object per line; warnings go to stderr. `--acp` cannot be combined with `-p`, `--json`, `--stream-json`, PROMPT..., `-c` or `--resume`.

```sh
hilda --acp -P openrouter -m vendor/model
```

- **Capabilities:** `loadSession`; text and `resource_link` prompt blocks. A prompt with neither is refused with `-32602`. A resource link reaches the model as its path, which it can read with `read`. No images, audio, embedded context or MCP: `mcpServers` is accepted and ignored. hilda uses its own tools, never the client's `fs/*` or `terminal/*`.
- **Sessions:** `session/new` takes the working directory for tools and AGENTS.md, and saves the session at once, so `session/load` finds it before any prompt. Sessions are the id sessions of `-p` runs, so `hilda --resume ID` continues an ACP session in the same directory, and `session/load` loads a `-p` run's `session_id`. `session/load` replays the history as `user_message_chunk`, `agent_message_chunk`, `tool_call` and `tool_call_update`. The session file keeps the session's cost, so `--max-cost` and `usage_update` count on after a load.
- **Updates:** reply text streams as `agent_message_chunk`, reasoning as `agent_thought_chunk`. Each tool call is a `tool_call` with `kind` `read`, `edit` or `execute`, then `tool_call_update` with the result. A call denied without asking goes from `pending` to `failed`. `toolCallId` is the model's id prefixed with the reply's number in the session, since models reuse ids across replies. A `usage_update` ends each turn when the context window is known: `used` is the last prompt's tokens, `size` the window, and `cost` the session's total, in `USD` on OpenRouter and `credits` elsewhere. OpenRouter reports the window; elsewhere pass `--context-window TOKENS`. Without a window, no `usage_update` is sent, since `size` is required.
- **Permissions:** `--acp` defaults to `-M ask`: each write, edit and bash call is a `session/request_permission` offering allow or reject, once or always. "Always" holds for that tool for the rest of the session. A cancelled request is a denial. Pass `-M yolo` to ask nothing; `-M read-only` offers the model only `read`.
- **Stop reasons:** `end_turn`; `max_tokens` when the reply hit the output limit (`finish_reason` `length`); `refusal` for `content_filter` or a provider refusal, which drops the prompt and its turn from the history but keeps the cost; `max_turn_requests` for `--max-turns`, and for `--max-cost`, which also stops before a model request; `cancelled` after `session/cancel`. A provider error answers the prompt with a JSON-RPC error. `--max-cost` counts the whole session.
- **Cancel:** `session/cancel` stops the model call and kills a running `bash` process group. Calls still running or awaiting permission end `failed`. The session keeps its history from before the prompt, as Ctrl-C does in the REPL.

Prompts run one at a time across sessions, since tools resolve paths against the process's working directory. Relative `--system-file`, `--append-system-file` and `--agents` paths resolve against the directory hilda started in.

Conformance with the minos suite, against `test/acp-mock.py`:

```sh
python3 test/acp-mock.py 18765 &
cd ../minos && ACP_STRICT=1 ACP_EXPECT_USAGE=1 ACP_SLOW_PROMPT=slow ACP_PERMISSION_PROMPT=permission make acp-conformance \
  ACP_AGENT="$(cd ../hilda && cabal list-bin hilda) --acp -P openai --base-url http://127.0.0.1:18765/v1 -m mock -M ask --context-window 128000"
```
