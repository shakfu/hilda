# hilda

A coding agent in Haskell. It talks to any OpenAI-compatible chat-completions server or to OpenRouter, and runs headless or as a REPL.

## Why write an agent in Haskell?

An agent loop is mostly plumbing: JSON arrives from an untrusted model and drives effects on the machine. Haskell's type system makes that boundary explicit.

- **The backend is a function type.** `Complete = Request -> IO (Either Text Reply)` is the loop's only dependency on the network. `test/AgentSpec.hs` substitutes a scripted backend without a mocking library.

- **Permissions are a total function.** `authorize :: Mode -> Effect -> Verdict` maps each mode and tool effect to allow, confirm or deny. With `-Wall`, a new mode produces a warning until it is handled. A new effect is allowed in `yolo`, confirmed in `ask` and denied in `read-only`.

- **Effects are visible in signatures.** Edits (`applyEdit`), rendering (`renderEvent`), option resolution (`resolveProvider`) and prompt assembly (`assemble`) have no `IO` in their types. Their tests call them directly, with no temp files or servers.

- **One loop, two monads.** `runTurn` runs in any `MonadIO`. Headless mode uses `IO`. The REPL uses haskeline's `InputT`, so `ask`-mode prompts share the line editor.

- **Model output cannot crash the loop.** Malformed tool arguments, unknown tools and IO errors become `Either` values. The model receives them as tool results.

- **The runtime handles cancellation.** Green threads, `timeout` and asynchronous exceptions implement the bash timeout (it kills the process group) and Ctrl-C cancellation of a REPL turn. The shell runner is about 40 lines.

- **One native executable.** No interpreter or package tree at run time. The binary links only system libraries: libc, libm, libz, libtinfo and libgmp.

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
hilda -p "fix the failing test"                 # one prompt, answer on stdout
git diff | hilda -p -                           # prompt from stdin
hilda -p "list TODOs" --json                    # one JSON object
hilda -p "list TODOs" --stream-json             # one JSON line per event
hilda --append-system-file style.md             # REPL with extra instructions
```

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

`--keep-reasoning` sends each reply's `reasoning_details` back with later requests. OpenRouter advises this for tool calls with reasoning models ([OpenRouter docs](https://openrouter.ai/docs/use-cases/reasoning-tokens)); without it, the model restarts its reasoning after each tool call. The blocks count toward `--context-budget`. Elision drops them from replies to earlier prompts, never from the current tool loop.

It is off by default. In one test run (2026-09-24), omitting the blocks did not fail on either model tried, and `openai/gpt-6-luna-pro` returned none. Gemini's blocks are thought signatures bound to the upstream that issued them. OpenRouter can route one conversation to Google Vertex and then to Google AI Studio, which rejects Vertex signatures ("Corrupted thought signature"). hilda then drops all kept reasoning from the history, prints `[the provider rejected the kept reasoning; resending without it]` (`reasoning_dropped` in `--stream-json`), and resends once.

Color is on when the output is a terminal. `NO_COLOR` or `TERM=dumb` turns it off.

Replies stream. The REPL prints text as it arrives. On a terminal, a `[waiting Ns]` line counts up until the first text and is then erased. It reads `[thinking Ns]` while a reasoning model reasons; the reasoning itself is not printed. Tool-call arguments are not shown while they stream, so a long `write` shows only the waiting line. Headless text mode prints only the final answer, because narration and answer cannot be told apart until the reply ends. A server that ignores `stream` and returns plain JSON also works.

`--stream-json` prints `text_delta` and `reasoning_delta` lines as the reply streams, and `text`, `tool_call` and `tool_result` lines as they happen. Its last line is the `result` object that `--json` prints alone.

## Context budget

hilda keeps the history under `--context-budget` tokens (default 100,000, estimated at four characters per token). Before each model call, if the history is over budget, the oldest tool results and long tool-call arguments (such as file contents sent to `write`) are elided until it fits three quarters of the budget. Each elision changes an early message and invalidates the prompt cache from there on, so trimming in larger steps keeps it rare. Elided results are replaced with a stub such as `[elided to fit the context budget: 20692 characters; run the tool again if needed]`. Elided arguments become `[elided to fit the context budget: N characters]` inside otherwise valid JSON. hilda prints `[context: elided N old tool messages]` when this happens.

- The last assistant message and the results after it are never elided. Neither is user or assistant text, so a history made mostly of text can still exceed the budget.
- Elision is written into the history, so the start of the conversation stays the same between calls and provider prompt caching keeps working.
- Set the budget below your model's context window. Local models often have 8,000 to 32,000 tokens.

The REPL footer and `/usage` show the context size: the prompt tokens of the last model call. `--json` output includes it as `context_tokens`.

`--max-cost USD` stops before the next model call once spending reaches the limit: per prompt in headless runs, per session in the REPL. It needs a provider that reports cost, such as OpenRouter; otherwise hilda warns that the limit has no effect. A single call can pass the limit, since cost is known only after it.

Exit codes: 0 finished, 1 error, 2 stopped by `--max-turns` (default 50), 3 stopped by `--max-cost`.

## Tools and modes

Tools:

- `read`: streams the file. Output stops at the line limit or about 30,000 characters and names the `offset` to continue from.
- `write`: writes through an exclusively created temporary file, then renames it into place.
- `edit`: exact, unique string replacement. Files over 10 MiB are refused.
- `bash`: `bash -c`, default timeout 120 s. The whole process group is killed on timeout.

Other tool output over 30,000 characters keeps its head and tail.

**Warning:** `yolo`, the default, runs every command the model asks for with your user's permissions. A model can delete files, rewrite your dotfiles or send data over the network. Use `-M ask` or `-M read-only` with untrusted prompts or models, or run hilda as an unprivileged user or in a container.

| `-M`, `--mode` | read | write, edit, bash |
|-|-|-|
| `yolo` (default) | run | run |
| `ask` | run | confirm on the terminal, showing every argument in full; refused without a terminal |
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

`/mode`, `/model`, `/tools`, `/system`, `/usage` (tokens and cost), `/clear`, `/quit`. Start a line with `//` to send a prompt that begins with `/`. Ctrl-C cancels the current turn. Ctrl-D exits.
