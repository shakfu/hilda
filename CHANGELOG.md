# Changelog

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). Versions follow the [Haskell PVP](https://pvp.haskell.org/) for the command-line interface; the library modules are internal.

## [Unreleased]

### Added

- `-r`/`--recurse` offers an `agent` tool that runs a sub-agent on a task and returns its final answer; `agent` calls in one reply run concurrently. Sub-agents are green threads in the same process. They inherit the parent's mode, so `ask` confirmations and `read-only` restrictions still hold. A bash-based design, as in minima, would have let one confirmed spawn start an unconfined child. `--budget-tokens N` is required and covers the whole family: each model call reserves its input and an output grant, which is sent as `max_tokens`, or as `max_completion_tokens` once a server refuses the first and names the second, as OpenAI does for its reasoning models. `--max-depth N` (default 2) limits nesting. Design and limits: `docs/dev/recursion.md`.

- `--acp` serves the Agent Client Protocol, version 1, on stdin and stdout, so editors such as Zed and the minos broker can drive hilda. It reuses the agent loop, tools, modes and id sessions: `-M ask` confirmations become `session/request_permission`, `session/cancel` stops the turn as Ctrl-C does, and `session/load` replays a session that `--resume ID` can also continue. hilda's own tools run instead of the client's `fs/*` and `terminal/*`, so it behaves the same under every client. It passes the minos conformance suite (`go/acpconformance`), strict mode included. See the README.

  A session's cost is saved in its file (absent reads as none), so `--max-cost` and `usage_update` no longer restart at each `session/load`. `usage_update` is sent only when the context window is known; its `size` is the window, not the budget. A reply cut by `finish_reason` `length` ends `max_tokens`; `content_filter`, OpenRouter's `native_finish_reason` `refusal` or an OpenAI `refusal` ends `refusal`. `session/new` saves the session, so it loads before its first prompt. Relative `--system-file`, `--append-system-file` and `--agents` paths resolve against the start directory, not the last prompt's. End of input waits for every prompt's answer, including one already finishing. A cancel read before the turn's thread started killed it before its cleanup was installed, so the prompt was never answered and the session refused every later prompt; the thread now starts with its cleanup in place.

  Under `--acp`, the mode defaults to `ask`, so the client decides each write, edit and bash call; `-M yolo` still runs all. A refused prompt is dropped from the history, as ACP requires; its cost stays. An empty prompt is refused with `-32602`. `toolCallId` is prefixed with the reply's number in the session, since models and the `call_N` fallback reuse ids across replies. A call denied by the mode or an "always reject" rule is sent `pending` then `failed`, never `in_progress`. A cancel ends calls still running or awaiting permission as `failed`. An unwritable `contexts.json` or `models.json` is logged instead of failing: the first left every prompt waiting forever, the second answered a finished turn `-32603` and did not save it. `usage_update.used` keeps the last value after a turn whose first model call failed, instead of 0.

- `--no-session` saves no transcript, for callers that never resume, such as a dispatcher running `-p` once per task. Each run also deletes id sessions last saved more than `HILDA_SESSION_DAYS` days ago (default 30, 0 keeps all). Every `-p` run saved its transcript, which holds file contents and command output, and nothing deleted them. Age over a count cap, because a burst of runs would evict a session still wanted. The per-directory REPL files stay: there is one per directory.

- `--context-window TOKENS` gives the model's window where hilda cannot look it up. It sets the default `--context-budget` and the ACP `usage_update` size.

- `-p` runs save their history under a session id, reported as `session_id` in the `--json` and `--stream-json` result. `hilda --resume ID -p TEXT` continues that session, so a caller can run hilda once per turn and keep its context. `--resume ID` also works in the REPL. An unknown id, or one saved in another working directory, exits 1 with the reason in the result's `error`. Ids get their own files instead of sharing the per-directory file, so headless runs never overwrite the session `-c` resumes.

- `hilda [options] [--] PROMPT...` starts the REPL and enters PROMPT as its first line, so a launcher can open an interactive session that starts working at once. Combining it with `-p`, `--json` or `--stream-json` is an error.

- `--reasoning EFFORT` asks the model to reason, sent as `reasoning.effort` to OpenRouter and `reasoning_effort` to the `openai` provider. Values are OpenRouter's: `none`, `minimal`, `low`, `medium`, `high`, `xhigh`, `max`. Without it, Anthropic models do not reason, so `--keep-reasoning` had nothing to keep for them.

- `-c`/`--continue` resumes the last REPL session in the working directory. The REPL saves its history after each turn and `/clear`, not only on exit, so a closed terminal or a stray Ctrl-D loses at most the current turn. Files are per directory under `$XDG_STATE_HOME/hilda/sessions/`, in a 0700 directory since transcripts hold file contents. Per-directory files, not one shared file, avoid lost updates between REPLs in different projects.

- Without `--context-budget`, the budget on OpenRouter is half the model's context window, at most 100,000. The window comes from OpenRouter's endpoints route, as the smallest among the model's upstreams, and is cached in `$XDG_STATE_HOME/hilda/contexts.json`. Half leaves room for the reply and for undercounting at 4 characters per token. A model with a 128k window now gets 64,000, not 100,000. Other providers keep 100,000, since they have no common way to report the window. `/model` in the REPL recomputes the budget for the new model; an explicit `--context-budget` is kept.

- Multi-line prompts in the REPL: Shift+Enter or Ctrl+J starts a new line, and pasted text stays one prompt. The line editor is now isocline, not haskeline, which reads single lines only. isocline 1.1.0 stops after two parameters of xterm's `ESC [ 27 ; mods ; key ~`, which kitty, WezTerm and Ghostty send for Shift+Enter, so hilda vendors it with an 8-line parser fix (`vendor/isocline.patch`) until upstream parses the sequence. Ctrl-C at an `ask` confirmation now declines the call instead of abandoning the turn. The binary no longer links libtinfo.

- Reply text is styled as markdown on a terminal: headings, bold, italic, code spans and code-block fences. The REPL styles it as it streams. A line-at-a-time renderer would delay a one-line paragraph until it ends, so hilda styles characters as they arrive and holds back only an ambiguous marker such as a trailing `*`. Styles end at each newline, so an unclosed marker affects one line.

  `--markdown=always|auto|never` sets styling; `auto`, the default, styles on a terminal without `NO_COLOR`. `--no-color` turns off color of status and tool lines; `NO_COLOR` turns off both. Separate flags let a user keep one without the other. `always` overrides `NO_COLOR`, as no-color.org asks of explicit flags, and serves pagers such as `less -R`.

- Model text shows control characters escaped (`\x1b`) in the REPL, in narration, and in headless answers that are styled or reach a terminal. Provider errors, tool errors and `/system` (which includes AGENTS.md) are escaped too, as the confirmation prompt already did. Before, a reply could clear the screen or retitle the terminal. Format characters such as bidi overrides are not escaped: prose uses RLM, and emoji use ZWJ. An unstyled answer piped elsewhere is not escaped, since it may be data.

- CI on GitHub Actions: build, tests, and a Haddock coverage check that fails below 100%.

### Changed

- Each tool result is limited to a quarter of `--context-budget` (in characters, at 4 per token), at most the previous 30,000. Before, one read could exceed a small budget. In a live run at 6000 tokens, elision then thrashed: the model re-read elided files, asked for files that do not exist and hit `--max-turns`. A quarter, not a half, because elision never touches the newest results, and three parallel results still fit in the three quarters it trims to. Rejecting small budgets would be simpler, but would exclude 8k-context local models.

### Fixed

- Provider errors no longer start with aeson's path prefix (`Error in $: provider error: ...`). The error object is now checked before the body is parsed.

## [0.1.1]

### Added

- `--keep-reasoning` stores each reply's `reasoning_details` and sends them back on later requests, as OpenRouter advises for tool calls with reasoning models. Streamed fragments sharing an `index` are merged into one block, keeping an Anthropic signature that arrives in a final text-less fragment. Blocks count toward `--context-budget`; elision drops them only from replies to earlier prompts, since providers need them unchanged within the current tool loop. Off by default: in a live test, omitting the blocks failed on neither model tried.

  OpenRouter can route one Gemini conversation to Google Vertex and then Google AI Studio, which rejects Vertex signatures as "Corrupted thought signature". On that error hilda drops the kept reasoning from the whole history and resends once, since the history then holds signatures no single upstream accepts. Detection matches the error text; pinning the upstream would avoid the rejection but gives up OpenRouter's failover.

### Changed

- A 429 retry waits for the server's `Retry-After` (seconds form, capped at 60 s) instead of the fixed 1, 2, 4 s backoff.

- `--max-turns` rejects zero and negative values, like `--context-budget` and `--max-cost`.

### Fixed

- Provider errors include OpenRouter's `metadata.raw`. Before, a non-streamed failure showed only "Provider returned error", without the upstream cause.

- End of input at a headless `ask` confirmation declines the call. Before, `getLine` raised an EOF error that ended the run with no result.

- The REPL prints the `--max-cost has no effect` warning once per session, not on every turn.

- `write` and `edit` keep the full mode of an existing file. Before, a 0600 file came back 0644 (0664 under umask 002), and group and other bits were lost: `directory`'s `setPermissions` copies only the owner bits.

- `edit` refuses a file that is not valid UTF-8. Before, it decoded leniently and wrote every invalid byte back as U+FFFD, so one edit to a Latin-1 file corrupted all its non-ASCII text.

- `ask` confirmations escape Unicode format characters such as U+202E. Before, a bidi override could display a command in a different order than bash runs it.

- The REPL session cost includes calls from turns cancelled with Ctrl-C, and `/clear` no longer resets it. Before, both dropped spent cost, so `--max-cost` could be exceeded.

- A connection reset while a response streams now fails the turn, and the REPL keeps its history. Before, the reset reached `send` as a raw `IOException`. `send` caught only `HttpException`, so the reset killed the process and lost the session. The request is not retried, since the completion may already be billed.

- A non-streaming or error response body that stops sending data now fails after 300 s, like a stream does. Before, it could hang the turn forever. The manager's 600 s response timeout covers only the status and headers.

## [0.1.0]

First release.

### Added

- Agent loop over the chat-completions API, with tool calling.

- Providers: `openai` (any OpenAI-compatible server) and `openrouter`. OpenRouter is the default when `OPENROUTER_API_KEY` is set. Select with `-P`/`--provider`. Requests retry up to 3 times on 429 and on connection failures before the request is sent. Other errors fail at once, since a retry could repeat a billed completion.

- The last model per provider that answered is saved atomically to `$XDG_STATE_HOME/hilda/models.json`, so `-m` is needed once.

- Tools: `read`, `write`, `edit`, `bash`. `read` streams files and pages long output with a continuation offset. Writes are atomic via an exclusively created temporary file, and keep symlinks and permissions. `edit` refuses files over 10 MiB. `bash` kills its whole process group on timeout, including commands that close their output and keep running, and captures at most 1 MiB of output.

- Permission modes via `-M`/`--mode`: `yolo` (default), `ask`, `read-only`. `ask` shows every argument of the call in full, with control characters escaped.

- Streamed replies over server-sent events. The REPL prints text as it arrives and shows `[thinking Ns]` while a model reasons; `--stream-json` emits `text_delta` and `reasoning_delta` events. A stream with no data for 300 s fails.

- `--context-budget` (default 100,000 tokens): the oldest tool results and long tool-call arguments are elided when the history exceeds it, down to three quarters of the budget. The context size shows in the REPL footer, `/usage` and `--json` output.

- Prompt caching: requests for `anthropic/` models on OpenRouter carry a top-level `cache_control` marker. Usage lines and `--json` report cached prompt tokens.

- Headless runs with `-p TEXT` or `-p -` (stdin). Output as text, `--json` (one object) or `--stream-json` (one line per event, then the result). Exit codes: 0 finished, 1 error, 2 turn limit, 3 cost limit.

- REPL with history, slash commands (`/mode`, `/model`, `/tools`, `/system`, `/usage`, `/clear`, `/quit`), `//` to send a prompt starting with `/`, and Ctrl-C to cancel a turn.

- One line per tool call with an estimated result size in tokens. ANSI color on terminals, disabled by `NO_COLOR`. A `[waiting Ns]` line counts up during model calls.

- Token and cost totals per turn and per session. Cost comes from OpenRouter's `usage.cost`.

- System prompt from `--system` or `--system-file`, extended by `--append-system` or `--append-system-file`, plus `AGENTS.md` files from the git root down to the working directory. `--agents` and `--no-agents` override discovery.

- `--max-cost USD`: stop before the next model call once the prompt (REPL: session) has cost this much.

- `-V`/`--version`.

[0.1.1]: https://github.com/shakfu/hilda/releases/tag/v0.1.1
[0.1.0]: https://github.com/shakfu/hilda/releases/tag/v0.1.0
