# Changelog

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). Versions follow the [Haskell PVP](https://pvp.haskell.org/) for the command-line interface; the library modules are internal.

## [Unreleased]

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
