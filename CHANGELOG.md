# Changelog

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). Versions follow the [Haskell PVP](https://pvp.haskell.org/).

## [Unreleased]

### Fixed

- A connection reset while a response streams now fails the turn, and the REPL keeps its history. Before, the reset reached `send` as a raw `IOException`. `send` caught only `HttpException`, so the reset killed the process and lost the session. The request is not retried, since the completion may already be billed.

## [0.1.0] - 2026-09-24

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

[0.1.0]: https://github.com/shakfu/hilda/releases/tag/v0.1.0
