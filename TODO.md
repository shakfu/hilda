# TODO

## Critical

## High

- **Pass `reasoning_details` back with tool calls.** OpenRouter requires the reasoning blocks of an assistant message to be returned unchanged in later requests for Anthropic (tool use), OpenAI reasoning (encrypted blocks) and Gemini (thought signatures). Without them the model restarts its reasoning after each tool call. hilda drops them. Needs the streamed fragments reassembled into complete blocks, signatures included; verify against a live model of each kind. See [reasoning tokens](https://openrouter.ai/docs/use-cases/reasoning-tokens).

- **Validate against a real provider.** Streaming, prompt caching, the context budget and `--max-cost` were tested against local fake servers only. Repeat a long run (e.g. the project review with a DeepSeek model) and compare input tokens, `(N cached)` and cost with the previous run: 1.34M input tokens, $0.0438.

## Medium

- **CI.** GitHub Actions running `cabal build` and `cabal test` on the pinned index state, caching `~/.cabal/store`. The fake-server tests use only localhost.

- **Resume a session (`--continue`).** Save the REPL history to the state directory on exit and reload it on request, so a stray Ctrl-D does not lose a long session.

- **Multi-line REPL input.** haskeline reads one line at a time, so pasted code is split into separate turns. Add a `"""`-delimited block mode.

## Low

- **Allow-list for `ask` mode** (e.g. `--allow 'git status*'`). Matching shell commands by pattern is unsafe (`git status; rm -rf ~`), so this needs real command parsing or a narrow design.

- **Default `--context-budget` from the model's `context_length`** (OpenRouter `/api/v1/models`). Costs a request at startup.

- **Sampling options** (`temperature`, `max_tokens`), mainly for local models.

- **Progress while tool-call arguments stream**, e.g. a character count during a long `write`. Today only `[waiting Ns]` shows.

- **Optional reasoning display.** Reasoning shows only as `[thinking Ns]`; `--stream-json` carries the text. A flag could print it dim in the REPL.

- **1-hour Anthropic cache TTL.** The default 5-minute cache expires during long REPL pauses; 1-hour writes cost more.
