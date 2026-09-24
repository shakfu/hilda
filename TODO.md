# TODO

## Critical

## High

- **Measure whether `--keep-reasoning` helps, then choose its default.** Live run (2026-09-24): omitting the blocks failed on neither `google/gemini-3.8-flash` nor `openai/gpt-6-luna-pro`, and the OpenAI model returned none. So the flag's benefit is unmeasured. Compare turns, tokens and answer quality on a longer task with and without it. Anthropic models reason only when the request sets `reasoning`, which hilda never sends. See [reasoning tokens](https://openrouter.ai/docs/use-cases/reasoning-tokens).

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
