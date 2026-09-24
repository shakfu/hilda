# TODO

## Critical

## High

- [ ] **Live check of the context-budget fix.** Unit-tested only. Re-run the long case from `/tmp/hilda-exp2/run.sh` (Haiku 4.5, `--context-budget 6000`). Before the fix it hit `--max-turns` after 20 calls, with re-reads and reads of files that do not exist; it should now finish.

- [ ] **First CI run.** `.github/workflows/ci.yml` was checked locally only: the YAML, the plan file behind the cache key, and the Haddock coverage check. Confirm the first push goes green, and that the second run restores the cabal store from the cache.

## Medium

## Low

- [ ] **Upstream the isocline fix.** Send `vendor/isocline.patch` (the `tty_esc.c` part) to daanx/isocline. Delete `vendor/` and depend on Hackage once a release parses `ESC [ 27 ; mods ; key ~`.

- [ ] **Context window from local servers.** The default budget uses OpenRouter only; local 8k models still need `--context-budget`. Candidates: vLLM's `max_model_len` in `/v1/models`, llama.cpp's `/props`, Ollama's `/api/show` (names from memory; unverified).

- [ ] **Harder `--keep-reasoning` benchmark.** A 7-fact lookup scored 7/7 with and without the flag on all three models, so it cannot show a quality difference. Use a task where models fail sometimes, with more than 2 runs each.

- [ ] **Allow-list for `ask` mode** (e.g. `--allow 'git status*'`). Matching shell commands by pattern is unsafe (`git status; rm -rf ~`), so this needs real command parsing or a narrow design.

- [ ] **Sampling options** (`temperature`, `max_tokens`), mainly for local models.

- [ ] **Progress while tool-call arguments stream**, e.g. a character count during a long `write`. Today only `[waiting Ns]` shows.

- [ ] **Optional reasoning display.** Reasoning shows only as `[thinking Ns]`; `--stream-json` carries the text. A flag could print it dim in the REPL.

- [ ] **1-hour Anthropic cache TTL.** The default 5-minute cache expires during long REPL pauses; 1-hour writes cost more.

- [ ] **`--continue` with `-p`.** Rejected today. Useful for scripted multi-step runs; needs headless runs to save sessions too.

## Watch

Risks not observed in use. Fix when one is.

- [ ] **Truncated streamed error.** A streamed OpenRouter error was recorded ending at "https:"; the non-streamed replay had the full text. Capture the raw SSE of a failing stream to find out whether OpenRouter or hilda cuts it.

- [ ] **CRLF files in `edit`.** `read` keeps `\r` on CRLF lines, so a multi-line `old_string` written without `\r` may fail with "not found". Not observed yet; fix when it is.

- [ ] **Empty assistant replies.** A reply with no text and no calls is sent back as `content: ""`, which some providers may reject. Not observed.

- [ ] **Multi-line SSE `data:` fields.** Each `data:` line is parsed alone; the SSE spec joins them into one event. OpenAI and OpenRouter send one line per event.

- [ ] **`atomicWrite` durability.** No fsync, so power loss can still truncate a file. Replacing the inode also splits hard links and changes the owner.

- [ ] **Live check of the Gemini reasoning resend.** Unit-tested with the observed error strings, but no live run has rerouted mid-turn (0 of 3 runs). If reroutes prove common, add `--provider-only SLUG` (OpenRouter `provider.only`) to pin the upstream instead.

- [ ] **Stale `contexts.json`.** Cached context windows never expire. Windows rarely change; delete the file to refresh. Add an expiry only if a stale window causes a failure.
