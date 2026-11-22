# TODO

## Critical

## High

- [ ] **Live check of the context-budget fix.** Unit-tested only. Re-run the long case from `/tmp/hilda-exp2/run.sh` (Haiku 4.5, `--context-budget 6000`). Before the fix it hit `--max-turns` after 20 calls, with re-reads and reads of files that do not exist; it should now finish.

- [ ] **First CI run.** `.github/workflows/ci.yml` was checked locally only: the YAML, the plan file behind the cache key, and the Haddock coverage check. Confirm the first push goes green, and that the second run restores the cabal store from the cache.

## Medium

- [ ] **`-r`: live runs on Gemini and OpenRouter.** Only OpenAI and Anthropic are measured (`docs/dev/recursion.md`, Measured). Run `scripts/live_recurse.py gemini openrouter --task sum --budget-tokens 50000` first: Gemini's OpenAI-compatible endpoint may reject `stream_options` or the tool format. Then `--task large --budget-tokens 400000`. The default model ids for both are unverified.

- [ ] **The context budget undercounts code by about 1.4x.** Under `--context-budget 8000`, calls averaged 12,570 prompt tokens: numeric code ran at about 2.8 characters per token against the estimate's 4, and tool specs are not counted (`docs/dev/recursion.md`, Measured). This affects hilda without `-r`, most on local models with 8k to 32k windows. Count the tool specs; consider 3 characters per token, or calibrate from each reply's reported `prompt_tokens`.

- [ ] **`-r`: approve a sub-agent's call in the REPL by hand.** In `-M ask`, the confirmation prompt then runs on a sub-agent's thread while the main thread waits in `mapConcurrently`. Check that y/n works and that Ctrl-C declines the call rather than hanging, since `throwTo` waits while the target is in a foreign call.

## Low

- [ ] **Prompt caching for Claude through `-P openai`.** Opus through `https://api.anthropic.com/v1` reported 0 cached tokens; 139,893 tokens on one `large` run were billed uncached. hilda adds `cache_control` only for `anthropic/` models on OpenRouter. Find out whether Anthropic's OpenAI-compatible endpoint caches at all; if not, document OpenRouter as the route for Claude.

- [ ] **Reasoning with tools on OpenAI's own API.** `gpt-6-luna` on `/v1/chat/completions` refuses function tools unless `reasoning_effort` is `none`, so it needs `--reasoning none` there. Reasoning with tools needs `/v1/responses`, a second wire format. OpenRouter is the workaround.

- [ ] **`-r`: a grant takes the whole remainder.** Under a budget below about one 32,000-token grant per running member, requests wait for each other. Measured effect is small (`broad` time ratio 2.64 at a 100,000 budget, 2.06 at 300,000). If it matters, grant `max(4096, min(ceiling, remaining / running members))`; the risk is cutting off a long reply.

- [ ] **`-r`: ACP `usage_update` cost counts only the top agent.** Sub-agent spending shows in REPL and headless usage lines, not in ACP. Add the family's usage to the session's cost.

- [ ] **`-r`: a cancel, or an error other than a 4xx, costs its full hold.** Its real usage is unknown. Settle with partial usage where the stream reported some before the failure.

- [ ] **`-r`: sub-agent transcripts are not saved.** Only the top agent's history is. Save each sub-agent under its label if a failed delegation needs debugging after the fact.

- [ ] **Upstream the isocline fix.** Send `vendor/isocline.patch` (the `tty_esc.c` part) to daanx/isocline. Delete `vendor/` and depend on Hackage once a release parses `ESC [ 27 ; mods ; key ~`.

- [ ] **Context window from local servers.** The default budget uses OpenRouter only; local 8k models still need `--context-budget`. Candidates: vLLM's `max_model_len` in `/v1/models`, llama.cpp's `/props`, Ollama's `/api/show` (names from memory; unverified).

- [ ] **Harder `--keep-reasoning` benchmark.** A 7-fact lookup scored 7/7 with and without the flag on all three models, so it cannot show a quality difference. Use a task where models fail sometimes, with more than 2 runs each.

- [ ] **Allow-list for `ask` mode** (e.g. `--allow 'git status*'`). Matching shell commands by pattern is unsafe (`git status; rm -rf ~`), so this needs real command parsing or a narrow design.

- [ ] **Sampling options** (`temperature`, `max_tokens`), mainly for local models.

- [ ] **Progress while tool-call arguments stream**, e.g. a character count during a long `write`. Today only `[waiting Ns]` shows.

- [ ] **Optional reasoning display.** Reasoning shows only as `[thinking Ns]`; `--stream-json` carries the text. A flag could print it dim in the REPL.

- [ ] **1-hour Anthropic cache TTL.** The default 5-minute cache expires during long REPL pauses; 1-hour writes cost more.

- [ ] **`--continue` with `-p`.** Rejected today; `--resume ID -p` covers scripted multi-step runs. `-c -p` would need to pick the newest id session for the directory.

## Watch

Risks not observed in use. Fix when one is.

- [ ] **`-r`: the output cap on local servers.** Inside a family each request carries `max_tokens` (or `max_completion_tokens`, once a server asks for it). Ollama and llama.cpp are untested.

- [ ] **Truncated streamed error.** A streamed OpenRouter error was recorded ending at "https:"; the non-streamed replay had the full text. Capture the raw SSE of a failing stream to find out whether OpenRouter or hilda cuts it.

- [ ] **CRLF files in `edit`.** `read` keeps `\r` on CRLF lines, so a multi-line `old_string` written without `\r` may fail with "not found". Not observed yet; fix when it is.

- [ ] **Empty assistant replies.** A reply with no text and no calls is sent back as `content: ""`, which some providers may reject. Not observed.

- [ ] **Multi-line SSE `data:` fields.** Each `data:` line is parsed alone; the SSE spec joins them into one event. OpenAI and OpenRouter send one line per event.

- [ ] **`atomicWrite` durability.** No fsync, so power loss can still truncate a file. Replacing the inode also splits hard links and changes the owner.

- [ ] **Live check of the Gemini reasoning resend.** Unit-tested with the observed error strings, but no live run has rerouted mid-turn (0 of 3 runs). If reroutes prove common, add `--provider-only SLUG` (OpenRouter `provider.only`) to pin the upstream instead.

- [ ] **Stale `contexts.json`.** Cached context windows never expire. Windows rarely change; delete the file to refresh. Add an expiry only if a stale window causes a failure.
