# Recursive agents

`-r`/`--recurse` lets an agent delegate tasks to sub-agents and collect their answers: local fork/join, in `src/Hilda/Family.hs`. Off by default; without `-r`, nothing below runs. User-facing summary: README, "Recursion".

minima's process-based design (`~/projects/personal/minima/docs/dev/recursion.md`) was the starting point. This design keeps its budget numbers, so the two can be compared, and runs children as threads instead.

## Model

- A **family** is the top agent and every sub-agent it starts, transitively.
- A sub-agent is `runTurn` in a green thread of the same process, with a fresh history: `[System prompt, User task]`.
- The model reaches it through a fifth tool, `agent {task}`. The call waits for the sub-agent's final answer, which comes back as the tool result. A sub-agent's text never carries the user's authority.
- `agent` calls next to each other in one reply run concurrently (`mapConcurrently`). Other calls run one at a time, since a later call may depend on an earlier one.
- Sub-agents inherit the parent's mode, model, system prompt, `--max-turns` and context budget. They get the `agent` tool too, down to `--max-depth`.
- The top agent fixes a token budget for the whole family. Every model call reserves against it first.

## Why threads, not processes

A direct port of minima's design needs a registry, a ledger, file locking, a parent-pid watch and stop files. It also breaks hilda's permission modes, because minima's agents reach the family through `bash`:

- **`read-only`:** `bash` is hidden, so a read-only agent could not delegate at all.
- **`ask`:** the user confirms one `bash` call, `hilda -r spawn ...`. The child then runs headless with stdin closed. It either cannot confirm anything, or it runs in `yolo`.

With threads, a sub-agent inherits the mode, and its confirmations go to the parent's `confirm`.

Costs, compared with minima:

- No `hilda --resume <child>`: sub-agent transcripts are not saved.
- No `agents` or `shutdown` from another terminal.
- No process isolation.
- The parent waits for its `agent` calls. It overlaps children only by sending several in one reply.

## Permissions

`Delegate` is a fourth `Effect`. `authorize _ Delegate = Allow` in every mode: the sub-agent runs in the same mode, so each of its own calls is checked. So `read-only` offers `read` and `agent`, and `ask` does not confirm the delegation itself.

The top agent's hooks sit behind one lock (`famLock`). An event prints whole, and one confirmation is asked at a time. While a confirmation waits, other sub-agents' events wait too.

## Call ids

Every sub-agent conversation reuses ids such as `call_0`. Wherever the parent sees a sub-agent's call (events and confirmations), its id is prefixed with the label and the sub-agent's reply number: `agent-1.2/call_0`. Labels count up per family. The sub-agent's own history keeps the model's ids. So ACP can announce sub-agent calls as ordinary `tool_call` updates without collisions.

## Events

`Sub label event` wraps a sub-agent's event; a grandchild's is wrapped twice.

| Front end | Shows |
|-|-|
| Text, REPL | One line per finished call: `  [agent-1] [bash] git status -> ~40`. The innermost label is shown. `agent` calls print as whole lines, since sub-agent lines come between their start and end |
| `--stream-json` | The event's usual object, with `agent` set to the innermost label |
| `--acp` | Sub-agent calls as `tool_call` and `tool_call_update`; sub-agent text is not sent |

Sub-agents do not stream text. Their backend is the family's budgeted one, without the display wrappers.

## Budget

`budgeted` wraps the backend (`Complete`), so `runTurn` has no budget code. The top agent uses the same wrapper.

| | |
|-|-|
| input | `historyTokens` of the messages, plus the tool specs at 4 characters per token |
| line | the budget less a 5% reserve |
| ceiling | 32,000 output tokens; model output limits are not looked up |
| floor | 4,096 |

One STM transaction reads settled `spent` and in-flight `held`:

| Outcome | When | Then |
|-|-|-|
| Fails | `spent + input + floor > line` | `Left "the family token budget is spent ..."`; the turn ends `Failed` |
| Waits | only `held` leaves less than the floor | STM `retry`: blocks until a hold settles |
| Granted | otherwise | `grant = min ceiling (line - spent - held - input)`; holds `input + grant` |

The request goes out with `max_tokens = grant`. After it:

- A reply with usage settles at prompt plus completion tokens.
- A reply without usage (local servers) settles at the input plus an estimate of the reply. Settling at the full hold would use up the budget about 15 times too fast at a 32,000 grant.
- A 4xx response settles at 0: the server refused the request before generating.
- Another error, or a cancel, settles at the full hold, since whether the provider billed it is unknown.

With `-r`, `--max-cost` counts the whole family: the wrapper refuses a call once the family's reported cost reaches it. Sub-agents get no `envCostLimit` of their own.

A failed sub-agent call reaches the parent as `error: agent-N failed: ...`. The parent's next call fails the same check, so the family stops top-down.

## Cancellation

`mapConcurrently` cancels its threads when the parent's thread gets an async exception: REPL Ctrl-C, ACP `session/cancel`, or a kill. A sub-agent's `bash` gets the same exception, and the existing handler kills its process group. A single `agent` call runs in the parent's own thread, so the exception reaches it directly. `FamilySpec` covers the single-call case.

## Tests

| File | Covers |
|-|-|
| `test/FamilySpec.hs` | answers; concurrency (a barrier that deadlocks if calls run in sequence); read-only and depth tools; ask confirmations, serialized, with tagged ids; failures; event labels; family `--max-cost`; cancellation; grants as `max_tokens`; failing, waiting, settling and cancelled holds |
| `test/PolicySpec.hs` | `Delegate` allowed in every mode |
| `hilda/test/RenderSpec.hs` | `agent` and `Sub` lines |
| `test/ProviderSpec.hs` | `max_tokens` only when capped |
| `hilda/test/CliSpec.hs` | `-r`, `--budget-tokens` and `--max-depth` validation |
| `hilda/test/HeadlessSpec.hs` | a `-r -p` run against `FakeServer`, with the family's spending in the result |

## Limits

- **The output cap field is learned.** hilda sends `max_tokens`. When a server refuses it and names `max_completion_tokens`, as OpenAI does for `gpt-6-luna` (live run, 2026-11-22), the backend resends with that field and keeps using it. Ollama and llama.cpp are untested.
- **A grant takes the whole remainder.** Below 32,000 tokens of room per member, calls run one at a time, as minima measured (3 s of a 7 s run). STM would make an equal-share grant easy; not done until measured.
- **A cancel, or an error other than a 4xx, costs its full hold.**
- **Confirmation from a sub-agent thread in the REPL is untested by hand.** isocline's `readline` then runs off the main thread while it waits in `mapConcurrently`. Ctrl-C at the prompt should read as a decline.
- **A concurrent group reports its results together.** All `CallFinished` events of a group arrive when its last sub-agent ends; progress shows only as `Sub` lines.
- **ACP's session cost counts only the top agent.** The family's cost shows in the REPL and headless usage lines, not in `usage_update`.
- **Only two providers measured.** See [Measured](#measured); Gemini and OpenRouter are untested.
- **Sub-agents cost about 1,300 tokens and two sequential model calls each** before they do any work. `-r` loses on tasks one agent can hold in its context.

## Measured

`scripts/live_recurse.py`, 2026-11-22. OpenAI's `gpt-6-luna` ran with `--reasoning none`: on `/v1/chat/completions` it accepts function tools only at that effort. Anthropic ran through its OpenAI-compatible endpoint, `https://api.anthropic.com/v1`.

| Model | Task | Budget | Tokens | Time | Result |
|-|-|-|-|-|-|
| `gpt-6-luna` | sum, `-r` | 50,000 | 3,893 | 7 s | 42; both `agent` calls in one reply |
| `claude-opus-5-5` | sum, `-r` | 50,000 | 9,333 | 8 s | 42; both `agent` calls in one reply |
| `gpt-6-luna` | broad, one agent | - | 1,867 | 3 s | 4/4 codenames |
| `gpt-6-luna` | broad, `-r` | 100,000 | 7,286 | 9 s | 4/4 codenames |

- The first run found two bugs, since fixed. OpenAI refused `max_tokens` for `gpt-6-luna`; the backend now learns `max_completion_tokens` from the refusal. The refused requests had settled at their full holds, 32,711 tokens for none spent; a 4xx now settles at 0. In the later runs, spent tokens equal reported usage.
- On broad, `-r` cost 3.9 times the tokens and 2.6 times the time, with equal correctness. The four files total about 1 KB, so one agent reads them in a turn. Each sub-agent repeats the system prompt and tool specs, about 1,300 tokens. So broad measures the fixed cost of a sub-agent, not a benefit.
- At `--budget-tokens 300000`, where no child waits for a hold, `-r` took 2.06 times the time instead of 2.64. So the budget explains little of it. The rest is depth: a delegated answer takes four model calls in sequence (top, child read, child answer, top) against two.

`large`, first version (four files of about 10,000 tokens, `--context-budget 8000`), `gpt-6-luna`, `--budget-tokens 400000`:

| | Tokens | Time | Codenames | Reads |
|-|-|-|-|-|
| one agent | 567,509 (460,164 cached) | 57 s | 1/4 | 67 |
| `-r` | 374,219 | 25 s | 2/4 | 42 |

- The task was wrong: the context budget also bound each sub-agent, and one file did not fit in it. The fixture now uses files of about 6,000 tokens and `--context-budget 12000`.
- `read` gave the right continuation offset every time. The model abandoned it after about four pages and probed offsets at random (1860, 1900, 5000): agent-3 never read lines 1435 to 1859 and missed line 1595. The single agent did the same.
- agent-4 probed for 20 reads and then failed on the family budget. The top agent reported only the codenames it had.
- The context budget undercounts code. The single agent's calls averaged 12,570 prompt tokens under a budget of 8,000: numeric code ran at about 2.8 characters per token against the estimate's 4, and tool specs are not counted. This holds for hilda without `-r` too; it matters most on local models with small windows.

`large`, corrected (four files of about 6,000 estimated tokens, three `read` pages each, `--context-budget 12000`), `--budget-tokens 400000`:

| Model | Run | Tokens | Time | Codenames | Reads |
|-|-|-|-|-|-|
| `gpt-6-luna` | one agent | 596,842 | 48 s | 1/4 | 45 |
| `gpt-6-luna` | `-r` | 79,383 | 8 s | 4/4 | 9 |
| `claude-opus-5-5` | one agent | 139,893 | 25 s | 4/4 | 16 |
| `claude-opus-5-5` | `-r` | 94,014 | 15 s | 4/4 | 8 |

- Every sub-agent paged in order and stopped at the page holding its codename: the fewest reads possible.
- One `gpt-6-luna` agent read page 1 of `big_a.py`, which holds the codename, and still answered "no codename line found": the page was elided before the model wrote the codename down. It then probed offsets at random, as in the first version.
- One Opus agent succeeded, with 8 re-reads after elision. It read files in parallel: 8 turns against `gpt-6-luna`'s 30.
- Opus through Anthropic's OpenAI-compatible endpoint reported 0 cached tokens on both runs. hilda marks requests for caching only for `anthropic/` models on OpenRouter; whether that endpoint caches at all is unverified.
- One run per row. The task is synthetic, and `-M read-only` forces paging that `bash` would avoid.

## Evaluation

`-r` pays off when one agent's reading would crowd its context, so the test is `--task large`. Four files of about 10,000 tokens each hide one codename apiece, at 40% to 85% of the file. Both runs use `-M read-only`, so the files must be paged with `read`, and `--context-budget 8000`, so one agent cannot hold more than one file at a time. Each sub-agent needs one file.

Judge `-r` on `large`: correctness first, then tokens and time. `broad` stays as the measure of fixed cost per sub-agent.

On the corrected `large`, `-r` met that bar on both models measured: equal or better correctness, with 0.13 to 0.67 times the tokens and 0.17 to 0.61 times the time. On `broad`, it cost about 4 times the tokens and 2 times the time.
