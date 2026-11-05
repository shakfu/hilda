# Native Anthropic provider

Status: proposed, not decided. Written 2026-11-05. The questions at the end block the decision.

Sizes are estimates from reading the code, not measurements. Claims about the Anthropic API come from its docs, read from memory. Items marked "verify" need a live check.

## Current state

`ProviderKind` (`src/Hilda/Provider.hs`) has two kinds: `OpenAICompatible` and `OpenRouter`. Anthropic models are reachable only through OpenRouter as `anthropic/...`. That path gets prompt caching and `usage.cost`.

## Reusable without change

- `Complete` (`src/Hilda/Types.hs`). The agent loop depends only on this type, so `Hilda.Agent` does not change.
- Transport: `send`, `receive`, `readIdle`, `readBody`. Retries, idle timeout and key-free error text do not depend on the provider. They need the URL path and headers as parameters.
- `sseData`. It reads only `data:` lines. Anthropic repeats the event type as `type` in the JSON payload.
- `providerError`. Anthropic errors have the shape `{"type":"error","error":{"message":...}}`.
- `Message` and `ToolCall`. They stay the internal and on-disk format. The Anthropic encoder translates them.

## Wire differences

References: [Messages API](https://docs.claude.com/en/api/messages), [streaming](https://docs.claude.com/en/docs/build-with-claude/streaming).

| Concern | Chat completions | Messages API |
|-|-|-|
| Endpoint, auth | `/chat/completions`, `Authorization: Bearer` | `/v1/messages`, `x-api-key`, `anthropic-version: 2023-06-01` |
| System prompt | `System` message | top-level `system` |
| `max_tokens` | optional | required |
| Tool calls | `tool_calls[].function.arguments`, JSON text | `tool_use` block, `input` is an object |
| Tool results | one `role: tool` message each | `tool_result` blocks in one `user` message; merge consecutive results |
| Tool specs | `{type:function, function:{name, description, parameters}}` | `{name, description, input_schema}` |
| Stream events | `choices[0].delta`, ends with `[DONE]` | `message_start`, `content_block_start/delta/stop`, `message_delta`, `message_stop`, `ping`, `error` |
| Argument stream | `function.arguments` pieces by `index` | `input_json_delta.partial_json` by block `index` |
| Usage | `prompt_tokens` includes cached tokens | `input_tokens` excludes them; `usagePrompt` is the sum of `input_tokens`, `cache_read_input_tokens` and `cache_creation_input_tokens` |
| Cost | `usage.cost` on OpenRouter | not reported |
| Caching | top-level `cache_control`; OpenRouter places it | `cache_control` per block, at most 4 breakpoints |
| Reasoning | `reasoning.effort` or `reasoning_effort` | `thinking` blocks, `signature_delta`, `redacted_thinking`; request shape varies by model generation |

## Work items

1. **Codec record** (about 40 lines). `readStream` calls `stepChunk` and `finishPartial` directly. `newCompleteWith` hardcodes `/chat/completions`. Extract a record holding the path, headers, encoder, stream step and non-stream decoder. This refactor changes no behaviour and can land on its own.
2. **`Hilda.Provider.Anthropic`** (200-300 lines):
   - request encoder for the table above;
   - event fold into `Partial`;
   - non-stream decoder.

   A `callArgs` that is not a JSON object needs a rule: send `{}`, or fail. Saved sessions can hold such arguments.
3. **`ProviderKind`**:
   - add `Anthropic`;
   - key variable `ANTHROPIC_API_KEY`, required;
   - base URL from `ANTHROPIC_BASE_URL`;
   - extend the default order in `resolveProvider` (`src/Hilda/Cli.hs`).
4. **Retry on 529 `overloaded_error`.** The server did not run the request, so it fits the rule in `retryableStatus`.
5. **Tests** (about 200 lines). Cover these with SSE fixtures:
   - plain text;
   - a tool call split across `partial_json` pieces;
   - thinking with a signature;
   - a mid-stream `error` event;
   - usage summing.

   Add one `FakeServer` round trip.
6. **Docs.** Update the README provider table and add a CHANGELOG entry. Extend item 1 of `external-ideas.md` (key out of `bash`) to `ANTHROPIC_API_KEY`.

Total: 450-650 lines including tests.

## Failure modes

- **Thinking with tool use.** With thinking enabled, the thinking blocks of the current tool loop must be sent back unchanged ([extended thinking](https://docs.claude.com/en/docs/build-with-claude/extended-thinking#preserving-thinking-blocks)). By default hilda wraps the client in `withoutReasoning` (`src/Hilda/Cli.hs`), which drops every block. So with `--reasoning`, the Anthropic provider must keep at least the current loop's blocks, whatever `--keep-reasoning` says. Verify.
- **Stored reasoning format.** `Assistant`'s `[Value]` holds OpenRouter `reasoning_details` blocks. Two options:
  - Convert to and from OpenRouter's `reasoning.text` shape (`signature`, `format: anthropic-claude-v1`). Sessions then stay portable between providers.
  - Store raw Anthropic blocks. This is simpler, but session files then depend on the provider.
- **Effort mapping.** hilda's 7 efforts must map onto Anthropic's thinking controls. Older models take a token budget (`budget_tokens`). Newer ones take an adaptive mode or an effort value. Verify the shape for each target model.
- **`--max-cost`.** Anthropic reports no cost, so the flag only warns, as with `openai`. Making it work needs a price table, which goes stale.
- **Context budget.** `knownContext` looks up only OpenRouter, so the default stays 100,000. Whether `/v1/models` reports a context window is unverified.
- **Cache placement.** hilda must place the breakpoints itself: system prompt, tools, and the last block of the newest message. That is 3 of the 4 allowed. This also makes the 1-hour TTL possible (`TODO.md`).

## Alternatives

- **Compatibility endpoint.** Use `-P openai --base-url https://api.anthropic.com/v1/ --api-key-env ANTHROPIC_API_KEY` ([docs](https://docs.claude.com/en/api/openai-sdk)). This needs no code. It loses caching and cost, and probably thinking. One live run would test it. Untested.
- **Status quo.** OpenRouter already gives caching and cost. A native provider adds:
  - the 1-hour cache TTL;
  - direct control of thinking;
  - no OpenRouter fee.

  It costs a second stream format to maintain, with no SDK.

## Open questions

1. Is `--max-cost` required on this provider? If yes, it needs a price table.
2. Must sessions move between providers? The answer decides the stored reasoning format.
3. Which model generations are targeted? The answer decides the effort mapping.
4. When both `ANTHROPIC_API_KEY` and `OPENROUTER_API_KEY` are set, which provider is the default?

## Suggested first step

Land the codec refactor (work item 1) first. `make test` checks it alone, and it is useful whatever the decision.
