# shenyu-plugin-ai-prompt

The prompt plugin injects fixed prompt items into AI protocol request bodies before
they are forwarded to the model provider: a prepend item
(`{"role": preRole, "content": prepend}`) at the head of the conversation and an append
item (`{"role": postRole, "content": append}`) at its tail. A prepend item is injected
only when both `prepend` and `preRole` are configured, an append item only when both
`append` and `postRole` are configured. It is aimed at AI gateways that must enforce a
system prompt or a suffix instruction on every matched request.

## How it works

- The plugin resolves the request protocol once through the shared AI protocol detector
  (request path, then header hints, then body shape, only when the stronger levels are
  inconclusive) and injects per protocol:
  - `openai-chat`: the items are injected at the head and tail of `messages`. A request
    without a non empty `messages` array is forwarded unchanged.
  - `openai-responses`: the items are injected at the head and tail of the `input`
    items. A **string** `input` is first converted to the equivalent items array
    (`[{"role": "user", "content": <string>}]`), then the configured items are injected,
    and the resulting array replaces `input` before the request is forwarded. An
    `input` that is missing or neither a string nor an array is forwarded unchanged.
- A recognizable request path takes precedence over the body shape: for example a
  messages shaped body sent to `/v1/responses` is dispatched as `openai-responses`
  and, having no `input`, is forwarded unchanged (no injection, no error).
- When the protocol stays unknown, the body is forwarded unchanged.
- Injection only adds the configured items; every other top level field of the request
  body is preserved and re-serialized with the rest of the payload.
- The plugin publishes the bounded wire view of the request body once and, when the
  decoration really changes the body, publishes the effective view with a bumped
  generation, so consumers of the normalized request view observe the injected items.

## Configuration

Plugin level (`config` of the plugin):

| field | type | default | description |
| --- | --- | --- | --- |
| `prepend` | string | null | content of the item injected before the conversation |
| `preRole` | string | null | role of the injected prepend item |
| `append` | string | null | content of the item injected after the conversation |
| `postRole` | string | null | role of the injected append item |

Rule level (`handle` of the rule): the same four fields; a non null handle replaces the
plugin level values for that rule.

Example:

```json
{
  "prepend": "Answer only in English.",
  "preRole": "system",
  "append": "Cite your sources.",
  "postRole": "system"
}
```
