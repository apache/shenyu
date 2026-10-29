<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements. See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# AI Proxy protocol conversion with Connect

The AI Proxy uses `io.github.agentic-ai-java:connect:0.1.0` for JSON protocol
conversion and explicitly selected provider dialects. ShenYu remains the
reactive gateway, not an agent runtime. This integration supports OpenAI Chat
Completions and OpenAI Responses clients. The built-in OpenAI provider uses the
native `/v1/responses` endpoint; Chat-only providers use protocol conversion.
It does not add native Claude or Gemini upstream transports.

## Responsibility boundary

| Concern | Connect | ShenYu |
| --- | --- | --- |
| Protocol semantics | Convert request/response JSON, messages, tool definitions/calls, reasoning, finish reasons and usage | Validate the HTTP request envelope and select the client protocol |
| Provider dialect | Apply an explicitly selected profile, such as DeepSeek Chat | Resolve provider, routed model and gateway parameter overrides |
| Conversion losses | Produce structured diagnostics and enforce the `SAFE` loss policy | Classify failures and log diagnostic codes without request content or secrets |
| Request context | Retain tool-name/type mappings needed for response restoration | Allocate one context per request and bind it to the matching response |
| Streaming | Convert JSON chunks into protocol events and finalize conversion state | Parse/frame SSE bytes, validate content type, preserve backpressure and cancellation, enforce conversion limits |
| Gateway execution | No networking or gateway policy | Configuration synchronization, selector/rule matching, credentials, endpoints, WebClient transport, HTTP status/headers, retries and fallback |
| Billing and agents | Normalize usage; no charging or tool execution | Billing/quota policy remains a gateway concern; this change adds no billing system or agent/tool execution |

Native request preparation belongs to the target protocol. Connect owns
cross-protocol conversion and explicitly selected provider dialects. A provider
declares each native protocol's endpoint and composable request transformer; a
library codec alone does not define endpoint or authentication contracts.

## Exchange lifecycle

1. ShenYu resolves global/selector configuration, proxy credentials and fallback.
2. The protocol decoder validates the envelope and retains the original payload.
3. ShenYu prefers a provider-native target protocol and otherwise selects a
   converter. The target protocol prepares its outgoing payload after routing.
4. The provider applies gateway overrides and its target-protocol binding. A
   selected Connect provider profile runs once after protocol conversion, then
   ShenYu applies the configured request-body patch for the native target protocol.
5. ShenYu constructs the HTTP endpoint and authorization headers and sends the
   request through its existing reactive transport.
6. For cross-protocol routes, the request-bound response adapter uses the same
   context to convert successful Chat responses back to Responses, including
   custom-tool restoration. Native routes, HTTP errors and Chat client responses
   remain passthrough.
7. For streams, ShenYu incrementally frames upstream SSE; Connect consumes JSON
   chunks; ShenYu emits `event: <type>` and `data: <json>` Responses SSE frames.
   Stream state is allocated per subscription, including retries. Finalization
   runs only on normal completion, never cancellation or transport failure.

## Plugin defaults and selector overrides

AI Proxy accepts defaults in the plugin's existing `config` JSON, synchronized
from Admin. For example:

```json
{
  "provider": "OpenAI",
  "baseUrl": "https://api.openai.com",
  "apiKey": "<upstream-api-key>",
  "model": "gpt-4o-mini",
  "maxTokens": 1024
}
```

A selector can then use `{}` to inherit these connection/model settings, or
supply a partial handle such as `{"model":"another-model"}`. Non-null selector
values take precedence. Existing selector defaults for temperature (`0.8`),
stream (`false`) and protocol (`openai-chat`) retain their current precedence;
set those selector fields explicitly when a route needs different behavior.

Plugin updates replace the defaults rather than retaining omitted fields.
Blank, `null` or `{}` configuration clears the previous connection/model values;
disabling or deleting the plugin also clears its defaults. Each request receives
a copy, so request-specific proxy credentials do not alter later requests.
Malformed plugin configuration clears the defaults and logs a warning without
interrupting configuration synchronization. A later valid update restores them.
Defaults belong to the handler/resolver instance wired by the starter; separate
application contexts do not share plugin configuration through a process-wide
`Singleton`. Programmatic construction must pass the same `AiProxyPluginHandler`
to `new AiProxyConfigService(handler)` and the plugin.

## Configuration and use

Add the existing AI Proxy starter to the gateway. Its configuration registers
both `openai-chat` and `openai-responses` protocol implementations.
Set the selector handle's existing `protocol` field explicitly for a Responses
route; the default remains `openai-chat` for compatibility. For example:

```json
{
  "provider": "OpenAI",
  "baseUrl": "https://api.openai.com",
  "apiKey": "<upstream-api-key>",
  "model": "gpt-4o-mini",
  "protocol": "openai-responses",
  "stream": false
}
```

Route a client request such as the following to that selector:

```json
{
  "model": "client-model-alias",
  "input": "Explain reactive backpressure briefly.",
  "max_output_tokens": 128,
  "stream": true
}
```

The OpenAI upstream receives Responses `input` and `max_output_tokens` at
`/v1/responses`. A Chat-only upstream receives converted `messages` and its
configured token field at its Chat endpoint. An explicit client `stream` setting
takes precedence over the configured default. Existing `maxTokens` and
`temperature` gateway overrides apply to the selected target protocol.

### Per-protocol request-body overrides

Stable, cross-provider options should continue to use typed fields such as
`maxTokens`, `temperature` and `stream`. Provider-specific or experimental fields
can be added through `requestBodyOverrides`, keyed by the native target protocol:

```json
{
  "provider": "OpenAI",
  "protocol": "openai-chat",
  "requestBodyOverrides": {
    "openai-chat": {
      "reasoning_effort": "high",
      "parallel_tool_calls": true,
      "metadata": {
        "tenant": "example"
      }
    },
    "openai-responses": {
      "reasoning": {
        "effort": "medium",
        "summary": "auto"
      }
    }
  },
  "requestBodyForceOverride": false
}
```

The key is the final provider-native protocol, not necessarily the client
protocol. For example, a Responses client routed to the Chat-only DeepSeek
provider uses the `openai-chat` patch after Responses-to-Chat conversion.

Patches run after typed protocol overrides and provider transformation. Plain
JSON objects merge recursively. Arrays, scalars and type mismatches are treated
as a single value. Patches deeper than 16 nested JSON levels are rejected before
merging to protect request processing from unbounded recursion. With
`requestBodyForceOverride` unset or `false`, existing
request or typed-option values win and the patch only fills missing fields. With
it set to `true`, patch values replace existing values. Therefore a forced patch
can intentionally replace the provider field generated from `maxTokens`; use
typed options for ordinary configuration and reserve forced patches for advanced
provider-specific behavior.

Final capability and streaming-consistency validation runs after the patch. A
patch cannot silently enable an unsupported feature or change the streaming mode
already selected by routing and retry policy. Request-body values are not printed
by configuration `toString()` methods; only configured protocol names are shown.

The existing `DeepSeek` provider identifier selects Connect's DeepSeek Chat
profile. Other compatible identifiers (`ALiYun`, `OpenAPI`, `Moonshot`) remain
generic; neither URLs nor model names are used to guess a dialect. DeepSeek's
profile maps token-limit and reasoning/thinking fields and rejects incompatible
thinking/tool configurations rather than silently changing their meaning.

## Provider profiles and capabilities

The explicit Provider registry remains the extension point. Built-in identifiers are
`OpenAI`, `OpenAICompatible`, `DeepSeek`, `ALiYun`, `OpenAPI`, and `Moonshot`; separators
and case are normalized by the registry. Both existing client protocols are supported.
The protocol decoder also declares the request operation (`CHAT_COMPLETION` or
`RESPONSE`), so a profile can restrict operations separately from protocol support.
The built-in OpenAI provider declares both `openai-chat` and `openai-responses`;
compatible and DeepSeek providers remain Chat-only unless explicitly configured
with another native protocol binding.

`AiProviderCapabilities` maps each native protocol to an `AiProviderProtocolBinding`,
which owns that protocol's endpoint path and provider request transformer. The transformer
receives an `AiProviderRequestContext` containing the provider, target protocol, mapped
model, normalized request and resolved configuration. Transformers can be composed, so
field renames, nested provider structures and model-specific behavior stay in the binding
instead of adding one option field per difference. Its binding keys
are the single source for `protocols()` and the default `supports()` implementation; a
provider does not maintain a second protocol-name set. Capabilities also declare streaming,
function/custom tools, JSON mode, JSON schema output, stream usage and audio output.
The engine validates decoded protocol, operation and effective
stream settings before invoking any provider, including custom implementations.
Providers also validate the final converted payload, including provider defaults. Unsupported
features or operations fail as request errors before HTTP execution. Generic capabilities
describe the service, not a guarantee for every model it hosts; configure a narrower
profile for a model or deployment with additional restrictions. Unknown fields that
need no transformation remain in the payload.

Built-in profiles enumerate their supported operations and features explicitly. Adding
an enum value does not enable it for existing profiles; review and opt in each profile.

OpenAI now emits one Chat token-limit field, `max_completion_tokens`. When both token
fields are supplied, the modern field takes precedence; gateway `maxTokens` overrides
it. This follows the [OpenAI Chat API](https://developers.openai.com/api/reference/resources/chat/subresources/completions/methods/create).
Generic compatible profiles preserve the client's field choice unless configured
otherwise. A custom binding can append a transformer with
`withRequestTransformer(...)`; it runs after any selected Connect dialect and after
Responses-to-Chat conversion. DeepSeek uses
its existing Connect profile, including token and thinking mappings. Its Chat capability
profile accepts function tools and JSON mode and rejects JSON-schema output, custom
tools and audio output, following the [DeepSeek Chat API](https://api-docs.deepseek.com/api/create-chat-completion/).

Provider paths may be relative to a base URL, or the base URL may already include the
complete configured operation path. `/v1` is not appended twice. DeepSeek uses
`/chat/completions`, including when the configured base URL ends in `/v1` or `/beta`.
Base URLs retain the existing HTTP(S), no-userinfo/query/fragment restriction; profile
query parameters are encoded separately. Header credentials support arbitrary header
names and prefixes (including Bearer); query credentials and unauthenticated local
services are also available. Generated content-type, accept and credential headers
replace conflicting profile headers case-insensitively.

Register another Chat-compatible service as a Spring bean without changing Engine,
Protocol or Transport. For example:

```java
@Bean
public AiProxyProvider tenantProvider() {
    ObjectNode defaults = JsonNodeFactory.instance.objectNode();
    defaults.put("temperature", 0.7);
    OpenAiProviderOptions options = new OpenAiProviderOptions(
            "/generate", OpenAiProviderOptions.Authentication.HEADER, "api-key", "",
            Map.of("X-Tenant", "tenant-a"), Map.of("api-version", "2026-01"),
            Map.of("client-alias", "upstream-model"), defaults,
            AiProviderCapabilities.chat(), null)
            .withRequestTransformer(OpenAiChat.NAME, (payload, context) -> {
                JsonNode tokens = payload.remove("max_completion_tokens");
                if (Objects.nonNull(tokens)) {
                    payload.set("max_tokens", tokens);
                }
                return payload;
            });
    return new OpenAiCompatible("tenant-provider", options);
}
```

Set the selector's `provider` to `tenant-provider`, its `baseUrl` to the service base
URL and its `apiKey` to the credential. The profile also works with a collection-based
`AiProxyProviderFactory` without Spring. Existing custom implementations keep their
interface contract and the `OpenAi(ProviderProfile)` constructor remains available.

Model precedence is configured routed model, then client model, then profile default.
Alias mapping applies to the selected model. Defaults fill only absent fields; explicit
client values survive unless a gateway override is configured. Defaults cannot replace
`messages`, `input`, `stream` or internal fallback configuration. Streaming selection
therefore remains consistent with Engine retry/response policy. A Connect dialect
that changes the selected streaming mode is rejected before HTTP execution. Profile maps and defaults
are copied and per-request changes do not mutate the registered profile. Profile string
representations omit headers, query values and body defaults.

Provider response differences remain in the request-bound Connect response adapter or
the existing `AiProxyProvider.adaptResponse` hook. DeepSeek Chat response fields, including
`reasoning_content` and vendor usage fields, remain passthrough for Chat clients; Connect
converts reasoning and usage for Responses clients. Providers do not open connections,
configure pools/timeouts, write client responses, or implement retry/fallback policy.

## Compatibility and limits

- Existing Chat routes retain their protocol identifier, upstream endpoint,
  credential handling, response bytes and SSE framing. Requests remain byte-for-byte
  unchanged when no gateway override, stream-usage addition or provider adaptation
  changes the payload; generic extension fields are retained on identity conversion.
- Conversion uses Connect's `SAFE` policy. Error-level losses, including Responses
  conversation state that cannot be represented by Chat, fail with HTTP 400 before
  sending a request. Warning-level conversions are permitted and logged by code.
  This is not a stateful Responses API service or hosted-tool implementation.
- Complete Responses conversion buffers at most 5 MiB. Streaming Responses
  conversion accepts at most 5 MiB of cumulative framed upstream data because the
  codec retains output/tool state. Existing Chat passthrough has no new cumulative
  response limit. Oversized conversions fail; after commitment the stream aborts.
- Successful conversion removes stale length/digest/ETag headers. Upstream HTTP
  errors retain their original status/body. Conversion errors are not retried;
  ShenYu's existing transport/status retry and fallback rules remain in force.
  No retry or fallback occurs after a converted response event reaches the client.
- Retry replay is bounded by a shared request deadline and configurable by failure
  stage, operation, and provider replay-safety declaration. By default ambiguous
  transport failures—including post-execution stream idle and response-size
  failures—are replayed only for requests without tool-bearing fields. Fallback
  uses the same replay policy as another upstream execution; an explicit `NEVER`
  policy blocks fallback as well as retry.
  `Retry-After` (delay-seconds or HTTP date), capped exponential backoff, and
  bounded jitter are applied together; an over-limit Retry-After stops replay
  rather than retrying earlier than the upstream allows. Primary retries and
  fallback share the deadline and attempt limits. Attempt events expose attempt
  identity, phase, reason, and the bounded Retry-After fact to an optional
  listener bean.
- Usage conversion is not billing. Provider charges and gateway quotas must not
  be inferred from synthetic or missing usage values.

No database/configuration migration is required. Existing Chat routes need no
change except for OpenAI token-field normalization described above. Responses routes
opt in through the selector handle; DeepSeek routes should review the capability and
dialect validation. Roll back by reverting the
integration commit and restoring any opt-in selectors to `openai-chat`.
