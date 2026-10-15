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
Completions and OpenAI Responses clients against existing Chat Completions
upstreams. It does not add Claude, Gemini, or native Responses upstream transports.

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

Protocol field mappings belong in Connect, not duplicated in provider classes.
A new upstream protocol still requires a ShenYu transport/provider integration:
a library codec alone does not define its endpoint or authentication contract.

## Exchange lifecycle

1. ShenYu resolves global/selector configuration, proxy credentials and fallback.
2. The protocol decoder validates the envelope and retains the original payload.
3. The provider applies gateway overrides, then `ConnectProtocolAdapter` converts
   to Chat using a request-scoped `ConversionContext`. The configured upstream
   model is authoritative. The provider profile runs once after conversion.
4. ShenYu constructs the HTTP endpoint and authorization headers and sends the
   request through its existing reactive transport.
5. The request-bound response adapter uses the same context to convert successful
   Chat responses back to Responses, including custom-tool restoration. HTTP
   errors and Chat client responses remain passthrough.
6. For streams, ShenYu incrementally frames upstream SSE; Connect consumes JSON
   chunks; ShenYu emits `event: <type>` and `data: <json>` Responses SSE frames.
   Stream state is allocated per subscription, including retries. Finalization
   runs only on normal completion, never cancellation or transport failure.

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

The upstream receives Chat `messages`, the configured routed model and
`max_completion_tokens` at `/v1/chat/completions`. The client receives Responses
events, including usage when supplied upstream. An explicit client `stream`
setting takes precedence over the configured default. Existing `maxTokens` and
`temperature` gateway overrides still apply before conversion.

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
Responses clients continue to use conversion to a Chat upstream; these declarations
do not enable native Responses endpoints or other HTTP operations.

`AiProviderCapabilities` declares streaming, function/custom tools, JSON mode, JSON
schema output, stream usage and audio output. The default `supports()` follows the
declared protocols. The engine validates decoded protocol, operation and effective
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
otherwise. `withMaxTokensField("max_tokens")` selects a legacy upstream dialect through
Connect's provider adaptation phase, after Responses-to-Chat conversion. DeepSeek uses
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
            AiProviderCapabilities.chat(), null, "max_tokens");
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
- Usage conversion is not billing. Provider charges and gateway quotas must not
  be inferred from synthetic or missing usage values.

No database/configuration migration is required. Existing Chat routes need no
change except for OpenAI token-field normalization described above. Responses routes
opt in through the selector handle; DeepSeek routes should review the capability and
dialect validation. Roll back by reverting the
integration commit and restoring any opt-in selectors to `openai-chat`.
