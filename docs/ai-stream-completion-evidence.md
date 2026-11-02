<!--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements. See the NOTICE file
distributed with this work for additional information
regarding copyright ownership. The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the
specific language governing permissions and limitations
under the License.
-->

# AI stream completion evidence

The default OpenAI Chat behavior remains compatible: at clean transport EOF the
incremental encoder may append `data: [DONE]`. An empty stream can therefore have
valid output framing without any evidence that a model completed a response.
The #8123 change makes those two facts independent.

Set `shenyu.ai.proxy.stream-completion.mode=STRICT` to require a complete terminal
marker in the Chat encoder's input. Missing markers, including empty streams and
streams containing a finish_reason, end with a PROTOCOL failure and no synthetic
marker. COMPATIBILITY is the default. Unknown mode names fail startup binding.
The existing public OpenAiChat() constructor retains compatibility mode; an
explicit mode constructor supports non-Spring embedding.

Both modes keep incremental byte-boundary/UTF-8/CRLF framing, current-event size
limits, cancellation and backpressure. A valid terminal marker cancels an upstream
that remains open. Strict mode does not hold back the whole response: valid
earlier events may already be visible when a missing marker is detected. The
existing owner propagates a post-commit failure; it never replaces an SSE body,
replays the call or falls back after output. A missing marker is a protocol
failure, not an invented transient network failure, even before any output.

## Independent body-free facts

Existing AiAttemptEvent and AiTerminalEvent now carry AiStreamCompletion.
The old public constructors remain, supplying NOT_INSPECTED. The logical event
carries only the last actual attempt's snapshot, as with usage. No attempt list,
payload, Throwable, arbitrary finish_reason value or new exporter is retained.

The raw upstream usage tap records completion evidence before provider/protocol
conversion, using its existing bounded parser and subscription. No second body
subscription or full-body copy is added:

| Evidence | Meaning |
| --- | --- |
| NOT_INSPECTED | Not an adopted successful native Chat stream, or no body reached inspection |
| PENDING | Inspection started, no terminal fact yet |
| UPSTREAM_MARKER | A fully framed raw upstream [DONE] was observed |
| FINISH_REASON | At least one raw choice supplied a nonblank finish_reason; weaker than a terminal marker |
| UNKNOWN | Clean EOF without stronger evidence, or bounded inspection was unavailable/exhausted |
| INCOMPLETE | EOF left a partial SSE line/event; compatibility may still flush a valid JSON payload |
| TRANSPORT_FAILURE | The raw body signalled an error |
| SOURCE_CANCELLED | The source was cancelled without an already-observed terminal marker |

FINISH_REASON does not prove all requested choices finished. An omitted marker
does not prove truncation either. Content correctness, refusals, output schema,
tool business success and every requested choice's completion remain separate
contracts.

The Chat encoder separately records NONE, PASSTHROUGH_MARKER or
SYNTHESIZED_MARKER framing. NOT_INSPECTED means that encoder was not used.
Converted output and gateway-generated framing never upgrade raw evidence.
The raw tap inspects successful native Chat SSE only; other native protocols
retain NOT_INSPECTED until their own explicit adoption. Responses-to-Chat
conversion can contribute raw Chat evidence, but its client Responses framing is
outside this Chat encoder policy. Strict mode is not advertised as universal
validation of every provider/converter protocol.

AiCallOutcome.SUCCESS retains its transport/HTTP completion meaning for existing
metrics. It is **not** a model-success or business-success assertion. Consumers
must inspect completion evidence independently. The existing attempt-reason
listener receives STREAM_COMPLETION_UNCONFIRMED for a completed streaming owner
without a confirmed raw terminal marker. Usage remains explicit provider-reported
data: a synthetic marker never invents tokens, costs, or complete usage coverage.

The evidence tap retains its existing 64 KiB event budget; the Chat framer retains
its existing 5 MiB current-event budget. Exhausting evidence coverage yields
UNKNOWN even if the downstream framer later sees a marker. Updates contain only
two closed enum values. State is subscription-local and discarded with the
existing request/attempt owner. No model call, new telemetry queue or on-path
authorization service is introduced.

## Verification

AiStreamCompletionTest exercises the real engine, raw tap, protocol encoder,
retry/fallback owner and observer for empty/unknown/finish-reason/partial/valid
streams, strict rejection, split UTF-8/CRLF, an upstream left open after [DONE],
transport errors, malformed EOF, cancellation, conversion, evidence-budget
exhaustion and concurrent resubscriptions. Startup tests prove default/strict
binding and reject an unknown mode. Existing protocol byte/framing, usage,
retry, HTTP compatibility and plugin tests characterize compatibility behavior.

Reproduce with the Maven wrapper:
```text
./mvnw -pl shenyu-spring-boot-starter/shenyu-spring-boot-starter-plugin/shenyu-spring-boot-starter-plugin-ai-proxy -am verify -Dtest=AiStreamCompletionTest,OpenAiChatResponseTest,AiUpstreamUsageTapTest,AiProxyObservationTest,AiProxyPluginConfigurationTest -Dsurefire.failIfNoSpecifiedTests=false
```
