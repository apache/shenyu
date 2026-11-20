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

# HTTP-facing gateway failure policies

The initial adoption of #8210 covers escaped exceptions in GlobalErrorHandler and
WebFluxResultUtils.noSelectorResult/noRuleResult. Register optional Spring
GatewayFailureMapper beans; ErrorHandlerConfiguration prepares their ordered list
once. Lower Ordered values run first; equal order retains registration order.
Optional.empty() tries the next policy. The first result wins. A null result,
invalid presentation or RuntimeException returns to the legacy owner immediately,
without trying another policy or recursively handling the policy failure. At most
32 policies are accepted at startup.

A mapper receives immutable GatewayFailure facts: closed reason, actual owner,
HTTP/HTTP-bridge/unknown context, output state and a bounded local plugin identity.
It receives no exchange, headers, body or Throwable. Internal causes stay with the
owner. A presentation provides an HTTP error status (400–599), application code,
a deliberate public message (at most 256 characters, no control characters), and an
optional empty-body decision. It cannot forward, authorize, invoke a chain,
retry, fallback or write. Mappers are trusted local extensions and must be
nonblocking with bounded work; there is no callback thread pool or remote policy
service.

For example, a Spring bean can return
Optional.of(new GatewayFailurePresentation(404, 40401, "Route unavailable", false))
for SELECTOR_MISS, and Optional.empty() for other reasons. The existing
ShenyuResult bean formats the result and WebFluxResultUtils writes it, so a
deployment's custom result envelope/content type remains in effect.

With no selected mapping, legacy behavior is retained: IllegalArgumentException
uses 400 and its existing public message, ResponseStatusException uses its status
and reason, and unknown escaped exceptions use 500 / Internal Server Error.
Legacy selector/rule misses retain their old application codes, messages,
content type and unset HTTP status (normally HTTP 200). Those old public messages
and existing error logging are compatibility behavior, not safe message sources
for a new policy.

The response owner claims the existing result write before running a mapper.
WebFluxResultUtils marks its output as started; duplicate result/error writes are
refused. Committed responses and already-owned output never start another writer
or change status/body. A global error after output propagates the original
failure. Cancellation also propagates without mapping or writing. Classification
is stored as a bounded exchange-lifetime attribute under
GatewayFailureMapping.FACTS_ATTRIBUTE; it is not a new telemetry queue. This is
not a lock or universal detector for arbitrary third-party response writes:
callers must retain the existing serial response-owner contract. A false
isCommitted() alone does not authorize a concurrent writer.

AI Proxy's authentication 401/empty body and local error owner, Sentinel fallback,
normal upstream non-2xx responses, HTTP-to-RPC result writers, native gRPC
status/trailers, WebSocket close frames and MCP/SSE protocol terminal errors are
not adopted. Their existing response/retry/buffer ownership is preserved. No
business-request interceptor or all-plugin rejection coverage is claimed.

GatewayFailureMappingTest exercises the actual global handler through reactive
WebHandler dispatch, both direct helpers, custom result envelopes, legacy
characterization, ordered/no-match/invalid/throwing policies, cancellation,
timeout/auth/policy/upstream/unknown classifications and committed SSE preservation.
AiProxyPluginTest explicitly installs a mapping bean while checking the unadopted
AI authentication and local failure owners.
