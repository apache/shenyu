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

# Opt-in AI exact response caching

This is the initial nonstreaming text Chat slice of
[#8049](https://github.com/apache/shenyu/issues/8049), not semantic caching.
It lives inside AI Proxy, after authentication, route selection, protocol
validation and provider request preparation. It is not the general HTTP cache
plugin, and it never uses URI equality alone to reuse model output.

## Defaults and deployment contract

Caching is **disabled by default**. Enabling it without an
`AiResponseCacheScopeResolver` still bypasses caching. The resolver is an
application-owned, nonblocking authorization adapter, not a header extractor.
It must approve stateless text routes and supply a current tenant, subject,
data-access scope, policy revision and prompt revision. The plugin binds the
scope to the actual matched selector and rule, regardless of route IDs returned
by the resolver. Empty, blank, missing or overlong scope dimensions bypass.

The resolver runs on each eligible request, including hits. Its error fails the
request through the existing proxy failure boundary; it does not retry as an
unauthenticated request. An empty result only declines **caching**, not proxy
execution. Keep normal authentication and route authorization ahead of AI Proxy;
the cache resolver does not replace them. Requests explicitly opting out of
caching do not need cache authorization.

`AgentRequestContext` is diagnostic context, not a cache authorization grant.
Neither it, an exchange attribute, `X-Tenant`, request JSON nor a client session
identifier is automatically accepted as a cache scope. An authentication adapter
must verify identity; the deployment must also approve the data-access and
route-equivalence policy. A gateway running trusted third-party Java code is
still one trusted process: this API is not a sandbox against malicious plugins.

```yaml
shenyu:
  plugins:
    ai:
      proxy:
        response-cache:
          enabled: true
          ttl-seconds: 60
          max-request-bytes: 65536
          max-entry-bytes: 1048576
          max-weight-bytes: 16777216
          max-concurrent-fills: 32
```

These limits are bound at application startup. No Admin schema migration is
required. Changing them requires rebuilding the application context. The
starter creates no cache bean while disabled and rejects invalid configured
limits and ambiguous scope resolvers.

### Adapter example

For a deployment already using Agent Gateway's trusted
`AgentMcpSecurityResolver`, the following bean adapts it for **one reviewed,
stateless FAQ route**. This example requires that plugin on the classpath; AI
Proxy itself does not depend on it. Replace the selector/rule IDs and revision
constants with your deployment's server-managed policy. Existing normal route
authorization must already protect this route. Do not copy this policy to
private retrieval, sessions, tools or arbitrary routes.

```java
@Bean
AiResponseCacheScopeResolver faqCacheScope(final AgentMcpSecurityResolver authentication) {
    return (exchange, selectorId, ruleId) -> {
        if (!"faq-selector".equals(selectorId) || !"faq-rule".equals(ruleId)) {
            return Mono.empty();
        }
        return authentication.resolve(exchange)
                .filter(identity -> identity.getTenant().isPresent())
                .map(identity -> new AiResponseCacheScope(
                        identity.getTenant().orElseThrow(), identity.getSubject(),
                        "public-faq-v1", "faq-policy-3", "faq-prompt-7", selectorId, ruleId));
    };
}
```

Imports: `org.springframework.context.annotation.Bean`,
`reactor.core.publisher.Mono`,
`org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver`, and
`org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiResponseCacheScope` /
`AiResponseCacheScopeResolver`.

An adapter for another authentication system must enforce the same contract.
Do not return a constant tenant/subject for anonymous requests. Revisions must
change whenever output-affecting authorization, retrieval data, routing or
prompt semantics change outside the request itself. If the deployment cannot
establish a complete scope, return `Mono.empty()`.

## Supported requests and responses

| Case | Behavior |
| --- | --- |
| Approved POST, nonstreaming native OpenAI Chat, bounded plain-text messages | Eligible |
| Missing cache authorization or unknown data-access equivalence | Bypass |
| Client Cookie, Cache-Control or Pragma header | Bypass |
| SSE, including truncated streams or synthesized `[DONE]` | Bypass |
| OpenAI Responses, converted protocols or custom protocol/provider implementations | Bypass |
| Tools/functions, multimodal messages, session/private state, unknown request fields | Bypass |
| Configured/request-selected fallback | Bypass, including an otherwise available primary hit |
| Oversized request or response, exhausted capture slots | Forward normally without caching |
| Failed, cancelled, truncated, incomplete or unsuccessful model output | Never publish |

The allowlist is intentionally conservative: built-in native Chat implementations
for `OpenAI`, `OpenAICompatible`, `DeepSeek`, `ALiYun` and `Moonshot` are considered;
other provider identifiers bypass, even if their wire format happens to look
compatible. Supported request parameters are
`model`, `messages`, `stream`, `max_tokens`, `max_completion_tokens`,
`temperature`, `top_p`, `presence_penalty`, `frequency_penalty`, `stop`, `seed`,
`n` and `logit_bias`. Message fields are `role` and plain-string `content`, with
system/user/assistant roles. Both the original and final provider payload must
satisfy eligibility. Future/unknown fields bypass rather than being silently
excluded from the key. Even temperature-zero requests are not asserted to be
deterministic: enabling the cache explicitly opts into reusing prior output.

A stored response must be HTTP 200, JSON Chat completion data with completed
text choices, and normal `stop` finish reasons. Length-limited, tool-calling,
error or invalid/truncated output is not reusable. Unsupported response variation,
private/no-store/no-cache controls, cookies and encoded responses bypass storage.
Hits replay the original structured JSON bytes, including original completion ID
and usage, and the supported content headers (`Content-Type`, `Content-Language`).
Hop-by-hop and Connection-nominated headers, Content-Length, Date, upstream
request IDs, quota headers and other per-exchange metadata are not replayed.
This is deliberately not arbitrary HTTP-header preservation.

SSE cannot safely use downstream `[DONE]` as its sole completion evidence: the
existing compatibility encoder can synthesize it at transport EOF. This slice
therefore does not cache or reconstruct SSE. Raw completion evidence belongs to
[#8123](https://github.com/apache/shenyu/issues/8123) /
[PR #8275](https://github.com/apache/shenyu/pull/8275), not this cache.

## Key and invalidation

Keys are SHA-256 digests over the complete approved isolation scope, matched
selector/rule, provider and native/client protocols, effective HTTP method and
endpoint, prepared headers (including credential partitioning), and canonical
original/effective request JSON. Object keys are sorted; array order and all
allowed request parameters remain significant. Prompt text and credentials are
not included in human-readable keys, metrics or logs. Hashing is not encryption:
cached response text remains in gateway memory and must be treated as sensitive.

A tenant, subject, access-scope, policy/prompt revision, model, credential,
endpoint or relevant payload change cannot hit the previous partition. The key
uses provider-prepared parameters rather than assuming the client's model alias
is the effective model. Entries expire after publication, not after every read.

`AiExactResponseCache.invalidateAll()` clears local entries and fences pending
fills, so pre-clear work cannot republish an old entry afterward. Use it for
administrative local invalidation; it is not exposed as an unauthenticated HTTP
endpoint. The starter calls it when its application context closes. Policy or
prompt revision changes provide namespace invalidation; stale namespaces remain
bounded by eviction and TTL. Revocation must first stop issuing the old scope:
clearing entries alone does not revoke authorization. No fleet-wide invalidation,
distributed single-flight or cross-node sharing is claimed.

The future GRAPH equivalence contract tracked by
[#8012](https://github.com/apache/shenyu/issues/8012) is not implemented here.
The resolver must decline unknown/unsupported graph paths, dynamic retrieval
state or downstream route choices that it cannot encode in its approved policy.
Do not authorize the same scope across such paths merely because they share a URI.

## Bounds, lifecycle and failures

- Local Caffeine storage is weighted by body, retained headers, key and fixed
  per-entry accounting, with a TTL and aggregate byte budget. This is an
  accounting bound, not a claim of exact JVM heap size; object/cache overhead is
  additional.
- Response capture runs on the existing single subscription. It does not buffer
  the full response before forwarding, create another upstream call, or hide
  cancellation. Over-limit bodies stop being captured and continue downstream.
- Capture slots bound concurrent fills. There are no waiting followers, unbounded
  pending-key maps, background fill subscriptions or shared cancellation owners.
  Concurrent misses can each reach the provider; this first slice deliberately
  implements no single-flight.
- In-progress capture memory is bounded by configured slots and per-entry bytes,
  with transient byte-copy and JSON-validation allocations proportional to those
  limits. Request inspection is separately size/depth bounded. Configure aggregate
  weight and capture limits together, leaving heap headroom for normal proxy work.
- Publication requires both successful complete body consumption and successful
  response-consumer completion. Upstream errors, downstream write errors,
  cancellation and early consumers do not populate entries.
- Cache-local lookup/storage failures bypass caching without swallowing request,
  provider or response-writing failures. The first implementation has no remote
  cache backend to become unavailable. A cache-hit write failure never triggers
  a second upstream attempt.

## Usage and savings

A hit creates no upstream attempt. Existing `AiCallObserver` logical events still
finish normally, report zero attempts for a pure hit, and retain unknown upstream
usage rather than presenting cached `usage` JSON as new provider consumption.
`AiExactResponseCache.stats()` exposes fixed aggregate counters for hits, misses,
bypasses and successful writes, without tenant/prompt labels. These counters can
be exported by the application's existing metrics integration; their lifetime
is the cache instance, not a distributed ledger.

A reused response is an avoided call under this policy, not proof of monetary
savings or of what the provider would have billed. No money estimate is emitted.
Attempt-usage normalization and versioned pricing remain owned by
[#7955](https://github.com/apache/shenyu/issues/7955) and
[#7934](https://github.com/apache/shenyu/issues/7934).
