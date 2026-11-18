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

# Plugin Graph Orchestration — Compatibility Audit (Phase 1)

Status: **design artifact for community review**, produced for issue #7994 under the parent proposal
#7993. This is the companion to [plugin-graph-orchestration.md](./plugin-graph-orchestration.md)
(the contract). It records proposed capability/placement constraints for #7995 (validator/compiler) and #7996
(graph runtime). Category labels are not a compatibility certification. In particular, plugin-store
entries and plugins without concrete GRAPH tests remain unverified; no runtime implementation or
accepted support list is supplied by this document.

The audit is **category-level with representative deep-dives**: §3 preserves the per-plugin review inventory
from the initial proposal, §4 digs into the five cases that actually decide the design
(global/context init, Sentinel-style wrapper, response owner/writer, HTTP pipeline + transport retry,
`fork()` internal dispatch), and §5–§6 turn the findings into machine-checkable rules.

## 1. Method and evidence base

The focused corrections for #8010/#8011/#8013/#8014/#8015/#8018 were checked against master
`d7e8282b44abe279f9a34602498342af0038976d`. The matrix is a review checklist, not an assertion that
all implementations (including migrated plugins) have been audited or tested for GRAPH:

- Plugin identity and default ordering come from `org.apache.shenyu.common.enums.PluginEnum`
  (`code` = sort key, `name` = the string a graph node binds via `pluginName`).
- Continuation review inspects a plugin's `execute`/`doExecute`: whether it calls
  `chain.execute(exchange)` exactly once, decorates the returned `Mono`
  (`.transform`/`.then`/`.onErrorResume`/`.doFinally`), resubscribes internally, or calls
  `chain.fork()`.
- Selector/rule matching stays owned by `AbstractShenyuPlugin`; a graph edge is not a rule.
  Matching/skip outcomes nevertheless matter to capability and authentication proofs. A structurally
  present node that does not run cannot be credited with producing a URI, response or authenticated
  identity.

This audit does **not** assert that a plugin is correct in LEGACY mode; it only asks whether the
plugin's chain usage survives being placed on a graph node under the Phase-1 restrictions
(contract §7).

## 2. Classification dimensions

Separate the **effect** a node produces from the **publisher region** it encloses:

| Role | Proposed requirement |
|------|----------------------|
| Context | `global` precedes context consumers and runs once per entry traversal. |
| Auth dependency | Required availability is checked independently of request-time authentication (contract §12). |
| Wrapper/decorator | Must precede and enclose its declared downstream region for every protected request; a suffix cannot protect earlier work. |
| Target / URI producer | Supplies capabilities required by later transport; conditional skip/no-match needs proof or a runtime guard. |
| Transport invocation | One compatible invocation stage on a normal forwarding path; internal retries are separate attempts within that stage. |
| Response commitment | Normal writer consumes the invocation result; a response decorator is not another independent writer. |
| Short circuit | May complete with zero calls without executing its structural successor; continuing outcomes still require a complete pipeline. |

The matrix uses these **candidate continuation properties**, not execution-count guarantees:

| Code | Meaning | Required audit |
|------|---------|----------------|
| (a) | delegates to a successor when continuing | Check skip, selector/rule miss, short circuits and actual invocation count. |
| (b) | retries a transport-internal response publisher, then delegates | Distinguish transport attempts from successor invocation/subscription. |
| (c) | wraps/decorates downstream work | Identify the actual region and verify failure, fallback, cancellation and cleanup. |

GRAPH's proposed one-call/one-subscription successor restriction and the five distinct execution
cases are defined in contract §11. No label grants permission to resubscribe the successor or cache a
plugin's output. The initial proposal excludes branch-local wrappers pending scope tests; this is a
support limitation, not a claim that they cannot enclose the common suffix.

## 3. Per-plugin review matrix

Names/codes from the original `PluginEnum` inventory (including compatibility names for migrated
plugins). Resolve the actual installed catalog before activation. "Cont." is the candidate property
from §2; every row still needs concrete GRAPH compatibility tests. "Scope-dependent" means the
protected region must be established, not that arbitrary placement is supported.

| code | name | role | Cont. | Proposed constraint (unverified for GRAPH) |
|------|------|-------|-------|-----------------|
| -1 | `global` | Global / context init | (a) | **Prefix-only** (mandatory, exactly once) |
| 0 | `tracing` | Wrapper (observability) | (c) | Scope-dependent (whole-pipeline wrapper) |
| 1 | `mock` | Short-circuit | (a) | Candidate branch-safe terminal |
| 10 | `cache` | Response decorator on miss / terminal on hit | (c) | Before the write it decorates; coexists with `response` |
| 15 | `metrics` | Wrapper (observability) | (c) | Scope-dependent |
| 20 | `sign` | Auth / security | (a) | **Prefix-only** if required by independent policy; availability only |
| 30 | `jwt` | Auth / security | (a) | **Prefix-only** |
| 40 | `oauth2` | Auth / security | (a) | **Prefix-only** |
| 40 | `casdoor` | Auth / security | (a) | **Prefix-only** |
| 50 | `waf` | Auth / security | (a) | **Prefix-only** |
| 60 | `rateLimiter` | Wrapper (resilience) | (c) | Scope-dependent |
| 65 | `aiRequestTransformer` | Transform (request) | (a) | Candidate branch transform; no duplicate |
| 66 | `aiResponseTransformer` | Transform (response) | (c) | Scope-dependent (response side) |
| 70 | `paramMapping` | Transform (request) | (a) | Candidate branch transform |
| 80 | `contextPath` | Transform (request) | (a) | Candidate branch transform |
| 90 | `rewrite` | Transform (request) | (a) | Candidate branch transform |
| 100 | `cryptorRequest` | Transform (request) | (a) | Candidate branch transform |
| 110 | `redirect` | Short-circuit | (a) | Candidate branch-safe terminal |
| 120 | `request` | Transform (request) | (a) | Candidate branch transform |
| 125 | `generalContext` | Transform (context) | (a) | Candidate branch transform |
| 125 | `mqtt` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 130 | `hystrix` | Wrapper (resilience) | (c) | Scope-dependent |
| 140 | `sentinel` | Wrapper (resilience) | (c) | Scope-dependent (see §4.2) |
| 150 | `keyAuth` | Auth / security | (a) | **Prefix-only** |
| 150 | `basicAuth` | Auth / security | (a) | **Prefix-only** |
| 160 | `loggingConsole` | Wrapper (logging) | (c) | Scope-dependent |
| 170 | `aiPrompt` | Transform (request) | (a) | Candidate branch transform |
| 170 | `loggingRocketMQ` | Wrapper (logging) | (c) | Scope-dependent |
| 171 | `loggingRabbitMQ` | Wrapper (logging) | (c) | Scope-dependent |
| 171 | `aiTokenLimiter` | Wrapper (resilience) | (c) | Scope-dependent |
| 175 | `loggingAliyunSls` | Wrapper (logging) | (c) | Scope-dependent |
| 176 | `loggingTencentCls` | Wrapper (logging) | (c) | Scope-dependent |
| 177 | `loggingHuaweiLts` | Wrapper (logging) | (c) | Scope-dependent |
| 180 | `loggingKafka` | Wrapper (logging) | (c) | Scope-dependent |
| 180 | `mcpServer` | Proxy / terminal | (a) | **Restricted** (exclusive; uses `fork()`, see §4.5) |
| 185 | `loggingPulsar` | Wrapper (logging) | (c) | Scope-dependent |
| 190 | `loggingElasticSearch` | Wrapper (logging) | (c) | Scope-dependent |
| 195 | `loggingClickHouse` | Wrapper (logging) | (c) | Scope-dependent |
| 197 | `sensitiveWord` | Transform (request/response) | (c) | Scope-dependent |
| 198 | `agentGateway` | Proxy / terminal | (a) | **Restricted** (exclusive; uses `fork()`, see §4.5) |
| 199 | `aiProxy` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 200 | `divide` | HTTP target/policy producer | (a) | Before `uri` and the HTTP client; may also wrap downstream outcome (§4.4) |
| 200 | `springCloud` | HTTP routing integration | (a) | Audit its target/URI/client prerequisites separately |
| 200 | `websocket` | Proxy / terminal | (a) | **Restricted** (exclusive, long-lived stream) |
| 205 | `uri` | URI producer | (a) | Requires domain; precedes HTTP client |
| 210 | `webClient` | HTTP transport invocation | (b) | One client stage; transport-internal retries only (§4.4) |
| 210 | `nettyHttpClient` | HTTP transport invocation | (b) | One client stage; transport-internal retries only (§4.4) |
| 220 | `modifyResponse` | Transform (response) | (c) | Scope-dependent (response side) |
| 300 | `paramTransform` | Transform (RPC request) | (a) | Candidate branch transform |
| 310 | `dubbo` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 310 | `sofa` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 310 | `tars` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 310 | `grpc` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 310 | `resilience4j` | Wrapper (resilience) | (c) | Scope-dependent |
| 320 | `tcp` | Proxy / terminal | (a) | **Restricted** (exclusive, long-lived stream) |
| 410 | `cryptorResponse` | Transform (response) | (c) | Scope-dependent (response side) |
| 420 | `response` | Response owner / writer | (c) | After compatible invocation result; terminal alternatives need no result (§4.3) |

Notes on the matrix:

- The graph makes ordering explicit but must respect capability dependencies, not just sort keys.
- The HTTP client's **(b)** property is transport-internal retry. It does not prove a need to replay
  downstream plugins, and this category-level inventory does not prove that no other installed
  plugin retries internally.
- `mock`, `redirect` and cache hits can short-circuit with no upstream call. Cache misses decorate
  the normal writer; coalescing followers can use the leader's response instead of invoking upstream.
- Whole-region wrappers must precede the work they protect. A branch-local wrapper encloses its
  branch remainder and the shared suffix for selected requests, not the other branch. A suffix-only
  wrapper cannot measure already-completed prefix work.

## 4. Representative deep-dives

### 4.1 Global / context init — `GlobalPlugin` (mandatory prefix, exactly once)

`GlobalPlugin#execute` builds the `ShenyuContext` and publishes it before delegating:

```java
ShenyuContext shenyuContext = builder.build(exchange);
exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
return chain.execute(exchange);
```

Context consumers such as `SentinelPlugin` and `ResponsePlugin` require the context established
by `global`. Consequences for the graph:

- `global` is a **hard mandatory dependency** (contract §12): a graph whose entry path does not run
  `global` before any context-reading node is rejected at validation.
- It must run **exactly once per request** and **before** GRAPH traverses a context-reading branch. The
  GRAPH executor therefore pins context init as the first node of the common prefix; it is never a
  CHOICE target and never duplicated on a path.
- Continuation class (a): the executor may treat its output normally.

### 4.2 Sentinel-style wrapper — `SentinelPlugin` (wrapper scope must be preserved)

`SentinelPlugin#doExecute` wraps the *entire downstream* and attaches fallback + metrics on the way
out:

```java
return chain.execute(exchange)
        .transform(new SentinelReactorTransformer<>(resourceName))
        .onErrorResume(throwable -> fallbackHandler.fallback(exchange, ...))
        .doFinally(monoV -> { /* report status to metrics consumer */ });
```

This is the canonical class (c) decorator. Its scope is the subscribed successor publisher,
including shared downstream nodes reached through that continuation:

| Placement | Protected request set | Enclosed work |
|-----------|-----------------------|---------------|
| Before CHOICE | Requests reaching the wrapper | Selected branch remainder plus common suffix; not work already done before the wrapper. |
| Inside branch A | Only requests taking branch A | Remaining A nodes **and common suffix**; branch B is unprotected by this instance. |
| After convergence | Requests reaching the common suffix | Only the remaining suffix; prefix/branch work is already outside its scope. |

For `A -> sentinel -> transform -> common response`, subscription enters the transform and response
under Sentinel. Reaching the common response does not unwind Sentinel. Completion/error/cancellation
unwinds the subscribed publisher; the fallback/metrics/cleanup operators observe that lifetime.
A branch B that goes directly to the same common response does not pass through Sentinel. Fallback
must also obey response-commit rules; it cannot safely write a second response after commitment.

Contract §9 initially excludes branch-local wrappers until tests establish branch selection,
downstream failure visibility, fallback and cancellation/cleanup. This is a support restriction.
For a whole-region policy, a wrapper must dominate every protected invocation and actually enclose
its publisher; a suffix position cannot satisfy protection of earlier branch work. Diagnostics must
name the unmet protected region/request set, not merely report a forbidden category.

`sentinel` installs a `Constants.WATCHER_HTTP_STATUS` consumer and reads `Constants.METRICS_SENTINEL`
in `doFinally`. Fresh traversal state alone does not make concurrent reuse of the same exchange safe;
child dispatch must isolate its mutable attributes and response state. Similar wrapper families need
individual audits rather than inheriting a blanket compatibility claim.

### 4.3 Response writer versus response decorator — `ResponsePlugin` and `CachePlugin`

`ResponsePlugin#execute` selects a protocol-specific `MessageWriter`:

```java
return writerMap.get(shenyuContext.getRpcType()).writeWith(exchange, chain);
```

The normal HTTP writer requires response state from the compatible HTTP client. `CachePlugin` does
not represent a competing normal writer: on a hit it writes the cached response and never invokes
the successor; on a miss its forwarding path installs `CacheHttpResponse`, delegates to the normal
HTTP pipeline and observes the write. Coalescing may allow a follower to replay a leader's response.
See [cache-request-eligibility.md](./cache-request-eligibility.md).

Consequences:

- `cache -> divide -> uri -> nettyHttpClient -> response` is legal in principle with compatible
  configuration. Cache and Response are two nodes but do not double-commit on the same outcome.
- Validate the **executed outcome**, not a count of every node that can ever write. Normal forwarding
  needs a compatible result and a single commit; cache hit/mock/redirect/rejection may commit without
  a call. Decorators must be installed before the write they transform.
- `modifyResponse`, `cryptorResponse` and `aiResponseTransformer` are response-side transforms, not
  independent writers. Their precise relative order needs plugin-specific tests.
- A skipped or unmatched invocation node has not supplied an upstream result. A normal writer after
  such a node needs an explicit prerequisite guard, not an inference from structural reachability.

### 4.4 Complete HTTP pipeline and transport retry

The current responsibilities cooperate in this order:

| Stage | Source evidence | Effect on continuing execution |
|-------|-----------------|--------------------------------|
| `divide` | `DividePlugin#forwardSelected` | Chooses an upstream and sets `HTTP_DOMAIN`, timeout and retry policy; also observes downstream outcomes. |
| `uri` | `URIPlugin#execute` | Builds `HTTP_URI` if a nonblank domain exists; otherwise continues without one. |
| HTTP client | `AbstractHttpClientPlugin#execute` | Consumes `HTTP_URI`, runs/retries the transport response publisher, then delegates. |
| `response` | `ResponsePlugin` / protocol `MessageWriter` | Consumes compatible response state and writes it. |

The HTTP client's order is, in simplified form:

```java
Mono<R> response = doRequest(...);
Mono<R> retriedResponse = retryTimes <= 0 ? response : executeWithRetry(response, ...);
return retriedResponse.flatMap(ignored -> chain.execute(exchange));
```

`DefaultRetryStrategy#resend` can call `doRequest` again and select a different upstream on failover.
Those are **transport attempts**, not subscriptions to the downstream graph. For a successful
logical invocation, downstream runs once after the response; terminal transport failure does not
invoke it. Retrying the entire returned plugin publisher would have different semantics.

- Preserve the existing transport retry/cancellation boundary. Do not add blanket `cache()` and do
  not replay a side-effecting successor to imitate transport retry.
- Reject a second HTTP client stage on one forwarding path, but accept target producer + URI producer
  + one client + writer. Internal transport retries remain allowed by their own policy; exactly-once
  remote effects are not promised.
- The five execution cases in contract §11 require separate tests. A transport-retry category is not
  evidence that repeated `continuation.execute` or successor subscription is safe.
- No cross-executor fallback or mid-request switch is allowed; it could repeat already-performed
  calls. `websocket`/`tcp`/`mqtt` need separate long-lived connection and capability audits; a graph
  edge never performs protocol conversion.

### 4.5 `fork()` internal dispatch — `AgentHttpToolExecutor` / `mcpServer`

`agentGateway` (via `AgentHttpToolExecutor`) and `mcpServer` re-enter the chain for an internal
sub-request:

```java
return chain.fork().execute(child).then(Mono.fromCallable(response::result)).timeout(remaining);
```

`ShenyuPluginChain#fork()` is documented as "an independent cursor over this request's plugin snapshot
from the first plugin … custom chains must explicitly support internal dispatch rather than sharing a
mutable cursor." Consequences:

- **Executor identity is inherited** (contract §10, rule 9): a `fork()` from a GRAPH node starts an
  independent GRAPH execution from the entry over the same pinned plan snapshot; it does **not**
  consult the global dispatcher again and must not flip to LEGACY.
- **Recursion is bounded.** Because a fork re-enters from the entry, an unbounded fork chain could
  recurse indefinitely; the contract caps fork/internal-dispatch recursion depth (default 8, contract
  §8). The runtime rejects a child dispatch exceeding the cap; a static graph check alone cannot prove
  request-dependent fork depth.
- The internal marker attributes (`HTTP_TOOL_CALL_ATTRIBUTE`, `MCP_TOOL_CALL`) are request-scoped.
  A fresh cursor does not clone an exchange or isolate all plugin attributes: child dispatch must
  establish its own exchange/response ownership and avoid sharing mutable traversal state.

## 5. Proposed validation/runtime obligations (input to #7995/#7996)

Errors should identify the offending node/plugin, missing capability or policy, and relevant producer
or protected region. Static proofs and request-time guards are separate obligations:

- **G1 — context.** Run `global` once before context readers; missing/skipped context is not a valid
  invocation precondition.
- **A1 — availability versus authentication.** Reject unavailable required dependencies. Independently
  enforce any authoritative security policy; do not treat required/enabled/JWT-node presence as
  authentication. Typed skip/no-match/public/authenticated/rejected outcomes are required before
  advertising enforced authentication (contract §12).
- **W1 — actual wrapper scope.** Whole-region wrappers precede every protected invocation and enclose
  its publisher for every protected request. A suffix wrapper cannot protect earlier work. The
  initial branch-local restriction is explicit pending tests; convergence is not an unwind boundary.
- **R1 — response effects.** Distinguish normal invocation result + writer from zero-call terminal
  outcomes and decorators. Accept cache + normal response on miss; require single commitment on each
  executed outcome. Decorators precede the write they transform.
- **P1 — invocation capabilities.** For HTTP, validate target, URI, compatible client and result/writer
  dependencies in order. One client stage per continuing path; transport retry is not a second graph
  stage. Reject skipped-protocol prerequisites and implicit HTTP-to-RPC conversion.
- **B1 — continuation ownership.** The proposed successor is one-call/one-subscription per invocation.
  Do not memoize a plugin output or replay successors implicitly. Verify transport-internal retry,
  fresh entry and bounded fork separately (contract §11).
- **F1 — bounded fork.** Check internal-dispatch depth at runtime (default 8); keep pinned executor/plan
  identity and isolate child state.
- **S1 — bounded structural/data-flow analysis.** Use iterative reachability, topological cycle/length
  checks, prior-plugin union and guaranteed-capability intersection (contract §7.1). Bound nodes,
  edges, total conditions and memory before compilation; never enumerate logical paths. Keep
  structured convergence as an explicit Phase-1 support restriction.
- **X1 — verified support.** Resolve the installed plugin catalog and reject unavailable or unverified
  behavior when it lacks a supported contract. This review matrix is not an allowlist certifying all
  plugins. Incompatible plugins remain available in LEGACY mode.

## 6. Initial graph shape and unresolved runtime proof

A proposed HTTP forwarding graph has a context prefix, explicitly configured security checks and
wrappers before the work they protect, optional same-protocol CHOICE transforms, then target/URI
production, one HTTP client, response-side decorators and a compatible writer. The examples in
contract §16 use a common forwarding suffix. Other placements require the same capability and scope
proofs; short-circuit outcomes can finish before that suffix.

Structured convergence is a scope/authoring choice. It does not make wrapper scope automatic, prove
request authentication, or eliminate the need for conditional capability guards. A layered diamond
has many logical paths but a bounded node/edge representation: use contract §7.1's data-flow passes.

## 7. Residual risks and acceptance work

1. **Runtime proof remains open.** Contract §19 maps the six review issues to missing compiler/runtime
   evidence. Documentation changes alone do not pass forwarding, authentication or lifecycle tests.
2. **Cache outcomes.** Verify hit, miss, follower replay, uncacheable responses, failures and cancellation
   with the normal writer. Legal coexistence is established by source structure, not a GRAPH test.
3. **AI transformer ordering.** Request/response transformations require finer ordering and body/state
   ownership checks when combined; the category inventory is insufficient.
4. **Long-lived connections.** Validate pinned plan/executor and new-connection-only revision cutover;
   protocol-specific transports need their own capability contracts.
5. **Migrated/custom plugins.** Resolve actual installed implementations, inspect their continuation
   use and add concrete tests before admitting them. Names retained in `PluginEnum` are not evidence
   that an implementation exists in the current deployment.
6. **Policy authority.** Authentication enforcement requires an independent policy source, typed
   results and live-update semantics. Required dependency flags provide availability only.
