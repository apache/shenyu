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
(the contract). Where the contract fixes *semantics*, this audit fixes *placement*: it classifies every
plugin currently shipped in `shenyu-plugin` into a wrapper-scope class, records how each plugin uses
the `ShenyuPluginChain` continuation, and derives the concrete placement and validation rules that
#7995 (validator/compiler) and #7996 (graph runtime) must enforce.

The audit is **category-level with representative deep-dives**: §3 gives the full per-plugin matrix
(one row per `PluginEnum` name), §4 digs into the five cases that actually decide the design
(global/context init, Sentinel-style wrapper, response owner/writer, proxy terminal + retry,
`fork()` internal dispatch), and §5–§6 turn the findings into machine-checkable rules.

## 1. Method and evidence base

Classification is grounded in the code as it exists on `master` at the time of writing:

- Plugin identity and default ordering come from `org.apache.shenyu.common.enums.PluginEnum`
  (`code` = sort key, `name` = the string a graph node binds via `pluginName`).
- Continuation behaviour is read from each plugin's `execute`/`doExecute`: whether it calls
  `chain.execute(exchange)` exactly once, decorates the returned `Mono`
  (`.transform`/`.then`/`.onErrorResume`/`.doFinally`), resubscribes internally, or calls
  `chain.fork()`.
- Selector/rule matching is **out of scope** for placement: `AbstractShenyuPlugin` already owns it and
  the graph never replaces it (contract §2 invariant). A graph edge is not a rule.

This audit does **not** assert that a plugin is correct in LEGACY mode; it only asks whether the
plugin's chain usage survives being placed on a graph node under the Phase-1 restrictions
(contract §7).

## 2. Classification dimensions

Each plugin gets three orthogonal labels.

**Wrapper-scope class** (contract §9) — decides *where* a node may sit:

| Class | Placement rule |
|-------|----------------|
| Global / context init | Mandatory **common prefix**; cannot be branched, skipped, or duplicated. |
| Auth / security | **Common prefix**; mandatory deps fail closed (contract §12); never bypassed by graph config. |
| Wrapper (observability / resilience / logging) | **Wrapper scope**: must fully enclose the sub-path it measures. A whole-pipeline wrapper belongs in the common prefix/suffix, not inside one branch. |
| Response owner / writer | **Common suffix**; exactly one response owner per root→END path. |
| Transform | **Branch-safe** if idempotent and not duplicated on a path; order-sensitive. |
| Proxy / terminal | **Exclusive** per path; side-effecting (upstream calls) — must not be duplicated or re-executed on fallback. |
| Short-circuit | **Branch-safe** terminal alternative (commits a response without an upstream). |

**Continuation behaviour** (contract §11) — decides whether the GRAPH executor may cache/re-run a
node's output `Mono`:

| Code | Meaning | Executor constraint |
|------|---------|---------------------|
| (a) | calls the continuation **exactly once** | free placement within its class rules |
| (b) | may **resubscribe** its own output (retry / failover) | executor must **not** insert a silent `cache()`; node output must stay re-subscribable |
| (c) | **wraps / decorates** the continuation (`transform`/`then`/response decoration) | wrapper scope must be preserved; cannot be split across a branch boundary |

**Phase-1 verdict**:

| Verdict | Meaning |
|---------|---------|
| Supported | may appear on a graph node in Phase 1 under its placement rule. |
| Prefix/Suffix-only | supported but constrained to the common prefix or suffix (cannot live inside a CHOICE branch). |
| Restricted | supported only in a single-execution position; validator rejects duplicate placement on a path. |
| Rejected (Phase 1) | referencing it in a graph node fails validation with an actionable error (contract §15); still fully usable in LEGACY. |

## 3. Full per-plugin matrix

Ordered by `PluginEnum.code` (the LEGACY sort key). "Cont." is the continuation behaviour from §2.

| code | name | class | Cont. | Phase-1 verdict |
|------|------|-------|-------|-----------------|
| -1 | `global` | Global / context init | (a) | **Prefix-only** (mandatory, exactly once) |
| 0 | `tracing` | Wrapper (observability) | (c) | Prefix/Suffix-only (whole-pipeline wrapper) |
| 1 | `mock` | Short-circuit | (a) | Supported (branch-safe terminal) |
| 10 | `cache` | Response owner / writer | (c) | Prefix/Suffix-only (wraps response write) |
| 15 | `metrics` | Wrapper (observability) | (c) | Prefix/Suffix-only |
| 20 | `sign` | Auth / security | (a) | **Prefix-only** (mandatory dep, fail closed) |
| 30 | `jwt` | Auth / security | (a) | **Prefix-only** |
| 40 | `oauth2` | Auth / security | (a) | **Prefix-only** |
| 40 | `casdoor` | Auth / security | (a) | **Prefix-only** |
| 50 | `waf` | Auth / security | (a) | **Prefix-only** |
| 60 | `rateLimiter` | Wrapper (resilience) | (c) | Prefix/Suffix-only |
| 65 | `aiRequestTransformer` | Transform (request) | (a) | Supported (branch-safe, no duplicate) |
| 66 | `aiResponseTransformer` | Transform (response) | (c) | Prefix/Suffix-only (response side) |
| 70 | `paramMapping` | Transform (request) | (a) | Supported |
| 80 | `contextPath` | Transform (request) | (a) | Supported |
| 90 | `rewrite` | Transform (request) | (a) | Supported |
| 100 | `cryptorRequest` | Transform (request) | (a) | Supported |
| 110 | `redirect` | Short-circuit | (a) | Supported (branch-safe terminal) |
| 120 | `request` | Transform (request) | (a) | Supported |
| 125 | `generalContext` | Transform (context) | (a) | Supported |
| 125 | `mqtt` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 130 | `hystrix` | Wrapper (resilience) | (c) | Prefix/Suffix-only |
| 140 | `sentinel` | Wrapper (resilience) | (c) | Prefix/Suffix-only (see §4.2) |
| 150 | `keyAuth` | Auth / security | (a) | **Prefix-only** |
| 150 | `basicAuth` | Auth / security | (a) | **Prefix-only** |
| 160 | `loggingConsole` | Wrapper (logging) | (c) | Prefix/Suffix-only |
| 170 | `aiPrompt` | Transform (request) | (a) | Supported |
| 170 | `loggingRocketMQ` | Wrapper (logging) | (c) | Prefix/Suffix-only |
| 171 | `loggingRabbitMQ` | Wrapper (logging) | (c) | Prefix/Suffix-only |
| 171 | `aiTokenLimiter` | Wrapper (resilience) | (c) | Prefix/Suffix-only |
| 175 | `loggingAliyunSls` | Wrapper (logging) | (c) | Prefix/Suffix-only |
| 176 | `loggingTencentCls` | Wrapper (logging) | (c) | Prefix/Suffix-only |
| 177 | `loggingHuaweiLts` | Wrapper (logging) | (c) | Prefix/Suffix-only |
| 180 | `loggingKafka` | Wrapper (logging) | (c) | Prefix/Suffix-only |
| 180 | `mcpServer` | Proxy / terminal | (a) | **Restricted** (exclusive; uses `fork()`, see §4.5) |
| 185 | `loggingPulsar` | Wrapper (logging) | (c) | Prefix/Suffix-only |
| 190 | `loggingElasticSearch` | Wrapper (logging) | (c) | Prefix/Suffix-only |
| 195 | `loggingClickHouse` | Wrapper (logging) | (c) | Prefix/Suffix-only |
| 197 | `sensitiveWord` | Transform (request/response) | (c) | Prefix/Suffix-only |
| 198 | `agentGateway` | Proxy / terminal | (a) | **Restricted** (exclusive; uses `fork()`, see §4.5) |
| 199 | `aiProxy` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 200 | `divide` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting; see §4.4) |
| 200 | `springCloud` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 200 | `websocket` | Proxy / terminal | (a) | **Restricted** (exclusive, long-lived stream) |
| 205 | `uri` | Transform (routing) | (a) | Supported |
| 210 | `webClient` | Proxy / terminal (HTTP client) | (b) | **Restricted** (internal resubscription; see §4.4) |
| 210 | `nettyHttpClient` | Proxy / terminal (HTTP client) | (b) | **Restricted** (internal resubscription; see §4.4) |
| 220 | `modifyResponse` | Transform (response) | (c) | Prefix/Suffix-only (response side) |
| 300 | `paramTransform` | Transform (RPC request) | (a) | Supported |
| 310 | `dubbo` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 310 | `sofa` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 310 | `tars` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 310 | `grpc` | Proxy / terminal | (a) | **Restricted** (exclusive, side-effecting) |
| 310 | `resilience4j` | Wrapper (resilience) | (c) | Prefix/Suffix-only |
| 320 | `tcp` | Proxy / terminal | (a) | **Restricted** (exclusive, long-lived stream) |
| 410 | `cryptorResponse` | Transform (response) | (c) | Prefix/Suffix-only (response side) |
| 420 | `response` | Response owner / writer | (c) | **Suffix-only** (mandatory single owner; see §4.3) |

Notes on the matrix:

- Several `code` values collide (e.g. `divide`/`springCloud`/`websocket` all 200; the RPC proxies and
  `resilience4j` all 310). LEGACY breaks ties by registration/insertion order; a graph makes the order
  **explicit**, which is one of the concrete benefits of #7993 and removes a real source of ambiguity.
- The HTTP clients (`webClient`, `nettyHttpClient`) are the only **(b)** plugins: they resubscribe
  their own upstream `Mono` for retry/failover. This is the single most important executor constraint
  (§4.4, contract §11).
- `mock` and `redirect` are the clean **short-circuit** terminals: they commit a response without an
  upstream, so they are natural exclusive branch leaves.
- Plugins marked "Prefix/Suffix-only" are wrappers (class (c)). They are *supported*, but a whole-
  pipeline wrapper placed inside one CHOICE branch would only measure that branch, silently changing
  its semantics — hence the restriction.

## 4. Representative deep-dives

### 4.1 Global / context init — `GlobalPlugin` (mandatory prefix, exactly once)

`GlobalPlugin#execute` builds the `ShenyuContext` and publishes it before delegating:

```java
ShenyuContext shenyuContext = builder.build(exchange);
exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
return chain.execute(exchange);
```

Every downstream plugin reads `exchange.getAttribute(Constants.CONTEXT)` and calls
`Objects.requireNonNull(shenyuContext)` (e.g. `SentinelPlugin`, `ResponsePlugin`). Consequences for
the graph:

- `global` is a **hard mandatory dependency** (contract §12): a graph whose entry path does not run
  `global` before any context-reading node is rejected at validation.
- It must run **exactly once per request** and **before** the dispatcher hands off to any branch. The
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

This is the canonical class (c) decorator. The wrapper's meaning depends on **what it encloses**:

- If `sentinel` sits in the common prefix, it wraps the whole pipeline (the LEGACY-equivalent
  semantics) — circuit breaking and fallback see the real downstream outcome.
- If `sentinel` were placed inside one CHOICE branch, it would only guard that branch; the other
  branches and the shared suffix would run un-guarded. That is a **silent semantic change**, so the
  validator restricts whole-pipeline wrappers to prefix/suffix placement (§5, rule W1).
- The same shape applies to `resilience4j`, `hystrix`, `rateLimiter`, `tracing`, `metrics`, the
  `logging*` family, and `cache`: all decorate `chain.execute(...)` rather than merely calling it.

`sentinel` also installs a `Constants.WATCHER_HTTP_STATUS` consumer and reads
`Constants.METRICS_SENTINEL` in `doFinally` — request-scoped attributes that are safe under the
per-subscription state model (contract §11) but would be corrupted by any cross-branch state sharing.

### 4.3 Response owner / writer — `ResponsePlugin` + `MessageWriter` (single owner, suffix)

`ResponsePlugin#execute` delegates by RPC type to a `MessageWriter` that owns the actual write:

```java
return writerMap.get(shenyuContext.getRpcType()).writeWith(exchange, chain);
```

`MessageWriter#writeWith(exchange, chain)` receives the chain and decorates the response write
(`NettyClientMessageWriter`, `RPCMessageWriter`, `WebClientMessageWriter`). `CachePlugin` participates
in the same ownership boundary (it caches/replays the written response — see
[cache-request-eligibility.md](./cache-request-eligibility.md)). Consequences:

- The response owner is a **common-suffix** concern: exactly one owner per root→END path. Two response
  owners on one path (e.g. `response` and a second writer) would double-write the response. The
  validator enforces "≤1 response-owner node per path" (§5, rule R1).
- Because the writer wraps the terminal write, the owner is class (c): it cannot be split across a
  branch boundary, and the proxy terminal it writes for must be on the same path (§4.4).
- `modifyResponse` / `cryptorResponse` / `aiResponseTransformer` mutate the response and must sit
  **before** the owner on the suffix; they are response-side transforms, not owners.

### 4.4 Proxy terminal + retry — `divide` → HTTP client (exclusive, side-effecting, re-subscribable)

The HTTP proxy path is `divide` (selects the upstream, runs the load balancer) followed by an HTTP
client (`webClient` / `nettyHttpClient`) that performs the actual call. The client is the only class
**(b)** plugin: on timeout/failure it **resubscribes its own upstream `Mono`** to retry or fail over.
`DefaultRetryStrategy#resend` re-invokes `httpClientPlugin.doRequest(...)` and, for failover, re-runs
the load balancer to pick a different upstream (this is the path fixed in #7974/#7975 for P2C inflight
accounting). Consequences for the graph:

- **No silent `cache()`.** If the GRAPH executor cached a proxy node's output `Mono` to "reuse" it, the
  retry/failover resubscription would be short-circuited and the plugin would stop retrying. The
  executor must keep proxy node output re-subscribable (contract §11).
- **Exclusive, non-duplicated.** A proxy node performs a side-effecting upstream call. The validator
  rejects any path that reaches the same proxy plugin twice, and rejects a proxy appearing in both the
  common prefix and a branch (contract §16.3 "duplicate side-effecting plugin on a path").
- **No cross-executor fallback.** Because a retry already resubscribes inside the node, a graph-level
  "fall back to LEGACY on error" would duplicate the upstream call. This is exactly why the contract
  forbids implicit fallback (§13) and mid-request executor switches (§10, rule 8).
- `websocket` / `tcp` / `mqtt` are long-lived streams: the executor pins the plan snapshot for the life
  of the stream; a revision swap affects new connections only (contract §10, rule 8; §14).

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
  §8). The validator/runtime reject graphs whose fork depth exceeds the cap.
- The internal marker attributes (`HTTP_TOOL_CALL_ATTRIBUTE`, `MCP_TOOL_CALL`) are request-scoped and
  safe under per-subscription state, but the forked child must not share mutable traversal state with
  the parent — which is precisely the property `fork()`'s independent-cursor contract guarantees.

## 5. Validation rules derived from the audit (input to #7995)

The validator (#7995) must reject, with the offending `nodeId`/`pluginName` in the error:

- **G1 — mandatory prefix.** The entry path runs `global` before any context-reading node; `global`
  appears exactly once and is never a CHOICE target.
- **A1 — auth fail closed.** Any node marked `required` (and any plugin in the Auth/security class)
  that is missing/disabled at activation rejects the plan (contract §12); auth nodes live in the
  common prefix and are never bypassed by a branch.
- **W1 — wrapper scope.** A class (c) whole-pipeline wrapper (tracing, metrics, sentinel, resilience4j,
  hystrix, rateLimiter, the `logging*` family, cache) is placed in the common prefix/suffix; it cannot
  be a node inside a CHOICE branch unless the branch is the entire enclosed sub-path.
- **R1 — single response owner.** At most one response-owner node (`response`, plus the cache writer
  role) per root→END path; response-side transforms (`modifyResponse`, `cryptorResponse`,
  `aiResponseTransformer`) precede the owner.
- **P1 — proxy exclusivity.** A proxy/terminal plugin appears at most once per path and never in both
  the common prefix and a branch.
- **B1 — no `cache()` on (b) nodes.** The compiler tags HTTP-client nodes as re-subscribable so the
  runtime does not memoize their output (contract §11).
- **F1 — bounded fork.** Fork/internal-dispatch recursion depth ≤ configured cap (default 8).
- **S1 — structural restrictions & limits.** The §7 shape (single entry, common prefix, simple-chain
  CHOICE branches, single convergence, common suffix) and the §8 limits hold; every reachable path
  terminates at an `END` or a response-committing terminal.
- **X1 — unsupported plugin.** Referencing a plugin the audit marks "Rejected (Phase 1)" fails
  validation (contract §15). No plugin in the current tree is fully rejected; the restriction set is
  the "Restricted"/"Prefix/Suffix-only" verdicts above, which are placement constraints rather than
  outright bans. This rule exists so future plugins that mutate the shared cursor can be rejected.

## 6. Phase-1 supported graph shape (concrete)

Combining §3–§5, the initial supported graphs are exactly:

```text
[common prefix]  global (mandatory) -> auth/security* -> whole-pipeline wrapper*
[CHOICE region]  zero or more CHOICE nodes whose branches are simple chains of
                 transform / proxy-terminal / short-circuit nodes (exclusive, non-duplicated)
[convergence]    a single designated convergence node
[common suffix]  response-side transform* -> response owner (single) -> END
```

This shape keeps path analysis polynomial (contract §7), preserves every wrapper's scope, guarantees a
single response owner, and never duplicates a side-effecting proxy — while still delivering the core
#7993 value: explicit ordering (no more `code` ties), conditional exclusive routing, and declarative
mandatory dependencies.

## 7. Residual risks and open questions

1. **Cache + response owner interaction.** `cache` both wraps the response write and replays cached
   responses; its exact node placement (prefix vs suffix, and relative to the owner) needs a dedicated
   decision before #7996. Tracked as an open question here rather than guessed.
2. **AI transformer ordering.** `aiRequestTransformer`/`aiResponseTransformer`/`aiPrompt`/
   `sensitiveWord` straddle the request/response boundary; the audit classifies them by the side they
   mutate, but a graph that mixes AI and non-AI proxies may need finer ordering rules.
3. **Long-lived streams under revision swap.** `websocket`/`tcp`/`mqtt` pin the plan for the life of the
   connection; the precise "new connections only" cutover semantics are a #7996/#7998 concern.
4. **Plugin-store / migrated plugins.** Some `PluginEnum` names (e.g. the `logging*` compatibility
   entries, `hystrix`) point at implementations that live in the plugin store. A graph node binding such
   a name must resolve against the *installed* catalog at activation; the fail-closed rule (A1/X1)
   covers the missing case, but the catalog-resolution contract is finalized in #7995/#7999.
