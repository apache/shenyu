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

# Plugin Graph Orchestration — Contract (Phase 1)

Status: **design contract for community review**, produced for issue #7994 under the parent
proposal #7993 (*optional DAG-based plugin orchestration*). Nothing here is an accepted design;
this document is the artifact intended to drive that review. It defines the versioned graph
schema, node and condition semantics, structural restrictions, graph limits, plugin requirement
and wrapper-scope classes, the dual-executor boundary, and explicit decisions for the hard cases.
The companion compatibility audit lives in [plugin-graph-compatibility-audit.md](./plugin-graph-compatibility-audit.md).

## 1. Scope and non-goals

Phase 1 adds an **optional, serial** plugin execution graph as a *second execution mode* beside the
existing sorted-list chain. It preserves the current `ShenyuPlugin.execute(exchange, chain)`
contract verbatim.

In scope: explicit dependency constraints, conditional **exclusive** branches, convergence of
mutually exclusive paths, one serial path per request.

Out of scope (future proposals): parallel execution, fan-out/fan-in join scheduling, duplicate
same-plugin invocation on one path, arbitrary executable scripts, a universal workflow engine, and
any HTTP/RPC **API aggregation** or backend method composition. Per-request executor routing is
deferred; only deployment-level mode selection ships first.

## 2. Grounding in the current runtime

The contract is written against the code as it exists today:

- `org.apache.shenyu.web.handler.ShenyuWebHandler#handle` builds a fresh
  `DefaultShenyuPluginChain(plugins)` per subscription and runs it, optionally on a scheduler,
  wrapped by `before`/`after` chain timing.
- `DefaultShenyuPluginChain` is a mutable `index` cursor over an immutable sorted
  `List<ShenyuPlugin>`; `execute` honours `plugin.skip`, then calls `plugin.before`,
  `plugin.execute(exchange, this)`, and `plugin.after` in a `finally`.
- `ShenyuPluginChain#fork()` returns an **independent cursor over the same snapshot** for internal
  dispatch.
- Plugin ordering comes from `PluginData.sort` (else `ShenyuPlugin#getOrder()`), re-sorted on
  `PluginHandlerEvent` (ENABLED / DISABLED / DELETE / SORTED).
- Selector/rule matching and condition evaluation already exist in `shenyu-plugin-base`
  (`AbstractShenyuPlugin`, `condition/judge/PredicateJudge`, `condition/strategy/MatchStrategy`,
  `condition/data/ParameterData`) over `org.apache.shenyu.common.dto.ConditionData`.

**Invariant:** graph orchestration chooses *which plugins run and in what order*; it never replaces
a plugin's own selector/rule matching. A graph edge is not a rule.

## 3. Terminology

| Term | Meaning |
|------|---------|
| Graph definition | Versioned, editable source-of-truth for a graph (draft → published revision). |
| Revision | Immutable, monotonically numbered publication of a graph definition. |
| Plan | Compiled, immutable, request-path-free form of a revision (validated + bound successors). |
| Executor | One of two runtimes: `LEGACY` (sorted cursor) or `GRAPH` (plan traversal). |
| Dispatcher | Selects the executor **before any** `ShenyuPlugin` runs and pins it for the subscription. |
| Node | A graph vertex: `PLUGIN`, `CHOICE`, or `END`. |
| Wrapper scope | The sub-path a wrapping plugin must fully enclose to preserve its semantics. |

## 4. Versioned graph schema

A graph definition carries the following fields. Types are indicative; the DTOs are defined in
`shenyu-common` by #7995.

| Field | Type | Required | Notes |
|-------|------|----------|-------|
| `schemaVersion` | int | yes | Contract version of this document; validators reject unknown majors. |
| `namespaceId` | string | yes | Must match the deployment's configured namespace. |
| `deploymentBinding` | string | no | Optional explicit deployment/group id; else namespace-wide. |
| `graphId` | string | yes | Stable identity across revisions. |
| `revision` | long | yes | Monotonic per `graphId`; activation ordering key. |
| `contentHash` | string | yes | Hash of the normalized node/edge content; change detection + idempotent sync. |
| `entryNodeId` | string | yes | Must reference a node in `nodes`. |
| `nodes` | list<Node> | yes | Non-empty; unique `nodeId`s. |

`Node` fields:

| Field | Type | Applies to | Notes |
|-------|------|-----------|-------|
| `nodeId` | string | all | Unique within the graph. |
| `type` | enum `PLUGIN\|CHOICE\|END` | all | See §5. |
| `pluginName` | string | PLUGIN | Binds an installed plugin **by name** (`ShenyuPlugin#named()` / `PluginData.name`). |
| `next` | string (nodeId) | PLUGIN | Exactly one successor. |
| `conditions` | list<Branch> | CHOICE | Ordered; first match wins. |
| `defaultTarget` | string (nodeId) | CHOICE | Required fallback when no condition matches. |
| `required` | bool | PLUGIN | If true, the plugin is a mandatory dependency (fail-closed, §12). |

`Branch` = `{ List<ConditionData> conditions; MatchStrategy match; String target; }` — reusing the
existing `ConditionData` model so operators author conditions exactly as they do for selectors/rules.

## 5. Node model and semantics

- **PLUGIN** — binds one installed plugin by name and has exactly one successor (`next`). At runtime
  the executor invokes `plugin.skip`, `before`, `execute(exchange, continuation)`, `after`, where the
  `continuation` executes **the successor node** rather than advancing a shared cursor. The plugin's
  own selector/rule matching is untouched.
- **CHOICE** — evaluates `conditions` **in declaration order** and selects the **first** branch whose
  conditions match (under the branch's `MatchStrategy`); if none match it selects `defaultTarget`.
  `defaultTarget` is mandatory, which makes selection total and deterministic. CHOICE has no plugin
  side effects of its own.
- **END** — terminal; completes the traversal. Every reachable path must terminate at an `END`
  (or at a plugin that owns/commits the response, e.g. a proxy — see the audit).

## 6. Condition semantics and determinism

- Conditions reuse `ConditionData` (`paramType`/`operator`/`paramName`/`paramValue`) and are
  evaluated by the existing `PredicateJudge`/`MatchStrategy` SPI in `shenyu-plugin-base` at request
  time (in the GRAPH runtime, #7996). #7995 only validates and *compiles* conditions structurally.
- **Determinism rules (validated by #7995):** ordered branches, first-match wins, mandatory
  `defaultTarget`, all branch targets reference existing nodes, and no two branches are
  byte-identical duplicates (ambiguous authoring).

## 7. Phase-1 structural restrictions

The audit (§9 and the companion doc) shows that arbitrary DAG re-convergence makes *path-wise*
guarantees (no duplicate plugin on a path; well-defined wrapper scope) expensive and error-prone.
Phase 1 therefore **restricts the graph shape** so that path analysis stays polynomial:

1. Exactly one `entryNodeId`.
2. A **common prefix** of zero or more PLUGIN nodes from the entry.
3. Zero or more **CHOICE** nodes whose branches are **simple chains** (no nested re-convergence
   inside a branch); a branch may itself contain a CHOICE, but branches must not merge back into a
   shared node other than a single, well-defined **convergence point**.
4. A **common suffix** (may be empty) leading to `END`.
5. No node is reachable by two different CHOICE branches at the same convergence unless it is the
   designated convergence point (exclusive-branch + single-convergence model).

Consequence: the set of distinct root→END paths equals the product of branch fan-outs, but because
branches are simple chains that converge once, **path-wise duplicate/scope checks reduce to checking
each chain segment plus the shared prefix/suffix** — polynomial in graph size. Arbitrary DAGs are a
future extension once the audit and runtime mature.

## 8. Graph limits

Validated by #7995; defaults are conservative and configurable. Rationale: bound compile cost,
plan memory, retained-revision memory, and internal-dispatch recursion.

| Limit | Default | Purpose |
|-------|---------|---------|
| max nodes per graph | 128 | bound validation/compile + plan size |
| max out-degree (CHOICE branches) | 16 | bound branch fan-out |
| max conditions per branch | 32 | bound condition compile |
| max path length (prefix+branch+suffix) | 64 | bound traversal depth |
| max fork/internal-dispatch recursion depth | 8 | bound `fork()` re-entry |
| max retained revisions per graph | 8 | bound admin/gateway memory |
| max graphs per namespace | 32 | bound sync + registry cardinality |

## 9. Plugin requirements and wrapper-scope classes

Plugins fall into wrapper-scope classes that determine whether they may live inside a branch or must
sit in the common prefix/suffix. The full per-category matrix and representative deep-dives are in
[plugin-graph-compatibility-audit.md](./plugin-graph-compatibility-audit.md); the classes are:

| Class | Examples | Placement rule |
|-------|----------|----------------|
| Global / context init | `GlobalPlugin` (builds `ShenyuContext`) | **Mandatory common prefix**; cannot be branched or skipped. |
| Auth / security | sign, jwt, oauth2, basic-auth, key-auth, waf | **Common prefix**; mandatory deps fail closed; never bypassed by graph config. |
| Wrapper (observability/resilience) | tracing, metrics, fault-tolerance, logging | **Wrapper scope**: must fully enclose the sub-path it measures; if it wraps the whole pipeline it belongs in the common prefix/suffix, not inside one branch. |
| Response owner / writer | response plugin, cache response writer | **Common suffix**; exactly one response owner per path. |
| Transform | context-path, rewrite, param-mapping, request, modify-response, redirect, uri | Branch-safe if idempotent and not duplicated on a path; order-sensitive. |
| Proxy / terminal | divide+httpclient, dubbo, grpc, spring-cloud, sofa, tars, websocket, mqtt | Exclusive per path; side-effecting (upstream calls) — must not be duplicated or re-executed on fallback. |
| Short-circuit | mock | Branch-safe terminal alternative. |

**Decision (initial supported graphs):** enforce a structured common prefix (global + auth + any
whole-pipeline wrapper) and common suffix (response owner), with CHOICE branches limited to
transform/proxy/short-circuit alternatives. This is the restriction that keeps §7 polynomial and
preserves wrapper semantics.

## 10. Dual-executor contract

Orchestration is a **separate execution mode**, never a plugin inserted into the legacy chain and
never an extension of its mutable cursor.

```text
Request / existing shared WebFilters
  -> execution-mode dispatcher (runs BEFORE any ShenyuPlugin)
       -> LEGACY : existing sorted List<ShenyuPlugin> + DefaultShenyuPluginChain cursor
       -> GRAPH  : immutable compiled GraphPlan + independent GraphExecution state
     (both executors reuse the SAME ShenyuPlugin instances)
```

Rules:

1. **Dispatch before plugins.** The dispatcher chooses the executor before the first `ShenyuPlugin`
   runs. No shared plugin prefix is executed and then re-entered by an executor.
2. **Deployment-level mode.** Initial selection is configuration, e.g.
   `shenyu.execution.mode=legacy|graph` and `shenyu.execution.graph-id`; `legacy` is the default.
   Namespace graph binding must match the deployment's namespace. Per-request routing is deferred.
3. **Pin per subscription.** The chosen executor is pinned for the whole subscription/request
   execution and for anything it `fork()`s.
4. **Separate lifecycle ownership.** Each executor owns its complete plugin lifecycle: exactly one
   global/context init, one auth wrapper pass, one proxy, one response owner per request.
5. **No graph state in the legacy cursor.** `DefaultShenyuPluginChain` and its `index` are unchanged;
   graph traversal state lives only in `GraphExecution`. Legacy sorting/semantics are untouched
   (any minimal entry refactoring to introduce the dispatcher must carry regression coverage).
6. **Shared WebFilters.** Existing `WebFilter`s remain shared and run before the dispatcher.
7. **Graph-not-ready rejection.** A GRAPH deployment without a ready required plan **rejects** the
   request (and affects readiness); it never silently runs LEGACY.
8. **No implicit fallback / no mid-request switch.** A failed graph execution never restarts on the
   legacy chain (that could duplicate upstream calls and side effects). Mode changes affect **new
   requests only**; in-flight requests, long-lived streams and forks keep their original
   executor/plan.
9. **fork() identity.** `fork()` inherits the executor kind and the structural plan snapshot and
   starts independent execution from the entry without consulting the global dispatcher again.
10. **Rollback.** Operational rollback to LEGACY is an explicit deployment configuration change (or
    restart). Graph *revision* rollback (publish old content under a new revision) is a **different**
    operation from executor-mode rollback. An optional live mode switch, if ever added, applies
    atomically to new requests only.

### 10.1 Sequence — LEGACY (unchanged)

```mermaid
sequenceDiagram
    participant WF as Shared WebFilters
    participant D as Mode Dispatcher
    participant H as ShenyuWebHandler
    participant C as DefaultShenyuPluginChain (cursor)
    participant P as ShenyuPlugin[i]
    WF->>D: request
    D->>D: mode = LEGACY (deployment config)
    D->>H: handle(exchange)
    H->>C: new chain(plugins).execute(exchange)
    loop index < plugins.size
        C->>P: skip? before; execute(exchange, chain); after
        P-->>C: Mono<Void> (may call chain.execute / chain.fork)
    end
    C-->>H: Mono.empty()
    H-->>WF: response
```

### 10.2 Sequence — GRAPH (new)

```mermaid
sequenceDiagram
    participant WF as Shared WebFilters
    participant D as Mode Dispatcher
    participant G as GraphExecution (per-request state)
    participant PL as Compiled GraphPlan (immutable)
    participant P as ShenyuPlugin (reused)
    WF->>D: request
    D->>D: mode = GRAPH; pin plan snapshot
    alt no ready required plan
        D-->>WF: reject (no legacy fallback)
    else plan ready
        D->>G: start(entryNodeId, exchange, plan)
        loop traverse successor / CHOICE branch
            G->>P: skip? before; execute(exchange, continuation->successor); after
            P-->>G: Mono<Void> (continuation runs the bound successor)
        end
        G-->>WF: END -> response (single owner)
    end
```

## 11. Repeated continuation and resubscription semantics

**Decision:** the continuation passed to a plugin executes its bound successor. Whether a plugin may
invoke the continuation more than once (or resubscribe the returned `Mono`) is **per the compatibility
contract**, not blanket-cached:

- Plugins that legitimately resubscribe (retry / failover in `shenyu-plugin-httpclient`) keep working:
  the GRAPH executor must not insert a silent `cache()` that would change retry semantics.
- The audit records, per plugin, whether it (a) calls the continuation exactly once, (b) may call it
  multiple times (retry), or (c) wraps it (decorator). Phase 1 supports (a) and (b); a plugin that
  would re-enter a side-effecting proxy successor more than once is flagged and restricted to a
  single-execution position.
- Repeated **subscription** of the request `Mono` re-runs traversal from the entry with fresh
  per-subscription state (matching today's `Mono.defer` behaviour in `ShenyuWebHandler#handle`).

## 12. Mandatory dependencies fail closed

**Decision:** a PLUGIN node marked `required` (and any plugin the audit classifies as a mandatory
security prerequisite) is a hard dependency. If it is missing, disabled, or unavailable at activation
time, the GRAPH plan is **rejected** for new requests rather than executed without it. Preserving an
old plan must not bypass an explicit disable. Graph configuration can never be used to skip mandatory
authentication. (Runtime enforcement and lifecycle re-evaluation are #7999; this contract fixes the
semantics.)

## 13. Legacy fallback

**Decision:** none. Graph mode never falls back to the legacy chain — not on missing plan, not on
compile failure, not on in-flight execution error. Rationale: cross-executor fallback can duplicate
upstream calls and side effects and hides misconfiguration. LEGACY deployments are unaffected by
graph publication/validation state.

## 14. Runtime configuration update

- Graph revisions are published immutably and synced (WebSocket/HTTP first, #7998) with
  full/incremental state, digests, deletion, reconnect and out-of-order/duplicate tolerance.
- The gateway compiles candidates **before** an atomic plan swap; each request holds one topology
  snapshot. A topology snapshot does **not** freeze selector/rule/plugin caches — those keep their
  existing live-update behaviour (documented limitation).
- Publication alone never changes executor mode; receiving/publishing a graph never switches a LEGACY
  deployment to GRAPH.

## 15. Unsupported-plugin behavior

A plugin that the audit marks as **graph-incompatible** for Phase 1 (e.g. one that mutates the shared
cursor, or cannot preserve wrapper scope under branching) is rejected at validation when referenced by
a graph node, with an actionable error naming the plugin and node id. Such plugins remain fully usable
in LEGACY mode. The supported-plugin list and constraints are published with the compatibility matrix.

## 16. Schema examples

### 16.1 Valid — dependency-only linear graph (compiles to a linear plan)

```json
{
  "schemaVersion": 1,
  "namespaceId": "ns-prod",
  "graphId": "g-linear",
  "revision": 3,
  "contentHash": "a1b2c3",
  "entryNodeId": "n-global",
  "nodes": [
    { "nodeId": "n-global", "type": "PLUGIN", "pluginName": "global", "required": true, "next": "n-sign" },
    { "nodeId": "n-sign",   "type": "PLUGIN", "pluginName": "sign",   "required": true, "next": "n-divide" },
    { "nodeId": "n-divide", "type": "PLUGIN", "pluginName": "divide", "next": "n-end" },
    { "nodeId": "n-end",    "type": "END" }
  ]
}
```

### 16.2 Valid — exclusive CHOICE branch with convergence

```json
{
  "schemaVersion": 1,
  "namespaceId": "ns-prod",
  "graphId": "g-choice",
  "revision": 1,
  "contentHash": "d4e5f6",
  "entryNodeId": "n-global",
  "nodes": [
    { "nodeId": "n-global", "type": "PLUGIN", "pluginName": "global", "required": true, "next": "n-choice" },
    { "nodeId": "n-choice", "type": "CHOICE", "defaultTarget": "n-http",
      "conditions": [
        { "match": "all", "target": "n-grpc",
          "conditions": [ { "paramType": "req_method", "operator": "match", "paramName": "", "paramValue": "grpc" } ] }
      ] },
    { "nodeId": "n-grpc", "type": "PLUGIN", "pluginName": "grpc",   "next": "n-resp" },
    { "nodeId": "n-http", "type": "PLUGIN", "pluginName": "divide", "next": "n-resp" },
    { "nodeId": "n-resp", "type": "PLUGIN", "pluginName": "response", "next": "n-end" },
    { "nodeId": "n-end",  "type": "END" }
  ]
}
```

### 16.3 Invalid examples (each must be rejected with the node id)

| Defect | Why rejected |
|--------|--------------|
| `entryNodeId` references a missing node | reference integrity |
| PLUGIN `next` → unknown node | dangling reference |
| CHOICE without `defaultTarget` | non-total / non-deterministic selection |
| `n-a → n-b → n-a` | cycle |
| node not reachable from entry | unreachable node |
| same proxy plugin on prefix **and** a branch | duplicate side-effecting plugin on a path |
| `nodes.size` > max | resource limit |
| a `required` plugin absent from the catalog | mandatory dependency (fail closed) |
| branch re-converges into two different shared nodes | violates §7 single-convergence restriction |

## 17. Acceptance mapping (#7994)

| Acceptance item | Where addressed |
|-----------------|-----------------|
| reviewed contract + compatibility matrix | this doc + [audit](./plugin-graph-compatibility-audit.md) |
| representative wrapper/response/retry cases | audit deep-dives (Sentinel-style wrapper, response owner, httpclient retry) |
| explicit decisions: repeated continuations, mandatory deps, legacy fallback | §11, §12, §13 |
| schema examples and invalid examples | §16 |
| dual-executor contract, dispatch-before-plugin, separate lifecycle, no graph state in cursor | §10 |
| namespace/deployment binding + bootstrap boundary | §4, §10.2 |
| graph-not-ready rejection, fork() executor identity, deployment/restart rollback, optional live switch = new requests only | §10 (rules 3,7,8,9,10) |
| sequence diagrams for both executors | §10.1, §10.2 |
| ownership of global/auth/proxy/response plugins | §9 |
| no automatic mode change on publication; explicit no-fallback / no-mid-request-switch | §10 (rules 8,10), §13, §14 |

## 18. Open questions for review

1. Are the §7 structural restrictions acceptable for Phase 1, or is arbitrary-DAG support required
   before adoption (at the cost of exponential path analysis)?
2. Should `deploymentBinding` be mandatory (safer canary story) or optional (namespace-wide default)?
3. Is the default limit set (§8) reasonable, and which limits must be hot-reconfigurable?
4. Do we want a machine-readable JSON Schema published alongside these examples in Phase 1?
