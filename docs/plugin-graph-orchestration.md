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

The corrections for #8010, #8011, #8013, #8014, #8015 and #8018 below are **proposed
contract requirements**, not evidence that a GRAPH compiler or executor exists. Their runtime
acceptance tests remain part of the owning implementation tasks.

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
| `required` | bool | PLUGIN | If true, installed/enabled dependency availability is mandatory; this does not establish request authentication (§12). |

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
- **END** — structural terminal; completes traversal. Every structural path must reach an `END`.
  A plugin may finish a request without invoking its successor (cache hit, mock, redirect, rejection).
  That runtime short circuit does not remove its structural `next` edge or prove that a normal
  forwarding path has all the capabilities required to produce a response (§9).

## 6. Condition semantics and determinism

- Conditions reuse `ConditionData` (`paramType`/`operator`/`paramName`/`paramValue`) and are
  evaluated by the existing `PredicateJudge`/`MatchStrategy` SPI in `shenyu-plugin-base` at request
  time (in the GRAPH runtime, #7996). #7995 only validates and *compiles* conditions structurally.
- **Determinism rules (validated by #7995):** ordered branches, first-match wins, mandatory
  `defaultTarget`, all branch targets reference existing nodes, and no two branches are
  byte-identical duplicates (ambiguous authoring).
- Resolve parameter extractors, operators and match strategies against the installed SPI during
  compilation; reject unknown names with the CHOICE node and branch index. Current strategy names
  are `and` and `or`, not `all`; the `=` operator resolves to the `equals` predicate.
- `req_method` reads the incoming HTTP method, such as `GET` or `POST`. It does not read or change
  `ShenyuContext.rpcType`. Choosing a `grpc`/`dubbo` node does not convert HTTP, resolve RPC metadata
  or prepare RPC arguments. Protocol-specific paths must meet their own context and capability
  requirements. A skipped protocol plugin has not performed an invocation.

## 7. Phase-1 structural restrictions

Phase 1 proposes a **structured CHOICE/convergence shape** to bound the supported semantics and make
authoring and wrapper-scope diagnostics tractable. This is a product/scope choice, not a claim that
arbitrary-DAG validation necessarily requires exponential path enumeration:

1. Exactly one `entryNodeId`.
2. A **common prefix** of zero or more PLUGIN nodes from the entry.
3. Zero or more **CHOICE** nodes whose branches are **simple chains** (no nested re-convergence
   inside a branch); a branch may itself contain a CHOICE, but branches must not merge back into a
   shared node other than a single, well-defined **convergence point**.
4. A **common suffix** (may be empty) leading to `END`.
5. No node is reachable by two different CHOICE branches at the same convergence unless it is the
   designated convergence point (exclusive-branch + single-convergence model).

### 7.1 Bounded analysis, without enumerating paths

For `V` nodes, `E` edges and bounded plugin/capability bitsets of width `K`:

1. Validate limits and references before allocating the plan. Build adjacency and predecessor lists.
   Use iterative reachability and Kahn topological sorting to reject unreachable nodes and cycles;
   these traversals take `O(V + E)` time and storage. Compute longest-path length in topological
   order; do not recurse over graph depth.
2. Propagate **may-have-run** plugin identities using predecessor **union**. If a PLUGIN identity
   is already in its incoming set, reject a possible duplicate on a path, then add its identity to
   the outgoing set. The same plugin in two mutually exclusive branches is not itself a duplicate:
   neither branch's node has the other in its incoming set. A later repeat after convergence is.
3. Propagate **must-have** capabilities using predecessor **intersection**, then apply the node's
   guaranteed effects on continuing executions. Check consumer requirements before adding effects.
   A producer that may skip or miss its selector/rule contributes no unconditional guarantee.
   Use an explicit runtime prerequisite guard for an otherwise legal conditional producer; reject
   a graph if neither a proof nor a supported guard exists. Node presence alone is insufficient.
4. Treat terminal short-circuit outcomes separately from continuing outcomes: a cache hit does not
   reach the HTTP client; a cache miss must still supply all of that client's prerequisites.
   Unknown branch feasibility is conservative (consider every declared edge). Do not infer
   authentication or successful invocation from a plugin's name or `Mono<Void>` completion.
5. Validate structured convergence and declared wrapper coverage separately. A dominator analysis
   can check that a required wrapper precedes every protected invocation; it must also check which
   downstream publisher is actually enclosed (§9 and audit §4.2). A join is not a wrapper-completion
   boundary. Reject unsupported shapes without attempting to enumerate logical paths.

The union/intersection passes take `O((V + E) * K)` bit operations and `O(V * K + E)` storage;
`K` is bounded by the installed catalog and compiler capability vocabulary. Any additional scope
analysis must have its own graph-size-based bound. A chain of diamonds may have exponentially many
logical paths, but must be processed using the bounded node/edge representation. This does not
expand Phase-1 acceptance to arbitrary DAGs.

## 8. Graph limits

Validated by #7995; defaults are conservative and configurable. Rationale: bound compile cost,
plan memory, retained-revision memory, and internal-dispatch recursion.

| Limit | Default | Purpose |
|-------|---------|---------|
| max nodes per graph | 128 | bound validation/compile + plan size |
| max edges per graph | 512 | bound adjacency, predecessor and analysis storage |
| max out-degree (including CHOICE default) | 16 | bound branch fan-out |
| max conditions per branch | 32 | bound condition compile |
| max conditions per graph | 1024 | bound aggregate condition compile |
| max path length (prefix+branch+suffix) | 64 | bound traversal depth |
| max fork/internal-dispatch recursion depth | 8 | bound `fork()` re-entry |
| max retained revisions per graph | 8 | bound admin/gateway memory |
| max graphs per namespace | 32 | bound sync + registry cardinality |

Check aggregate bounds before materializing compiled predicates. Also bound source bytes, condition
string lengths, installed catalog size and retained compiled-plan bytes in the implementation;
defaults require measurement. Errors name the graph/node/branch and exceeded limit. Limit checks
must not rely on enumerating paths, recursive DFS or request-controlled recursion.

## 9. Plugin capabilities and wrapper scope

A node's name or category is not a proof of its request-time effects. For a normal HTTP forwarding
path, the cooperating roles are:

| Role | Representative producer/consumer | Requirement on continuing executions |
|------|----------------------------------|--------------------------------------|
| Context | `global` builds `ShenyuContext` | Must precede context readers; establishes protocol metadata, not authentication. |
| Target selection | `divide` selects upstream and sets `HTTP_DOMAIN` / invocation policy | Requires matching configuration and a usable upstream; otherwise may reject or continue without a target. |
| URI construction | `uri` consumes `HTTP_DOMAIN`, produces `HTTP_URI` | Missing/blank domain does not produce a URI. |
| Transport invocation | **one of** `nettyHttpClient` / `webClient` consumes `HTTP_URI` | Supplies the response state consumed by the corresponding writer; transport retries stay inside this stage. |
| Response decoration | `cache` on miss, `modifyResponse`, response transformers | Installs a decorator before the downstream write; is not an additional independent commit. |
| Response commitment | `response` dispatches the protocol-specific `MessageWriter` | Requires compatible response state; commits the normal response once. |
| Zero-call terminal outcome | cache hit, `mock`, `redirect`, policy rejection | Completes without invoking the structural successor; no upstream result is required on that outcome. |

`divide -> uri -> nettyHttpClient -> response` is therefore a legal cooperating sequence, not four
mutually exclusive proxies. The compiler must reject a missing producer, wrong order, incompatible
protocol, or duplicated invocation/commit role with node IDs. Conditional producers require the
runtime prerequisite guards described in §7.1; `required: true` alone supplies none of these effects.
"One invocation stage" does not promise exactly one network attempt: configured transport retry may
make several attempts, and provides no exactly-once guarantee for remote side effects. A successful
cache miss with retries disabled calls the upstream once; a hit calls it zero times. Cache coalescing
may also let a concurrent follower finish without its own upstream call.

Wrapper scope is the **downstream publisher actually enclosed** by the plugin. A wrapper before a
CHOICE encloses the selected branch and its suffix. A wrapper inside one branch encloses that branch's
remaining nodes **and the common suffix**, only for requests selecting that branch. A wrapper after
convergence cannot retroactively protect prefix/branch work. Convergence does not complete or unwind
an enclosing publisher. Fallback, errors, cancellation and cleanup follow that actual lifetime.

**Phase-1 scope proposal:** require whole-region wrappers to precede every protected node and cover
every protected request. Initially exclude branch-local wrappers until the scope tests in audit §4.2
exist; this is an explicit support limitation, not an inherent incompatibility. Response decorators
may be in a suffix if it precedes every write they must decorate. Auth dependencies are availability
constraints; any stronger request policy follows §12. Protocol-specific invocation paths require
separate capability audits and cannot be enabled by relabeling an HTTP branch.

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
4. **Separate traversal ownership.** The selected executor owns request traversal and its continuation
   state. Both modes reuse the installed plugin catalog/instances and existing configuration lifecycle;
   graph publication must not duplicate plugin initialization or destruction. GRAPH validates its
   context, dependency, invocation and response capabilities (§9); it does not require an upstream
   call on short-circuit outcomes or infer authentication from an auth node.
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

The method signature alone does not preserve continuation semantics. LEGACY advances a mutable
cursor; a naive successor-bound GRAPH publisher could restart downstream nodes on every subscription.
These five cases must be specified and tested independently:

| Case | Proposed GRAPH behavior | Required observation |
|------|-------------------------|----------------------|
| Repeated calls to the same `continuation.execute(exchange)` | One call per plugin invocation; reject a second before executing successor work. | No duplicate successor invocation or response write, including concurrent calls. |
| Repeated subscription to the returned successor publisher | One subscription per continuation; fail a second before successor side effects. | No silent replay/cache and no restart of an already used downstream region. |
| Transport-internal retry/failover | Keep the HTTP client's existing bounded request retry inside its response publisher. | N transport attempts, then one downstream continuation on success; none on terminal failure. |
| Fresh subscription at the request entry | Fresh traversal state and CHOICE evaluation, as with current `ShenyuWebHandler`'s `Mono.defer`. | Do not share visited/continuation state; this is not permission to replay a consumed body or committed exchange. |
| `fork()` child dispatch | Independent traversal from entry, pinned executor/plan identity, bounded recursion. | Separate child exchange/traversal; no parent cursor reuse or dispatcher re-selection. |

The one-shot continuation is a **proposed compatibility restriction**, not implemented behavior or
an assertion that all existing plugins are compatible. Reject plugins requiring successor replay
until concrete tests establish a supported contract. Do not insert blanket `cache()` or manually
subscribe/detach from cancellation. The invocation guard and subscription guard must be atomic and
separate: returning a publisher is not the same event as subscribing to it.

`AbstractHttpClientPlugin` constructs/retries its transport response publisher and only then calls
`chain.execute(exchange)` in `flatMap`. Its transport retry does **not** require repeated subscription
to the successor graph. GRAPH must preserve that retry boundary rather than retry the entire plugin
output. An explicitly supported fresh entry/fork evaluates CHOICE against that attempt's exchange;
transport retry stays in the selected path and does not re-evaluate graph CHOICE. Tests must account
for live plugin caches, body reuse limits, fallback, cancellation and response commitment. LEGACY's
cursor behavior remains unchanged.

## 12. Required availability and request-time security

`required: true` is an **availability dependency**. A missing, disabled or unavailable required plugin
rejects activation/new request execution; preserving an old plan must not bypass an explicit disable.
Lifecycle re-evaluation belongs to #7999. This flag neither proves selector/rule matching nor proves
that the request authenticated. `AbstractShenyuPlugin` can continue on disabled/missing configuration
or a selector/rule miss, and `plugin.skip` may bypass execution. A required, enabled JWT node with no
matching rule must never be reported as successful authentication.

If GRAPH offers enforced authentication, that is a separate explicit policy contract:

- Mandatory policy comes from independently administered deployment/namespace security policy, not
  solely from graph-author-controlled nodes/flags. Reject omission of its required checks even when
  the graph author removes a node or clears `required`.
- Use a typed, request-scoped result (at least unknown/no-match/skipped, authenticated, rejected and
  explicitly public) produced by an audited authentication adapter. Never infer it from node presence
  or successful `Mono<Void>` completion. The policy decides which alternative identities satisfy it.
- Unknown/no-match/skip fails a mandatory authentication policy; explicit rejection terminates;
  a public route is allowed only by authoritative public-route policy. Plugin-disabled handling is
  an availability failure, not a successful public/authenticated outcome.
- Define live policy invalidation and test skip/no-match/disabled/success/rejection/public routes
  before advertising enforcement. A topology snapshot does not freeze live selector/rule caches.

These adapters and policies are requirements for a future enforced mode, not functionality supplied
by this document. Until implemented, GRAPH may claim only dependency availability. Do not silently
change LEGACY matching/authentication behavior.

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

These are **authoring examples**, not executable deployment fixtures. Replace `contentHash` with the
publisher's normalized-content digest. Install/enable the named plugins and configure matching
sign/Divide/transform rules, a healthy HTTP upstream and a compatible HTTP client/writer. The proposed
compiler/runtime must enforce the conditional capability guards in §7.1. No forwarding test is
claimed by these JSON examples.

### 16.1 Complete linear HTTP pipeline

```json
{
  "schemaVersion": 1,
  "namespaceId": "ns-prod",
  "graphId": "g-linear",
  "revision": 3,
  "contentHash": "<computed-from-normalized-content>",
  "entryNodeId": "n-global",
  "nodes": [
    { "nodeId": "n-global", "type": "PLUGIN", "pluginName": "global", "required": true, "next": "n-sign" },
    { "nodeId": "n-sign", "type": "PLUGIN", "pluginName": "sign", "required": true, "next": "n-divide" },
    { "nodeId": "n-divide", "type": "PLUGIN", "pluginName": "divide", "required": true, "next": "n-uri" },
    { "nodeId": "n-uri", "type": "PLUGIN", "pluginName": "uri", "required": true, "next": "n-client" },
    { "nodeId": "n-client", "type": "PLUGIN", "pluginName": "nettyHttpClient", "required": true, "next": "n-response" },
    { "nodeId": "n-response", "type": "PLUGIN", "pluginName": "response", "required": true, "next": "n-end" },
    { "nodeId": "n-end", "type": "END" }
  ]
}
```

### 16.2 Same-protocol CHOICE with a complete common HTTP suffix

The GET branch selects `rewrite`; other methods select `request`. Both converge on the same
`divide -> uri -> nettyHttpClient -> response` pipeline. This changes middleware selection, not the
protocol established by `global`. `and`, `req_method` and `=` resolve through the existing condition
SPI/factory. The `sign` node's required flag is only an availability dependency (§12).

```json
{
  "schemaVersion": 1,
  "namespaceId": "ns-prod",
  "graphId": "g-choice",
  "revision": 1,
  "contentHash": "<computed-from-normalized-content>",
  "entryNodeId": "n-global",
  "nodes": [
    { "nodeId": "n-global", "type": "PLUGIN", "pluginName": "global", "required": true, "next": "n-sign" },
    { "nodeId": "n-sign", "type": "PLUGIN", "pluginName": "sign", "required": true, "next": "n-choice" },
    { "nodeId": "n-choice", "type": "CHOICE", "defaultTarget": "n-request",
      "conditions": [
        { "match": "and", "target": "n-rewrite",
          "conditions": [ { "paramType": "req_method", "operator": "=", "paramName": "", "paramValue": "GET" } ] }
      ] },
    { "nodeId": "n-rewrite", "type": "PLUGIN", "pluginName": "rewrite", "next": "n-divide" },
    { "nodeId": "n-request", "type": "PLUGIN", "pluginName": "request", "next": "n-divide" },
    { "nodeId": "n-divide", "type": "PLUGIN", "pluginName": "divide", "required": true, "next": "n-uri" },
    { "nodeId": "n-uri", "type": "PLUGIN", "pluginName": "uri", "required": true, "next": "n-client" },
    { "nodeId": "n-client", "type": "PLUGIN", "pluginName": "nettyHttpClient", "required": true, "next": "n-response" },
    { "nodeId": "n-response", "type": "PLUGIN", "pluginName": "response", "required": true, "next": "n-end" },
    { "nodeId": "n-end", "type": "END" }
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
| HTTP client without a URI producer or supported prerequisite guard | missing `HTTP_URI` capability at the client node |
| `divide -> response` as a normal HTTP forwarding path | missing URI, invocation and compatible upstream result |
| both HTTP clients on one continuing path | duplicate invocation stage |
| CHOICE using `match: all` or an unknown extractor/operator | unavailable condition SPI |
| whole-region wrapper after a protected invocation | wrapper cannot cover already-executed work |
| enabled enforced-auth policy omitted by graph author | independent security-policy requirement, not the `required` flag |

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
| context, availability, invocation, decoration and response roles | §9, §12 |
| no automatic mode change on publication; explicit no-fallback / no-mid-request-switch | §10 (rules 8,10), §13, §14 |

## 18. Open questions for review

1. Are the §7 structured-convergence restrictions acceptable for Phase 1, or is broader DAG support
   required? The decision concerns supported semantics; §7.1 does not enumerate logical paths.
2. Should `deploymentBinding` be mandatory (safer canary story) or optional (namespace-wide default)?
3. Is the default limit set (§8) reasonable, and which limits must be hot-reconfigurable?
4. Do we want a machine-readable JSON Schema published alongside these examples in Phase 1?

## 19. Follow-up verification (not completed by a document change)

| Issue | Contract correction | Implementation evidence still required |
|-------|---------------------|----------------------------------------|
| #8010 | §9 and complete HTTP examples | Test upstream forwarding, cache hit/miss/coalescing, missing URI/result and duplicate call/write rejection. |
| #8011 | §6 and same-protocol CHOICE | HTTP/RPC skip and metadata cases, real SPI resolution and valid branch selection. |
| #8013 | §9 and audit §4.2 | Actual protected region, downstream failures, fallback and cleanup/cancellation. |
| #8014 | §12 | Availability versus typed authentication outcomes, public routes and live authoritative policy changes. |
| #8015 | §11 | All five execution cases, atomic guards, transport/downstream counts and cancellation. |
| #8018 | §7.1 and §8 | Layered diamonds within bounds; exclusive duplicate distinction; must-capability intersection and useful limit errors. |

Keep these implementation acceptance items open until the relevant compiler/runtime tests exist.
