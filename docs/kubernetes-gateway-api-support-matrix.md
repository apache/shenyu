<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements.  See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License.  You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Kubernetes Gateway API support matrix

This document addresses #8067. It is the single source of truth for **what the ShenYu Kubernetes
Gateway API controller supports, on which versions, and what it explicitly does not support**.
It also records the limits of the claim itself: which parts have been exercised by tests, and
which parts have only been implemented but not yet certified.

Read it as three separate statements:

1. **Tested versions** — only the combinations in the table in [§1](#1-tested-versions) have
   been run. Anything else is untested, not "probably fine".
2. **Implemented, not necessarily tested** — [§3](#3-resource-support) to
   [§5](#5-what-happens-to-unsupported-input) describe what the code does. They were verified by
   reading the reconcilers and parsers and by the unit tests in `shenyu-kubernetes-controller`;
   only one end-to-end HTTPRoute case runs in CI, so "implemented" is a weaker claim than
   "tested".
3. **Conformance** — no official Gateway API conformance run has been recorded yet, so this
   controller **must not be described as conformant or certified** for any profile. See
   [§6](#6-conformance-status) for the concrete work that blocks a `GATEWAY-HTTP` run.

Everything below describes `shenyu-kubernetes-controller` plus
`shenyu-spring-boot-starter-k8s` at `2.7.2-SNAPSHOT`, following #6347.

## 1. Tested versions

| Component | Version used in CI | Notes |
| --- | --- | --- |
| Kubernetes | v1.31.0 (`kindest/node:v1.31.0`) | Any other minor version is untested. |
| Gateway API CRDs | standard channel **v1.5.1** (`standard-install.yaml`) | Minimum accepted bundle version is **v1.5.0**, because `referencegrants` serve `gateway.networking.k8s.io/v1` only from there. |
| Gateway API API version used by the controller | `gateway.networking.k8s.io/v1` | Only the v1 GA surface is read; v1alpha2/v1beta1-only fields are not. |
| Java | 17 | |
| Java Kubernetes client | `client-java` 17.0.2 | |
| Spring Boot | 3.3.1 | The controller is embedded in the data-plane JVM. |
| ShenYu | 2.7.2-SNAPSHOT | |

Upstream claims in this document (specification wording, condition reasons, field support levels
and the composition of the conformance suite's core test set) were read from the Gateway API
**v1.5.1** source (`apis/v1`, `pkg/features`, `conformance/tests`) rather than from memory. If the
pinned version changes, the affected statements must be re-derived — the profile's core feature
set has changed shape between minor versions before.

Version handling at startup:

- **Fail fast**: if any of `gatewayclasses`, `gateways`, `httproutes`, `referencegrants` is not
  served at `gateway.networking.k8s.io/v1`, the bootstrap refuses to start when
  `shenyu.k8s.mode=gateway-api`. A missing CRD set is a deployment error, never a silent
  degradation.
- **Degrade, do not fail**: the CRD bundle version is read from the
  `gateway.networking.k8s.io/bundle-version` annotation and reported through the GatewayClass
  `SupportedVersion` condition. An unannotated, unreadable or older CRD bundle keeps the
  controller running best-effort and flips `SupportedVersion=False` / `UnsupportedVersion`.
- **No version is pinned at the data-plane level**: the controller only relies on the v1 GA
  surface, so a newer CRD bundle is expected to work, but only v1.5.x is *tested*.

## 2. Deployment model assumed by this matrix

The controller is not a standalone deployment. It runs inside the same JVM as the ShenYu data
plane (`shenyu.k8s.mode=gateway-api`), and writes parsed config straight into the in-process
`BaseDataCache` — no admin, no database, no data-sync channel is involved.

Consequences that the rest of this document depends on:

- **The listener port must equal `server.port`** (default `9195`). A listener on any other port
  is rejected with `PortUnavailable`; there is no port remapping. The conformance suite's base
  manifests use listener ports 80 (HTTP) and 443 (HTTPS), so this alone blocks a conformance run
  — see [§7.1](#71-listener-port-is-not-decoupled-from-the-data-plane-port-blocks-conformance).
- **Only plain HTTP listeners are served.** `HTTPS`, `TLS` and every other protocol are rejected
  with `UnsupportedProtocol`; TLS termination is expected to happen in front of the gateway. See
  [§7.2](#72-https-listeners-are-not-supported-blocks-conformance).
- **Routes from all accepted Gateways share one selector space.** Isolation between Gateways is
  therefore only as strong as the hostname/path conditions themselves.
- **Multiple replicas need no leader election**, but readiness must be gated on the
  `k8sCacheReadiness` health indicator, otherwise a cold pod serves traffic with empty config.
- Informers resync every 60s; ReferenceGrant changes additionally re-queue the affected routes
  immediately.

## 3. Resource support

### 3.1 GatewayClass

| Field / behaviour | Support |
| --- | --- |
| `spec.controllerName` | Only `gateway.shenyu.apache.org/shenyu-controller`. Other classes are ignored entirely. |
| `spec.parametersRef` | **Rejected**: `Accepted=False`, reason `InvalidParameters`. ShenYu has no class-level parameters. |
| `status.conditions[Accepted]` | Written, with `observedGeneration` and stable `lastTransitionTime`. |
| `status.conditions[SupportedVersion]` | Written from the CRD bundle version (see §1). |
| `status.supportedFeatures` | Written as a list of `{name}` entries: `Gateway`, `HTTPRoute`, `ReferenceGrant`, sorted per spec. These are exactly the `GATEWAY-HTTP` profile's core features at v1.5.1 (§6). |
| Finalizer `gateway-exists-finalizer.gateway.networking.k8s.io` | Held while at least one Gateway references the class; released when the class is deleted or stops being managed by ShenYu. |

### 3.2 Gateway and listeners

| Field / behaviour | Support |
| --- | --- |
| `spec.listeners[].protocol` | `HTTP` only. Everything else → listener `Accepted=False`, `UnsupportedProtocol`. |
| `spec.listeners[].port` | Must equal the data-plane port. Otherwise → listener `Accepted=False`, `PortUnavailable`. |
| `spec.listeners[].hostname` | Honoured; intersected with `HTTPRoute.spec.hostnames`. Exact names and `*.` suffix wildcards, which match one or more leading labels (`*.example.com` matches `a.example.com` and `a.b.example.com`). |
| `spec.listeners[].allowedRoutes.namespaces.from` | `Same` (default) and `All`. `Selector` → attachment through that listener is denied with route `Accepted=False`, `UnsupportedValue`; the listener's own status stays `Accepted=True`, so the listener status alone does not show the problem. |
| `spec.listeners[].allowedRoutes.kinds` | Honoured; absent means "all protocol-matching kinds". HTTPRoute only matches `HTTP` and `HTTPS` listeners, so a `TLS`/`TCP`/`UDP` listener permits no HTTPRoute whether or not `kinds` is declared. |
| `spec.listeners[].tls` | Not supported (no TLS listener). Ignored for HTTP listeners. |
| `spec.addresses` | **Ignored** — the controller does not bind the requested address (see §5.2). |
| `spec.infrastructure` | **Ignored** — no labels/annotations are propagated to generated resources, and `parametersRef` is not read. |
| `status.addresses` | **Not written** (gap, see §7.9). |
| `status.listeners[]` | Written: `name`, `supportedKinds`, `attachedRoutes`, `conditions[Accepted,Programmed]`. `supportedKinds` is `[HTTPRoute]` only for `HTTP`/`HTTPS` listeners and empty for `TLS`/`TCP`/`UDP`, matching which listeners can actually carry the route. |
| `status.conditions` | `Accepted` and `Programmed`. `Programmed=False` with `ListenersNotValid` when no listener is usable. |

`attachedRoutes` counts only routes actually bound to that listener, so it is accurate after
edits, deletion and restart; a Gateway deleted while routes still reference it has its parsed
config removed, and routes still bound to another ShenYu Gateway keep theirs.

### 3.3 HTTPRoute

Field-level support levels below follow the `Support:` annotations in the v1.5.1 API reference
where they exist, because that is what decides whether a gap can block core conformance.

| Area | Support |
| --- | --- |
| `spec.parentRefs[].group` / `.kind` | `gateway.networking.k8s.io` / `Gateway` (both optional). Foreign kinds are skipped without writing status, per spec. |
| `spec.parentRefs[].namespace` | Cross-namespace attachment allowed only by the listener's `allowedRoutes`; `ReferenceGrant` never applies to parentRefs. |
| `spec.parentRefs[].sectionName` | Honoured; a non-matching value reports `Accepted=False` / `NoMatchingParent`. |
| `spec.parentRefs[].port` | Honoured; a non-matching value reports `Accepted=False` / `NoMatchingParent`. |
| `spec.hostnames` | Exact names and `*.` suffix wildcards (`*.example.com` matches one or more leading labels, so both `a.example.com` and `a.b.example.com` qualify, while the bare domain `example.com` does not). Empty means "any host". Response precedence across routes deviates for wildcard/wildcard ties (§7.7). |
| `spec.rules[].matches[].path` | `Exact`, `PathPrefix` (default), `RegularExpression`. `PathPrefix` matches on path element boundaries (`/foo` does not match `/foobar`). API level: Core for Exact/PathPrefix, implementation-specific for RegularExpression. |
| `spec.rules[].matches[].method` | Honoured. API level: Extended, but exercised by a core test. |
| `spec.rules[].matches[].headers` | `Exact` and `RegularExpression`, name + value; header **names** are matched case-insensitively (Spring's header map), values case-sensitively. |
| `spec.rules[].matches[].queryParams` | `Exact` and `RegularExpression`, name + value. API level: Extended. |
| Match combination | Conditions inside one `matches` entry are AND-ed; multiple `matches` in a rule are OR-ed. CRD caps: 16 rules, 64 matches per rule, <128 matches per route, 16 headers and 16 query params per match. |
| Rule precedence | Spec order: hostname (exact > wildcard, more labels > fewer up to the six-label clamp in §7.7, none lowest), path (exact > longest prefix > regex), method present, header count, query count, then rule order. Rules are capped at 16 by the CRD, which fits the 4-bit rule-order field exactly. |
| Cross-route precedence | **Not implemented**: the spec's "oldest Route, then `namespace/name`" tie-break is missing (§7.6). |
| `spec.rules[].backendRefs[].kind` | **`Service` (core group) only.** Any other kind → `ResolvedRefs=False`, `InvalidKind`. |
| `spec.rules[].backendRefs[].port` | Required when the Service has more than one port; omitted with a single-port Service is resolved from it. |
| `spec.rules[].backendRefs[].weight` | Honoured, default `1`, `0` removes the backend from rotation. Weights are spread proportionally across endpoints, so replica count does not distort the split. |
| `spec.rules[].backendRefs[].namespace` | Requires a `ReferenceGrant`, otherwise `ResolvedRefs=False`, `RefNotPermitted` — the correct reason for this condition. |
| Backend resolution | Ready addresses from the Service's `Endpoints`; numeric and named `targetPort` are resolved via the Service spec. No ready address → `ResolvedRefs=False`, `BackendNotFound`. Services whose endpoints exist only as manually created `EndpointSlice` objects are not resolved (§7.5). |
| `spec.rules[].filters` | **Not supported at all.** Any filter on a rule removes the route's whole config and sets `Accepted=False`, `UnsupportedValue`. At v1.5.1 a core test requires `RequestHeaderModifier` and another requires `RequestRedirect` (§7.3). |
| `spec.rules[].backendRefs[].filters` | **Silently ignored** (see §5.2). The spec marks this implementation-specific, so ignoring it is permitted — but then it must be documented, which is what this row does. |
| `spec.rules[].retry` | **Silently ignored** (see §5.2). The data plane always uses 3 retries. API level: Extended. |
| `spec.rules[].timeouts` | **Silently ignored** (see §5.2). The data plane always uses a fixed 3000 ms timeout. API level: Extended. |
| `spec.rules[].sessionPersistence` | **Silently ignored** (see §5.2). Load balancing is always random. API level: Extended. |
| `spec.rules[].name` | Ignored; carries no routing semantics. API level: Extended. |
| `status.parents[]` | Written per parentRef with `controllerName`, `parentRef`, and `conditions[Accepted,ResolvedRefs]` including `observedGeneration`. |

Note on status semantics: HTTPRoute has no `Programmed` condition in the specification, and
this controller does not invent one. `Accepted=True` / `ResolvedRefs=True` means the route was
*validated and translated* into data-plane config; it is not a probe of the data plane itself.
That distinction is deliberate — do not read it as "traffic is flowing".

Note on wildcard hostnames: the API reference for `HTTPRouteSpec.Hostnames` defines `*.` as a
suffix match — "a match for `*.example.com` would match both `test.example.com`, and
`foo.test.example.com`, but not `example.com`" — so the multi-label behaviour in the two
rows above is spec-mandated rather than a ShenYu extension. The core test
`HTTPRouteHostnameIntersection` pins it in both directions: a listener hostname of
`*.wildcard.io` serves `foo.bar.wildcard.io` while the bare `wildcard.io` is rejected, and a
route hostname of `*.anotherwildcard.io` serves `foo.bar.anotherwildcard.io`.

### 3.4 ReferenceGrant

| Field / behaviour | Support |
| --- | --- |
| `spec.from` | Matched on group/kind `gateway.networking.k8s.io`/`HTTPRoute` and `namespace` (the route's namespace). |
| `spec.to` | Matched on group (`""` for core), kind (`Service`) and name (omitted = any name). |
| Status | None written — ReferenceGrant has no status per spec. |
| Change propagation | Creating or deleting a grant immediately re-queues the routes that reference the grant's namespace; revocation therefore stops traffic at once instead of at the next resync. |

## 4. Status contract

| Resource | Conditions written | `observedGeneration` |
| --- | --- | --- |
| GatewayClass | `Accepted`, `SupportedVersion` | Yes |
| Gateway | `Accepted`, `Programmed`; per-listener `Accepted`, `Programmed` | Yes |
| HTTPRoute | per-parent `Accepted`, `ResolvedRefs` | Yes |
| ReferenceGrant | none | n/a |

Invariants that the implementation maintains:

- A status patch is skipped when the desired state equals the existing state, so steady state
  causes no `resourceVersion` churn and no reconcile loop.
- `lastTransitionTime` is preserved across reconciles while the status value is unchanged.
- Conditions that carry ShenYu's verdict are only rewritten for Gateways/GatewayClasses the
  controller still manages; when ownership is lost, `Accepted` is flipped to `False` rather
  than left stale at `True`.
- `HTTPRoute.status.parents` entries written by **other** controllers are preserved verbatim;
  ShenYu only replaces its own `controllerName` entries. Multiple Gateways in the same
  `parentRefs` each get their own status entry.
- A status patch that the API server rejects is logged and **not requeued**; the route recovers
  at the next 60 s informer resync. See §7.13.

## 5. What happens to unsupported input

Unsupported input falls into three classes. The classification is deliberate and is the reason
`Accepted=True` alone is not a promise that everything in the resource is honoured.

### 5.1 Rejected — fail-closed, visible in status

| Input | Outcome |
| --- | --- |
| Route-level `filters` | Route config removed entirely, `Accepted=False` / `UnsupportedValue`. |
| Non-Service `backendRef`, missing Service/Endpoints, denied cross-namespace reference | Only that backend is dropped; its declared weight becomes a fail-target (`127.0.0.1:1`, connection refused) so the invalid share fails instead of silently re-flowing onto healthy backends. `ResolvedRefs=False` with `InvalidKind` / `BackendNotFound` / `RefNotPermitted`. |
| Listener with unsupported protocol or port | Listener `Accepted=False` with `UnsupportedProtocol` / `PortUnavailable`. The route's own reason follows the precedence in §7.4: an accepting served listener first, then `UnsupportedValue` when only an unserved listener would accept the route, then the served listener's denial, then the unserved listener's denial. |
| Route denied by listener policy (namespace or kind) | Route not attached, `Accepted=False` / `NotAllowedByListeners`, when the denying listener can attach an HTTPRoute; a `TLS`/`TCP` listener denies the route by kind regardless of its `allowedRoutes` (§7.4). |
| Route hostnames not intersecting a listener hostname | Route not attached, `Accepted=False` / `NoMatchingListenerHostname`. |
| Route whose `sectionName`/`port` matches no listener, or whose Gateway does not exist | Route not attached, `Accepted=False` / `NoMatchingParent`. Reporting a missing parent Gateway with this reason is a ShenYu extension of the definition (it names ports and sectionNames only), documented in §7.4. |
| `allowedRoutes.from=Selector` | Route not attached, `Accepted=False` / `UnsupportedValue` when the listener can attach an HTTPRoute; on a `TLS`/`TCP` listener it is a plain kind denial instead. The listener status is unaffected (§3.2). |

In all three cases the request either fails or reaches nothing: routing is never broadened, and
never silently redirected to a backend the author did not ask for.

The fail-target is effective, not cosmetic: the divide plugin defaults to retry strategy
`CURRENT` (retry the *same* upstream, `RetryEnum.CURRENT`), so a refused `127.0.0.1:1` stays
refused for all 3 retries and the request ends 5xx. The cost is four refused connect attempts on
that share of traffic, which is why it must stay a loopback address.

### 5.2 Silently ignored — documented, not rejected

The following fields are read by the CRD, accepted by the controller, **and then not honoured**.
The route remains `Accepted=True` and `ResolvedRefs=True`.

| Field | Current behaviour |
| --- | --- |
| `HTTPRoute.rules[].backendRefs[].filters` | Ignored entirely; the backend is programmed as if no filter existed. Permitted by the spec (implementation-specific support). |
| `HTTPRoute.rules[].retry` | Ignored; fixed 3 retries apply. |
| `HTTPRoute.rules[].timeouts` | Ignored; fixed 3000 ms timeout applies. |
| `HTTPRoute.rules[].sessionPersistence` | Ignored; random load balancing applies. |
| `Gateway.spec.addresses` | Ignored; the controller does not bind the requested address. |
| `Gateway.spec.infrastructure` | Ignored; nothing is propagated to generated resources. |

None of these broadens routing, so they are not correctness bugs of the kind §5.1 prevents, but
they are the weakest part of the current support claim and should be closed in one of two ways:
honour the field, or reject the route (`Accepted=False` / `UnsupportedValue`). Until then, this
table is the honest answer to "is my timeout honoured?" — and the answer is no.

### 5.3 Narrowed

Match `type` on headers and queryParams: only `Exact` and `RegularExpression` are interpreted;
any other enum value would be evaluated as case-sensitive `Exact`. At v1.5.1 those are the only
two values in the CRD, so the branch is currently unreachable — it is defensive coding for a
future enum value, not a live limitation.

### 5.4 Documented behaviour that looks like a gap but is not

These were checked against the v1.5.1 specification and are within the implementations'
discretion. They are listed so that nobody "fixes" them or re-reports them:

- **Repeated header or query parameter names**: only the first value is matched. The spec says
  the representation of a repeated header is implementation-specific, and for query params the
  behaviour "is purposely left undefined" with a *recommendation* to match the first value.
- **`RegularExpression` dialect**: matching is RE2 (`com.google.re2j`), full-match. The API
  reference marks RegularExpression support implementation-specific and asks implementations to
  document their dialect, which is exactly what this line does.
- **Path matching on the raw, undecoded path** (`%2F` stays `%2F`): the v1.5.1 API reference
  states no normalisation requirement for HTTPRoute path matching.

## 6. Conformance status

**No official conformance run has been recorded. This controller is not certified for any
profile and must not be described as such.**

### 6.1 What is declared

`GatewayClass.status.supportedFeatures` lists `Gateway`, `HTTPRoute` and `ReferenceGrant`. At
v1.5.1 the `GATEWAY-HTTP` profile's `CoreFeatures` set is exactly those three
(`conformance/utils/suite/profiles.go`), so the declaration matches the profile's core feature
set — it is a declaration of *implemented surface*, not a conformance result.

### 6.2 What a core `GATEWAY-HTTP` run actually requires

Core is not "resources only". At v1.5.1 the suite has 33 core tests, 24 of them HTTPRoute tests
including `HTTPRouteMatching`, `HTTPRouteExactPathMatching`, `HTTPRouteHeaderMatching`,
`HTTPRouteListenerHostnameMatching`, `HTTPRouteHostnameIntersection`, `HTTPRoutePathMatchOrder`,
`HTTPRouteMatchingAcrossRoutes`, `HTTPRouteWeight`, `HTTPRouteCrossNamespace`,
`HTTPRouteReferenceGrant`, `HTTPRouteObservedGenerationBump` and the invalid-input tests. Most of
those ShenYu already implements per §3. The following **core** tests failed before this document
was written. The four rejection-reason assertions are now correct, but the fixture Gateways still
cannot be `Programmed` under the port model, so the tests remain blocked until §7.1 lands:

| Core test | Blocked by / status |
| --- | --- |
| every test (suite dials the Gateway's declared port) | §7.1 port model |
| `HTTPRouteHTTPSListener` | §7.2 no HTTPS listener |
| `HTTPRouteRequestHeaderModifier` | §7.3 no filters |
| `HTTPRouteRedirectHostAndStatus` | §7.3 no filters |
| `HTTPRouteInvalidParentRefNotMatchingSectionName`, `HTTPRouteInvalidParentRefNotMatchingListenerPort`, `HTTPRouteInvalidParentRefSectionNameNotMatchingPort` | reason fixed by #8163 (`NoMatchingParent`); still blocked by §7.1 |
| `HTTPRouteInvalidCrossNamespaceParentRef` | reason fixed by #8163 (`NotAllowedByListeners`); still blocked by §7.1 |
| `HTTPRouteServiceTypes` | §7.5 `Endpoints`-only backend resolution |

Everything marked *Extended* in the API reference (response header modification, rewrites,
mirrors, timeouts, CORS, backend-scoped filters, `TLSRoute`-dependent cases such as
`HTTPRouteDisallowedKind`) is out of scope until the core set passes.

### 6.3 Rules for wiring the suite up

The feature list passed to the suite must be derived from the exact upstream version recorded in
§1, and any feature that has to be skipped must be listed as a known gap here in the same PR. A
green job with silently skipped features is worse than a red job.

## 7. Known limitations / open gaps

Ordered by what blocks what. §7.1–§7.3 and §7.5 block the core `GATEWAY-HTTP` run; the remaining
entries are correctness or completeness gaps to be scheduled. §7.4 is kept as the record of a fix
shipped in #8163, not as an open gap. Everything here was verified against the source of this
repository and, where it concerns the specification, against Gateway API v1.5.1.

### 7.1 Listener port is not decoupled from the data-plane port *(blocks conformance)*

`GatewayReconciler` rejects any listener whose port differs from `server.port`, while the
conformance suite's base manifests declare listeners on 80 (HTTP) and 443 (HTTPS). Running the
suite requires either a real port mapping model (declared listener port → served port, validated
against the Service/NodePort that fronts the pod) or binding the data plane on those ports. Needs
a design decision before implementation.

### 7.2 HTTPS listeners are not supported *(blocks conformance)*

Only `HTTP` listeners are accepted; `HTTPS`/`TLS` are rejected with `UnsupportedProtocol`. The
core test `HTTPRouteHTTPSListener` declares a Gateway with an HTTPS listener on 443, a
`tls.certificateRefs` entry, and then asserts routing over HTTPS. Supporting it means terminating
TLS in the data plane (or documenting a hard dependency on an external terminator and declaring
the profile as not achievable), which is a product decision, not a bug fix.

### 7.3 No support for HTTPRoute filters *(blocks conformance)*

Filters are rejected wholesale, so any filter-based core test fails. At v1.5.1 **core** requires
at least `RequestHeaderModifier` (rule level) and `RequestRedirect` (hostname + status code);
`ResponseHeaderModifier`, `URLRewrite`, `RequestMirror`, CORS, backend-scoped filters and
`*Redirect` status-code cases are *extended*. Mapping the two core filters onto the existing
request/rewrite/redirect plugins is the largest single piece of remaining work.

### 7.4 Route rejection reasons *(fixed in #8163)*

The v1.5.1 API reference is explicit:

- `NotAllowedByListeners` — "the route has not been accepted by a Gateway because the Gateway has
  no Listener whose `allowedRoutes` criteria permit the route".
- `NoMatchingParent` — "a Route ParentRef specifies a Port and/or SectionName that does not match
  any Listeners in the Gateway".

ShenYu used to report `RefNotPermitted` for the first case and `UnsupportedValue` for the second.
`HTTPRouteReconciler#evaluateListeners` now reports the specified reasons, and `RefNotPermitted` is
left to `ResolvedRefs` where it belongs. One deliberate extension: a `parentRef` naming a Gateway
that does not exist is also reported as `NoMatchingParent`, because no other spec reason describes a
parent that resolves to nothing, and reporting nothing would leave the route silently unacknowledged.

Listeners are evaluated in two tiers and the verdicts are merged, so neither tier can hide the
other's answer:

- A served listener that accepts the route wins over any denial, so one restrictive listener cannot
  veto another one.
- A denial by a served listener is reported before a denial by a listener this controller cannot
  serve, because the served one is where the route would actually be programmed.
- When a served listener denies but an unserved listener's `allowedRoutes` criteria (namespaces and
  kinds) and hostname would accept the route, the reason is `UnsupportedValue`: the route is blocked
  by what this controller can serve, and `NotAllowedByListeners` — "the Gateway has no Listener whose
  `allowedRoutes` criteria permit the route" — would be false for that Gateway. "Would accept"
  requires a listener protocol that can attach an HTTPRoute (`HTTP` or `HTTPS`): a `TLS`/`TCP`
  listener permits no HTTPRoute, so neither its policy nor its `from=Selector` can mask a denial.
- When no served listener reached any verdict, the unserved listeners' denials are reported, so a
  policy or hostname denial behind a listener on an unmapped port (all of them, under the current
  port model, §7.1) still surfaces as such.

Without that split the reported reason would follow the port model rather than the specification,
and would change back once #8165 lands.

`UnsupportedValue` is now limited to controller-capability statements: `allowedRoutes.from=Selector`
(an unsupported value, see §3.2) and a route whose only permitting listener is one this controller
cannot serve. Neither is a parent-matching or policy problem; the second one is what #8165 (port
model) and #8167 (HTTPS listeners) remove. The generic "no listener with a supported protocol on a
served port" branch is kept only as a defensive fallthrough — with a non-empty listener selection it
is unreachable, because every listener either records an acceptance, a denial, or that an unserved
listener permits the route.

Every route rejection path listed in §5.1 now has a unit test asserting its reason: route-level
`filters`, missing Gateway, unmatched `sectionName`, unmatched `port`, a `sectionName` and `port` that
only match different listeners, namespace denial (served and unserved listener), kind denial, a
protocol-incompatible listener that must not override a served denial, a Gateway whose only listener
cannot attach an HTTPRoute, `from=Selector` on a protocol-incompatible listener, hostname mismatch,
`from=Selector` on an HTTP listener, an unserved listener that would have accepted the route, a
port-only capability limit, the reason priority between two served listeners denying differently, a
served listener accepting over another one denying, a rejected parentRef reported next to an accepted
one, config cleanup when every parent rejects, and the patch that corrects a route already reported
with the old reason.
`ListenerSupportTest` pins the protocol/kind matrix those paths rest on (which protocols can carry an
HTTPRoute, and how a declared, empty or absent `allowedRoutes.kinds` is read), and `HttpRouteParserTest`
pins the other half of the move: a cross-namespace `backendRef` without a ReferenceGrant still reports
`ResolvedRefs=False` / `RefNotPermitted`.

### 7.5 Backends are resolved only from `Endpoints` *(blocks conformance)*

`HttpRouteParser` reads `V1Endpoints`. The core test `HTTPRouteServiceTypes` relies on Services
whose endpoints exist **only as manually created `EndpointSlice` objects** (no `Endpoints`
object), so ShenYu reports `ResolvedRefs=False`/`BackendNotFound` and the test fails. Separately,
`Endpoints` is deprecated upstream, so this is both a conformance blocker and a migration to
schedule.

### 7.6 No cross-Route tie-break

The spec requires that when precedence ties remain across Routes, the winner is the Route with the
oldest creation timestamp, then the first in `namespace/name` order. The selector sort encodes
hostname/path/method/header/query/rule order only, so cross-route ties fall through to data-cache
insertion order — i.e. the winner is effectively arbitrary and can change after a restart.

### 7.7 Wildcard hostname precedence loses specificity beyond six labels

For two intersecting wildcard hostnames the spec ranks by "characters in a matching hostname";
`HttpRouteParser#hostnameScore` ranks by wildcard label count and clamps it with
`Math.min(suffixLabelCount, 6)`. Below the clamp the two orderings agree, because two wildcards
that both match one hostname must have suffixes where one contains the other, and the longer
suffix then has both more labels and more characters. At or above the clamp they diverge:
`*.a.b.c.d.e.com` and `*.x.a.b.c.d.e.com` both score 6, both match `h.x.a.b.c.d.e.com`, and the
remaining path/method/header/query/rule-order fields — not the hostname — then decide the winner,
so the less specific `*.a.b.c.d.e.com` can win. Exact-versus-wildcard ordering is correct.

### 7.8 A `matches` entry with more than 16 conditions escapes the padding

The data plane picks a selector by **condition count first**, then sort
(`AbstractShenyuPlugin#matchSelector`). The parser neutralises that by padding every selector to
16 conditions so the packed sort alone decides. The CRD allows up to 34 conditions in one match
(path + method + 16 headers + 16 query params), and any match above 16 then wins by condition
count, overriding hostname/path precedence.

### 7.9 `Gateway.status.addresses` is never written

`spec.addresses` and `spec.infrastructure` are ignored and `status.addresses` is never populated.
The conformance suite can be pointed at an explicit address, so this is not a hard blocker, but a
gateway that does not publish its address is incomplete for every consumer that reads status.

### 7.10 Per-route tuning is ignored, data-plane defaults are hard-coded

`rules[].retry`, `timeouts` and `sessionPersistence` are accepted and dropped (§5.2) while
`DivideRuleHandle` is emitted with `retry=3`, `timeout=3000`, `loadBalance=RANDOM`. A route that
declares a 30 s timeout is accepted and then times out at 3 s. All three fields are Extended.

### 7.11 `backendRefs[].filters` is ignored while the route reports success

A backend-scoped filter (for example `BackendRequestHeaderModifier`) is silently dropped and
`ResolvedRefs` stays `True`. The spec marks backend-scoped filter support implementation-specific,
so ignoring it is allowed; the problem is that nothing in the status says so. Either honour it or
reject the route.

### 7.12 `RegularExpression` values are never validated at reconcile time

A pattern that is malformed for RE2 (a PCRE-only lookahead, a backreference) is passed straight to
the data plane and throws at request time: every request reaching that selector fails, and no
condition, reason or log line on the route says why. Compiling the pattern during parsing and
rejecting the route with `Accepted=False` / `UnsupportedValue` would turn a runtime failure into a
visible one.

### 7.13 A rejected status patch is never retried

`sendStatusPatch` logs an `ApiException` and returns `Result(false)` (no requeue). Status therefore
converges only on the next informer resync (60 s) or the next unrelated event, which is weak
against the #8067 acceptance criterion that status stays accurate after edits under concurrent
changes. A failed patch should requeue with backoff.

### 7.14 Out of scope for #8067

`GRPCRoute`, `TLSRoute` and `BackendTLSPolicy` are deliberately not covered here; they are
separate follow-ups after the HTTP profile is stable.

### 7.15 Claims checked and withdrawn

Kept for the record, so these are not re-added as gaps without new evidence:

- ~~Rule order collapses beyond 16 rules~~ — the CRD caps `rules` at 16 entries and all 16 rule
  indexes still produce distinct sort values.
- ~~Header/query matching only reads the first value is non-conformant~~ — the specification
  leaves this implementation-specific and recommends the first value (§5.4).
- ~~Match `type` narrowing hits real inputs~~ — at v1.5.1 the enum has only `Exact` and
  `RegularExpression`, both implemented.

## 8. How to reproduce

CI job: `.github/workflows/integrated-test-k8s-gateway-api.yml` — kind v1.31.0, Gateway API
standard CRDs v1.5.1, ShenYu deployed from
`shenyu-integrated-test/shenyu-integrated-test-k8s-gateway-api-http/deploy/deploy-shenyu.yaml`,
HTTPRoute case from `shenyu-examples/shenyu-examples-http/k8s/gateway-api.yml`.

Locally, the same shape:

```bash
kind create cluster --image=kindest/node:v1.31.0 \
  --config=shenyu-integrated-test/shenyu-integrated-test-k8s-gateway-api-http/deploy/kind-config.yaml
kubectl apply -f https://github.com/kubernetes-sigs/gateway-api/releases/download/v1.5.1/standard-install.yaml
# build + load the images, then:
kubectl apply -f shenyu-examples/shenyu-examples-http/k8s/shenyu-examples-http.yml
kubectl apply -f shenyu-integrated-test/shenyu-integrated-test-k8s-gateway-api-http/deploy/deploy-shenyu.yaml
kubectl apply -f shenyu-examples/shenyu-examples-http/k8s/gateway-api.yml
```

Unit-level coverage for the claims in §3–§5 lives in
`shenyu-kubernetes-controller/src/test/java/org/apache/shenyu/k8s/` (reconcilers, parsers,
`common/ListenerSupportTest`, `common/GatewayApiCrdVerifierTest`, `common/GatewayApiCrdVersionsTest`).

## 9. Maintenance rules

- Any PR that changes controller behaviour updates this document in the same PR.
- Any PR that widens the claimed version range updates §1 **and** the kind image / CRD bundle
  version in `.github/workflows/integrated-test-k8s-gateway-api.yml`, and re-derives §6 from the
  new version's conformance suite.
- Any PR that adds a feature updates §3, §5 and, when conformance is wired up, the declared
  feature list together. The three must never disagree.
- Known gaps move from §7 into §3 once a test in this repository fails without the fix; the §7
  entry is then reduced to a record of the fix and its tests, and is dropped at the next revision.
- A withdrawn claim (§7.15) may only be reinstated together with the evidence that contradicts
  it.
