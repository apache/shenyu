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

# Read-only base configuration: first adoption slice

This implements the **BaseDataCache matching-configuration slice** of
[issue 8223](https://github.com/apache/shenyu/issues/8223). It is not full
immutability for all gateway caches, transactional configuration activation, or
a security sandbox for untrusted Java code.

## Contract and ownership

Synchronization owns mutable `PluginData`, `SelectorData`, `RuleData` and
`ConditionData` objects. Normal `BaseDataCache` publication methods retain the
legacy mutable references and separately publish frozen copies. Copies are made
on update, not during ordinary configuration lookup. The copy includes current
and previous condition lists and their elements. Strings and boxed scalar values
are immutable; untouched published entries are reused across bucket changes.

`BaseDataCache.getInstance().readOnly()` is the adopted read contract. Lookup
returns the already-published object/list or null; it does not deep-copy,
serialize, recompute fingerprints or lock readers. Map getters return prebuilt
unmodifiable views: entries, values, lists and condition elements cannot be used
to write back into publication state. They are **views of one backing-map
generation**, not a pinned transaction over plugins, selectors and rules. A held
DTO or bucket remains unchanged; map membership can change on incremental writes.
Batch map replacement leaves old views pointing at their old generation.

`DtoSnapshots.freeze(...)` creates the same standard concrete DTO class, not a
subclass with different equality. Frozen and mutable equal-valued objects retain
symmetric equals/hashCode behavior. Every supported setter, including base and
fluent RuleData overrides, rejects changes on a frozen snapshot. `frozen()` is a
non-bean accessor; the internal transient flag is not a wire configuration field.
Gson/Jackson round trips produce ordinary mutable build DTOs. Unknown subclasses
are not silently projected or treated as immutable: snapshot creation rejects
unsupported concrete classes. Extensions publishing custom DTO subclasses must
explicitly map their configuration to the standard DTO contract.

Writers serialize the paired legacy/published updates so concurrent publication
calls cannot leave the two channels in opposite update orders. This is a local
writer-only monitor, not a request lock, distributed revision protocol or atomic
multi-object activation. One publication method returning establishes its own
updates; it does not pin a whole request to one configuration revision.

## Adoption and legacy compatibility

`AbstractShenyuPlugin.useReadOnlyConfiguration()` defaults to false. Existing
plugins and external subclasses retain the existing getter signatures, mutable
build DTOs and `doExecute(exchange, chain, selector, rule)` signature. A plugin
opts in by overriding that method to return true. Its enabled check, selector and
rule matching, match-cache values and `doExecute` parameters then use the frozen
base configuration. It must treat those shared parameters as read-only and keep
request-specific overrides in request-owned variables or objects.

Divide is the first built-in adopter. Its `continued=false` default rule remains
a newly created request-owned object, as before; modifying that object does not
mutate any published rule. Negative match sentinels are frozen and reused. Derived
matches use a separate read-only channel: a mutable value submitted to that
channel is rejected, never copied on a cache-hit path. Existing subscriber
invalidation methods invalidate both channels, including dangling IDs and
negative entries. The existing matching algorithm, ordering, specificity and TTL
remain in use.

Legacy `getPluginMap()`, `getSelectorMap()`, `getRuleMap()` and match-cache map
getters still expose the legacy storage. **Writing these maps or mutating their
DTOs does not update the frozen channel.** This isolation is intentional. Use the
normal publication and subscriber APIs for configuration changes; do not mutate
a live getter and expect every plugin to observe it. The gateway still receives
configuration through existing synchronization; it does not read the Admin DB.

### Consumer inventory for this slice

| Consumer / storage | Behavior |
| --- | --- |
| CommonPluginDataSubscriber cache/refresh/remove/clean operations | Publish or remove both base channels; handler callbacks retain their original mutable DTOs |
| AbstractShenyuPlugin legacy subclasses | Existing mutable base and match-cache path |
| DividePlugin base matching | Explicitly adopts frozen Plugin/Selector/Rule/Condition values |
| LocalPluginController / Admin / Kubernetes DTO builders | Mutable build DTOs; publish through existing APIs |
| MatchDataCache legacy map getters | Legacy only; cannot inject a mutable object into adopted hits |
| MetaDataCache and metadata consumers | **Not migrated; remain legacy mutable** |
| CommonHandleCache, including DivideRuleHandle and nested canary/sticky configuration | **Not migrated; remain legacy mutable** |
| UpstreamCacheManager health, counters, inflight, lag and circuit-breaker state | Intentionally live and mutable; not frozen |

Accordingly, this PR does **not** claim that every effective Divide setting or
all gateway configuration is immutable. In particular, a trusted plugin holding
a parsed handle or metadata reference can still mutate that separate legacy
state. The migration boundary is explicit rather than hidden behind an
unmodifiable outer Map.

## Consistency limits and related work

[Issue 8060](https://github.com/apache/shenyu/issues/8060) owns multi-object
transactional publication. [Issue 8061](https://github.com/apache/shenyu/issues/8061)
owns resource leases. This slice adds neither a revision manifest nor per-request
leases, and retains no history indexed by requests. Old DTOs remain alive only
while a reader or an existing bounded derived cache retains them.

Existing match-cache invalidation races and selector-scoped rule-cache keys are
separate work in [PR 7681](https://github.com/apache/shenyu/pull/7681) and
[PR 8103](https://github.com/apache/shenyu/pull/8103). Frozen data alone does not
fix stale-match publication across an in-flight update; no such claim is made.
[PR 8291](https://github.com/apache/shenyu/pull/8291) owns matching observation.
The new channel must continue to receive those shared invalidations when these
changes integrate; it is not an alternative matching engine.

## Cost and acceptance status

This compatibility period deliberately retains both the mutable and frozen
versions. Update cost includes copying the changed DTO and nested conditions,
maintaining the second index, and sorting the affected bucket; batch refreshes
retain the existing once-per-bucket sorting approach. Unchanged frozen elements
are reused, but full-map batch replacement still allocates a new index as the
legacy algorithm already does. A retired view may be retained by application
code; there is no automatic history or new request lease. The derived cache has
the existing configurable capacity/TTL independently for each channel.

Ordinary reads must remain reference lookups and bounded existing matching, with
no configuration deep copy, whole-map reconstruction, serialization, remote call
or global read lock. The extra index and publication allocation are **not free**;
measurement must report update allocations/convergence and peak/retained memory
as well as steady-state forwarding.

The companion configuration mode in `docs/benchmarks/divide-gateway` reuses the
real HTTP proxy harness and separate gateway/load/upstream JVMs. Compare the
fixed base `548bec1f9` and this change using repeated, alternating small/large
matched, no-match and match-cache-hit cases plus bounded update bursts. Record
throughput, p95/p99, CPU, allocation per operation, convergence and peak/retained
memory. An offered-rate run alone is not proof of equal saturation throughput;
repeat near saturation and report run variability rather than selecting one good
sample.

**No before/after performance acceptance result is claimed here.** On the local
Windows/JDK 17.0.16 host, even a standalone `Selector.open()` probe fails with
`IOException: Unable to establish loopback connection`, caused by
`SocketException: Invalid argument: connect` in `UnixDomainSockets`. Both default
and WindowsSelectorProvider probes fail before any gateway code starts.
Until the required forwarding measurements are produced on a working host and
show no measurable ordinary-path regression, this change must remain a **Draft**
for design/compatibility feedback. Compilation, unit tests and an in-memory
forwarding test are not substitutes for that merge gate.
