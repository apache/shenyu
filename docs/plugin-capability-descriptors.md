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

# Optional implementation capability declarations

`PluginCapabilityDescriptor` and `PluginCapabilityRegistry` are optional static
companions in shenyu-common. They add no mandatory method to ShenyuPlugin, no
PluginEnum entry for external plugins, and no request-path work. A legacy/custom
implementation without a declaration continues to execute and reports UNKNOWN.
Exact class identity is required; a subclass does not inherit audited promises.

Register an immutable companion at application setup with `registry.register`.
Registration does not load or instantiate the named implementation. The catalog
holds at most 256 declarations and rejects identity/version collisions rather
than silently replacing facts. Format version 1 bounds names, descriptions,
contexts, modes and capability sets. Unsupported schemas fail explicitly.
This is a small metadata catalog, not a new provider or plugin lifecycle system.

The format records declared protocol/context support, conditional request/body
modes, cooperating routing/result roles, response decoration/short-circuit/commit
effects, supporting-provider requirements, declaration version/provenance and an
optional configuration-contract reference. Configuration references may remain
UNRESOLVED; no schema validator or graph executor is required. UNKNOWN and
conditional modes are intentional: there is no universal streamingSafe flag.
Declarations contain no configured handles, credential values or request data.

## Audited HTTP pilot

| Implementation | Cooperating behavior |
| --- | --- |
| Divide | Produces HTTP_DOMAIN/settings after matching; can reject locally; does not invoke the upstream itself |
| URI | Consumes domain and produces HTTP_URI; delegates when the domain is absent |
| Selected Netty/WebClient | Invokes HTTP_URI and produces response metadata/body handle for a later writer; body retries use the existing bounded replay cache |
| Response | Dispatches to the configured MessageWriter, consuming the transport result and writing the downstream response |
| Cache hit | Eligible reusable bodyless GET hit writes the cached response with zero upstream calls |
| Cache miss | Continues through URI/client/Response while decorating the response; the current writeWith path aggregates the whole body with DataBufferUtils.join |

The Cache miss aggregation currently has no established local aggregate cap.
Uncacheable metadata does not by itself prove a streaming path: eligibility and
the writer's streaming flush path matter. These limitations are explicitly
declared instead of being converted into an unconditional bounded/streaming
promise. Cache and Response are cooperating hit/miss roles, not mutually
exclusive plugin categories. Only the HTTP pilot context is declared here;
other contexts remain unknown rather than being inferred from a category.

## Bounded diagnostics and conservative preflight

The existing PluginInventoryCollector includes declaration/version provenance
alongside implementation identity/order. It labels its scope as the observed
LEGACY chain, not a complete installation catalog, and requestSelection UNKNOWN.
It does not infer actual matching, enabled configuration, authentication or an
active graph path from declaration presence. Observed package version is UNKNOWN
when unavailable; static declaration provenance is separate.

Collection caps records at 64 and accounts encoded bytes before adding each
record. Identifiers are bounded and truncation is explicit; small budgets can
produce a content-free TRUNCATED section. No full configuration is traversed or
serialized. Existing diagnostic worker/time/redaction/archive limits remain.

`registry.preflight(observedImplementations, observedSupportingCapabilities,
"http")` compares declaration requirements against explicit observations. It
reports unknown descriptors/contexts, unsupported contexts, missing cooperating
execution capabilities and unresolved supporting providers. Its bounded report
is an installation observation, never READY, an order validator or proof of a
selected request path. The caller must observe provider availability honestly;
an Admin dictionary entry alone is not an installed gateway SPI implementation.
Inspection invokes no execute/skip/before/after callback, remote provider or SDK.
It never rewrites, sorts, activates or disables a legacy pipeline.

## Verification

The opt-in HttpCapabilityPilotTest runs a complete real loopback HTTP chain:
Cache, Divide, URI, Netty client and Response with its actual Netty writer.
It verifies the first miss forwards and writes the upstream body, the subsequent
reusable hit makes zero additional upstream calls, and the observed cooperating
implementations satisfy the HTTP preflight. Storage is the existing MemoryCache;
no mock executor or copied chain is used. The profile adds test dependencies and
sources only, leaving the default/runtime dependency graph unchanged.

```text
./mvnw -B -ntp -Phttp-capability-pilot -pl shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide -am verify checkstyle:check -Dtest=HttpCapabilityPilotTest,PluginCapabilityRegistryTest,PluginInventoryCollectorTest,AbstractHttpClientPluginTest,NettyHttpClientPluginTest,WebClientPluginTest,CachePluginTest -Dsurefire.failIfNoSpecifiedTests=false -Djacoco.skip=true -DskipRemoteResources=true -Dmaven.javadoc.skip=true
```

Common tests cover optional external identities, immutable metadata, schema and
version collisions, unknowns and missing requirements. Inventory tests verify
byte/record budgets and that request callbacks are never invoked. Existing
transport/cache tests remain the owners of protocol and buffering correctness.
This source-build delivery does not imply release publication or GRAPH support.
