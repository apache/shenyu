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

# shenyu-examples-plugin-contract

This example is an **external consumer** of the `shenyu-web` test-jar contract kit. It has no
production sources and is built entirely from test-scope dependencies, so it also proves that the
published fixture is consumable outside the `shenyu-web` module.

## Prerequisites

- Java 17
- Maven Wrapper from the ShenYu repository
- The `shenyu-web` main artifact and test-jar installed in the local repository

```bash
./mvnw -pl shenyu-web -am install -DskipTests
./mvnw -f shenyu-examples/shenyu-examples-plugin-contract/pom.xml test
```

`-DskipTests` still compiles the `shenyu-web` test sources, which is required for the test-jar.

## What it covers

| Test | Contract surface |
| --- | --- |
| `transformPluginObservesRootSubscriptionBoundary` | Effect is deferred until root subscription; transform delegates once |
| `shortCircuitPluginDoesNotReachDownstream` | Response owner never invokes downstream |
| `wrapperPluginMayDecorateDownstream` | Wrapper may decorate without being rejected as pass-through |
| `cancellationPluginObservesRootCancellation` | Cancellation reaches the wrapped plugin |
| `badPassThroughPluginIsRejected` | Wrong continuation shape is reported |
| `eagerEffectIsRejectedBeforeSubscription` | An already-observed effect is rejected before root subscription; direct execute effects remain allowed |
| `selectorRulePluginMatchesAndFallsBackAfterRemoval` | Real selector/rule matching and cache miss fallback |
| `metadataRecordsKitCoreAndSkippedCapabilities` | Diagnostics include kit/core versions and skipped capabilities |

`AdvancedPluginContractTest` adds owned-resource cleanup and a detached-upstream negative,
real nested response decorators (body/headers/signals), duplicate continuation rejection,
wrapper resubscription, error cleanup, construction-time hooks, isolated fork, and supported
`PluginDataHandler` update/removal. Plugin-specific assertions check effects and state alongside
probe counters.

The contract guide is [`docs/plugin-contract-test-kit.md`](../../docs/plugin-contract-test-kit.md).
