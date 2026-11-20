<!--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

# Optional plugin configuration contracts

`shenyu-common` provides `ConfigurationContractProvider` and
`ConfigurationContractRegistry`. A contract is keyed by the stable plugin name,
one existing storage scope (`PLUGIN`, `SELECTOR`, `RULE`), and a contract version.
It does not change `PluginData.config`, selector/rule handle strings, plugin
names, synchronization envelopes, or the existing `PluginHandle` form metadata.
Its version is not a fleet configuration revision.

## Registration and inspection

Put a pure provider in a lightweight shared contract artifact and register its
class in `META-INF/services/org.apache.shenyu.common.config.contract.ConfigurationContractProvider`.
The shared registry discovers these providers with Java ServiceLoader from the
contract API's classloader. Applications can also register trusted providers
explicitly during startup. It rejects duplicate plugin/scope/version keys and
limits the catalog to 1024 registrations. Provider construction and validation
must not initialize a request plugin, an SDK client, perform remote calls, or
modify configuration/caches. This is a trusted extension contract, not a Java
sandbox. Admin must install the lightweight artifact, never a gateway plugin JAR
or its SDK implementation to obtain a validator.

`registry.catalog()` returns immutable, deterministically ordered metadata.
Admin exposes the same catalog at `GET /plugin-template/config-contracts`, using
the existing `system:plugin:edit` permission. It contains field descriptions and
effective defaults, never configured values. Existing form metadata remains
available for unconverted plugins and forms; the endpoint does not generate or
replace forms.

```java
ConfigurationValidation result = ConfigurationContractRegistry.shared()
    .validate("agentGateway", ConfigurationScope.PLUGIN, "1", originalJson);
```

Treat `VALID`, `INVALID`, `UNREGISTERED`, and `UNSUPPORTED_VERSION` distinctly.
`UNREGISTERED` preserves the legacy path but is not a validation/readiness claim.
An explicitly unsupported version requires an availability/compatibility
decision, never implicit success. The existing versionless production adapter
selects version `1`; introducing another wire/version-selection policy needs a
separate compatibility change. A provider may be installed without capability
descriptors, a graph executor or the reactive test kit.

## Structural checks and semantics

Field descriptors record JSON type, required/null behavior, effective default,
unit, sensitivity, optional inclusive numeric bounds and optional closed string
enumerations. SPI names must not be turned into closed built-in enums.
`structuralValidation=true` opts into strict JSON syntax/duplicate/trailing-token
checks, declared field types, required/null checks, integral/range constraints,
and enumerations. Dotted declared paths can describe nested object fields;
unknown-field rejection currently applies to the root object's names. Nested
unknown-field policy and cross-field rules belong to the semantic provider.
Defaults describe execution and are never inserted into stored input.

`structuralValidation=false` leaves coercion, syntax and null compatibility to
the adopted existing parser. `UnknownFields` explicitly describes allow,
root-level reject, or semantic-validator ownership. This lets a legacy parser
keep its accepted input set without globally making other plugins strict.
Custom semantic checks run after successful optional structural checks. They
return stable `ConfigurationViolation` codes and declared paths, not exception
messages or configuration values. Errors are capped at 16; paths at 128
characters. Parser/provider exceptions become `PROVIDER_FAILURE` without their
message/cause. Unknown supplied field names are reported at `$`, so a secret
used as a key cannot leak through diagnostics. Sensitivity is descriptive
metadata for tooling, not permission to expose other configured values.

Runtime dictionary/SPI, credential and endpoint availability remain binding
checks. `RUNTIME_BINDING_UNAVAILABLE` is a separate diagnostic code available
to adapters. A successful shared schema/semantic check does not establish that
an extension or credential exists on a particular gateway, that resources were
prepared, or that a candidate became active.

## Operation ownership and initial production adapter

Validation returns facts and never clears, retains, replaces or deletes a cache.
Each descriptor documents operation semantics; the owning data handler continues
to implement them. Missing input, explicit JSON null, blank clearing, `{}`,
invalid update, disable and removal are separate cases. Do not copy another
plugin's clearing or eviction policy merely because both use this registry.

The first production adapter delegates Agent Gateway plugin configuration to
its existing `AgentGatewayAggregationConfig.parsePluginConfig` implementation.
Missing/blank/empty-object input keeps local-only behavior. Literal JSON null,
unknown fields and malformed aggregation objects remain invalid. The generic
Admin validation entry uses the registry for create/update/import, including
persisted plugin identity on update. Gateway catalog reload validates before
credential resolution/resource preparation and then retains its existing parser
and lifecycle. Invalid candidates leave the previous directory active; removal
and disable remain lifecycle operations. No request-path work is added.

`META-INF/shenyu/config-contract-fixtures/agentGateway-plugin-v1.tsv` is the
versioned fixture corpus packaged with the contract artifact. The common parser/
registry, Admin validator, and real gateway catalog-reload tests consume it.
`@missing` and `@blank` distinguish missing values from literal JSON null.
Production service tests also check invalid create, persisted-identity update
and import rejection before writes/events. Generic fixtures exercise all three
scopes, defaults/units, type/range/enum/null policies, immutable metadata,
duplicates, unknown versions, legacy fallback and bounded secret-free failures.

Use the repository Maven wrapper to run the focused reactor:

```text
./mvnw -B -ntp -pl shenyu-admin,shenyu-plugin/shenyu-plugin-agent-gateway -am verify -Dtest=ConfigurationContractRegistryTest,AgentGatewayAggregationConfigTest,AgentGatewayPluginConfigValidatorTest,PluginServiceTest,PluginControllerTest,ManagedRemoteMcpCatalogTest,HttpLongPollingDataChangedListenerTest -Dsurefire.failIfNoSpecifiedTests=false -Djacoco.skip=true -DskipRemoteResources=true -Dmaven.javadoc.skip=true
```
