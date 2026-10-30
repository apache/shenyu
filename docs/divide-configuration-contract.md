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

# Divide rule configuration, contract version 1

`DivideRuleContract` registers the existing HTTP routing rule handle with the
shared registry. It inventories every current root, Canary, predicate, sticky-key
and session-affinity DTO field. It does not invent plugin or selector scopes,
change JSON storage, construct SDK clients, or change routing/response ownership.
The machine-readable catalog is also available through Admin's existing
`/plugin-template/config-contracts` metadata API.

| Root field | Omitted-field default | Units / interpretation |
| --- | --- | --- |
| `loadBalance` | `random` | Open SPI name, including external implementations |
| `retryStrategy` | `current` | Existing retry strategy selection |
| `retryBackOffSpec` | `default` | Existing default/fixed/exponential interpretation and fallback |
| `retry` | 3 | Retry count |
| `timeout` | 3000 | Milliseconds |
| `headerMaxSize` | 0 | UTF-8 bytes plus framing allowance; non-positive disables this check |
| `requestMaxSize` | 0 | Body bytes; non-positive disables enforcement |
| `canary` | absent | Legacy routing; a present configuration uses existing partition validation |
| `sticky` | absent | Existing optional HTTP affinity |

Defaults are descriptive and never inserted into stored JSON. Gson's existing
root-field coercion, explicit-null and unknown-field behavior remains in force.
For primitive fields, explicit null retains the initializer; reference fields can
remain null. This version adds no universal positive-range rule for legacy root
fields and no closed built-in allowlist for extension names. A descriptor does
not prove an extension is installed.

## Nested fields

| Field group | Current meaning / default |
| --- | --- |
| `canary.enabled` | false selects Stable; present value must be a JSON boolean |
| `canary.conditions` | Optional array; empty allows participation; explicit null is invalid |
| `canary.conditions[].paramType`, `.operator` | Existing ParameterData and PredicateJudge SPI names |
| `canary.conditions[].paramName`, `.paramValue` | Existing source/operator-specific requirements, including regex/time applicability |
| `canary.matchMode` | 0 AND; 1 OR; no fractional/string/null coercion |
| `canary.percentage` | 0, range 0..100; same existing strict integral validation |
| `canary.stickyKey.paramType`, `.paramName` | Source required when enabled at 1..99%; availability is a separate binding fact |
| `canary.canaryLabels`, `.stableLabels` | Both nonempty string maps; a shared key must have different values |
| `canary.fallbackPolicy` | STABLE, or REJECT |
| `sticky.enabled` | false; existing affinity activation requirements still apply |
| `sticky.cookieName` | Shenyu-Route base name, with the existing rule suffix; a custom name remains verbatim/shared |
| `sticky.maxAgeSeconds` | 3600 seconds |
| `sticky.secrets` | First HMAC key signs, all keys verify; configured values are sensitive |
| `sticky.onUnavailable` | REBIND; existing interpretation is unchanged |
| `sticky.key.type`, `.name` | Optional HEADER/COOKIE deterministic key; HEADER is the DTO default |

For a present Canary object, the existing parser validates labels and conditional
requirements even when Canary is disabled. The contract reuses
`CanaryConfigValidator`; it does not create a second parser. Canary plus retries
to another target remains rejected until partition-aware failover exists.

Sticky parsing retains its historical coercion and unusable-configuration
handling. A parseable sticky object is not a promise that affinity will activate:
the existing runtime still needs usable secrets, cookie settings and lifetime.
Array element descriptions and conditional constraints are owned by semantic
validation, rather than silently enabling generic strict structural validation.

## Validation, binding and updates

Admin create/update/import/registration paths dispatch shared semantic validation
through the registry before writes. Admin's Canary dictionary checks remain
separate. The gateway validates a complete candidate before replacing the typed
handle and checks its actual ParameterData/PredicateJudge providers. A dictionary
entry can therefore be accepted by Admin and still be unavailable on a gateway.
Both failures use bounded field paths and codes; a missing binding is explicitly
`RUNTIME_BINDING_UNAVAILABLE`, with no raw provider exception/configuration value.

| Boundary | Preserved version-one behavior |
| --- | --- |
| missing Java update value | no-op; previous typed handle remains |
| blank Admin value | accepted legacy input |
| blank gateway update | existing parse failure; previous typed handle remains |
| malformed/invalid candidate | rejected before replacing the previous handle |
| `{}` | parsed DTO defaults replace the previous handle |
| `removeRule` with null handle | current conditional removal leaves the typed handle present |
| `removeRule` with non-null handle | removes it; repeated removal is safe |
| disable | existing matching/lifecycle policy; this schema adds no universal cache clearing |

The conditional removal limitation is intentionally characterized, not silently
strengthened by adopting a schema. Rewrite's blank-clear/invalid-expression
eviction policy is not applied to Divide. None of these per-handle checks
establishes atomic publication across selector, upstream or plugin caches.

## Form projection and verification

Metadata tests inventory every writable DTO property and verify the maintained
MySQL/PostgreSQL/Oracle/H2 field/control seeds. Their existing seven root controls
remain intact; the optional full catalog additionally describes nested JSON
fields. No IDs, seed rows or upgrade scripts change. In particular, existing
forms preset header/body limits to 10240/102400, while an omitted raw JSON field
has the runtime default 0. Both facts are explicitly described rather than
normalizing stored values or changing form behavior.

The versioned `divide-rule-v1.tsv` corpus runs through common validation, real
Admin create/update/import services and the real gateway data handler. It checks
shared error paths/codes, missing/null/empty input, custom names, coercion,
Canary types/ranges/partitions/retry restrictions, sticky variants, binding
differences, valid-to-invalid retention and conditional removal. Existing routing,
Canary/failover, affinity, deterministic-key, retry, timeout and size-limit tests
are included in the focused verification; their behavior is not redesigned.

```text
./mvnw -B -ntp -pl shenyu-admin,shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide -am verify -Dtest=RuleServiceTest,DivideRuleContractTest,CanaryConfigValidatorTest,*Divide*,*Canary*,*Sticky*,ConfigurationContractRegistryTest,RewriteRuleContractTest,PluginControllerTest -Dsurefire.failIfNoSpecifiedTests=false -Djacoco.skip=true -DskipRemoteResources=true -Dmaven.javadoc.skip=true
```

No full distribution/Docker E2E or released-version availability is implied by
the local deterministic checks. Pure validation runs on configuration updates;
no registry parsing or metadata projection is added to request forwarding.
