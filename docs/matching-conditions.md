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

# Matching-condition metadata

ShenYu represents selector and rule conditions as a source (`paramType`), an operator, a parameter name, and a parameter value. The matching-condition catalog is read-only metadata describing the built-in names and their current runtime semantics. It does not change condition evaluation. Admin can optionally enforce the shared structural validation described below.

The authoritative catalog is `MatchingConditionCatalog` in `shenyu-common`. Admin exposes it at `GET /matching-conditions`. A client can omit `version` or send a blank `version` to request the current catalog, or send `version=1`. An unsupported non-blank version returns a bounded Admin error result rather than an exception. Catalog version `1` follows the declaration order of `ParamTypeEnum` and `OperatorEnum`; incompatible metadata changes require a new catalog version.

Display keys use `matching.condition.source.<id>.display` and `matching.condition.operator.<key>.display`. Help keys use the corresponding `.help` suffix. The operator key for the stored alias `=` is `eq`, because a raw equals sign is never used in a localization key. Example descriptions use `matching.condition.example.<n>`.

## Sources

| Source | Status | Value type | `paramName` | Built-in registration | Runtime semantics |
| --- | --- | --- | --- | --- | --- |
| `path` | `DECLARED_NOT_REGISTERED` | `PATH` | Not required by the descriptor | No | Declared by `ParamTypeEnum`, but no built-in `ParameterData` implementation exists. Configuring it without a custom extension fails at runtime. Its help key must be rendered as unavailable in the built-in gateway. Existing judges would treat this non-`uri` source as text. |
| `form-data` | `DECLARED_NOT_REGISTERED` | `TEXT` | Not required by the descriptor | No | Declared by `ParamTypeEnum`, but no built-in `ParameterData` implementation exists. Configuring it without a custom extension fails at runtime. Its help key must be rendered as unavailable in the built-in gateway. |
| `post` | `LEGACY` | `CONTEXT_FIELD` | Required | Yes | Reads the named field from the `ShenyuContext` stored under `Constants.CONTEXT` by reflection. It is **not a request-body extractor**. Available fields are `module`, `method`, `rpcType`, `httpMethod`, `sign`, `timestamp`, `appKey`, `path`, `contextPath`, `realUrl`, and `startDateTime`. String fields are returned unchanged, date-time fields use `yyyy-MM-dd HH:mm:ss`, and unsupported values are treated as absent. |
| `uri` | `SUPPORTED` | `PATH` | Ignored at runtime | Yes | `RequestUrlUtils.getRewrittenRawPath(exchange)`: the rewritten raw path, not the original request URI. Admin currently requires and stores a placeholder `paramName` for custom-flow selector conditions even though the gateway ignores it. |
| `query` | `SUPPORTED` | `TEXT` | Required | Yes | First query-string value for the named parameter. |
| `host` | `SUPPORTED` | `HOST` | Ignored | Yes | Client host string from `RemoteAddressResolver.resolve(exchange)`. This is not the request URI host. |
| `ip` | `SUPPORTED` | `IP` | Ignored | Yes | Client numeric address from `RemoteAddressResolver.resolve(exchange).getAddress().getHostAddress()`. This is not the request URI host. |
| `header` | `SUPPORTED` | `TEXT` | Required | Yes | First request-header value for the named header. |
| `cookie` | `SUPPORTED` | `TEXT` | Required | Yes | First request-cookie value for the named cookie. |
| `req_method` | `SUPPORTED` | `HTTP_METHOD` | Ignored | Yes | `exchange.getRequest().getMethod().name()`. |
| `domain` | `SUPPORTED` | `HOST` | Ignored | Yes | `exchange.getRequest().getURI().getHost()`: the request URI host, unlike the client address exposed by `host` and `ip`. |

`uri` and the declared-but-unregistered `path` source describe `=`, `match`, `regex`, `contains`, `startsWith`, `endsWith`, `exclude`, `pathPattern`, and `isBlank`. If a custom `path` source is supplied, the current judges still take their non-`uri` branches, so `match`, `exclude`, and `pathPattern` use containment semantics rather than URI path semantics. Every other source describes `=`, `regex`, `contains`, `startsWith`, `endsWith`, `match`, `exclude`, and `isBlank`. `post` additionally describes `TimeBefore` and `TimeAfter`, because a selected `ShenyuContext` field can contain a date-time value.

## Operators

| Operator | Status | Value format | Requires value | Runtime semantics |
| --- | --- | --- | --- | --- |
| `=` | `SUPPORTED` | `TEXT` | Yes | Exact string equality. The stored alias maps to the `equals` `PredicateJudge` SPI key. |
| `regex` | `SUPPORTED` | `REGEX` | Yes | Regular-expression match. |
| `contains` | `SUPPORTED` | `TEXT` | Yes | Plain string containment. |
| `startsWith` | `SUPPORTED` | `TEXT` | Yes | Plain string prefix comparison. |
| `endsWith` | `SUPPORTED` | `TEXT` | Yes | Plain string suffix comparison. |
| `isBlank` | `SUPPORTED` | `NONE` | No | Asserts that the resolved real value is blank and ignores `paramValue`. |
| `match` | `ADVANCED` | `PATH_ANT` | Yes | For `uri`, uses Spring `AntPathMatcher` through `PathMatchUtils.match`, including `*`, `**`, and `{var}`. For every other source, it is plain `String.contains`. |
| `exclude` | `ADVANCED` | `TEXT` | Yes | For `uri`, negates the Ant-style match. For every other source, it negates plain `String.contains`. |
| `pathPattern` | `ADVANCED` | `PATH_SPRING` | Yes | For `uri`, uses Spring `PathPattern`; a mid-path `/**/` is not supported. For every other source, the registered judge uses plain `String.contains`, although the built-in catalog does not list this operator for those sources. |
| `TimeBefore` | `ADVANCED` | `DATE_TIME` | Yes | With blank `paramName`, compares `LocalDateTime.now()` with `paramValue`. Otherwise parses the resolved real value and compares it with `paramValue`. |
| `TimeAfter` | `ADVANCED` | `DATE_TIME` | Yes | With blank `paramName`, compares `LocalDateTime.now()` with `paramValue`. Otherwise parses the resolved real value and compares it with `paramValue`. |

Date-time operators use `yyyy-MM-dd HH:mm:ss` (`DateUtils.DATE_FORMAT_DATETIME`); an unparseable value does not match. For every operator except `isBlank`, `PredicateJudgeFactory.judge` returns false before invoking the judge when the resolved real value is blank.

Admin's existing validation is deliberately narrower than this metadata. It validates only `uri` conditions: `pathPattern` with Spring `PathPatternParser`, `regex` with RE2J, `isBlank` with a blank value, and the other recognized URI operators with a leading `/` and no whitespace. Catalog publication does not alter that behavior.

## Write-boundary validation

Shared structural validation is report-only by default. Set `shenyu.matching-condition.strict-validation=true` to reject known-invalid conditions during selector or rule `createOrUpdate`. With the default `false`, each write with known-invalid conditions emits one bounded WARN containing the same `conditions[<i>].<field>=<CODE>` summaries used by strict failures, but produces no exception and does not change persistence behavior. The warning never contains configured values, parameter names, header or cookie names, or parser messages.

The property is applied when the Admin application context starts and the previous process value is restored when that context closes. A later Admin context in the same JVM therefore does not inherit strict validation from an earlier context.

The stable diagnostic codes are `SOURCE_REQUIRED`, `OPERATOR_REQUIRED`, `NAME_REQUIRED`, `VALUE_REQUIRED`, `INVALID_REGEX`, `INVALID_DATE_TIME`, `UNKNOWN_SOURCE`, and `UNKNOWN_OPERATOR`. Unknown or custom source and operator identifiers are classified as unknown legacy and are never rejected by this structural layer. Declared-but-unregistered sources such as `path` and `form-data` are also not rejected because a customized gateway can provide them. Operator applicability is editor guidance rather than a rejection rule. An `isBlank` condition can have a blank name, and operators that do not require a value can have a blank value.

Strict failures use the bounded format `Matching condition validation failed: conditions[<i>].<field>=<CODE>`. At most four diagnostics are included, separated by commas, with `, ...` when more exist. Messages contain no configured values, parameter names, header or cookie names, or parser messages.

Only the `SelectorService` and `RuleService` `createOrUpdate` boundaries are integrated, and selector validation applies only to custom-flow selectors. Configuration import through `importData` and client registration through `registerDefault` write through other paths and are unaffected. Internal rule publication in the Admin OpenAPI import flow calls `RuleService.createOrUpdate`, so it is subject to the same opt-in check.

## Shared fixture corpus

`matching-condition/corpus-v1.json` in `shenyu-common`'s test sources is the single fixture corpus for these semantics, published through the `tests` classifier JAR so both planes consume the same data:

- the `structural` section drives `MatchingConditionValidator` in `shenyu-common` and the Admin write-boundary policy test,
- the `semantics` section is executed in `shenyu-plugin-base` against the real `ParameterData` and `PredicateJudge` SPI implementations, including working, prefix, Ant, Spring `PathPattern`, RE2J, method, named request field, client address, context field, blank and time cases.

A fixture is only authoritative for behavior the gateway actually exhibits; when a fixture and the implementation disagree, the implementation is the source of truth and the fixture is corrected.

## Availability and extensions

`runtimeRegistered` describes only the built-in gateway registration: `true` means the source is present in the built-in `shenyu-plugin-base` registration, while `false` does not rule out a custom deployment providing it. It is not proof that a selected or customized gateway deployment has that extension on its classpath. Conversely, unknown source and operator identifiers can be valid custom extensions and remain representable in stored conditions even though this built-in catalog does not describe them.

A custom source implements `ParameterData`, is annotated with `@Join`, and is registered under its key in `META-INF/shenyu/org.apache.shenyu.plugin.base.condition.data.ParameterData`. A custom judge similarly implements `PredicateJudge`, uses `@Join`, and is registered in the corresponding `META-INF/shenyu/...PredicateJudge` resource. Deploy the implementation and registration in each gateway that must evaluate the custom condition.

## Compatibility policy

Catalog publication remains descriptive and read-only. Shared structural enforcement is a separate Admin opt-in through `shenyu.matching-condition.strict-validation`; the default remains report-only. The validator never normalizes, rewrites, or imports configuration.

The per-source `operatorIds` list is presentation guidance for guided editors, not a runtime rejection list. This includes the absence of `TimeBefore` and `TimeAfter` from the `uri` descriptor. The gateway still evaluates any stored source/operator combination, and Admin must continue to accept previously stored configurations unchanged.

Structural validation distinguishes known-valid, known-invalid, and unknown-legacy conditions. Only known-invalid conditions can be rejected, and only when strict validation is explicitly enabled. Unknown or custom conditions remain representable, while import and registration paths continue to accept previously stored configurations without this new enforcement or normalization.
