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

# Rewrite rule configuration, contract version 1

`RewriteRuleContract` publishes the existing rule-handle behavior through the
optional shared registry. Rewrite has no plugin- or selector-specific fields in
this contract. The shared catalog and Admin metadata endpoint describe these
four fields; `RewriteHandle` and the original JSON storage are unchanged.

| Field | Declared type | Effective execution default | Meaning |
| --- | --- | --- | --- |
| `regex` | string | absent: no rewrite | Java regex, or existing brace-based placeholder expression |
| `replace` | string | absent: no rewrite | Replacement template |
| `percentage` | integer | omitted/null: 100 | Compared with an integer drawn from 0 through 99 |
| `rewriteMetaData` | boolean | omitted/null: false | Resolve metadata using the rewritten URI and effective context-path prefix |

Version 1 preserves Gson coercion and unknown-field behavior. Field types describe
the DTO; this version does not enable generic strict structural validation. In
particular, it does not clamp or reject existing percentage values: 0 and negative
values never rewrite, 100 and values above 100 always pass the threshold. The
compatibility fixtures test these deterministic cases without statistical
sampling. A stricter range policy would require a separate version and migration.

## Examples / 配置示例

An existing two-field handle still rewrites all matching requests by default:

```json
{"regex":"/api/(.*)","replace":"/v2/$1"}
```

旧版双字段配置仍然有效；未填写 `percentage` 时默认按 100 处理，
未填写 `rewriteMetaData` 时默认不切换元数据。契约中的默认值仅用于描述执行行为，
不会自动写回已保存的 JSON。

The existing placeholder branch applies when both strings contain `{`:

```json
{"regex":"/shenyu/{id}","replace":"/target/{id}","percentage":100,"rewriteMetaData":true}
```

当两个字符串都包含 `{` 时，仍使用现有占位符分支，不引入新的占位符语法。
只填写一个表达式时允许保存，但不会执行改写。正则或替换模板确实不可用时，
仍由现有 `RewriteHandleUtils` 判断；运行时的原始 URI 回退保护保持不变。

## Update boundaries / 更新边界

| Input or operation | Admin acceptance | Gateway handle update |
| --- | --- | --- |
| missing Java value | accepted | leave previous cached handle unchanged |
| blank string | accepted | remove previous cached handle |
| `{}` | accepted | replace with a no-op handle |
| literal JSON `null` | rejected | evict previous cached handle |
| malformed JSON / incompatible object shape | rejected | existing parse exception; previous cached handle remains |
| unusable applicable regex/replacement | rejected | evict previous cached handle |
| incomplete expression | accepted | cache it; execution skips rewriting |
| rule removal | operation-specific | idempotent removal; handle is not required |

“未传更新”“清空字符串”和“空对象”不是同一操作；无效表达式更新仍会淘汰旧改写，
不能套用 Divide 的“无效更新保留旧句柄”策略。契约返回校验事实，具体清除/淘汰行为
仍由原数据处理器负责。

Admin create/update/import uses the generic registry, with no new rewrite-name
branch. Its existing direct validator remains a compatibility entry point.
Gateway publication uses the same applicability provider while preserving parse,
clear and eviction boundaries. No contract parsing runs on the request path.
Unknown configuration fields remain ignored, and existing matching, URI/context
handling, downstream continuation and metadata/RPC-type rules are unchanged.

## Evidence

The versioned `rewrite-rule-v1.tsv` corpus is packaged with the common contract.
Common validation, Admin direct validation and real create/update/import services,
and the gateway data handler plus real `RewritePlugin` execution consume it.
It covers existing two-field JSON, null defaults, legacy coercion/unknown fields,
0/100/out-of-range thresholds, blank/no-op/null/invalid updates, capture groups,
placeholder mode, repeated removal and metadata enabled/disabled/missing/context
prefix cases. Existing applicability, stale-handle and runtime fallback tests are
also retained.

Metadata tests compare the four contract fields with DTO bean properties and the
maintained MySQL, PostgreSQL, Oracle and H2 form seed representations. No SQL seed
or field ID changes are needed: regex/replace use string controls, percentage a
numeric control, and rewriteMetaData a select control. Existing seeds explicitly
default rewriteMetaData to false; percentage's effective default comes from the
runtime, rather than being newly materialized in existing seed rows.

```text
./mvnw -B -ntp -pl shenyu-admin,shenyu-plugin/shenyu-plugin-rewrite -am verify -Dtest=RewriteRuleContractTest,RewriteRuleHandleValidatorTest,RuleServiceTest,RewriteHandleUtilsTest,RewritePluginDataHandlerTest,RewritePluginTest,RewriteContractCompatibilityTest -Dsurefire.failIfNoSpecifiedTests=false -Djacoco.skip=true -DskipRemoteResources=true -Dmaven.javadoc.skip=true
```

This is source-build behavior for the current development branch, not a claim
that the contract has shipped in a released distribution. Full Docker E2E remains
separate from these local deterministic publication/runtime fixtures.
