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

# Agent Gateway governance: execution order and upgrade

## Single matched entry

The default AI path is `authentication -> agentGateway -> aiPrompt -> aiTokenLimiter -> aiProxy`.
KeyAuth and BasicAuth default to 150, Agent Gateway to 169, Prompt to 170,
Token Limiter to 171 and AI Proxy to 199. JWT/OAuth2 remain earlier.
169 was unused in PluginEnum before this change; 170 is already used by both
Prompt and RocketMQ logging. No other plugin enum or sorting algorithm changes.

Agent Gateway keeps its existing selector/rule matching and is the only AG entry.
A matched LLM request establishes its context before continuing through the
existing AI chain. AG must not read, buffer or subscribe to the LLM request body;
body consumption remains with the existing AI plugins. A matched MCP request
terminates in AG and does not enter Prompt, the LLM Token Limiter or AI Proxy.
Unmatched/disabled AG continues the original chain. Moving the terminal entry
earlier also bypasses later plugins for matched MCP, including `mcpServer` and
`loggingKafka` (180), `sensitiveWord` and `loggingElasticSearch` (190), the LLM
Token Limiter (171), RocketMQ logging (170), Prompt and AI Proxy. In particular,
this flips the previous overlap with `mcpServer`: AG at 198 formerly ran after
it, while AG at 169 now runs first and terminates the matched MCP path. Do not
assume those plugins still observe, filter or route MCP; review any deployment
relying on their old position. Authentication plugins must still be enabled and
match the intended paths; order alone is not authentication.

## Effective order is configuration, not only the enum

`ShenyuWebHandler.sortPlugins` uses cached `PluginData.sort`, falling back to
`getOrder()` only when no cached sort exists. Its constructor retains the supplied
list; sort/enable events perform the runtime sort. The namespace mapper selects
`namespace_plugin_rel.sort` for synchronized configuration. New namespace plugin
relations inherit plugin defaults via `NamespacePluginRelDO`. Therefore changing
only PluginEnum or only the global plugin row does not repair an existing deployment.

Custom values remain authoritative, even if they put AG after the limiter or
before authentication. Equal sort values do not establish a safe relative order.
This change neither forbids custom ordering nor claims to correct arbitrary
custom mistakes, missing plugins or stale gateway caches. The required invariant
for each enabled LLM path is strictly:

`authentication < agentGateway < aiPrompt < aiTokenLimiter < aiProxy`

If Prompt is absent/disabled, use `authentication < agentGateway < aiTokenLimiter < aiProxy`.
Also check extension plugins for a local collision at 169.

## Default SQL changes

- MySQL/OceanBase: `db/init/mysql/schema.sql`.
- PostgreSQL/openGauss: `db/init/pg/create-table.sql`.
- Oracle: `db/init/oracle/schema.sql`.
- H2: `shenyu-admin/src/main/resources/sql-script/h2/schema.sql`.
- Agent Gateway seeds in all five `db/upgrade/2.7.1-upgrade-2.7.2-*.sql` dialects.

Both `plugin` and the default `namespace_plugin_rel` AG values are 169. Fresh-install
namespace Prompt values previously set to 171 are aligned to 170; H2 global Proxy
171 and namespace Proxy 200 are aligned to 199. These are seed corrections, not
instructions to overwrite existing custom values. Older 2.7.0 -> 2.7.1 scripts
remain historical; upgrading through them still requires the inspection below.

## Existing installations: inspect before migration

Do not rerun destructive initialization scripts or the full insert-based version
upgrade on a database that already contains AG. H2 `INSERT IGNORE` also does not
update existing rows. Use the configured database/schema and back up these rows
before changing anything. The following queries intentionally omit credentials:

```sql
SELECT id, name, sort, enabled
FROM plugin
WHERE name IN ('jwt', 'oauth2', 'keyAuth', 'basicAuth',
               'agentGateway', 'aiPrompt', 'aiTokenLimiter', 'aiProxy')
   OR sort = 169
ORDER BY sort, name;

SELECT n.id, n.namespace_id, p.name, p.sort AS plugin_sort,
       n.sort AS namespace_sort, n.enabled
FROM namespace_plugin_rel n JOIN plugin p ON p.id = n.plugin_id
WHERE p.name IN ('jwt', 'oauth2', 'keyAuth', 'basicAuth',
                 'agentGateway', 'aiPrompt', 'aiTokenLimiter', 'aiProxy')
   OR n.sort = 169
ORDER BY n.namespace_id, n.sort, p.name;

-- All AG migration candidates, including disabled namespace relations.
SELECT n.id, n.namespace_id, n.sort, n.enabled
FROM namespace_plugin_rel n JOIN plugin p ON p.id = n.plugin_id
WHERE p.name = 'agentGateway' AND n.sort = 198
ORDER BY n.namespace_id;

-- Enabled adjacent AI plugins that violate the required strict order.
SELECT a.namespace_id, pa.name AS earlier_plugin, a.sort AS earlier_sort,
       pb.name AS later_plugin, b.sort AS later_sort
FROM namespace_plugin_rel a
JOIN plugin pa ON pa.id = a.plugin_id
JOIN namespace_plugin_rel b ON b.namespace_id = a.namespace_id
JOIN plugin pb ON pb.id = b.plugin_id
WHERE a.enabled = 1 AND b.enabled = 1 AND a.sort >= b.sort
  AND ((pa.name IN ('jwt', 'oauth2', 'keyAuth', 'basicAuth') AND pb.name = 'agentGateway')
    OR (pa.name = 'agentGateway' AND pb.name IN ('aiPrompt', 'aiTokenLimiter', 'aiProxy'))
    OR (pa.name = 'aiPrompt' AND pb.name = 'aiTokenLimiter')
    OR (pa.name = 'aiTokenLimiter' AND pb.name = 'aiProxy'));
```

An empty last query is not proof of a valid complete deployment: missing/disabled
plugins, selector matching and unsynchronized caches require separate inspection.

## Explicit upgrade procedure

1. Drain affected traffic and coordinate Admin writers and the gateway rollout.
   Record the global row and every affected namespace row's old sort; review
   custom extension plugins and the checks above.
2. Confirm that each AG value of 198 is the old default. SQL cannot distinguish
   an intentional custom 198 from the default. If any such row is custom, do not
   run the broad script: restrict each UPDATE to explicitly approved row IDs.
   Other custom values, including invalid ones, are left untouched.
3. For approved default-only deployments, run the database's
   `db/upgrade/2.7.2-upgrade-2.7.3-<dialect>.sql` migration. Its Agent Gateway
   section updates only AG rows with `sort = 198` to 169, globally and in all
   namespaces, and is repeatable. It does not change enabled/config fields or
   other plugins. Run the whole migration in one transaction, inspect affected
   counts before committing, and roll back if they differ from the reviewed
   candidate list.
4. Inspect legacy Prompt/Proxy drift separately. For each reviewed row, use the
   Admin namespace plugin editor to set Prompt 170 or Proxy 199, or an explicit
   row-ID-and-old-sort-guarded UPDATE. Never blanket-normalize custom orders.
   For example, after replacing the placeholder with an approved ID:

   ```sql
   UPDATE namespace_plugin_rel SET sort = 170
   WHERE id = '<approved-prompt-relation-id>' AND sort = 171;
   UPDATE namespace_plugin_rel SET sort = 199
   WHERE id = '<approved-proxy-relation-id>' AND sort = 200;
   UPDATE plugin SET sort = 199
   WHERE id = '<approved-proxy-plugin-id>' AND name = 'aiProxy' AND sort = 171;
   ```

   The older version-upgrade Proxy default may be 200 globally too; review and
   guard against that actual old value if applicable. Do not run placeholders.
5. Direct SQL does not publish configuration-change events. Reload/restart Admin
   as required, then use its supported namespace synchronization operation for
   every affected namespace and ensure gateway cache refresh/sort events complete.
   Do not assume an updated database means an updated running chain.
6. Verify effective order on every gateway before restoring traffic: a matched
   LLM request follows AG -> Prompt -> Limiter -> Proxy, a limiter rejection sees
   the established AG context, and matched MCP never reaches the LLM chain.
   Verify unmatched/disabled AG retains existing behavior.
7. To roll back, drain traffic again and restore only the recorded row IDs/old
   values, then resynchronize. Do not globally replace all 169 values with 198.

## Regression scope and handoff

`AgentGatewayPluginOrderTest` checks the enum's relative order/slot uniqueness,
AG plugin/namespace seeds in nine scripts, the full fresh-install AI chain in
four schemas, and the explicit default-only migration statement contract.
`AgentGatewayPluginOrderingTest` exercises the real handler/cache sorting path
with lightweight recording plugins: defaults, cached defaults, old 198 values,
unsafe custom values, dynamic sort events/cache removal, LLM continuation, terminal
AG and disabled AG. Assertions use plugin names/relative order, not a hardcoded
170 AG value. These are ordering tests, not proof of real MCP/LLM body handling.

Mainline owns Maven execution; this change must not launch a concurrent Maven run.
The focused command for that owner (from the repository root) is:

```powershell
.\mvnw.cmd -B -pl shenyu-web -am test '-Dtest=AgentGatewayPluginOrderTest,PluginEnumTest,AgentGatewayPluginOrderingTest,ShenyuWebHandlerTest' -Dsurefire.failIfNoSpecifiedTests=false -DskipRemoteResources=true -Dmaven.javadoc.skip=true
```

The reported common Checkstyle null-comparison violation was replaced with
`Objects.nonNull`. Java test success must be recorded from the mainline run, not
inferred from source checks. Native multi-database execution and end-to-end Admin
synchronization are not established by the seed-pattern or handler unit tests.
No AG/context/AI/Starter production implementation, main progress document,
generic sorting algorithm or other plugin enum is changed by this order slice.
