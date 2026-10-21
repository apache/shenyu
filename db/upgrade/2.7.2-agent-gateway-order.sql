-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements.  See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership.  The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- "License"); you may not use this file except in compliance
-- with the License.  You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- Explicit, repeatable migration for an EXISTING Agent Gateway installation.
-- Use the application's database/schema; these statements use common SQL.
-- Before running, follow docs/agent-gateway-governance-order.md: back up and
-- review BOTH plugin and namespace_plugin_rel rows, including disabled rows.
-- A custom value equal to the old default 198 cannot be distinguished by SQL.
-- If any 198 row was deliberately customized, do NOT run this whole script;
-- restrict each UPDATE to the explicitly approved row IDs instead.
-- No plugin is enabled, no credentials/configuration are changed, and values
-- other than 198 are preserved. Prompt/limiter/proxy sorts are NOT migrated.
-- Run both UPDATEs in one operator-managed transaction, verify, then commit.
-- Database writes alone do not refresh Admin/gateway caches: resynchronize
-- all affected namespaces before routing traffic through the updated gateway.

UPDATE plugin SET sort = 169 WHERE name = 'agentGateway' AND sort = 198;

UPDATE namespace_plugin_rel SET sort = 169
WHERE sort = 198
  AND plugin_id IN (SELECT id FROM plugin WHERE name = 'agentGateway');
