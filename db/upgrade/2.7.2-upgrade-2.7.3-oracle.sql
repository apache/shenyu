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

-- this file works for Oracle.
ALTER TABLE selector RENAME COLUMN loged TO logged;
ALTER TABLE rule RENAME COLUMN loged TO logged;

-- Move Agent Gateway ahead of the AI governance chain for existing installs.
-- Preserve deliberate custom orders: only the old default sort value changes.
UPDATE plugin SET sort = 169 WHERE name = 'agentGateway' AND sort = 198;
UPDATE namespace_plugin_rel SET sort = 169
WHERE sort = 198
  AND plugin_id IN (SELECT id FROM plugin WHERE name = 'agentGateway');

-- Stop all Admin writers and back up instance_info before this one-time migration.
-- Retain the latest heartbeat, then latest update/create timestamp, then greatest id.
-- Rows upgraded from 2.7.1 have heartbeat 0 and are ordered by timestamps and id.
DELETE FROM instance_info duplicate_instance
WHERE EXISTS (
    SELECT 1 FROM instance_info retained_instance WHERE
    retained_instance.namespace_id = duplicate_instance.namespace_id
    AND retained_instance.instance_ip = duplicate_instance.instance_ip
    AND retained_instance.instance_port = duplicate_instance.instance_port
    AND retained_instance.instance_type = duplicate_instance.instance_type
    AND (retained_instance.last_heart_beat_time > duplicate_instance.last_heart_beat_time
        OR (retained_instance.last_heart_beat_time = duplicate_instance.last_heart_beat_time
            AND retained_instance.date_updated > duplicate_instance.date_updated)
        OR (retained_instance.last_heart_beat_time = duplicate_instance.last_heart_beat_time
            AND retained_instance.date_updated = duplicate_instance.date_updated
            AND retained_instance.date_created > duplicate_instance.date_created)
        OR (retained_instance.last_heart_beat_time = duplicate_instance.last_heart_beat_time
            AND retained_instance.date_updated = duplicate_instance.date_updated
            AND retained_instance.date_created = duplicate_instance.date_created
            AND retained_instance.id > duplicate_instance.id))
);
CREATE UNIQUE INDEX uk_instance_info_identity ON instance_info (namespace_id, instance_ip, instance_port, instance_type);

-- Preserve complete API-debug request headers, including long Authorization values.
-- Stop Admin writers and back up mock_request_record before the one-time migration.
ALTER TABLE mock_request_record ADD header_clob CLOB;
UPDATE mock_request_record SET header_clob = COALESCE(TO_CLOB(header), EMPTY_CLOB());
ALTER TABLE mock_request_record DROP COLUMN header;
ALTER TABLE mock_request_record RENAME COLUMN header_clob TO header;
ALTER TABLE mock_request_record MODIFY header DEFAULT EMPTY_CLOB() NOT NULL;
COMMENT ON COLUMN mock_request_record.header IS 'the request param in header';

-- Revoke JWTs issued before a password change or an explicit revocation.
ALTER TABLE dashboard_user ADD token_version NUMBER(10) DEFAULT 0 NOT NULL;
COMMENT ON COLUMN dashboard_user.token_version IS 'token version used to revoke issued JWTs';
-- Add the opt-in rate limiter backend-failure policy selector.
INSERT INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1890000000000000001', '4', 'backendFailurePolicy', 'backendFailurePolicy', 3, 2, 5, '{"required":"0","defaultValue":"ALLOW","rule":""}');
INSERT INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1890000000000000002', 'backendFailurePolicy', 'BACKEND_FAILURE_POLICY_ALLOW', 'ALLOW', 'ALLOW', 'Allow requests when the Redis backend fails', 0, 1);
INSERT INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1890000000000000003', 'backendFailurePolicy', 'BACKEND_FAILURE_POLICY_REJECT', 'REJECT', 'REJECT', 'Reject requests when the Redis backend fails', 1, 1);

-- Register the tracing plugin. The row is inserted enabled on purpose: tracing
-- shipped in 2.7.2 as a property-only plugin, so an upgrade must not switch it
-- off for deployments which already enabled it with shenyu.tracing.enabled, and
-- a namespace created later copies the enabled flag and the sort from this row.
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled, date_created, date_updated) VALUES ('70', 'tracing', NULL, 'Tracing', 0, 1, sysdate, sysdate);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id, namespace_id, plugin_id, config, sort, enabled, date_created, date_updated) VALUES ('1890000000000000011', '649330b6-c2d7-4edc-be8e-8a54df9eb385', '70', NULL, 0, 1, sysdate, sysdate);

-- The dashboard entry and the button permissions of the tracing plugin: the resources the admin
-- registers for a plugin without one, plus the admin grant every seeded resource carries
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status, date_created, date_updated) VALUES ('1890000000000000020', '1346775491550474240', 'tracing', 'tracing', '/plug/tracing', 'tracing', 1, 0, 'pic-center', 0, 0, '', 1, TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'), TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'));
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status, date_created, date_updated) VALUES ('1890000000000000021', '1890000000000000020', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:tracingSelector:add', 1, TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'), TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'));
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status, date_created, date_updated) VALUES ('1890000000000000022', '1890000000000000020', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:tracingSelector:query', 1, TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'), TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'));
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status, date_created, date_updated) VALUES ('1890000000000000023', '1890000000000000020', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:tracingSelector:edit', 1, TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'), TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'));
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status, date_created, date_updated) VALUES ('1890000000000000024', '1890000000000000020', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:tracingSelector:delete', 1, TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'), TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'));
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status, date_created, date_updated) VALUES ('1890000000000000025', '1890000000000000020', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:tracingRule:add', 1, TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'), TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'));
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status, date_created, date_updated) VALUES ('1890000000000000026', '1890000000000000020', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:tracingRule:query', 1, TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'), TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'));
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status, date_created, date_updated) VALUES ('1890000000000000027', '1890000000000000020', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:tracingRule:edit', 1, TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'), TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'));
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status, date_created, date_updated) VALUES ('1890000000000000028', '1890000000000000020', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:tracingRule:delete', 1, TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'), TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'));
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status, date_created, date_updated) VALUES ('1890000000000000029', '1890000000000000020', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:tracing:modify', 1, TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'), TO_DATE('2026-10-31 00:00:00', 'SYYYY-MM-DD HH24:MI:SS'));
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id, date_created, date_updated) VALUES ('1890000000000000040', '1346358560427216896', '1890000000000000020', sysdate, sysdate);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id, date_created, date_updated) VALUES ('1890000000000000041', '1346358560427216896', '1890000000000000021', sysdate, sysdate);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id, date_created, date_updated) VALUES ('1890000000000000042', '1346358560427216896', '1890000000000000022', sysdate, sysdate);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id, date_created, date_updated) VALUES ('1890000000000000043', '1346358560427216896', '1890000000000000023', sysdate, sysdate);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id, date_created, date_updated) VALUES ('1890000000000000044', '1346358560427216896', '1890000000000000024', sysdate, sysdate);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id, date_created, date_updated) VALUES ('1890000000000000045', '1346358560427216896', '1890000000000000025', sysdate, sysdate);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id, date_created, date_updated) VALUES ('1890000000000000046', '1346358560427216896', '1890000000000000026', sysdate, sysdate);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id, date_created, date_updated) VALUES ('1890000000000000047', '1346358560427216896', '1890000000000000027', sysdate, sysdate);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id, date_created, date_updated) VALUES ('1890000000000000048', '1346358560427216896', '1890000000000000028', sysdate, sysdate);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id, date_created, date_updated) VALUES ('1890000000000000049', '1346358560427216896', '1890000000000000029', sysdate, sysdate);
