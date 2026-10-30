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

-- this file works for H2.
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

-- Preserve complete API-debug request headers.
ALTER TABLE mock_request_record ALTER COLUMN header CLOB;

CREATE TABLE IF NOT EXISTS registration_fencing_target (
    id VARCHAR(128) NOT NULL PRIMARY KEY,
    date_created TIMESTAMP NOT NULL,
    date_updated TIMESTAMP NOT NULL,
    domain_id VARCHAR(128) NOT NULL,
    namespace_id VARCHAR(64) NOT NULL,
    selector_id VARCHAR(128) NOT NULL,
    plugin_id VARCHAR(128) NOT NULL,
    protocol VARCHAR(32) NOT NULL,
    host VARCHAR(255) NOT NULL,
    port INTEGER NOT NULL,
    principal_id VARCHAR(128) NOT NULL,
    enabled TINYINT NOT NULL,
    state VARCHAR(16) NOT NULL,
    generation BIGINT NOT NULL,
    owner_startup_id VARCHAR(64),
    session_id VARCHAR(128),
    last_sequence BIGINT NOT NULL,
    last_payload CLOB,
    last_result CLOB,
    last_request_id VARCHAR(64),
    last_request_payload CLOB,
    retired_startups CLOB,
    CONSTRAINT uk_registration_fencing_target_address UNIQUE (selector_id, host, port)
);
CREATE INDEX IF NOT EXISTS idx_registration_fencing_target_address
    ON registration_fencing_target (namespace_id, host, port);

-- Revoke JWTs issued before a password change or an explicit revocation.
ALTER TABLE dashboard_user ADD COLUMN token_version INT NOT NULL DEFAULT 0;

-- Add the opt-in rate limiter backend-failure policy selector.
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`)
VALUES ('1890000000000000001', '4', 'backendFailurePolicy', 'backendFailurePolicy', 3, 2, 5, '{"required":"0","defaultValue":"ALLOW","rule":""}');
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`)
VALUES ('1890000000000000002', 'backendFailurePolicy', 'BACKEND_FAILURE_POLICY_ALLOW', 'ALLOW', 'ALLOW', 'Allow requests when the Redis backend fails', 0, 1);
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`)
VALUES ('1890000000000000003', 'backendFailurePolicy', 'BACKEND_FAILURE_POLICY_REJECT', 'REJECT', 'REJECT', 'Reject requests when the Redis backend fails', 1, 1);
