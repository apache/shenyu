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

-- Add the opt-in rate limiter backend-failure policy selector.
INSERT INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1890000000000000001', '4', 'backendFailurePolicy', 'backendFailurePolicy', 3, 2, 5, '{"required":"0","defaultValue":"ALLOW","rule":""}');
INSERT INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1890000000000000002', 'backendFailurePolicy', 'BACKEND_FAILURE_POLICY_ALLOW', 'ALLOW', 'ALLOW', 'Allow requests when the Redis backend fails', 0, 1);
INSERT INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1890000000000000003', 'backendFailurePolicy', 'BACKEND_FAILURE_POLICY_REJECT', 'REJECT', 'REJECT', 'Reject requests when the Redis backend fails', 1, 1);
