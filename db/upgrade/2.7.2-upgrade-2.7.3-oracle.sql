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

-- Plugin graph drafts, immutable publications and Admin permissions.
CREATE TABLE plugin_graph_draft (
    namespace_pk VARCHAR2(128) NOT NULL,
    namespace_id VARCHAR2(50 CHAR) NOT NULL,
    graph_id VARCHAR2(128 CHAR) NOT NULL,
    draft_version NUMBER(19) NOT NULL,
    draft_json CLOB,
    published_revision NUMBER(19) DEFAULT 0 NOT NULL,
    PRIMARY KEY (namespace_id, graph_id),
    FOREIGN KEY (namespace_pk) REFERENCES namespace(id) ON DELETE CASCADE
);

CREATE TABLE plugin_graph_revision (
    namespace_id VARCHAR2(50 CHAR) NOT NULL,
    graph_id VARCHAR2(128 CHAR) NOT NULL,
    revision NUMBER(19) NOT NULL,
    graph_json CLOB,
    PRIMARY KEY (namespace_id, graph_id, revision),
    FOREIGN KEY (namespace_id, graph_id) REFERENCES plugin_graph_draft(namespace_id, graph_id) ON DELETE CASCADE
);

-- Graph permissions are available for explicit role grants.
INSERT INTO "resource" (id, parent_id, title, resource_type, sort, is_leaf, is_route, perms, status)
SELECT '1997997000000000001', '1346777449787125760', 'View plugin graphs', 2, 10, 1, 0, 'system:graph:view', 1 FROM dual
WHERE NOT EXISTS (SELECT 1 FROM "resource" WHERE id = '1997997000000000001');
INSERT INTO "resource" (id, parent_id, title, resource_type, sort, is_leaf, is_route, perms, status)
SELECT '1997997000000000002', '1346777449787125760', 'Manage plugin graphs', 2, 11, 1, 0, 'system:graph:edit', 1 FROM dual
WHERE NOT EXISTS (SELECT 1 FROM "resource" WHERE id = '1997997000000000002');

-- Match the built-in super role grants for other Admin resources.
INSERT INTO permission (id, object_id, resource_id)
SELECT '1997997000000000011', '1346358560427216896', '1997997000000000001' FROM dual
WHERE NOT EXISTS (SELECT 1 FROM permission WHERE object_id = '1346358560427216896' AND resource_id = '1997997000000000001');
INSERT INTO permission (id, object_id, resource_id)
SELECT '1997997000000000012', '1346358560427216896', '1997997000000000002' FROM dual
WHERE NOT EXISTS (SELECT 1 FROM permission WHERE object_id = '1346358560427216896' AND resource_id = '1997997000000000002');
