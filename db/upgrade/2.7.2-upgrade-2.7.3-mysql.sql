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

-- this file works for MySQL.
ALTER TABLE `selector` RENAME COLUMN `loged` TO `logged`;
ALTER TABLE `rule` RENAME COLUMN `loged` TO `logged`;

-- Move Agent Gateway ahead of the AI governance chain for existing installs.
-- Preserve deliberate custom orders: only the old default sort value changes.
UPDATE `plugin` SET `sort` = 169 WHERE `name` = 'agentGateway' AND `sort` = 198;
UPDATE `namespace_plugin_rel` SET `sort` = 169
WHERE `sort` = 198
  AND `plugin_id` IN (SELECT `id` FROM `plugin` WHERE `name` = 'agentGateway');

-- Stop all Admin writers and back up instance_info before this one-time migration.
-- Retain the latest heartbeat, then latest update/create timestamp, then greatest id.
-- Rows upgraded from 2.7.1 have heartbeat 0 and are ordered by timestamps and id.
DELETE duplicate_instance FROM instance_info duplicate_instance
JOIN instance_info retained_instance ON
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
            AND retained_instance.id > duplicate_instance.id));
CREATE UNIQUE INDEX uk_instance_info_identity ON instance_info (namespace_id, instance_ip, instance_port, instance_type);

-- Preserve complete API-debug request headers, including long Authorization values.
-- Stop Admin writers and back up mock_request_record before the one-time migration.
ALTER TABLE mock_request_record MODIFY COLUMN header TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the request param in header';

-- Agent Gateway OpenAPI import ownership: Admin-only provenance, never synchronized to gateways.
-- Stop Admin writers and back up rule before this one-time migration: handle expands to MEDIUMTEXT.
CREATE TABLE `agent_gateway_import`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
  `rule_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'target agent gateway rule id',
  `namespace_id` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'namespace id',
  `manifest` mediumtext CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'per-source import ownership manifest',
  `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
  `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic COMMENT 'agent gateway openapi import ownership';
CREATE UNIQUE INDEX uk_agent_gateway_import_rule ON `agent_gateway_import` (`rule_id`);
ALTER TABLE `rule` MODIFY COLUMN `handle` MEDIUMTEXT;
-- Rule update events snapshot the full handle into operation_record_log.context.
ALTER TABLE `operation_record_log` MODIFY COLUMN `context` MEDIUMTEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'log context';

CREATE TABLE IF NOT EXISTS `registration_fencing_target` (
    `id` varchar(128) NOT NULL,
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3),
    `domain_id` varchar(128) NOT NULL,
    `namespace_id` varchar(64) NOT NULL,
    `selector_id` varchar(128) NOT NULL,
    `plugin_id` varchar(128) NOT NULL,
    `protocol` varchar(32) NOT NULL,
    `host` varchar(255) NOT NULL,
    `port` int NOT NULL,
    `principal_id` varchar(128) NOT NULL,
    `enabled` tinyint NOT NULL,
    `state` varchar(16) NOT NULL,
    `generation` bigint NOT NULL,
    `owner_startup_id` varchar(64) DEFAULT NULL,
    `session_id` varchar(128) DEFAULT NULL,
    `last_sequence` bigint NOT NULL,
    `last_payload` text,
    `last_result` text,
    `last_request_id` varchar(64) DEFAULT NULL,
    `last_request_payload` text,
    `retired_startups` text,
    PRIMARY KEY (`id`),
    UNIQUE KEY `uk_registration_fencing_target_address` (`selector_id`, `host`, `port`),
    KEY `idx_registration_fencing_target_address` (`namespace_id`, `host`, `port`),
    KEY `idx_fencing_domain_session` (`domain_id`, `session_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Revoke JWTs issued before a password change or an explicit revocation.
ALTER TABLE dashboard_user ADD COLUMN token_version INT NOT NULL DEFAULT 0 COMMENT 'token version used to revoke issued JWTs';

-- Add the opt-in rate limiter backend-failure policy selector.
INSERT INTO `plugin_handle` (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`)
VALUES ('1890000000000000001', '4', 'backendFailurePolicy', 'backendFailurePolicy', 3, 2, 5, '{"required":"0","defaultValue":"ALLOW","rule":""}', '2026-10-30 00:00:00', '2026-10-30 00:00:00');
INSERT INTO `shenyu_dict` (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`)
VALUES ('1890000000000000002', 'backendFailurePolicy', 'BACKEND_FAILURE_POLICY_ALLOW', 'ALLOW', 'ALLOW', 'Allow requests when the Redis backend fails', 0, 1, '2026-10-30 00:00:00', '2026-10-30 00:00:00');
INSERT INTO `shenyu_dict` (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`)
VALUES ('1890000000000000003', 'backendFailurePolicy', 'BACKEND_FAILURE_POLICY_REJECT', 'REJECT', 'REJECT', 'Reject requests when the Redis backend fails', 1, 1, '2026-10-30 00:00:00', '2026-10-30 00:00:00');

-- AI request id plugin: one logical request id shared by AI governance and upstream attempts.
INSERT INTO `plugin` VALUES ('70', 'aiRequestId', NULL, 'Ai', 168, 0, '2026-11-10 00:00:00', '2026-11-10 00:00:00', NULL);
INSERT INTO `plugin_handle` VALUES ('1960000000000001070', '70', 'responseHeaderEnabled', 'responseHeaderEnabled', 3, 2, 0, '{"required":"0","defaultValue":"true","rule":""}', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `namespace_plugin_rel` (`id`,`namespace_id`,`plugin_id`, `config`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('1960000000000001071','649330b6-c2d7-4edc-be8e-8a54df9eb385','70', NULL, 168, 0, '2026-11-10 00:00:00.000', '2026-11-10 00:00:00.000');
INSERT INTO `resource` VALUES ('1960000000000001072', '1346775491550474240', 'aiRequestId', 'aiRequestId', '/plug/aiRequestId', 'aiRequestId', 1, 0, 'pic-center', 0, 0, '', 1, '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001073', '1960000000000001072', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:aiRequestIdSelector:add', 1, '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001074', '1960000000000001072', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:aiRequestIdSelector:query', 1, '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001075', '1960000000000001072', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:aiRequestIdSelector:edit', 1, '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001076', '1960000000000001072', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:aiRequestIdSelector:delete', 1, '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001077', '1960000000000001072', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:aiRequestIdRule:add', 1, '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001078', '1960000000000001072', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:aiRequestIdRule:query', 1, '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001079', '1960000000000001072', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:aiRequestIdRule:edit', 1, '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001080', '1960000000000001072', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:aiRequestIdRule:delete', 1, '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001081', '1960000000000001072', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:aiRequestId:modify', 1, '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001082', '1346358560427216896', '1960000000000001072', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001083', '1346358560427216896', '1960000000000001073', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001084', '1346358560427216896', '1960000000000001074', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001085', '1346358560427216896', '1960000000000001075', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001086', '1346358560427216896', '1960000000000001076', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001087', '1346358560427216896', '1960000000000001077', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001088', '1346358560427216896', '1960000000000001078', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001089', '1346358560427216896', '1960000000000001079', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001090', '1346358560427216896', '1960000000000001080', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001091', '1346358560427216896', '1960000000000001081', '2026-11-10 00:00:00', '2026-11-10 00:00:00');
