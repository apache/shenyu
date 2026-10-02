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

-- this file works for Oceanbase.
INSERT INTO `plugin` VALUES ('67', 'sensitiveWord', NULL, 'Ai', 197, 0, '2026-09-21 00:00:00', '2026-09-21 00:00:00', null);
INSERT INTO `plugin_handle` VALUES ('1942847622591684609', '67', 'url', 'url', 2, 3, 0, '{\"required\":\"0\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684610', '67', 'password', 'password', 2, 3, 1, '{\"required\":\"0\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684611', '67', 'database', 'database', 1, 3, 2, '{\"required\":\"0\",\"defaultValue\":\"0\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684612', '67', 'mode', 'mode', 2, 3, 3, '{\"required\":\"0\",\"defaultValue\":\"standalone\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684613', '67', 'master', 'master', 2, 3, 4, '{\"required\":\"0\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684614', '67', 'maxIdle', 'maxIdle', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":\"8\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684615', '67', 'minIdle', 'minIdle', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":\"0\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684616', '67', 'maxActive', 'maxActive', 1, 3, 7, '{\"required\":\"0\",\"defaultValue\":\"8\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684617', '67', 'maxWait', 'maxWait', 1, 3, 8, '{\"required\":\"0\",\"defaultValue\":\"-1\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684618', '67', 'redisKey', 'redisKey', 2, 2, 0, '{\"required\":\"0\",\"defaultValue\":\"shenyu:sensitive:words\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684619', '67', 'refreshIntervalSeconds', 'refreshIntervalSeconds', 1, 2, 1, '{\"required\":\"0\",\"defaultValue\":\"300\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684620', '67', 'failClosed', 'failClosed', 3, 2, 2, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684621', '67', 'words', 'words', 2, 2, 3, '{\"required\":\"0\",\"placeholder\":\"words separated by commas or by new lines\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1942847622591684622', '67', 'maxBodySize', 'maxBodySize', 1, 2, 4, '{\"required\":\"0\",\"defaultValue\":\"0\",\"placeholder\":\"the largest body that is scanned, in bytes, 0 means unlimited\",\"rule\":\"\"}', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `resource` (`id`, `parent_id`, `title`, `name`, `url`, `component`, `resource_type`, `sort`, `icon`, `is_leaf`, `is_route`, `perms`, `status`, `date_created`, `date_updated`) VALUES ('1953048313980116905', '1346775491550474240', 'sensitiveWord', 'sensitiveWord', '/plug/sensitiveWord', 'sensitiveWord', 1, 0, 'pic-center', 0, 0, '', 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `resource` (`id`, `parent_id`, `title`, `name`, `url`, `component`, `resource_type`, `sort`, `icon`, `is_leaf`, `is_route`, `perms`, `status`, `date_created`, `date_updated`) VALUES ('1953048313980116906', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordSelector:add', 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `resource` (`id`, `parent_id`, `title`, `name`, `url`, `component`, `resource_type`, `sort`, `icon`, `is_leaf`, `is_route`, `perms`, `status`, `date_created`, `date_updated`) VALUES ('1953048313980116907', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordSelector:query', 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `resource` (`id`, `parent_id`, `title`, `name`, `url`, `component`, `resource_type`, `sort`, `icon`, `is_leaf`, `is_route`, `perms`, `status`, `date_created`, `date_updated`) VALUES ('1953048313980116908', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordSelector:edit', 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `resource` (`id`, `parent_id`, `title`, `name`, `url`, `component`, `resource_type`, `sort`, `icon`, `is_leaf`, `is_route`, `perms`, `status`, `date_created`, `date_updated`) VALUES ('1953048313980116909', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordSelector:delete', 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `resource` (`id`, `parent_id`, `title`, `name`, `url`, `component`, `resource_type`, `sort`, `icon`, `is_leaf`, `is_route`, `perms`, `status`, `date_created`, `date_updated`) VALUES ('1953048313980116910', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordRule:add', 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `resource` (`id`, `parent_id`, `title`, `name`, `url`, `component`, `resource_type`, `sort`, `icon`, `is_leaf`, `is_route`, `perms`, `status`, `date_created`, `date_updated`) VALUES ('1953048313980116911', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordRule:query', 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `resource` (`id`, `parent_id`, `title`, `name`, `url`, `component`, `resource_type`, `sort`, `icon`, `is_leaf`, `is_route`, `perms`, `status`, `date_created`, `date_updated`) VALUES ('1953048313980116912', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordRule:edit', 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `resource` (`id`, `parent_id`, `title`, `name`, `url`, `component`, `resource_type`, `sort`, `icon`, `is_leaf`, `is_route`, `perms`, `status`, `date_created`, `date_updated`) VALUES ('1953048313980116913', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordRule:delete', 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `resource` (`id`, `parent_id`, `title`, `name`, `url`, `component`, `resource_type`, `sort`, `icon`, `is_leaf`, `is_route`, `perms`, `status`, `date_created`, `date_updated`) VALUES ('1953048313980116914', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWord:modify', 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1679002911061737584', 'failClosed', 'FAIL_CLOSED', 'open', 'true', '', 1, 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1679002911061737585', 'failClosed', 'FAIL_CLOSED', 'close', 'false', '', 2, 1, '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `permission` (`id`, `object_id`, `resource_id`, `date_created`, `date_updated`) VALUES ('1953049887387303965', '1346358560427216896', '1953048313980116905', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `permission` (`id`, `object_id`, `resource_id`, `date_created`, `date_updated`) VALUES ('1953049887387303966', '1346358560427216896', '1953048313980116906', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `permission` (`id`, `object_id`, `resource_id`, `date_created`, `date_updated`) VALUES ('1953049887387303967', '1346358560427216896', '1953048313980116907', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `permission` (`id`, `object_id`, `resource_id`, `date_created`, `date_updated`) VALUES ('1953049887387303968', '1346358560427216896', '1953048313980116908', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `permission` (`id`, `object_id`, `resource_id`, `date_created`, `date_updated`) VALUES ('1953049887387303969', '1346358560427216896', '1953048313980116909', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `permission` (`id`, `object_id`, `resource_id`, `date_created`, `date_updated`) VALUES ('1953049887387303970', '1346358560427216896', '1953048313980116910', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `permission` (`id`, `object_id`, `resource_id`, `date_created`, `date_updated`) VALUES ('1953049887387303971', '1346358560427216896', '1953048313980116911', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `permission` (`id`, `object_id`, `resource_id`, `date_created`, `date_updated`) VALUES ('1953049887387303972', '1346358560427216896', '1953048313980116912', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `permission` (`id`, `object_id`, `resource_id`, `date_created`, `date_updated`) VALUES ('1953049887387303973', '1346358560427216896', '1953048313980116913', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `permission` (`id`, `object_id`, `resource_id`, `date_created`, `date_updated`) VALUES ('1953049887387303974', '1346358560427216896', '1953048313980116914', '2026-09-21 00:00:00', '2026-09-21 00:00:00');
INSERT INTO `namespace_plugin_rel` (`id`,`namespace_id`,`plugin_id`, `config`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('1907261515594055681', '649330b6-c2d7-4edc-be8e-8a54df9eb385', '67', NULL, 197, 0, '2026-09-21 00:00:00', '2026-09-21 00:00:00');

-- Agent Gateway: keep upgrade seeds consistent with the fresh-install schema.
INSERT INTO `plugin` VALUES ('68', 'agentGateway', NULL, 'Ai', 198, 0, '2026-09-19 00:00:00', '2026-09-19 00:00:00', null);
INSERT INTO `plugin_handle` VALUES ('1960000000000001000', '68', 'trafficType', 'trafficType', 2, 2, 0, '{"required":"1","defaultValue":"LLM","rule":""}', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `plugin_handle` VALUES ('1960000000000001001', '68', 'responseRequestId', 'responseRequestId', 3, 2, 1, '{"required":"0","defaultValue":"false","rule":""}', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001010', '1346775491550474240', 'agentGateway', 'agentGateway', '/plug/agentGateway', 'agentGateway', 1, 0, 'pic-center', 0, 0, '', 1, '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001011', '1960000000000001010', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:agentGatewaySelector:add', 1, '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001012', '1960000000000001010', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:agentGatewaySelector:query', 1, '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001013', '1960000000000001010', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:agentGatewaySelector:edit', 1, '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001014', '1960000000000001010', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:agentGatewaySelector:delete', 1, '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001015', '1960000000000001010', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:agentGatewayRule:add', 1, '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001016', '1960000000000001010', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:agentGatewayRule:query', 1, '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001017', '1960000000000001010', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:agentGatewayRule:edit', 1, '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001018', '1960000000000001010', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:agentGatewayRule:delete', 1, '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `resource` VALUES ('1960000000000001019', '1960000000000001010', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:agentGateway:modify', 1, '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001020', '1346358560427216896', '1960000000000001010', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001021', '1346358560427216896', '1960000000000001011', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001022', '1346358560427216896', '1960000000000001012', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001023', '1346358560427216896', '1960000000000001013', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001024', '1346358560427216896', '1960000000000001014', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001025', '1346358560427216896', '1960000000000001015', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001026', '1346358560427216896', '1960000000000001016', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001027', '1346358560427216896', '1960000000000001017', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001028', '1346358560427216896', '1960000000000001018', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `permission` VALUES ('1960000000000001029', '1346358560427216896', '1960000000000001019', '2026-09-19 00:00:00', '2026-09-19 00:00:00');
INSERT INTO `namespace_plugin_rel` (`id`,`namespace_id`,`plugin_id`, `config`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('1960000000000001030','649330b6-c2d7-4edc-be8e-8a54df9eb385','68', NULL, 198, 0, '2026-09-19 00:00:00.000', '2026-09-19 00:00:00.000');
-- add index to speed up the meta data path uniqueness check
ALTER TABLE `meta_data` ADD INDEX `idx_meta_data_namespace_path` (`namespace_id`, `path`) USING BTREE;

-- Keep the tag name limit consistent with PostgreSQL, openGauss, and Oracle.
ALTER TABLE `tag` MODIFY COLUMN `tag_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'tag name';

CREATE TABLE IF NOT EXISTS `scale_policy` (
    `id` varchar(128) NOT NULL COMMENT 'primary key id',
    `sort` int NOT NULL COMMENT 'sort',
    `status` int NOT NULL COMMENT 'status 1:enable 0:disable',
    `num` int COMMENT 'number of bootstrap',
    `begin_time` datetime(3) COMMENT 'begin time',
    `end_time` datetime(3) COMMENT 'end time',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
INSERT IGNORE INTO `scale_policy` (`id`, `sort`, `status`, `num`, `begin_time`, `end_time`, `date_created`, `date_updated`) VALUES ('3', 1, 0, 10, NULL, NULL, '2024-07-31 20:00:00.000', '2024-07-31 20:00:00.000');
INSERT IGNORE INTO `scale_policy` (`id`, `sort`, `status`, `num`, `begin_time`, `end_time`, `date_created`, `date_updated`) VALUES ('2', 2, 0, 10, '2024-07-31 20:00:00.000', '2024-08-01 20:00:00.000', '2024-07-31 20:00:00.000', '2024-07-31 20:00:00.000');
INSERT IGNORE INTO `scale_policy` (`id`, `sort`, `status`, `num`, `begin_time`, `end_time`, `date_created`, `date_updated`) VALUES ('1', 3, 0, NULL, NULL, NULL, '2024-07-31 20:00:00.000', '2024-07-31 20:00:00.000');
CREATE TABLE IF NOT EXISTS `scale_rule` (
    `id` varchar(128) NOT NULL COMMENT 'primary key id',
    `metric_name` varchar(128) NOT NULL COMMENT 'metric name',
    `type` int NOT NULL COMMENT 'type 0:shenyu 1:k8s 2:others',
    `sort` int NOT NULL COMMENT 'sort',
    `status` int NOT NULL COMMENT 'status 1:enable 0:disable',
    `minimum` varchar(128) COMMENT 'minimum of metric',
    `maximum` varchar(128) COMMENT 'maximum of metric',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `scale_history` (
    `id` varchar(128) NOT NULL COMMENT 'primary key id',
    `config_id` int NOT NULL COMMENT '0:manual 1:period 2:dynamic',
    `num` int NOT NULL COMMENT 'number of bootstrap',
    `action` int NOT NULL COMMENT 'status 1:enable 0:disable',
    `msg` text COMMENT 'message',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
