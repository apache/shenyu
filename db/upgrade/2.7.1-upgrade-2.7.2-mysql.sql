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
INSERT INTO `plugin` VALUES ('67', 'sensitiveWord', null, 'Ai', 197, 0, '2026-09-21 00:00:00', '2026-09-21 00:00:00', null);
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
INSERT INTO `namespace_plugin_rel` (`id`,`namespace_id`,`plugin_id`, `config`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('1907261515594055681', '649330b6-c2d7-4edc-be8e-8a54df9eb385', '67', null, 197, 0, '2026-09-21 00:00:00', '2026-09-21 00:00:00');

-- Secondary indexes for namespace filtering, relation lookups and ordered listings.
CREATE INDEX idx_selector_ns_plugin_name ON `selector` (namespace_id, plugin_id, selector_name);
CREATE INDEX idx_rule_ns_selector_name ON `rule` (namespace_id, selector_id, rule_name);
CREATE INDEX idx_metadata_path_ns ON `meta_data` (path, namespace_id);
CREATE INDEX idx_metadata_ns_service ON `meta_data` (namespace_id, service_name);
CREATE INDEX idx_metadata_ns_app ON `meta_data` (namespace_id, app_name);
CREATE INDEX idx_app_auth_ns_key ON `app_auth` (namespace_id, app_key);
CREATE INDEX idx_auth_path_auth ON `auth_path` (auth_id);
CREATE INDEX idx_auth_param_auth ON `auth_param` (auth_id);
CREATE INDEX idx_permission_object_resource ON `permission` (object_id, resource_id);
CREATE INDEX idx_data_permission_user_type ON `data_permission` (user_id, data_type, data_id);
CREATE INDEX idx_tag_relation_api_tag ON `tag_relation` (api_id, tag_id);
CREATE INDEX idx_tag_relation_tag_api ON `tag_relation` (tag_id, api_id);
CREATE INDEX idx_api_path_method_rpc ON `api` (api_path, http_method, rpc_type);
CREATE INDEX idx_api_context ON `api` (context_path);
CREATE INDEX idx_api_state_created ON `api` (state, date_created);
CREATE INDEX idx_api_created ON `api` (date_created);
CREATE INDEX idx_api_rule_api_rule ON `api_rule_relation` (api_id, rule_id);
CREATE INDEX idx_ns_plugin_ns_plugin ON `namespace_plugin_rel` (namespace_id, plugin_id, enabled);
CREATE INDEX idx_discovery_rel_proxy ON `discovery_rel` (proxy_selector_id);
CREATE INDEX idx_discovery_rel_selector ON `discovery_rel` (selector_id);
CREATE INDEX idx_discovery_rel_handler ON `discovery_rel` (discovery_handler_id);
CREATE INDEX idx_discovery_handler_discovery ON `discovery_handler` (discovery_id);
CREATE INDEX idx_discovery_ns_plugin ON `discovery` (namespace_id, plugin_name);
CREATE INDEX idx_proxy_selector_ns ON `proxy_selector` (namespace_id);
CREATE INDEX idx_operation_log_time ON `operation_record_log` (operation_time);
CREATE INDEX idx_operation_log_operator_time ON `operation_record_log` (operator, operation_time);
CREATE INDEX idx_instance_ns_ip ON `instance_info` (namespace_id, instance_ip);
CREATE INDEX idx_mock_record_api ON `mock_request_record` (api_id);
CREATE INDEX idx_namespace_user_ns_user ON `namespace_user_rel` (namespace_id, user_id);
CREATE INDEX idx_namespace_user_user_ns ON `namespace_user_rel` (user_id, namespace_id);
CREATE INDEX idx_user_role_user_role ON `user_role` (user_id, role_id);
CREATE INDEX idx_resource_parent ON `resource` (parent_id);
