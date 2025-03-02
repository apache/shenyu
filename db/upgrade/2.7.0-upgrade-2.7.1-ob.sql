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
INSERT INTO `plugin_handle` VALUES ('1722804548510507032', '19', 'handleType', 'handleType', 2, 3, 1, '{"required":"0","rule":""}', '2025-01-02 17:20:50.233', '2025-01-02 17:20:50.233');

INSERT INTO `plugin` VALUES ('50', 'aiProxy', '{"provider":"OpenAI","baseUrl":"https://api.openai.com/v1/chat/completions","model":"gpt-4o-mini","apiKey":"your_api_key","temperature":"0.5","maxTokens":"1000","stream":"false","prompt":""}', 'Ai', 200, 0, '2023-12-20 18:02:53', '2023-12-20 18:02:53', null);

INSERT INTO `plugin_handle` VALUES ('1722804548510507033', '50', 'provider', 'provider', 3, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"OpenAI\",\"placeholder\":\"provider\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507034', '50', 'baseUrl', 'baseUrl', 2, 3, 2, '{\"required\":\"1\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507036', '50', 'model', 'model', 2, 3, 3, '{\"required\":\"1\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507035', '50', 'apiKey', 'apiKey', 2, 3, 4, '{\"required\":\"1\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507037', '50', 'temperature', 'temperature', 2, 3, 5, '{\"required\":\"0\",\"rule\":\"\", \"placeholder\":\"optional,0,0.01~1\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507038', '50', 'maxTokens', 'maxTokens', 2, 3, 6, '{\"required\":\"0\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507039', '50', 'stream', 'stream', 3, 3, 7, '{\"defaultValue\":\"false\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507040', '50', 'prompt', 'prompt', 2, 3, 8, '{\"required\":\"0\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');

INSERT INTO `shenyu_dict` VALUES ('1679002911061737482', 'provider', 'PROVIDER_TYPE_OPENAI', 'OpenAI', 'OpenAI', 'OpenAI', 0, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO `shenyu_dict` VALUES ('1679002911061737483', 'provider', 'PROVIDER_TYPE_DEEPSEEK', 'DeepSeek', 'DeepSeek', 'DeepSeek', 1, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO `shenyu_dict` VALUES ('1679002911061737484', 'provider', 'PROVIDER_TYPE_MOONSHOT', 'Moonshot', 'Moonshot', 'Moonshot', 2, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO `shenyu_dict` VALUES ('1679002911061737485', 'provider', 'PROVIDER_TYPE_OPENAPI', 'OpenAPI', 'OpenAPI', 'OpenAPI', 3, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO `shenyu_dict` VALUES ('1679002911061737486', 'provider', 'PROVIDER_TYPE_ALIYUN', 'ALiYun', 'ALiYun', 'ALiYun', 4, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');

INSERT INTO `namespace_plugin_rel` (`id`,`namespace_id`,`plugin_id`, `config`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('1801816010882822187','649330b6-c2d7-4edc-be8e-8a54df9eb385','50', '{\"provider\":\"OpenAI\",\"baseUrl\":\"https://api.openai.com/v1/chat/completions\",\"model\":\"gpt-4o-mini\",\"apiKey\":\"your_api_key\",\"temperature\":\"0.5\",\"maxTokens\":\"1000\",\"stream\":\"false\",\"prompt\":\"\"}', 171, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');

INSERT INTO `resource` VALUES ('1844026099075534849', '1346775491550474240', 'aiProxy', 'aiProxy', '/plug/aiProxy', 'aiProxy', 1, 0, 'pic-center', 0, 0, '', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1844026099075534850', '1844026099075534849', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:aiProxySelector:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1844026099075534851', '1844026099075534849', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:aiProxySelector:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1844026099075534852', '1844026099075534849', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:aiProxySelector:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1844026099075534853', '1844026099075534849', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:aiProxySelector:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1844026099075534854', '1844026099075534849', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:aiProxyRule:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1844026099075534855', '1844026099075534849', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:aiProxyRule:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1844026099075534856', '1844026099075534849', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:aiProxyRule:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1844026099075534857', '1844026099075534849', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:aiProxyRule:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1844026099075534858', '1844026099075534849', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:aiProxy:modify', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `permission` VALUES ('1697146860569542741', '1346358560427216896', '1844026099075534849', '2023-08-31 06:59:01', '2023-08-31 06:59:01');
INSERT INTO `permission` VALUES ('1697146860569542742', '1346358560427216896', '1844026099075534850', '2023-08-31 07:22:07', '2023-08-31 07:22:07');
INSERT INTO `permission` VALUES ('1697146860569542743', '1346358560427216896', '1844026099075534851', '2023-08-31 07:14:26', '2023-08-31 07:14:26');
INSERT INTO `permission` VALUES ('1697146860569542744', '1346358560427216896', '1844026099075534852', '2023-08-31 07:22:07', '2023-08-31 07:22:07');
INSERT INTO `permission` VALUES ('1697146860569542745', '1346358560427216896', '1844026099075534853', '2023-08-31 07:18:37', '2023-08-31 07:18:37');
INSERT INTO `permission` VALUES ('1697146860569542746', '1346358560427216896', '1844026099075534854', '2023-08-31 07:18:37', '2023-08-31 07:18:37');
INSERT INTO `permission` VALUES ('1697146860569542747', '1346358560427216896', '1844026099075534855', '2023-08-31 07:18:37', '2023-08-31 07:18:37');
INSERT INTO `permission` VALUES ('1697146860569542748', '1346358560427216896', '1844026099075534856', '2023-08-31 07:18:37', '2023-08-31 07:18:37');
INSERT INTO `permission` VALUES ('1697146860569542749', '1346358560427216896', '1844026099075534857', '2023-08-31 07:18:37', '2023-08-31 07:18:37');
INSERT INTO `permission` VALUES ('1697146860569542750', '1346358560427216896', '1844026099075534858', '2023-08-31 07:18:37', '2023-08-31 07:18:37');

INSERT INTO `plugin_handle` VALUES ('1722804548510507041', '50', 'provider', 'provider', 3, 1, 0, '{\"required\":\"1\",\"defaultValue\":\"OpenAI\",\"placeholder\":\"provider\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507042', '50', 'baseUrl', 'baseUrl', 2, 1, 1, '{\"required\":\"1\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507043', '50', 'model', 'model', 2, 1, 2, '{\"required\":\"1\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507044', '50', 'apiKey', 'apiKey', 2, 1, 3, '{\"required\":\"1\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507045', '50', 'temperature', 'temperature', 2, 1, 4, '{\"required\":\"0\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507046', '50', 'maxTokens', 'maxTokens', 2, 1, 5, '{\"required\":\"0\",\"rule\":\"\", \"placeholder\":\"optional,0,0.01~1\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507047', '50', 'stream', 'stream', 3, 1, 6, '{\"defaultValue\":\"false\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
INSERT INTO `plugin_handle` VALUES ('1722804548510507048', '50', 'prompt', 'prompt', 2, 1, 7, '{\"required\":\"0\",\"rule\":\"\"}', '2024-01-02 17:20:50.233', '2024-01-02 17:20:50.233');
