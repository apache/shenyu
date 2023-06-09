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

/* insert plugin for brpc */
INSERT INTO `plugin` VALUES ('41', 'brpc', '{\"address\":\"127.0.0.1\", \"port\":\"8005\", \"corethreads\":0, \"threads\":2147483647, \"queues\":0, \"threadpool\":\"shared\"}', 'Proxy', 310, 0, '2023-01-10 10:08:01', '2023-01-10 10:08:01');

/*insert plugin_handle data for plugin brpc*/
INSERT INTO `plugin_handle` VALUES ('1529402613204172957', '41', 'address', 'address', 2, 3, 0, '{\"required\":\"1\",\"defaultValue\":\"127.0.0.1\",\"placeholder\":\"address\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1529402613204172958', '41', 'port', 'port', 2, 3, 0, '{\"required\":\"1\",\"defaultValue\":\"8005\",\"placeholder\":\"port\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1529402613204172959', '41', 'corethreads', 'corethreads', 1, 3, 0, '{\"required\":\"0\",\"defaultValue\":\"0\",\"placeholder\":\"corethreads\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1529402613204172960', '41', 'threads', 'threads', 1, 3, 0, '{\"required\":\"0\",\"defaultValue\":\"2147483647\",\"placeholder\":\"threads\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1529402613204172961', '41', 'queues', 'queues', 1, 3, 0, '{\"required\":\"0\",\"defaultValue\":\"0\",\"placeholder\":\"queues\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1529402613204172962', '41', 'threadpool', 'threadpool', 3, 3, 0, '{\"required\":\"0\",\"defaultValue\":\"shared\",\"placeholder\":\"threadpool\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');

/*insert plugin_handle data for plugin loggingElasticSearch*/
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1529402613204172906', '32', 'username', 'username', 2, 3, 3, '{"required":"0","defaultValue":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1529402613204172907', '32', 'password', 'password', 2, 3, 4, '{"required":"0","defaultValue":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1529402613204172908', '32', 'authCache', 'authCache', 2, 3, 5, '{"required":"0","defaultValue":"","placeholder":"true|false"}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1529402613204172909', '29', 'accessKey', 'accessKey', 2, 3, 4, '{"required":"0","defaultValue":"","placeholder":"accessKey"}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1529402613204172910', '29', 'secretKey', 'secretKey', 2, 3, 5, '{"required":"0","defaultValue":"","placeholder":"secretKey"}');
-- resort relation fields
UPDATE plugin_handle SET sort = sort + 3 WHERE plugin_id = '32' AND field in ('sampleRate', 'maxResponseBody', 'maxRequestBody', 'compressAlg');
UPDATE plugin_handle SET sort = sort + 2 WHERE plugin_id = '29' AND field in ('sampleRate', 'maxResponseBody', 'maxRequestBody', 'compressAlg');

/* insert plugin for loggingPulsar */
INSERT INTO `plugin` VALUES ('35', 'loggingPulsar', '{\"topic":\"shenyu-access-logging\", \"serviceUrl\": \"pulsar://localhost:6650\"}', 'Logging', 185, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');
-- insert loggingTencentCls plugin
INSERT INTO `plugin` VALUES ('36', 'loggingTencentCls','{\"endpoint\": \"ap-guangzhou.cls.tencentcs.com\", \"topic\": \"shenyu-topic\"}', 'Logging', 176, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin for loggingClickHouse */
INSERT INTO `plugin` VALUES ('38', 'loggingClickHouse', '{\"host\":\"127.0.0.1\",\"port\":\"8123\",\"databse\":\"shenyu-gateway\",\"username\":\"foo\",\"password\":\"bar\"}', 'Logging', 195, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');

/* insert plugin_handle data for plugin loggingPulsar */
INSERT INTO `plugin_handle` VALUES ('1529402613204172916', '35', 'topic', 'topic', 2, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"shenyu-access-logging\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172917', '35', 'serviceUrl', 'serviceUrl', 2, 3, 2, '{\"required":"1",\"defaultValue\":\"pulsar://localhost:6650\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172918', '35', 'sampleRate', 'sampleRate', 2, 3, 4, '{\"required":"0",\"defaultValue\":\"1\",\"placeholder\":\"optional,0,0.01~1\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172919', '35', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172920', '35', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172921', '35', 'compressAlg', 'compressAlg', 3, 3, 7, '{\"required\":\"0\",\"defaultValue\":\"none\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin_handle data for plugin loggingTencentCls */
INSERT INTO `plugin_handle` VALUES ('1529402613204172922', '36', 'secretId', 'secretId', 2, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172923', '36', 'secretKey', 'secretKey', 2, 3, 2, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172924', '36', 'endpoint', 'endpoint', 2, 3, 3, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172925', '36', 'topic', 'topic', 2, 3, 4, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172926', '36', 'sendThreadCount', 'sendThreadCount', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":1,\"placeholder\":\"1-500\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172927', '36', 'totalSizeInBytes', 'totalSizeInBytes', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":104857600}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172928', '36', 'maxSendThreadCount', 'maxSendThreadCount', 1, 3, 7, '{\"required\":\"0\",\"defaultValue\":1,\"placeholder\":\"availableProcessors + 1\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172929', '36', 'maxBlockSec', 'maxBlockSec', 1, 3, 8, '{\"required\":\"0\",\"defaultValue\":60000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172930', '36', 'maxBatchSize', 'maxBatchSize', 1, 3, 9, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172931', '36', 'maxBatchCount', 'maxBatchCount', 1, 3, 10, '{\"required\":\"0\",\"defaultValue\":4096}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172932', '36', 'lingerMs', 'lingerMs', 1, 3, 11, '{\"required\":\"0\",\"defaultValue\":2000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172933', '36', 'retries', 'retries', 1, 3, 12, '{\"required\":\"0\",\"defaultValue\":10}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172934', '36', 'maxReservedAttempts', 'maxReservedAttempts', 1, 3, 13, '{\"required\":\"0\",\"defaultValue\":11}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172935', '36', 'baseRetryBackoffMs', 'baseRetryBackoffMs', 1, 3, 14, '{\"required\":\"0\",\"defaultValue\":100}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172936', '36', 'maxRetryBackoffMs', 'maxRetryBackoffMs', 1, 3, 15, '{\"required\":\"0\",\"defaultValue\":50000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin_handle data for plugin loggingClickHouse */
INSERT INTO `plugin_handle` VALUES ('1529402613204172767', '38', 'keyword', 'keyword', 2, 2, 0, '{\"required\":\"0\",\"placeholder\":\"please use ‘;’ to split keyword\",\"rule\":\"\"}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO `plugin_handle` VALUES ('1529402613204172768', '38', 'maskType', 'maskType', 3, 2, 1, '{\"required\":\"0\",\"defaultValue\":\"dataMaskByMD5\",\"rule\":\"\"}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO `plugin_handle` VALUES ('1529402613204172769', '38', 'maskStatus', 'maskStatus', 3, 2, 2, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172770', '38', 'host', 'host', 2, 3, 3, '{\"required\":\"1\",\"defaultValue\":\"127.0.0.1\",\"rule\":\"\"}', '2022-12-30 00:17:21.150', '2022-12-30 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172771', '38', 'port', 'port', 2, 3, 4, '{\"required\":\"1\",\"defaultValue\":\"8123\",\"rule\":\"\"}', '2022-12-30 00:17:21.150', '2022-12-30 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172772', '38', 'database', 'database', 2, 3, 5, '{\"required\":\"0\",\"defaultValue\":\"shenyu-gateway\",\"rule\":\"\"}', '2022-12-30 00:17:21.150', '2022-12-30 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172773', '38', 'username', 'username', 2, 3, 6, '{\"required\":\"0\",\"defaultValue\":\"\",\"rule\":\"\"}', '2022-12-30 00:17:21.150', '2022-12-30 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172774', '38', 'password', 'password', 2, 3, 7, '{\"required\":\"0\",\"defaultValue\":\"\",\"rule\":\"\"}', '2022-12-30 00:17:21.150', '2022-12-30 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172775', '38', 'engine', 'engine', 3, 3, 8, '{\"required\":\"0\",\"defaultValue\":\"MergeTree\",\"rule\":\"\"}', '2022-12-30 00:17:21.150', '2022-12-30 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172776', '38', 'clusterName', 'clusterName', 3, 3, 9, '{\"required\":\"1\",\"defaultValue\":\"cluster\",\"rule\":\"\"}', '2022-12-30 00:17:21.150', '2022-12-30 00:17:21.150');
INSERT INTO `shenyu_dict` VALUES ('1572621976689762305', 'engine', 'engine', 'ReplacingMergeTree', 'ReplacingMergeTree', '', 0, 1, '2022-09-22 00:21:11.924', '2022-09-22 00:21:11.924');
INSERT INTO `shenyu_dict` VALUES ('1572621976689762306', 'engine', 'engine', 'MergeTree', 'MergeTree', '', 1, 1, '2022-09-22 00:21:11.924', '2022-09-22 00:21:11.924');

/* insert plugin_handle data for plugin loggingKafka */
INSERT INTO `shenyu_dict` VALUES ('1529402613195784271', 'securityProtocol', 'SECURITY_PROTOCOL', 'default', '', '', 0, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784272', 'securityProtocol', 'SECURITY_PROTOCOL', 'SSL', 'SSL', '', 1, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784273', 'securityProtocol', 'SECURITY_PROTOCOL', 'PLAINTEXT', 'PLAINTEXT', '', 2, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784274', 'securityProtocol', 'SECURITY_PROTOCOL', 'SASL_PLAINTEXT', 'SASL_PLAINTEXT', '', 3, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784275', 'securityProtocol', 'SECURITY_PROTOCOL', 'SASL_SSL', 'SASL_SSL', '', 4, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784276', 'saslMechanism', 'SASL_MECHANISM', 'default', '', '', 0, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784277', 'saslMechanism', 'SASL_MECHANISM', 'GSSAPI', 'GSSAPI', '', 1, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784278', 'saslMechanism', 'SASL_MECHANISM', 'PLAIN', 'PLAIN', '', 2, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784279', 'saslMechanism', 'SASL_MECHANISM', 'OAUTHBEARER', 'OAUTHBEARER', '', 3, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784280', 'saslMechanism', 'SASL_MECHANISM', 'SCRAM-SHA-256', 'SCRAM-SHA-256', '', 4, 1,'2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784281', 'saslMechanism', 'SASL_MECHANISM', 'SCRAM-SHA-512', 'SCRAM-SHA-512', '', 5, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');

INSERT INTO `plugin_handle` VALUES ('1529402613204172777', '33', 'securityProtocol', 'securityProtocol', 3, 3, 8, '{\"required\":\"0\",\"defaultValue\":\"\"}', '2022-09-01 22:00:00', '2022-09-01 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172778', '33', 'saslMechanism', 'saslMechanism', 3, 3, 9,'{\"required\":\"0\",\"defaultValue\":\"\"}', '2022-07-04 22:00:00', '2022-09-01 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172779', '33', 'userName', 'userName', 2, 3, 10, '{\"required\":\"0\",\"defaultValue\":\"\"}', '2022-09-01 22:00:00', '2022-09-01 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172780', '33', 'passWord', 'passWord', 2, 3, 11, '{\"required\":\"0\",\"defaultValue\":\"\"}', '2022-09-01 22:00:00', '2022-09-01 22:00:00');

/* fix issue #3945 */
INSERT INTO `plugin_handle` VALUES ('1529402613204172742', '8', 'loadBalance', 'loadStrategy', 3, 2, 3, '{\"defaultValue\":\"roundRobin\",\"rule\":\"\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');

INSERT INTO `plugin_handle` VALUES ('1529402613204172743', '10', 'flowRuleMaxQueueingTimeMs', 'flowRuleMaxQueueingTimeMs', 1, 2, 6, '{\"required\":\"0\",\"defaultValue\":\"500\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');
INSERT INTO `plugin_handle` VALUES ('1529402613204172744', '10', 'flowRuleWarmUpPeriodSec', 'flowRuleWarmUpPeriodSec', 1, 2, 6, '{\"required\":\"0\",\"defaultValue\":\"10\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');

/*create plugin resource and permission for admin #3964 */
INSERT INTO `resource` VALUES ('1572525965625266176', '1346777449787125760', 'SHENYU.BUTTON.SYSTEM.RESOURCE', '', '', '',2, 6, '', 1, 0, 'system:plugin:resource', 1, '2022-09-28 11:50:58', '2022-09-28 11:50:58');
INSERT INTO `permission` VALUES ('1572525965658820608', '1346358560427216896', '1572525965625266176', '2022-09-28 11:50:58', '2022-09-28 11:50:58');

/*insert plugin for casdoor*/
INSERT INTO `plugin` VALUES ('39', 'casdoor', '{\"endpoint\":\"http://localhost:8000\"}', 'Authentication', 40, 0, '2022-09-11 12:00:00', '2022-09-11 12:00:00');

/* insert plugin_handle data for plugin casdoor */
INSERT INTO `plugin_handle` VALUES ('1570590990341775360', '39', 'endpoint', 'casdoor endpoint', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46.925', '2022-09-16 09:50:46.925');
INSERT INTO `plugin_handle` VALUES ('1570591047635968000', '39', 'client_id', 'client_id', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46.925', '2022-09-16 09:50:46.925');
INSERT INTO `plugin_handle` VALUES ('1570591109623586816', '39', 'client_secrect', 'client_secrect', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46.925', '2022-09-16 09:50:46.925');
INSERT INTO `plugin_handle` VALUES ('1570591165374275584', '39', 'certificate', 'certificate', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46.925', '2022-09-16 09:50:46.925');
INSERT INTO `plugin_handle` VALUES ('1570591215131303936', '39', 'organization-name', 'organization-name', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46.925', '2022-09-16 09:50:46.925');
INSERT INTO `plugin_handle` VALUES ('1570591265492312064', '39', 'application-name', 'application-name', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46.925', '2022-09-16 09:50:46.925');

/* fix issue 3966 */
INSERT INTO `plugin_handle` VALUES ('1529402613204172745', '14', 'addPrefixed', 'addPrefixed', 3, 2, 3, '{\"required\":\"1\",\"defaultValue\":\"false\"}', '2022-09-27 12:00:00', '2022-09-27 12:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784282', 'addPrefixed', 'ADD_PREFIXED', 'open', 'true', '', 0, 1, '2022-09-27 12:00:00', '2022-09-27 12:00:00');
INSERT INTO `shenyu_dict` VALUES ('1529402613195784283', 'addPrefixed', 'ADD_PREFIXED', 'close', 'false', '', 1, 1, '2022-09-27 12:00:00', '2022-09-27 12:00:00');
INSERT INTO `shenyu_dict` VALUES ('1572621976689762307', 'loadBalance', 'LOAD_BALANCE', 'leastActive', 'leastActive', 'leastActive', 3, 1, '2023-01-17 18:02:52', '2023-01-17 18:02:52');

/* FIX ISSUE 3997 */
INSERT INTO `resource` VALUES ('1534585531108564993', '1346775491550474240', 'loggingAliyunSls', 'loggingAliyunSls', '/plug/loggingAliyunSls', 'loggingAliyunSls', 1, 0, 'pic-center', 0, 0, '', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108564994', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsSelector:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108564995', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsSelector:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108564996', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsSelector:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108564997', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsSelector:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108564998', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsRule:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108564999', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsRule:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565000', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsRule:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565001', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsRule:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565002', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSls:modify', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `resource` VALUES ('1534585531108565003', '1346775491550474240', 'loggingTencentCls', 'loggingTencentCls', '/plug/loggingTencentCls', 'loggingTencentCls', 1, 0, 'pic-center', 0, 0, '', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565004', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsSelector:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565005', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsSelector:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565006', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsSelector:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565007', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsSelector:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565008', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsRule:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565009', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsRule:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565010', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsRule:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565011', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsRule:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565012', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentCls:modify', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `resource` VALUES ('1534585531108565013', '1346775491550474240', 'loggingKafka', 'loggingKafka', '/plug/loggingKafka', 'loggingKafka', 1, 0, 'pic-center', 0, 0, '', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565014', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaSelector:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565015', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaSelector:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565016', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaSelector:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565017', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaSelector:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565018', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaRule:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565019', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaRule:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565020', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaRule:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565021', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaRule:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565022', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafka:modify', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `resource` VALUES ('1534585531108565023', '1346775491550474240', 'loggingPulsar', 'loggingPulsar', '/plug/loggingPulsar', 'loggingPulsar', 1, 0, 'pic-center', 0, 0, '', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565024', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarSelector:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565025', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarSelector:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565026', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarSelector:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565027', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarSelector:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565028', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarRule:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565029', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarRule:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565030', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarRule:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565031', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarRule:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565032', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsar:modify', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `resource` VALUES ('1534585531108565033', '1346775491550474240', 'loggingElasticSearch', 'loggingElasticSearch', '/plug/loggingElasticSearch', 'loggingElasticSearch', 1, 0, 'pic-center', 0, 0, '', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565034', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchSelector:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565035', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchSelector:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565036', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchSelector:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565037', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchSelector:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565038', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchRule:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565039', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchRule:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565040', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchRule:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565041', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchRule:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565042', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearch:modify', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `resource` VALUES ('1534585531108565043', '1346775491550474240', 'loggingClickHouse', 'loggingClickHouse', '/plug/loggingClickHouse', 'loggingClickHouse', 1, 0, 'pic-center', 0, 0, '', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565044', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseSelector:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565045', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseSelector:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565046', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseSelector:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565047', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseSelector:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565048', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseRule:add', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565049', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseRule:query', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565050', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseRule:edit', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565051', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseRule:delete', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `resource` VALUES ('1534585531108565052', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouse:modify', 1, '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `permission` VALUES ('1534585531389583361', '1346358560427216896', '1534585531108564993', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583362', '1346358560427216896', '1534585531108564994', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583363', '1346358560427216896', '1534585531108564995', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583364', '1346358560427216896', '1534585531108564996', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583365', '1346358560427216896', '1534585531108564997', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583366', '1346358560427216896', '1534585531108564998', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583367', '1346358560427216896', '1534585531108564999', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583368', '1346358560427216896', '1534585531108565000', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583369', '1346358560427216896', '1534585531108565001', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583370', '1346358560427216896', '1534585531108565002', '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `permission` VALUES ('1534585531389583371', '1346358560427216896', '1534585531108565003', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583372', '1346358560427216896', '1534585531108565004', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583373', '1346358560427216896', '1534585531108565005', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583374', '1346358560427216896', '1534585531108565006', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583375', '1346358560427216896', '1534585531108565007', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583376', '1346358560427216896', '1534585531108565008', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583377', '1346358560427216896', '1534585531108565009', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583378', '1346358560427216896', '1534585531108565010', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583379', '1346358560427216896', '1534585531108565011', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583380', '1346358560427216896', '1534585531108565012', '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `permission` VALUES ('1534585531389583381', '1346358560427216896', '1534585531108565013', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583382', '1346358560427216896', '1534585531108565014', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583383', '1346358560427216896', '1534585531108565015', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583384', '1346358560427216896', '1534585531108565016', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583385', '1346358560427216896', '1534585531108565017', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583386', '1346358560427216896', '1534585531108565018', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583387', '1346358560427216896', '1534585531108565019', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583388', '1346358560427216896', '1534585531108565020', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583389', '1346358560427216896', '1534585531108565021', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583390', '1346358560427216896', '1534585531108565022', '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `permission` VALUES ('1534585531389583391', '1346358560427216896', '1534585531108565023', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583392', '1346358560427216896', '1534585531108565024', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583393', '1346358560427216896', '1534585531108565025', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583394', '1346358560427216896', '1534585531108565026', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583395', '1346358560427216896', '1534585531108565027', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583396', '1346358560427216896', '1534585531108565028', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583397', '1346358560427216896', '1534585531108565029', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583398', '1346358560427216896', '1534585531108565030', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583399', '1346358560427216896', '1534585531108565031', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583400', '1346358560427216896', '1534585531108565032', '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `permission` VALUES ('1534585531389583401', '1346358560427216896', '1534585531108565033', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583402', '1346358560427216896', '1534585531108565034', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583403', '1346358560427216896', '1534585531108565035', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583404', '1346358560427216896', '1534585531108565036', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583405', '1346358560427216896', '1534585531108565037', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583406', '1346358560427216896', '1534585531108565038', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583407', '1346358560427216896', '1534585531108565039', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583408', '1346358560427216896', '1534585531108565040', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583409', '1346358560427216896', '1534585531108565041', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583410', '1346358560427216896', '1534585531108565042', '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `permission` VALUES ('1534585531389583411', '1346358560427216896', '1534585531108565043', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583412', '1346358560427216896', '1534585531108565044', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583413', '1346358560427216896', '1534585531108565045', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583414', '1346358560427216896', '1534585531108565046', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583415', '1346358560427216896', '1534585531108565047', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583416', '1346358560427216896', '1534585531108565048', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583417', '1346358560427216896', '1534585531108565049', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583418', '1346358560427216896', '1534585531108565050', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583419', '1346358560427216896', '1534585531108565051', '2022-05-25 18:02:58', '2022-05-25 18:02:58');
INSERT INTO `permission` VALUES ('1534585531389583420', '1346358560427216896', '1534585531108565052', '2022-05-25 18:02:58', '2022-05-25 18:02:58');

INSERT INTO `plugin_handle` VALUES ('1529402613204172746', '18', 'keyword', 'keyword', 2, 2, 0, '{\"required\":\"0\",\"placeholder\":\"please use ‘;’ to split keyword\",\"rule\":\"\"}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO `plugin_handle` VALUES ('1529402613204172747', '18', 'maskType', 'maskType', 3, 2, 1, '{\"required\":\"0\",\"defaultValue\":\"dataMaskByMD5\",\"rule\":\"\"}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO `plugin_handle` VALUES ('1529402613204172748', '18', 'maskStatus', 'maskStatus', 3, 2, 2, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172749', '29', 'keyword', 'keyword', 2, 2, 0, '{\"required\":\"0\",\"placeholder\":\"please use ‘;’ to split keyword\",\"rule\":\"\"}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO `plugin_handle` VALUES ('1529402613204172750', '29', 'maskType', 'maskType', 3, 2, 1, '{\"required\":\"0\",\"defaultValue\":\"dataMaskByMD5\",\"rule\":\"\"}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO `plugin_handle` VALUES ('1529402613204172751', '29', 'maskStatus', 'maskStatus', 3, 2, 2, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172752', '32', 'keyword', 'keyword', 2, 2, 0, '{\"required\":\"0\",\"placeholder\":\"please use ‘;’ to split keyword\",\"rule\":\"\"}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO `plugin_handle` VALUES ('1529402613204172753', '32', 'maskType', 'maskType', 3, 2, 1, '{\"required\":\"0\",\"defaultValue\":\"dataMaskByMD5\",\"rule\":\"\"}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO `plugin_handle` VALUES ('1529402613204172754', '32', 'maskStatus', 'maskStatus', 3, 2, 2, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172755', '33', 'keyword', 'keyword', 2, 2, 0, '{\"required\":\"0\",\"placeholder\":\"please use ‘;’ to split keyword\",\"rule\":\"\"}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO `plugin_handle` VALUES ('1529402613204172756', '33', 'maskType', 'maskType', 3, 2, 1, '{\"required\":\"0\",\"defaultValue\":\"dataMaskByMD5\",\"rule\":\"\"}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO `plugin_handle` VALUES ('1529402613204172757', '33', 'maskStatus', 'maskStatus', 3, 2, 2, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172758', '34', 'keyword', 'keyword', 2, 2, 0, '{\"required\":\"0\",\"placeholder\":\"please use ‘;’ to split keyword\",\"rule\":\"\"}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO `plugin_handle` VALUES ('1529402613204172759', '34', 'maskType', 'maskType', 3, 2, 1, '{\"required\":\"0\",\"defaultValue\":\"dataMaskByMD5\",\"rule\":\"\"}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO `plugin_handle` VALUES ('1529402613204172760', '34', 'maskStatus', 'maskStatus', 3, 2, 2, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172761', '35', 'keyword', 'keyword', 2, 2, 0, '{\"required\":\"0\",\"placeholder\":\"please use ‘;’ to split keyword\",\"rule\":\"\"}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO `plugin_handle` VALUES ('1529402613204172762', '35', 'maskType', 'maskType', 3, 2, 1, '{\"required\":\"0\",\"defaultValue\":\"dataMaskByMD5\",\"rule\":\"\"}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO `plugin_handle` VALUES ('1529402613204172763', '35', 'maskStatus', 'maskStatus', 3, 2, 2, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO `plugin_handle` VALUES ('1529402613204172764', '36', 'keyword', 'keyword', 2, 2, 0, '{\"required\":\"0\",\"placeholder\":\"please use ‘;’ to split keyword\",\"rule\":\"\"}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO `plugin_handle` VALUES ('1529402613204172765', '36', 'maskType', 'maskType', 3, 2, 1, '{\"required\":\"0\",\"defaultValue\":\"dataMaskByMD5\",\"rule\":\"\"}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO `plugin_handle` VALUES ('1529402613204172766', '36', 'maskStatus', 'maskStatus', 3, 2, 2, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');

/* insert plugin for keyAuth */
INSERT INTO `plugin` VALUES ('40', 'keyAuth', NULL, 'Authentication', 150, 0, '2022-07-24 19:00:00', '2022-07-24 19:00:00');

-- ----------------------------
-- Table structure for mock_request_record
-- ----------------------------
CREATE TABLE IF NOT EXISTS `mock_request_record`  (
    `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `api_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the api id',
    `host` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the request host',
    `port` int(5) NOT NULL COMMENT 'the request port',
    `url` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the request url',
    `path_variable` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'the request param in url',
    `query` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'the request param after url',
    `header` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'the request param in header',
    `body` text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci COMMENT 'the request body',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for api
-- ----------------------------
CREATE TABLE IF NOT EXISTS `api` (
    `id`           varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `context_path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the context_path',
    `api_path`     varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the api_path',
    `http_method`  int(0) NOT NULL COMMENT '0-get,1-head,2-post,3-put,4-patch,5-delete,6-options,7-trace',
    `consume`      varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'consume content-type',
    `produce`      varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'produce content-type',
    `version`      varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'api version,for example V0.01',
    `rpc_type`     varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'http,dubbo,sofa,tars,websocket,springCloud,motan,grpc',
    `state`        tinyint(4) NOT NULL COMMENT '0-unpublished,1-published,2-offline',
    `ext`          varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'extended fields',
    `api_owner`    varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  NOT NULL COMMENT 'api_owner',
    `api_desc`     varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the api description',
    `api_source`   int(0) NOT NULL COMMENT '0-swagger,1-annotation generation,2-create manually,3-import swagger,4-import yapi',
    `document`     text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'complete documentation of the api, including request parameters and response parameters',
    `document_md5` char(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'document_md5',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for api_rule_relation
-- ----------------------------
CREATE TABLE IF NOT EXISTS `api_rule_relation`  (
    `id`           varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `api_id`       varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the table api primary key id',
    `rule_id`      varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the table rule primary key id',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for tag
-- ----------------------------
CREATE TABLE IF NOT EXISTS `tag`  (
    `id`            varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `name`          varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'tag name',
    `tag_desc`      varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'tag description',
    `parent_tag_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'parent tag_id',
    `ext`           varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'extension info',
    `date_created`  timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated`  timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP (3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = 'api doc tag table' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for tag_relation
-- ----------------------------
CREATE TABLE IF NOT EXISTS `tag_relation`  (
    `id`           varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `api_id`       varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'api id',
    `tag_id`       varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'tag id',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP (3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = 'api doc and tag bind table' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for param
-- ----------------------------
CREATE TABLE IF NOT EXISTS `param`  (
    `id`           varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `api_id`       varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the api id',
    `model_id`     varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the model id, empty if not a model',
    `type`         int(0) NOT NULL COMMENT '0-requestPathVariable,1-requestUrlParam,2-requestHeader,3-requestBody,4-responseHeader,5-responseBody',
    `name`         varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the param name',
    `param_desc`   varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the param description',
    `required`     tinyint(0) NOT NULL COMMENT 'whether to require (0 not required, 1 required)',
    `ext`          varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'extended fields',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for model
-- ----------------------------
CREATE TABLE IF NOT EXISTS `model`  (
    `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the model name',
    `model_desc`   varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the model description',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for field
-- ----------------------------
CREATE TABLE IF NOT EXISTS `field`  (
    `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `model_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'this field belongs to which model',
    `self_model_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'which model of this field is',
    `name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'field name',
    `field_desc` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'field description',
    `required`     tinyint(0) NOT NULL COMMENT 'whether to require (0 not required, 1 required)',
    `ext`          varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'extended fields,can store genericTypes,eg..{"genericTypes":[model_id1,model_id2]}',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = 'field document table' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for detail
-- ----------------------------
CREATE TABLE IF NOT EXISTS `detail`  (
    `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `field_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the field id',
    `is_example` tinyint(0) NOT NULL COMMENT 'is example or not (0 not, 1 is)',
    `field_value` text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the field value',
    `value_desc` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'field value description',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = 'field value detail table' ROW_FORMAT = Dynamic;
