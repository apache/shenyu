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
INSERT INTO `plugin` VALUES ('36', 'loggingTencentCls','{\"endpoint\": \"ap-guangzhou.cls.tencentcs.com\, \"topic\": \"shenyu-topic\"}', 'Logging', 176, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin for loggingClickHouse */
INSERT INTO `plugin` VALUES ('38', 'loggingClickHouse', '{\"host\":\"127.0.0.1\",\"port\":\"8123\",\"databse\":\"shenyu-gateway\",\"username\":\"foo\",\"password\":\"bar\"}', 'Logging', 195, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');

/* insert plugin_handle data for plugin loggingPulsar */
INSERT INTO `plugin_handle` VALUES ('1529402613204172906', '35', 'topic', 'topic', 2, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"shenyu-access-logging\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172907', '35', 'serviceUrl', 'serviceUrl', 2, 3, 2, '{\"required":"1",\"defaultValue\":\"pulsar://localhost:6650\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172908', '35', 'sampleRate', 'sampleRate', 2, 3, 4, '{\"required":"0",\"defaultValue\":\"1\",\"placeholder\":\"optional,0,0.01~1\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172909', '35', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172910', '35', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172911', '35', 'compressAlg', 'compressAlg', 3, 3, 7, '{\"required\":\"0\",\"defaultValue\":\"none\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172912', '36', 'secretId', 'secretId', 2, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin_handle data for plugin loggingTencentCls */
INSERT INTO `plugin_handle` VALUES ('1529402613204172913', '36', 'secretKey', 'secretKey', 2, 3, 2, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172914', '36', 'endpoint', 'endpoint', 2, 3, 3, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172915', '36', 'topic', 'topic', 2, 3, 4, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172916', '36', 'sendThreadCount', 'sendThreadCount', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":1,\"placeholder\":\"1-500\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172917', '36', 'totalSizeInBytes', 'totalSizeInBytes', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":104857600}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172918', '36', 'maxSendThreadCount', 'maxSendThreadCount', 1, 3, 7, '{\"required\":\"0\",\"defaultValue\":1,\"placeholder\":\"availableProcessors + 1\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172919', '36', 'maxBlockSec', 'maxBlockSec', 1, 3, 8, '{\"required\":\"0\",\"defaultValue\":60000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172920', '36', 'maxBatchSize', 'maxBatchSize', 1, 3, 9, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172921', '36', 'maxBatchCount', 'maxBatchCount', 1, 3, 10, '{\"required\":\"0\",\"defaultValue\":4096}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172922', '36', 'lingerMs', 'lingerMs', 1, 3, 11, '{\"required\":\"0\",\"defaultValue\":2000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172923', '36', 'retries', 'retries', 1, 3, 12, '{\"required\":\"0\",\"defaultValue\":10}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172924', '36', 'maxReservedAttempts', 'maxReservedAttempts', 1, 3, 13, '{\"required\":\"0\",\"defaultValue\":11}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172925', '36', 'baseRetryBackoffMs', 'baseRetryBackoffMs', 1, 3, 14, '{\"required\":\"0\",\"defaultValue\":100}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172926', '36', 'maxRetryBackoffMs', 'maxRetryBackoffMs', 1, 3, 15, '{\"required\":\"0\",\"defaultValue\":50000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin_handle data for plugin loggingClickHouse */
INSERT INTO `plugin_handle` VALUES ('1529402613204172798', '38', 'host', 'host', 2, 3, 1, '{"required":"0","defaultValue":"127.0.0.1"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172799', '38', 'port', 'port', 2, 3, 2, '{"required":"0","defaultValue":"8123"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1556899301440503808', '38', 'database', 'database', 2, 2, 0, '{"required":"0","defaultValue":"shenyu-gateway","placeholder":"database"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1556899400849702912', '38', 'username', 'username', 2, 2, 0, '{"required":"0","defaultValue":"foo","placeholder":"username"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1556899492809818112', '38', 'password', 'password', 2, 2, 0, '{"required":"0","defaultValue":"bar","placeholder":"password"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');

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
INSERT INTO `plugin_handle` VALUES ('1529402613204172891', '33', 'securityProtocol', 'securityProtocol', 3, 3, 8, '{\"required\":\"0\",\"defaultValue\":\"\"}', '2022-09-01 22:00:00', '2022-09-01 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172894', '33', 'saslMechanism', 'saslMechanism', 3, 3, 9,'{\"required\":\"0\",\"defaultValue\":\"\"}', '2022-07-04 22:00:00', '2022-09-01 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172895', '33', 'userName', 'userName', 2, 3, 10, '{\"required\":\"0\",\"defaultValue\":\"\"}', '2022-09-01 22:00:00', '2022-09-01 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172896', '33', 'passWord', 'passWord', 2, 3, 11, '{\"required\":\"0\",\"defaultValue\":\"\"}', '2022-09-01 22:00:00', '2022-09-01 22:00:00');

