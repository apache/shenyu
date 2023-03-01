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

-- this file works for PostgreSQL, can not use "`" syntax.

/* insert plugin for brpc */
INSERT INTO "public"."plugin" VALUES ('41', 'brpc', '{"address":"127.0.0.1","port":"8005","corethreads":0,"threads":2147483647,"queues":0,"threadpool":"shared"}', 'Proxy', 310, 0, '2023-01-10 10:08:01', '2023-01-10 10:08:01');

/*insert plugin_handle data for plugin brpc*/
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172957', '41', 'address', 'address', 2, 3, 0, '{"required":"1","defaultValue":"127.0.0.1","placeholder":"address","rule":""}', '2023-01-10 10:08:01', '2023-01-10 10:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172958', '41', 'port', 'port', 2, 3, 0, '{"required":"1","defaultValue":"8005","placeholder":"port","rule":""}', '2023-01-10 10:08:01', '2023-01-10 10:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172959', '41', 'corethreads', 'corethreads', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}', '2023-01-10 10:08:01', '2023-01-10 10:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172960', '41', 'threads', 'threads', 1, 3, 0, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}', '2023-01-10 10:08:01', '2023-01-10 10:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172961', '41', 'queues', 'queues', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}', '2023-01-10 10:08:01', '2023-01-10 10:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172962', '41', 'threadpool', 'threadpool', 3, 3, 0, '{"required":"0","defaultValue":"shared","placeholder":"threadpool","rule":""}', '2023-01-10 10:08:01', '2023-01-10 10:08:01');

/*insert plugin_handle data for plugin loggingElasticSearch*/
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524972', '32', 'username', 'username', 2, 3, 4, '{"required":"0","defaultValue":""}', '2022-08-20 21:00:00', '2022-08-20 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524973', '32', 'password', 'password', 2, 3, 5, '{"required":"0","defaultValue":""}', '2022-08-20 21:00:00', '2022-08-20 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524974', '32', 'authCache', 'authCache', 2, 3, 6, '{"required":"0","defaultValue":"","placeholder":"true|false"}', '2022-08-20 21:00:00', '2022-08-20 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524975', '29', 'accessKey', 'accessKey', 2, 3, 4, '{"required":"0","defaultValue":"","placeholder":"accessKey"}', '2022-08-20 21:00:00', '2022-08-20 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524976', '29', 'secretKey', 'secretKey', 2, 3, 5, '{"required":"0","defaultValue":"","placeholder":"secretKey"}', '2022-08-20 21:00:00', '2022-08-20 21:00:00');
-- resort relation fields
UPDATE "public"."plugin_handle" SET sort = sort + 3 WHERE plugin_id = '32' AND field in ('sampleRate', 'maxResponseBody', 'maxRequestBody', 'compressAlg');
UPDATE "public"."plugin_handle" SET sort = sort + 2 WHERE plugin_id = '29' AND field in ('sampleRate', 'maxResponseBody', 'maxRequestBody', 'compressAlg');

/* insert plugin for loggingPulsar */
INSERT INTO "public"."plugin" VALUES ('35', 'loggingPulsar', '{"topic":"shenyu-access-logging", "serviceUrl": "pulsar://localhost:6650"}', 'Logging', 185, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
-- insert loggingTencentCls plugin
INSERT INTO "public"."plugin" VALUES ('36', 'loggingTencentCls', '{"endpoint": "ap-guangzhou.cls.tencentcs.com", "topic": "shenyu-topic"}', 'Logging', 176, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin for loggingClickHouse */
INSERT INTO "public"."plugin" VALUES ('38', 'loggingClickHouse', '{"host":"127.0.0.1","port":"8123","databse":"shenyu-gateway","username":"foo","password":"bar"}', 'Logging', 195, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');

/* insert plugin_handle data for plugin loggingPulsar */
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524976', '35', 'topic', 'topic', 2, 3, 1, '{"required":"1","defaultValue":"shenyu-access-logging"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524977', '35', 'serviceUrl', 'serviceUrl', 2, 3, 2, '{"required":"1","defaultValue":"pulsar://localhost:6650"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524978', '35', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524979', '35', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524980', '35', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524981', '35', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin_handle data for plugin loggingTencentCls */
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524982', '36', 'secretId', 'secretId', 2, 3, 1, '{"required":"1","defaultValue":"","placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524983', '36', 'secretKey', 'secretKey', 2, 3, 2, '{"required":"1","defaultValue":"","placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524984', '36', 'endpoint', 'endpoint', 2, 3, 3, '{"required":"1","defaultValue":"","placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524985', '36', 'topic', 'topic', 2, 3, 4, '{"required":"1","defaultValue":"","placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524986', '36', 'sendThreadCount', 'sendThreadCount', 1, 3, 5, '{"required":"0","defaultValue":1,"placeholder":"1-500"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524987', '36', 'totalSizeInBytes', 'totalSizeInBytes', 1, 3, 6, '{"required":"0","defaultValue":104857600}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524988', '36', 'maxSendThreadCount', 'maxSendThreadCount', 1, 3, 7, '{"required":"0","defaultValue":1,"placeholder":"availableProcessors + 1"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524989', '36', 'maxBlockSec', 'maxBlockSec', 1, 3, 8, '{"required":"0","defaultValue":60000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524990', '36', 'maxBatchSize', 'maxBatchSize', 1, 3, 9, '{"required":"0","defaultValue":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524991', '36', 'maxBatchCount', 'maxBatchCount', 1, 3, 10, '{"required":"0","defaultValue":4096}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524992', '36', 'lingerMs', 'lingerMs', 1, 3, 11, '{"required":"0","defaultValue":2000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524993', '36', 'retries', 'retries', 1, 3, 12, '{"required":"0","defaultValue":10}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524994', '36', 'maxReservedAttempts', 'maxReservedAttempts', 1, 3, 13, '{"required":"0","defaultValue":11}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524995', '36', 'baseRetryBackoffMs', 'baseRetryBackoffMs', 1, 3, 14, '{"required":"0","defaultValue":100}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524996', '36', 'maxRetryBackoffMs', 'maxRetryBackoffMs', 1, 3, 15, '{"required":"0","defaultValue":50000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin_handle data for plugin loggingClickHouse */
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172830', '38', 'host', 'host', 2, 3, 3, '{"required":"1","defaultValue":"127.0.0.1"}', '2023-01-02 00:17:21.150', '2023-01-02 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172831', '38', 'port', 'port', 2, 3, 4, '{"required":"1","defaultValue":"8123"}', '2023-01-02 00:17:21.150', '2023-01-02 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172832', '38', 'database', 'database', 2, 3, 5, '{"required":"0","defaultValue":"shenyu-gateway"}', '2023-01-02 00:17:21.150', '2023-01-02 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172833', '38', 'username', 'username', 2, 3, 6, '{"required":"1","defaultValue":""}', '2023-01-02 00:17:21.150', '2023-01-02 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172834', '38', 'password', 'password', 2, 3, 7, '{"required":"1","defaultValue":""}', '2023-01-02 00:17:21.150', '2023-01-02 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172835', '38', 'engine', 'engine', 3, 3, 8, '{"required":"0","defaultValue":"MergeTree"}', '2023-01-02 00:17:21.150', '2023-01-02 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172836', '38', 'clusterName', 'clusterName', 3, 3, 9, '{"required":"1","defaultValue":"cluster"}', '2023-01-02 00:17:21.150', '2023-01-02 00:17:21.150');
INSERT INTO "public"."shenyu_dict" VALUES ('1572621976689762305', 'engine', 'engine', 'ReplacingMergeTree', 'ReplacingMergeTree', '', 0, 1, '2022-09-22 00:21:11.924', '2022-09-22 00:21:11.924');
INSERT INTO "public"."shenyu_dict" VALUES ('1572621976689762306', 'engine', 'engine', 'MergeTree', 'MergeTree', '', 1, 1, '2022-09-22 00:21:11.924', '2022-09-22 00:21:11.924');

/* insert plugin_handle data for plugin loggingKafka */

INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172858', '33', 'securityProtocol', 'securityProtocol', 3, 3, 8, '{"required":"0","defaultValue":""}', '2022-09-01 22:00:00', '2022-09-01 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172859', '33', 'saslMechanism', 'saslMechanism', 3, 3, 9, '{"required":"0","defaultValue":""}', '2022-09-01 22:00:00', '2022-09-01 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172860', '33', 'userName', 'userName', 2, 3, 10, '{"required":"0","defaultValue":""}', '2022-09-01 22:00:00', '2022-09-01 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172861', '33', 'passWord', 'passWord', 2, 3, 11, '{"required":"0","defaultValue":""}', '2022-09-01 22:00:00', '2022-09-01 22:00:00');

INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784271', 'securityProtocol', 'SECURITY_PROTOCOL', 'default', '', '', 0, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784272', 'securityProtocol', 'SECURITY_PROTOCOL', 'SSL', 'SSL', '', 1, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784273', 'securityProtocol', 'SECURITY_PROTOCOL', 'PLAINTEXT', 'PLAINTEXT', '', 2, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784274', 'securityProtocol', 'SECURITY_PROTOCOL', 'SASL_PLAINTEXT', 'SASL_PLAINTEXT', '', 3, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784275', 'securityProtocol', 'SECURITY_PROTOCOL', 'SASL_SSL', 'SASL_SSL', '', 4, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784276', 'saslMechanism', 'SASL_MECHANISM', 'default', '', '', 0, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784277', 'saslMechanism', 'SASL_MECHANISM', 'GSSAPI', 'GSSAPI', '', 1, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784278', 'saslMechanism', 'SASL_MECHANISM', 'PLAIN', 'PLAIN', '', 2, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784279', 'saslMechanism', 'SASL_MECHANISM', 'OAUTHBEARER', 'OAUTHBEARER', '', 3, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784280', 'saslMechanism', 'SASL_MECHANISM', 'SCRAM-SHA-256', 'SCRAM-SHA-256', '', 4, 1,'2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784281', 'saslMechanism', 'SASL_MECHANISM', 'SCRAM-SHA-512', 'SCRAM-SHA-512', '', 5, 1, '2022-09-02 00:00:00', '2022-09-02 00:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1572621976689762307', 'loadBalance', 'LOAD_BALANCE', 'leastActive', 'leastActive', 'leastActive', 3, 1, '2023-01-17 18:02:52.924', '2023-01-17 18:02:52.924');

/*fix issue #3945 */
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172802', '8', 'loadBalance', 'loadStrategy', 3, 2, 3, '{"defaultValue":"roundRobin","rule":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');

INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172803', '10', 'flowRuleMaxQueueingTimeMs', 'flowRuleMaxQueueingTimeMs', 1, 2, 6, '{"required":"0","defaultValue":"500"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172804', '10', 'flowRuleWarmUpPeriodSec', 'flowRuleWarmUpPeriodSec', 1, 2, 6, '{"required":"0","defaultValue":"10"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');

/*create plugin resource and permission for admin #3964 */
INSERT INTO "public"."resource" VALUES ('1572525965625266176', '1346777449787125760', 'SHENYU.BUTTON.SYSTEM.RESOURCE', '', '', '', 2, 6, '', 1, 0, 'system:plugin:resource', 1, '2022-09-28 11:50:58', '2022-09-28 11:50:58');
INSERT INTO "public"."permission" VALUES ('1572525965658820608', '1346358560427216896', '1572525965625266176', '2022-09-28 11:50:58', '2022-09-28 11:50:58');

/*insert plugin for casdoor*/
INSERT INTO "public"."plugin" VALUES ('39', 'casdoor', '{"endpoint":"http://localhost:8000"}', 'Authentication', 40, 0, '2022-09-11 12:00:00', '2022-09-11 12:00:00');

/* insert plugin_handle data for plugin casdoor */
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172802', '8', 'loadBalance', 'loadStrategy', 3, 2, 3, '{"defaultValue":"roundRobin","rule":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1570590990341775360', '39', 'endpoint', 'casdoor endpoint', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46', '2022-09-16 09:50:46');
INSERT INTO "public"."plugin_handle" VALUES ('1570591047635968000', '39', 'client_id', 'client_id', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46', '2022-09-16 09:50:46');
INSERT INTO "public"."plugin_handle" VALUES ('1570591109623586816', '39', 'client_secrect', 'client_secrect', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46', '2022-09-16 09:50:46');
INSERT INTO "public"."plugin_handle" VALUES ('1570591165374275584', '39', 'certificate', 'certificate', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46', '2022-09-16 09:50:46');
INSERT INTO "public"."plugin_handle" VALUES ('1570591215131303936', '39', 'organization-name', 'organization-name', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46', '2022-09-16 09:50:46');
INSERT INTO "public"."plugin_handle" VALUES ('1570591265492312064', '39', 'application-name', 'application-name', 2, 3, 0, '{"required":"1","rule":""}', '2022-09-16 09:50:46', '2022-09-16 09:50:46');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172803', '10', 'flowRuleMaxQueueingTimeMs', 'flowRuleMaxQueueingTimeMs', 1, 2, 6, '{"required":"0","defaultValue":"500"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172804', '10', 'flowRuleWarmUpPeriodSec', 'flowRuleWarmUpPeriodSec', 1, 2, 6, '{"required":"0","defaultValue":"10"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');


/* fix issue 3966 */
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172805', '14', 'addPrefixed', 'addPrefixed', 3, 2, 3, '{"required":"1","defaultValue":"false"}', '2022-09-27 12:00:00', '2022-09-27 12:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784282', 'addPrefixed', 'ADD_PREFIXED', 'open', 'true', '', 0, 1, '2022-09-27 12:00:00', '2022-09-27 12:00:00');
INSERT INTO "public"."shenyu_dict" VALUES ('1529402613195784283', 'addPrefixed', 'ADD_PREFIXED', 'close', 'false', '', 1, 1, '2022-09-27 12:00:00', '2022-09-27 12:00:00');

/* FIX ISSUE 3997 */
INSERT INTO "public"."resource" VALUES ('1534585531108564993', '1346775491550474240', 'loggingAliyunSls', 'loggingAliyunSls', '/plug/loggingAliyunSls', 'loggingAliyunSls', 1, 0, 'block', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108564994', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108564995', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108564996', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108564997', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108564998', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108564999', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565000', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565001', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSlsRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565002', '1534585531108564993', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingAliyunSls:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');

INSERT INTO "public"."resource" VALUES ('1534585531108565003', '1346775491550474240', 'loggingTencentCls', 'loggingTencentCls', '/plug/loggingTencentCls', 'loggingTencentCls', 1, 0, 'block', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565004', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565005', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565006', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565007', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565008', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565009', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565010', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565011', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentClsRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565012', '1534585531108565003', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingTencentCls:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');

INSERT INTO "public"."resource" VALUES ('1534585531108565013', '1346775491550474240', 'loggingKafka', 'loggingKafka', '/plug/loggingKafka', 'loggingKafka', 1, 0, 'block', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565014', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565015', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565016', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565017', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565018', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565019', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565020', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565021', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafkaRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565022', '1534585531108565013', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingKafka:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');

INSERT INTO "public"."resource" VALUES ('1534585531108565023', '1346775491550474240', 'loggingPulsar', 'loggingPulsar', '/plug/loggingPulsar', 'loggingPulsar', 1, 0, 'block', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565024', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565025', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565026', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565027', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565028', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565029', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565030', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565031', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsarRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565032', '1534585531108565023', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingPulsar:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');

INSERT INTO "public"."resource" VALUES ('1534585531108565033', '1346775491550474240', 'loggingElasticSearch', 'loggingElasticSearch', '/plug/loggingElasticSearch', 'loggingElasticSearch', 1, 0, 'block', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565034', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565035', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565036', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565037', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565038', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565039', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565040', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565041', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearchRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565042', '1534585531108565033', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingElasticSearch:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');

INSERT INTO "public"."resource" VALUES ('1534585531108565043', '1346775491550474240', 'loggingClickHouse', 'loggingClickHouse', '/plug/loggingClickHouse', 'loggingClickHouse', 1, 0, 'block', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565044', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565045', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565046', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565047', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565048', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565049', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565050', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565051', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouseRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534585531108565052', '1534585531108565043', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingClickHouse:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');

INSERT INTO "public"."permission" VALUES ('1529403932886044770', '1346358560427216896', '1534585531108564993', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044771', '1346358560427216896', '1534585531108564994', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044772', '1346358560427216896', '1534585531108564995', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044773', '1346358560427216896', '1534585531108564996', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044774', '1346358560427216896', '1534585531108564997', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044775', '1346358560427216896', '1534585531108564998', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044776', '1346358560427216896', '1534585531108564999', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044777', '1346358560427216896', '1534585531108565000', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044778', '1346358560427216896', '1534585531108565001', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044779', '1346358560427216896', '1534585531108565002', '2022-06-09 01:17:57', '2022-06-09 01:17:56');

INSERT INTO "public"."permission" VALUES ('1529403932886044780', '1346358560427216896', '1534585531108565003', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044781', '1346358560427216896', '1534585531108565004', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044782', '1346358560427216896', '1534585531108565005', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044783', '1346358560427216896', '1534585531108565006', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044784', '1346358560427216896', '1534585531108565007', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044785', '1346358560427216896', '1534585531108565008', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044786', '1346358560427216896', '1534585531108565009', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044787', '1346358560427216896', '1534585531108565010', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044788', '1346358560427216896', '1534585531108565011', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044789', '1346358560427216896', '1534585531108565012', '2022-06-09 01:17:57', '2022-06-09 01:17:56');

INSERT INTO "public"."permission" VALUES ('1529403932886044790', '1346358560427216896', '1534585531108565013', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044791', '1346358560427216896', '1534585531108565014', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044792', '1346358560427216896', '1534585531108565015', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044793', '1346358560427216896', '1534585531108565016', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044794', '1346358560427216896', '1534585531108565017', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044795', '1346358560427216896', '1534585531108565018', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044796', '1346358560427216896', '1534585531108565019', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044797', '1346358560427216896', '1534585531108565020', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044798', '1346358560427216896', '1534585531108565021', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044799', '1346358560427216896', '1534585531108565022', '2022-06-09 01:17:57', '2022-06-09 01:17:56');

INSERT INTO "public"."permission" VALUES ('1529403932886044800', '1346358560427216896', '1534585531108565023', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044801', '1346358560427216896', '1534585531108565024', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044802', '1346358560427216896', '1534585531108565025', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044803', '1346358560427216896', '1534585531108565026', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044804', '1346358560427216896', '1534585531108565027', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044805', '1346358560427216896', '1534585531108565028', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044806', '1346358560427216896', '1534585531108565029', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044807', '1346358560427216896', '1534585531108565030', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044808', '1346358560427216896', '1534585531108565031', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044809', '1346358560427216896', '1534585531108565032', '2022-06-09 01:17:57', '2022-06-09 01:17:56');

INSERT INTO "public"."permission" VALUES ('1529403932886044810', '1346358560427216896', '1534585531108565033', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044811', '1346358560427216896', '1534585531108565034', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044812', '1346358560427216896', '1534585531108565035', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044813', '1346358560427216896', '1534585531108565036', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044814', '1346358560427216896', '1534585531108565037', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044815', '1346358560427216896', '1534585531108565038', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044816', '1346358560427216896', '1534585531108565039', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044817', '1346358560427216896', '1534585531108565040', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044818', '1346358560427216896', '1534585531108565041', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044819', '1346358560427216896', '1534585531108565042', '2022-06-09 01:17:57', '2022-06-09 01:17:56');

INSERT INTO "public"."permission" VALUES ('1529403932886044820', '1346358560427216896', '1534585531108565043', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044821', '1346358560427216896', '1534585531108565044', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044822', '1346358560427216896', '1534585531108565045', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044823', '1346358560427216896', '1534585531108565046', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044824', '1346358560427216896', '1534585531108565047', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044825', '1346358560427216896', '1534585531108565048', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044826', '1346358560427216896', '1534585531108565049', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044827', '1346358560427216896', '1534585531108565050', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044828', '1346358560427216896', '1534585531108565051', '2022-06-09 01:17:57', '2022-06-09 01:17:56');
INSERT INTO "public"."permission" VALUES ('1529403932886044829', '1346358560427216896', '1534585531108565052', '2022-06-09 01:17:57', '2022-06-09 01:17:56');


INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172806', '18', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172807', '18', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172808', '18', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172809', '29', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172810', '29', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172811', '29', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172812', '32', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172813', '32', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172814', '32', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172815', '33', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172816', '33', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172817', '33', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172818', '34', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172819', '34', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172820', '34', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172821', '35', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172822', '35', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172823', '35', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172824', '36', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172825', '36', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172826', '36', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172827', '38', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}', '2022-09-22 00:15:56.158', '2022-09-22 00:23:36.169');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172829', '38', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}', '2022-09-22 00:16:27.342', '2022-09-22 00:16:27.342');
INSERT INTO "public"."plugin_handle" VALUES ('1529402613204172830', '38', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}', '2022-09-22 00:17:21.150', '2022-09-22 00:17:21.150');

/* insert plugin for keyAuth */
INSERT INTO "public"."plugin" VALUES ('40', 'keyAuth', NULL, 'Authentication', 150, 0, '2022-07-24 19:00:00', '2022-07-24 19:00:00');

-- ----------------------------
-- Table structure for mock_request_record
-- ----------------------------
CREATE TABLE "public"."mock_request_record"  (
     "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
     "api_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
     "host" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
     "port" int4 NOT NULL,
     "url" varchar(1024) COLLATE "pg_catalog"."default" NOT NULL,
     "path_variable" varchar(255) COLLATE "pg_catalog"."default" NOT NULL DEFAULT '',
     "query" varchar(1024) COLLATE "pg_catalog"."default" NOT NULL DEFAULT '',
     "header" varchar(1024) COLLATE "pg_catalog"."default" NOT NULL DEFAULT '',
     "body" text COLLATE "pg_catalog"."default",
     "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
     "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."mock_request_record"."id" IS 'id';
COMMENT ON COLUMN "public"."mock_request_record"."api_id" IS 'the api id';
COMMENT ON COLUMN "public"."mock_request_record"."host" IS 'the request host';
COMMENT ON COLUMN "public"."mock_request_record"."port" IS 'the request port';
COMMENT ON COLUMN "public"."mock_request_record"."url" IS 'the request url';
COMMENT ON COLUMN "public"."mock_request_record"."path_variable" IS 'the request param in url';
COMMENT ON COLUMN "public"."mock_request_record"."query" IS 'the request param after url';
COMMENT ON COLUMN "public"."mock_request_record"."header" IS 'the request param in header';
COMMENT ON COLUMN "public"."mock_request_record"."body" IS 'the request body';
COMMENT ON COLUMN "public"."mock_request_record"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."mock_request_record"."date_updated" IS 'update time';

-- ----------------------------
-- Table structure for api
-- ----------------------------
CREATE TABLE "public"."api" (
    "id"           varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "context_path" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "api_path"     varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "http_method" int4 NOT NULL,
    "consume"      varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "produce"      varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "version"      varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "rpc_type"     varchar(64) COLLATE "pg_catalog"."default"  NOT NULL,
    "state" int2 NOT NULL,
    "ext"          varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "api_owner"    varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "api_desc"     varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "api_source" int4 NOT NULL,
    "document"     text COLLATE "pg_catalog"."default"         NOT NULL,
    "document_md5" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now()):: timestamp (0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now()):: timestamp (0) without time zone)
)
;
COMMENT ON COLUMN "public"."api"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."api"."context_path" IS 'the context_path';
COMMENT ON COLUMN "public"."api"."api_path" IS 'the api_path';
COMMENT ON COLUMN "public"."api"."http_method" IS '0-get,1-head,2-post,3-put,4-patch,5-delete,6-options,7-trace';
COMMENT ON COLUMN "public"."api"."consume" IS 'consume content-type';
COMMENT ON COLUMN "public"."api"."produce" IS 'produce content-type';
COMMENT ON COLUMN "public"."api"."version" IS 'api version,for example V0.01';
COMMENT ON COLUMN "public"."api"."rpc_type" IS 'http,dubbo,sofa,tars,websocket,springCloud,motan,grpc';
COMMENT ON COLUMN "public"."api"."state" IS '0-unpublished,1-published,2-offline';
COMMENT ON COLUMN "public"."api"."ext" IS 'extended fields';
COMMENT ON COLUMN "public"."api"."api_owner" IS 'api_owner';
COMMENT ON COLUMN "public"."api"."api_desc" IS 'the api description';
COMMENT ON COLUMN "public"."api"."api_source" IS '0-swagger,1-annotation generation,2-create manually,3-import swagger,4-import yapi';
COMMENT ON COLUMN "public"."api"."document" IS 'complete documentation of the api, including request parameters and response parameters';
COMMENT ON COLUMN "public"."api"."document_md5" IS 'document_md5';
COMMENT ON COLUMN "public"."api"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."api"."date_updated" IS 'update time';

-- ----------------------------
-- Table structure for api_rule_relation
-- ----------------------------
CREATE TABLE "public"."api_rule_relation" (
    "id"           varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "api_id"       varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "rule_id"      varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now()):: timestamp (0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now()):: timestamp (0) without time zone)
)
;
COMMENT ON COLUMN "public"."api_rule_relation"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."api_rule_relation"."api_id" IS 'the table api primary key id';
COMMENT ON COLUMN "public"."api_rule_relation"."rule_id" IS 'the table rule primary key id';
COMMENT ON COLUMN "public"."api_rule_relation"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."api_rule_relation"."date_updated" IS 'update time';

-- ----------------------------
-- Table structure for tag
-- ----------------------------
CREATE TABLE "public"."tag" (
    "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "tag_desc" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "parent_tag_id" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "ext" varchar(1024) COLLATE "pg_catalog"."default" NOT NULL,
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."tag"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."tag"."name" IS 'tag name';
COMMENT ON COLUMN "public"."tag"."tag_desc" IS 'tag desc';
COMMENT ON COLUMN "public"."tag"."parent_tag_id" IS 'parent tag id';
COMMENT ON COLUMN "public"."tag"."ext" IS 'extension';
COMMENT ON COLUMN "public"."tag"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."tag"."date_updated" IS 'update time';

-- ----------------------------
-- Table structure for tag_relation
-- ----------------------------
CREATE TABLE "public"."tag_relation" (
    "id"           varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "api_id"       varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "tag_id"      varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."tag_relation"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."tag_relation"."api_id" IS 'the table api primary key id';
COMMENT ON COLUMN "public"."tag_relation"."tag_id" IS 'tag id';
COMMENT ON COLUMN "public"."tag_relation"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."tag_relation"."date_updated" IS 'update time';

-- ----------------------------
-- Table structure for param
-- ----------------------------
CREATE TABLE "public"."param" (
    "id"           varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "api_id"       varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "model_id"     varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "type"         int4 NOT NULL,
    "name"         varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "param_desc"   varchar(1024) COLLATE "pg_catalog"."default" NOT NULL,
    "required"     int2 NOT NULL,
    "ext"          varchar(1024) COLLATE "pg_catalog"."default" NOT NULL,
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."param"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."param"."api_id" IS 'the api id';
COMMENT ON COLUMN "public"."param"."model_id" IS 'the model id, empty if not a model';
COMMENT ON COLUMN "public"."param"."type" IS '0-requestPathVariable,1-requestUrlParam,2-requestHeader,3-requestBody,4-responseHeader,5-responseBody';
COMMENT ON COLUMN "public"."param"."name" IS 'the param name';
COMMENT ON COLUMN "public"."param"."param_desc" IS 'the param description';
COMMENT ON COLUMN "public"."param"."required" IS 'whether to require (0 not required, 1 required)';
COMMENT ON COLUMN "public"."param"."ext" IS 'extended fields';
COMMENT ON COLUMN "public"."param"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."param"."date_updated" IS 'update time';

-- ----------------------------
-- Table structure for model
-- ----------------------------
CREATE TABLE "public"."model"  (
    "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "name" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "model_desc" varchar(1024) COLLATE "pg_catalog"."default" NOT NULL,
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."model"."id" IS 'id';
COMMENT ON COLUMN "public"."model"."name" IS 'the model name';
COMMENT ON COLUMN "public"."model"."model_desc" IS 'the model description';
COMMENT ON COLUMN "public"."model"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."model"."date_updated" IS 'update time';

-- ----------------------------
-- Table structure for field
-- ----------------------------
CREATE TABLE "public"."field" (
    "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "model_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "self_model_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "name" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "field_desc" varchar(1024) COLLATE "pg_catalog"."default" NOT NULL,
    "required" int2 NOT NULL,
    "ext" varchar(1024) COLLATE "pg_catalog"."default" NOT NULL,
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."field"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."field"."model_id" IS 'this field belongs to which model';
COMMENT ON COLUMN "public"."field"."self_model_id" IS 'which model of this field is';
COMMENT ON COLUMN "public"."field"."name" IS 'field name';
COMMENT ON COLUMN "public"."field"."field_desc" IS 'field description';
COMMENT ON COLUMN "public"."field"."required" IS 'whether to require (0 not required, 1 required)';
COMMENT ON COLUMN "public"."field"."ext" IS 'extended fields,can store genericTypes,eg..{"genericTypes":[model_id1,model_id2]}';
COMMENT ON COLUMN "public"."field"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."field"."date_updated" IS 'update time';

-- ----------------------------
-- Table structure for detail
-- ----------------------------
CREATE TABLE "public"."detail" (
    "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "field_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "is_example" int2 NOT NULL,
    "field_value" text COLLATE "pg_catalog"."default" NOT NULL,
    "value_desc" varchar(1024) COLLATE "pg_catalog"."default" NOT NULL,
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."detail"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."detail"."field_id" IS 'the field id';
COMMENT ON COLUMN "public"."detail"."is_example" IS 'is example or not (0 not, 1 is)';
COMMENT ON COLUMN "public"."detail"."field_value" IS 'the field value';
COMMENT ON COLUMN "public"."detail"."value_desc" IS 'field value description';
COMMENT ON COLUMN "public"."detail"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."detail"."date_updated" IS 'update time';
