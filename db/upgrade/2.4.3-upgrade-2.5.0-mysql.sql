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

-- Note: it doesn't matter if you don't execute this SQL
-- Note: because most users have changed ZK configuration, this SQL is annotated to prevent erroneous execution
-- UPDATE plugin SET config='{"register":"127.0.0.1:2181","corethreads":0,"threads":2147483647,"queues":0}' WHERE `name` = 'motan';
-- if you want to execute this SQL, please replace it with your ZK configuration

-- insert plugin_handle data for motan
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1510270286164094976', '17', 'corethreads', 'corethreads', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1510270476329644032', '17', 'threads', 'threads', 1, 3, 0, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1510270555383885824', '17', 'queues', 'queues', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1515116191850078208', '17', 'threadpool', 'threadpool', 3, 3, 0, '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}');

-- Note: it doesn't matter if you don't execute this SQL
-- Note: because most users have changed ZK configuration, this SQL is annotated to prevent erroneous execution
-- UPDATE plugin SET config='{"register":"zookeeper://localhost:2181","corethreads":0,"threads":2147483647,"queues":0,"threadpool":"cached"}' WHERE `name` = 'motan';
--                                                    ^^^^^^^^^^^^^^ if you want to execute this SQL, please replace it with your own ZK configuration

-- insert dict for all plugin
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1516043399649357824', 'operator', 'OPERATOR', 'startsWith', 'startsWith', 'startsWith', 7, 1);
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1516043495265869824', 'operator', 'OPERATOR', 'endsWith', 'endsWith', 'endsWith', 8, 1);
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1629402613195884212', 'operator', 'OPERATOR', 'pathPattern', 'pathPattern', 'pathPattern', 9, 1);
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1529402613195784246', 'threadpool', 'THREADPOOL', 'default', 'default', '', 5, 1);
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1545811989312315392', 'apidoc', 'API_DOC_GLOBAL_FLAG', 'status', 'true', 'Global switching (on or off) of API documents.', 0, 1);
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1545812101056962560', 'apidocEnv', 'ENV_LABEL_OFFLINE', 'Offline', 'http://127.0.0.1:9195', 'Offline environment', 0, 1);
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1545812228228259840', 'apidocEnv', 'ENV_LABEL_ONLINE', 'Online', 'http://127.0.0.1:9196', 'Online environment', 1, 1);
-- insert shenyu_dict data for sign
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1529402613195784247', 'signRequestBody', 'SIGN_REQUEST_BODY', 'close', 'false', 'close', 1, 1);
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1529402613195784248', 'signRequestBody', 'SIGN_REQUEST_BODY', 'open', 'true', 'open', 0, 1);

-- refactor logging name
UPDATE plugin SET name = 'LoggingConsole' WHERE name = 'logging';

-- new table operation_record_log
-- ----------------------------
-- Table structure for operation_record_log
-- ----------------------------
CREATE TABLE IF NOT EXISTS `operation_record_log`
(
    `id`             bigint auto_increment        not null comment 'id' primary key,
    `color`          varchar(20)                  not null comment 'log color',
    `context`        text                         not null comment 'log context',
    `operator`       varchar(200)                 not null comment 'operator [user or app]]',
    `operation_time` datetime    default now()    not null comment 'operation time',
    `operation_type` varchar(60) default 'update' not null comment 'operation type：create/update/delete/register...'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC  comment 'operation record log';

-- insert plugin_handle data for tars
INSERT INTO `plugin_handle` VALUES ('1529402613204172868', '13', 'corethreads', 'corethreads', 1, 3, 3, '{\"required\":\"0\",\"defaultValue\":\"0\",\"placeholder\":\"corethreads\",\"rule\":\"\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');
INSERT INTO `plugin_handle` VALUES ('1529402613204172869', '13', 'threads', 'threads', 1, 3, 4, '{\"required\":\"0\",\"defaultValue\":\"2147483647\",\"placeholder\":\"threads\",\"rule\":\"\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');
INSERT INTO `plugin_handle` VALUES ('1529402613204172870', '13', 'queues', 'queues', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":\"0\",\"placeholder\":\"queues\",\"rule\":\"\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');
INSERT INTO `plugin_handle` VALUES ('1529402613204172871', '13', 'threadpool', 'threadpool', 3, 3, 2, '{\"required\":\"0\",\"defaultValue\":\"default\",\"placeholder\":\"threadpool\",\"rule\":\"\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');
-- insert plugin_handle data for sofa
INSERT INTO `plugin_handle` VALUES ('1529402613204172872', '11', 'corethreads', 'corethreads', 1, 3, 4, '{\"required\":\"0\",\"defaultValue\":\"0\",\"placeholder\":\"corethreads\",\"rule\":\"\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');
INSERT INTO `plugin_handle` VALUES ('1529402613204172873', '11', 'threads', 'threads', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":\"2147483647\",\"placeholder\":\"threads\",\"rule\":\"\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');
INSERT INTO `plugin_handle` VALUES ('1529402613204172874', '11', 'queues', 'queues', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":\"0\",\"placeholder\":\"queues\",\"rule\":\"\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');
INSERT INTO `plugin_handle` VALUES ('1529402613204172875', '11', 'threadpool', 'threadpool', 3, 3, 3, '{\"required\":\"0\",\"defaultValue\":\"default\",\"placeholder\":\"threadpool\",\"rule\":\"\"}', '2022-05-25 18:02:53', '2022-05-25 18:02:53');

INSERT INTO `resource` VALUES ('1534577121923309568', '', 'Document', '', '/document', '', 0, 3, 'file-text', 0, 0, '', 1, '2022-06-09 00:44:32', '2022-06-09 01:06:45');
INSERT INTO `resource` VALUES ('1534585430311051264', '1534577121923309568', 'API document', '', '/document/apidoc', '', 1, 0, 'api', 0, 0, '', 1, '2022-06-09 01:17:32', '2022-06-09 01:17:32');
INSERT INTO `resource` VALUES ('1534585531108564992', '1534585430311051264', 'send', '', '', '', 2, 0, '', 1, 0, 'document:apirun:send', 1, '2022-06-09 01:17:56', '2022-06-09 01:17:56');

INSERT INTO `permission` VALUES ('1534577122279825408', '1346358560427216896', '1534577121923309568', '2022-06-09 00:44:32', '2022-06-09 00:44:31');
INSERT INTO `permission` VALUES ('1534585430587875328', '1346358560427216896', '1534585430311051264', '2022-06-09 01:17:33', '2022-06-09 01:17:32');
INSERT INTO `permission` VALUES ('1534585531389583360', '1346358560427216896', '1534585531108564992', '2022-06-09 01:17:57', '2022-06-09 01:17:56');

/* insert plugin for loggingElasticSearch  */
INSERT INTO `plugin` VALUES ('32', 'loggingElasticSearch','{\"host\":\"localhost\", \"port\": \"9200\"}', 'Logging', 190, 0, '2022-06-19 22:00:00', '2022-06-19 22:00:00');

/*insert plugin_handle data for plugin loggingElasticSearch*/
INSERT INTO `plugin_handle` VALUES ('1529402613204172876', '32', 'host', 'host', 2, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"localhost\"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172877', '32', 'port', 'port', 2, 3, 2, '{\"required\":\"1\",\"defaultValue\":\"9200\"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172878', '32', 'sampleRate', 'sampleRate', 2, 3, 3, '{\"required\":\"0\",\"defaultValue\":\"1\",\"placeholder\":\"optional,0,0.01~1\"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172879', '32', 'maxResponseBody', 'maxResponseBody', 1, 3, 4, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172880', '32', 'maxRequestBody', 'maxRequestBody', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172881', '32', 'compressAlg', 'compressAlg', 3, 3, 6, '{\"required\":\"0\",\"defaultValue\":\"none\"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172882', '32', 'index', 'index', 2, 1, 1, '{\"required\":\"0\",\"defaultValue\":\"\",\"placeholder\":\"optional\"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172883', '32', 'sampleRate', 'sampleRate', 2, 1, 2, '{\"required\":\"0\",\"defaultValue\":\"\",\"placeholder\":\"optional,0,0.01~1\"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');

/* insert plugin for loggingAliyunSls */
INSERT INTO `plugin` VALUES ('34', 'loggingAliyunSls','{\"projectName\": \"shenyu\", \"logStoreName\": \"shenyu-logstore\", \"topic\": \"shenyu-topic\"}', 'Logging', 175, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');

/* insert plugin_handle data for plugin loggingAliyunSls */
INSERT INTO `plugin_handle` VALUES ('1529402613204172892', '34', 'accessId', 'accessId', 2, 3, 0, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172893', '34', 'accessKey', 'accessKey', 2, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172894', '34', 'host', 'host', 2, 3, 2, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172895', '34', 'projectName', 'projectName', 2, 3, 3, '{\"required\":\"0\",\"defaultValue\":\"shenyu\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172896', '34', 'logStoreName', 'logStoreName', 2, 3, 4, '{\"required\":\"0\",\"defaultValue\":\"shenyu-logstore\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172897', '34', 'topic', 'topic', 2, 3, 5, '{\"required\":\"0\",\"defaultValue\":\"shenyu-topic\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172898', '34', 'ttlInDay', 'ttlInDay', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":3,\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172899', '34', 'shardCount', 'shardCount', 1, 3, 7, '{\"required\":\"0\",\"defaultValue\":10,\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172900', '34', 'sendThreadCount', 'sendThreadCount', 1, 3, 8, '{\"required\":\"0\",\"defaultValue\":1,\"placeholder\":\"1-500\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172901', '34', 'ioThreadCount', 'ioThreadCount', 1, 3, 9, '{\"required\":\"0\",\"defaultValue\":1,\"placeholder\":\"1-500\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172902', '34', 'sampleRate', 'sampleRate', 2, 3, 10, '{\"required\":\"0\",\"defaultValue\":\"1\",\"placeholder\":\"optional,0,0.01~1\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172903', '34', 'maxRequestBody', 'maxRequestBody', 1, 3, 11, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172904', '34', 'maxResponseBody', 'maxResponseBody', 1, 3, 12, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172905', '34', 'bufferQueueSize', 'bufferQueueSize', 1, 3, 13, '{\"required\":\"0\",\"defaultValue\":50000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');

/* insert plugin for loggingElasticSearch  */
INSERT INTO `plugin` VALUES ('33', 'loggingKafka','{\"host\":\"localhost\", \"port\": \"9092\"}', 'Logging', 180, 0, '2022-07-04 22:00:00', '2022-07-02 22:00:00');

/*insert plugin_handle data for plugin loggingKafka*/
INSERT INTO `plugin_handle` VALUES ('1529402613204172885', '33', 'topic', 'topic', 2, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"shenyu-access-logging\"}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172886', '33', 'namesrvAddr', 'namesrvAddr', 2, 3, 2, '{\"required\":\"1\",\"defaultValue\":\"localhost:9092\"}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172887', '33', 'sampleRate', 'sampleRate', 2, 3, 4, '{\"required\":\"0\",\"defaultValue\":\"1\",\"placeholder\":\"optional,0,0.01~1\"}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172888', '33', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172889', '33', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172890', '33', 'compressAlg', 'compressAlg', 3, 3, 7, '{\"required\":\"0\",\"defaultValue\":\"none\"}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');

-- insert plugin_handle data for sign
INSERT INTO `plugin_handle` VALUES ('1529402613204172884', '1', 'signRequestBody', 'signRequestBody', 3, 2, 9, '{"required":"0","defaultValue":"false","placeholder":"signRequestBody","rule":""}', '2022-06-29 10:08:02', '2022-06-29 10:08:02');

-- use shared thread pool as default
UPDATE plugin SET config='{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"shared"}' WHERE "name" = 'grpc';
UPDATE plugin SET config='{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"shared"}' WHERE "name" = 'tars';
-- Note: because most users have changed ZK configuration, the following SQLs are annotated to prevent erroneous execution
-- UPDATE plugin SET config='{"register":"127.0.0.1:2181","threadpool":"shared"}' WHERE "name" = 'motan';
-- UPDATE plugin SET config='{"protocol":"zookeeper","register":"127.0.0.1:2181","threadpool":"shared"}' WHERE "name" = 'sofa';
-- UPDATE plugin SET config='{"register":"zookeeper://localhost:2181","multiSelectorHandle":"1","threadpool":"shared"}' WHERE "name" = 'dubbo';
-- if you want to execute this SQL, please replace it with your ZK configuration
