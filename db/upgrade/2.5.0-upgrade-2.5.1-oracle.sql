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

-- this file works for Oracle, can not use "`" syntax.

-- Note: it doesn't matter if you don't execute this SQL
-- Note: because most users have changed ZK configuration, this SQL is annotated to prevent erroneous execution
-- UPDATE plugin SET config='{"register":"127.0.0.1:2181","corethreads":0,"threads":2147483647,"queues":0}' WHERE "name" = 'motan';
-- if you want to execute this SQL, please replace it with your ZK configuration

-- insert brpc plugin start
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES VALUES ('41', 'brpc', 'Proxy', 310,'{"address":"127.0.0.1","port":"8005","corethreads":0,"threads":2147483647,"queues":0,"threadpool":"shared"}','0');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204172957', '41', 'address', 'address', 2, 3, 0, '{"required":"1","defaultValue":"127.0.0.1","placeholder":"address","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204172958', '41', 'port', 'port', 2, 3, 0, '{"required":"1","defaultValue":"8005","placeholder":"port","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204172959', '41', 'corethreads', 'corethreads', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204172960', '41', 'threads', 'threads', 1, 3, 0, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204172961', '41', 'queues', 'queues', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204172962', '41', 'threadpool', 'threadpool', 3, 3, 0, '{"required":"0","defaultValue":"shared","placeholder":"threadpool","rule":""}');
-- insert brpc plugin end

-- insert loggingPulsar plugin start
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled) VALUES ('35', 'loggingPulsar', '{"topic":"shenyu-access-logging", "serviceUrl": "pulsar://localhost:6650"}', 'Logging', 185, '0');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468201', '35', 'topic', 'topic', 2, 3, 1, '{"required":"1","defaultValue":"shenyu-access-logging"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468202', '35', 'serviceUrl', 'serviceUrl', 2, 3, 2, '{"required":"1","defaultValue":"pulsar://localhost:6650"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468203', '35', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468204', '35', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468205', '35', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468206', '35', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}');

-- insert loggingPulsar plugin end

-- insert loggingTencentCls plugin start
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled) VALUES ('36', 'loggingTencentCls', '{"endpoint": "ap-guangzhou.cls.tencentcs.com", "topic": "shenyu-topic"}', 'Logging', 176, '0');
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468207', '36', 'secretId', 'secretId', 2, 3, 1, '{"required":"1","defaultValue":"","placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468208', '36', 'secretKey', 'secretKey', 2, 3, 2, '{"required":"1","defaultValue":"","placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468209', '36', 'endpoint', 'endpoint', 2, 3, 3, '{"required":"1","defaultValue":"","placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468210', '36', 'topic', 'topic', 2, 3, 4, '{"required":"1","defaultValue":"","placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468211', '36', 'sendThreadCount', 'sendThreadCount', 1, 3, 5, '{"required":"0","defaultValue":1,"placeholder":"1-500"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468212', '36', 'totalSizeInBytes', 'totalSizeInBytes', 1, 3, 6, '{"required":"0","defaultValue":104857600}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468213', '36', 'maxSendThreadCount', 'maxSendThreadCount', 1, 3, 7, '{"required":"0","defaultValue":1,"placeholder":"availableProcessors + 1"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468214', '36', 'maxBlockSec', 'maxBlockSec', 1, 3, 8, '{"required":"0","defaultValue":60000}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468215', '36', 'maxBatchSize', 'maxBatchSize', 1, 3, 9, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468216', '36', 'maxBatchCount', 'maxBatchCount', 1, 3, 10, '{"required":"0","defaultValue":4096}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468217', '36', 'lingerMs', 'lingerMs', 1, 3, 11, '{"required":"0","defaultValue":2000}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468218', '36', 'retries', 'retries', 1, 3, 12, '{"required":"0","defaultValue":10}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468219', '36', 'maxReservedAttempts', 'maxReservedAttempts', 1, 3, 13, '{"required":"0","defaultValue":11}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468220', '36', 'baseRetryBackoffMs', 'baseRetryBackoffMs', 1, 3, 14, '{"required":"0","defaultValue":100}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468221', '36', 'maxRetryBackoffMs', 'maxRetryBackoffMs', 1, 3, 15, '{"required":"0","defaultValue":50000}');
-- insert loggingTencentCls plugin End

-- insert loggingClickHouse plugin start
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled) VALUES ('38', 'loggingClickHouse', '{"host":"127.0.0.1","port":"8123","databse":"shenyu-gateway","username":"foo","password":"bar"}', 'Logging', 195, '0');


insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468255', '38', 'host', 'host', 2, 3, 3, '{"required":"1","defaultValue":"127.0.0.1"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468256', '38', 'port', 'port', 2, 3, 4, '{"required":"1","defaultValue":"8123"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468257', '38', 'database', 'database', 2, 3, 5, '{"required":"0","defaultValue":"shenyu-gateway"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468258', '38', 'username', 'username', 2, 3, 6, '{"required":"1","defaultValue":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468259', '38', 'password', 'password', 2, 3, 7, '{"required":"1","defaultValue":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468260', '38', 'password', 'password', 2, 3, 7, '{"required":"1","defaultValue":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468261', '38', 'engine', 'engine', 3, 3, 8, '{"required":"0","defaultValue":"MergeTree"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172862', '38', 'clusterName', 'clusterName', 3, 3, 9, '{"required":"1","defaultValue":"cluster"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1545812228228259841', 'engine', 'engine', 'ReplacingMergeTree', 'ReplacingMergeTree', '', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1545812228228259842', 'engine', 'engine', 'MergeTree', 'MergeTree', '', 1, 1);
-- insert loggingClickHouse plugin End

-- insert loggingKafka plugin start

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468184', '33', 'securityProtocol', 'securityProtocol', 3, 3, 8, '{"required":"0","defaultValue":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468185', '33', 'saslMechanism', 'saslMechanism', 3, 3, 9, '{"required":"0","defaultValue":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468186', '33', 'userName', 'userName',  2, 3, 10, '{"required":"0","defaultValue":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468187', '33', 'passWord', 'passWord', 2, 3, 11, '{"required":"0","defaultValue":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784271', 'securityProtocol', 'SECURITY_PROTOCOL', 'default', '', '', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784272', 'securityProtocol', 'SECURITY_PROTOCOL', 'PLAINTEXT', 'PLAINTEXT', '', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784273', 'securityProtocol', 'SECURITY_PROTOCOL', 'SSL', 'SSL', '', 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784274', 'securityProtocol', 'SECURITY_PROTOCOL', 'SASL_PLAINTEXT', 'SASL_PLAINTEXT', '', 3, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784275', 'securityProtocol', 'SECURITY_PROTOCOL', 'SASL_SSL', 'SASL_SSL', '', 4, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784276', 'saslMechanism', 'SASL_MECHANISM', 'default', '', '', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784277', 'saslMechanism', 'SASL_MECHANISM', 'GSSAPI', 'GSSAPI', '', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784278', 'saslMechanism', 'SASL_MECHANISM', 'PLAIN', 'PLAIN', '', 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784279', 'saslMechanism', 'SASL_MECHANISM', 'OAUTHBEARER', 'OAUTHBEARER', '', 3, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784280', 'saslMechanism', 'SASL_MECHANISM', 'SCRAM-SHA-256', 'SCRAM-SHA-256', '', 4, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784281', 'saslMechanism', 'SASL_MECHANISM', 'SCRAM-SHA-512', 'SCRAM-SHA-512', '', 5, 1);

-- insert loggingKafka plugin end

-- fix issue #3945
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468227', '8', 'loadBalance', 'loadStrategy', 3, 2, 3, '{"defaultValue":"roundRobin","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468228', '10', 'flowRuleMaxQueueingTimeMs', 'flowRuleMaxQueueingTimeMs', 1, 2, 8, '{"required":"0","defaultValue":"500"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468229', '10', 'flowRuleWarmUpPeriodSec', 'flowRuleWarmUpPeriodSec', 1, 2, 8, '{"required":"0","defaultValue":"10"}');

/*create plugin resource and permission for admin #3964 */
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status)VALUES ('1572525965625266176', '1346777449787125760', 'SHENYU.BUTTON.SYSTEM.RESOURCE', '', '', '', 2, 6, '', 1, 0, 'system:plugin:resource', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1572525965658820608', '1346358560427216896', '1572525965625266176');

/*insert plugin for casdoor*/
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled) VALUES ('39', 'casdoor', '{"endpoint":"http://localhost:8000"}' ,'Authentication', 40, '0');

/* insert plugin_handle data for plugin casdoor */
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1570590990341775360', '39', 'endpoint', 'casdoor endpoint', 2, 3, 0, '{"required":"1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1570591047635968000', '39', 'client_id', 'client_id', 2, 3, 0, '{"required":"1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1570591109623586816', '39', 'client_secrect', 'client_secrect', 2, 3, 0, '{"required":"1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1570591165374275584', '39', 'certificate', 'certificate', 2, 3, 0, '{"required":"1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1570591215131303936', '39', 'organization-name', 'organization-name', 2, 3, 0, '{"required":"1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1570591265492312064', '39', 'application-name', 'application-name', 2, 3, 0, '{"required":"1","rule":""}');


/* fix issue 3966 */
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1518229897214468230', '14', 'addPrefixed', 'addPrefixed', 3, 2, 3, '{"required":"1","defaultValue":"false"}');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784282', 'addPrefixed', 'ADD_PREFIXED', 'open', 'true', '', 0, 1);

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1529402613195784283', 'addPrefixed', 'ADD_PREFIXED', 'close', 'false', '', 1, 1);

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1545812228228259843', 'loadBalance', 'LOAD_BALANCE', 'leastActive', 'leastActive', 'leastActive', 3, 1);

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468231', '18', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468232', '18', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468233', '18', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468234', '29', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468235', '29', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468236', '29', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468237', '32', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468238', '32', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468239', '32', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468240', '33', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468241', '33', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468242', '33', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468243', '34', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468244', '34', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468245', '34', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468246', '35', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468247', '35', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468248', '35', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468249', '36', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468250', '36', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468251', '36', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468252', '38', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468253', '38', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468254', '38', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}');

-- insert keyAuth plugin start
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('40', 'keyAuth', 'Authentication', 150, '0');
-- insert keyAuth plugin end

-- create mock_request_record table start
create table mock_request_record
(
    id VARCHAR2(128) not null PRIMARY KEY,
    api_id VARCHAR2(128) not null,
    host VARCHAR2(32) not null,
    port NUMBER(5) not null,
    url VARCHAR2(1024) not null,
    path_variable VARCHAR2(255) default '' not null,
    query VARCHAR2(1024) default '' not null,
    header VARCHAR2(1024) default '' not null,
    body CLOB,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null
);
-- Add comments to the table
comment on table MOCK_REQUEST_RECORD
  is 'mock request records';
-- Add comments to the columns
comment on column MOCK_REQUEST_RECORD.id
  is 'id';
comment on column MOCK_REQUEST_RECORD.api_id
  is 'the api id';
comment on column MOCK_REQUEST_RECORD.host
  is 'the request host';
comment on column MOCK_REQUEST_RECORD.port
  is 'the request port';
comment on column MOCK_REQUEST_RECORD.url
    is 'the request url';
comment on column MOCK_REQUEST_RECORD.path_variable
  is 'the request param in url';
comment on column MOCK_REQUEST_RECORD.query
  is 'the request param after url';
comment on column MOCK_REQUEST_RECORD.header
  is 'the request param in header';
comment on column MOCK_REQUEST_RECORD.body
  is 'the request body';
comment on column MOCK_REQUEST_RECORD.date_created
  is 'create time';
comment on column MOCK_REQUEST_RECORD.date_updated
  is 'update time';
-- create mock_request_record table end

create table api
(
    id VARCHAR2 (128) not null,
    context_path VARCHAR2 (255) not null,
    api_path VARCHAR2 (255) not null,
    http_method NUMBER (10) not null,
    consume VARCHAR2 (255) not null,
    produce VARCHAR2 (255) not null,
    version VARCHAR2 (255) not null,
    rpc_type VARCHAR2 (64) not null,
    state NUMBER (10) not null,
    ext VARCHAR2 (1025) not null,
    api_owner VARCHAR2 (255) not null,
    api_desc VARCHAR2 (1024) not null,
    document CLOB not null,
    document_md5 VARCHAR2 (32) not null,
    api_source NUMBER (10) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the table
comment on table API
    is 'api document';
-- Add comments to the columns
comment on column API.id
    is 'primary key id';
comment on column API.context_path
    is 'the context_path';
comment on column API.api_path
    is 'the api_path';
comment on column API.http_method
    is '0-get,1-head,2-post,3-put,4-patch,5-delete,6-options,7-trace';
comment on column API.consume
    is 'consume content-type';
comment on column API.produce
    is 'produce content-type';
comment on column API.version
    is 'api version,for example V0.01';
comment on column API.rpc_type
    is 'http,dubbo,sofa,tars,websocket,springCloud,motan,grpc';
comment on column API.state
    is '0-unpublished,1-published,2-offline';
comment on column API.ext
    is 'extended fields';
comment on column API.api_owner
    is 'api_owner';
comment on column API.api_desc
    is 'the api description';
comment on column API.api_source
    is '0-swagger,1-annotation generation,2-create manually,3-import swagger,4-import yapi';
comment on column API.document
    is 'complete documentation of the api, including request parameters and response parameters';
comment on column API.document_md5
    is 'document_md5';
comment on column API.date_created
    is 'create time';
comment on column API.date_updated
    is 'update time';

create table api_rule_relation
(
    id VARCHAR2 (128) not null,
    api_id VARCHAR2 (128) not null,
    rule_id VARCHAR2 (128) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column API_RULE_RELATION.id
    is 'primary key id';
comment on column API_RULE_RELATION.api_id
    is 'the table api primary key id';
comment on column API_RULE_RELATION.rule_id
    is 'the table rule primary key id';
comment on column API_RULE_RELATION.date_created
    is 'create time';
comment on column API_RULE_RELATION.date_updated
    is 'update time';

create table tag
(
    id            VARCHAR2(128) not null,
    name          VARCHAR2(255) not null,
    tag_desc      VARCHAR2(255) not null,
    parent_tag_id VARCHAR2(128) not null,
    ext           VARCHAR2(1024) not null,
    date_created  timestamp(3) default SYSDATE not null,
    date_updated  timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment
    on column TAG.id
    is 'primary key id';
comment
    on column TAG.name
    is 'tag name';
comment
    on column TAG.tag_desc
    is 'tag desc';
comment
    on column TAG.parent_tag_id
    is 'parent tag id';
comment
    on column TAG.ext
    is 'extension info';
comment
    on column TAG.date_created
    is 'create time';
comment
    on column TAG.date_updated
    is 'update time';

create table tag_relation
(
    id                VARCHAR2(128) not null,
    api_id            VARCHAR2(128) not null,
    tag_id            VARCHAR2(128) not null,
    date_created      timestamp(3) default SYSDATE not null,
    date_updated      timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column TAG_RELATION.id
    is 'primary key id';
comment on column TAG_RELATION.api_id
    is 'api_id';
comment on column TAG_RELATION.tag_id
    is 'parent tag id';
comment on column TAG_RELATION.date_created
    is 'create time';
comment on column TAG_RELATION.date_updated
    is 'update time';

create table param
(
    id           VARCHAR2(128) not null,
    api_id       VARCHAR2(128) not null,
    model_id     VARCHAR2(128) not null,
    type         NUMBER(10) not null,
    name         VARCHAR2(255) not null,
    param_desc   VARCHAR2(1024) not null,
    required     NUMBER(3) not null,
    ext          VARCHAR2(1024) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the table
comment on table PARAM
    is 'param document';
-- Add comments to the columns
comment on column PARAM.id
    is 'primary key id';
comment on column PARAM.api_id
    is 'the api id';
comment on column PARAM.model_id
    is 'the model id, empty if not a model';
comment on column PARAM.type
    is '0-requestPathVariable,1-requestUrlParam,2-requestHeader,3-requestBody,4-responseHeader,5-responseBody';
comment on column PARAM.name
    is 'the param name';
comment on column PARAM.param_desc
    is 'the param description';
comment on column PARAM.required
    is 'whether to require (0 not required, 1 required)';
comment on column PARAM.ext
    is 'extended fields';
comment on column PARAM.date_created
    is 'create time';
comment on column PARAM.date_updated
    is 'update time';

create table model
(
    id VARCHAR2(128) not null PRIMARY KEY,
    name VARCHAR2(128) not null,
    model_desc VARCHAR2(1024) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null
);
-- Add comments to the table
comment on table MODEL
    is 'model desc table';
-- Add comments to the columns
comment on column MODEL.id
    is 'id';
comment on column MODEL.name
    is 'the model name';
comment on column MODEL.model_desc
    is 'the model description';
comment on column MODEL.date_created
    is 'create time';
comment on column MODEL.date_updated
    is 'update time';

create table field
(
    id           VARCHAR2(128) not null,
    model_id     VARCHAR2(128) not null,
    self_model_id VARCHAR2(128) not null,
    name         VARCHAR2(128) not null,
    field_desc   VARCHAR2(1024) not null,
    required     NUMBER(3) not null,
    ext          VARCHAR2(1024) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the table
comment on table field
    is 'field document table';
-- Add comments to the columns
comment on column FIELD.id
    is 'primary key id';
comment on column FIELD.model_id
    is 'this field belongs to which model';
comment on column FIELD.self_model_id
    is 'which model of this field is';
comment on column FIELD.name
    is 'field name';
comment on column FIELD.field_desc
    is 'field description';
comment on column FIELD.required
    is 'whether to require (0 not required, 1 required)';
comment on column FIELD.ext
    is 'extended fields,can store genericTypes,eg..{"genericTypes":[model_id1,model_id2]}';
comment on column FIELD.date_created
    is 'create time';
comment on column FIELD.date_updated
    is 'update time';

create table detail
(
    id           VARCHAR2(128) not null,
    field_id     VARCHAR2(128) not null,
    is_example   NUMBER(3) not null,
    field_value  CLOB not null,
    value_desc   VARCHAR2(1024) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the table
comment on table DETAIL
    is 'field value detail table';
-- Add comments to the columns
comment on column DETAIL.id
    is 'primary key id';
comment on column DETAIL.field_id
    is 'the field id';
comment on column DETAIL.is_example
    is 'is example or not (0 not, 1 is)';
comment on column DETAIL.field_value
    is 'the field value';
comment on column DETAIL.value_desc
    is 'field value description';
comment on column DETAIL.date_created
    is 'create time';
comment on column DETAIL.date_updated
    is 'update time';
