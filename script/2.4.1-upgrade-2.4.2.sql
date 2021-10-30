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

-- upgrade plguin
-- name & role
UPDATE plugin SET role = 'Authentication' WHERE name = 'sign';
UPDATE plugin SET role = 'FaultTolerance' WHERE name = 'sentinel';
UPDATE plugin SET role = 'Proxy' WHERE name = 'sofa';
UPDATE plugin SET role = 'FaultTolerance' WHERE name = 'resilience4j';
UPDATE plugin SET role = 'Proxy' WHERE name = 'tars';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'context_path';
UPDATE plugin SET role = 'Proxy' WHERE name = 'grpc';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'redirect';
UPDATE plugin SET role = 'Proxy' WHERE name = 'motan';
UPDATE plugin SET role = 'Logging' WHERE name = 'logging';
UPDATE plugin SET role = 'Authentication' WHERE name = 'jwt';
UPDATE plugin SET role = 'Authentication' WHERE name = 'waf';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'request';
UPDATE plugin SET role = 'Authentication' WHERE name = 'oauth2';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'param_mapping';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'modifyResponse';
UPDATE plugin SET role = 'Cryptor' WHERE name = 'cryptor_request';
UPDATE plugin SET role = 'Cryptor' WHERE name = 'cryptor_response';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'websocket';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'rewrite';
UPDATE plugin SET role = 'FaultTolerance' WHERE name = 'rate_limiter';
UPDATE plugin SET role = 'Proxy' WHERE name = 'divide';
UPDATE plugin SET role = 'Proxy' WHERE name = 'dubbo';
UPDATE plugin SET role = 'Monitor' WHERE name = 'monitor';
UPDATE plugin SET role = 'Proxy' WHERE name = 'springCloud';
UPDATE plugin SET role = 'FaultTolerance' WHERE name = 'hystrix';

-- upgrade resource
-- title & name & component &url
UPDATE resource SET title = 'rateLimiter', name = 'rateLimiter', component = 'rateLimiter', url = REPLACE(url, 'rate_limiter', 'rateLimiter') WHERE title = 'rate_limiter';
UPDATE resource SET title = 'contextPath', name = 'contextPath', component = 'contextPath', url = REPLACE(url, 'context_path', 'contextPath') WHERE title = 'context_path';
UPDATE resource SET title = 'cryptorRequest', name = 'cryptorRequest', component = 'cryptorRequest', url = REPLACE(url, 'cryptor_request', 'cryptorRequest') WHERE title = 'cryptor_request';
UPDATE resource SET title = 'cryptorResponse', name = 'cryptorResponse', component = 'cryptorResponse', url = REPLACE(url, 'cryptor_response', 'cryptorResponse') WHERE title = 'cryptor_response';
UPDATE resource SET title = 'modifyResponse', name = 'modifyResponse', component = 'modifyResponse', url = REPLACE(url, 'modifyResponse', 'modifyResponse') WHERE title = 'modify_response';
UPDATE resource SET title = 'paramMapping', name = 'paramMapping', component = 'paramMapping', url = REPLACE(url, 'param_mapping', 'paramMapping') WHERE title = 'param_mapping';

-- perms
UPDATE resource SET perms = REPLACE(perms, 'rate_limiter', 'rateLimiter') WHERE perms LIKE 'plugin:rate_limiter%';
UPDATE resource SET perms = REPLACE(perms, 'context_path', 'contextPath') WHERE perms LIKE 'plugin:context_path%';
UPDATE resource SET perms = REPLACE(perms, 'cryptor_r', 'cryptorR') WHERE perms LIKE 'plugin:cryptor_r%';
UPDATE resource SET perms = REPLACE(perms, 'modifyResponse', 'modifyResponse') WHERE perms LIKE 'plugin:modify_response%';
UPDATE resource SET perms = REPLACE(perms, 'param_mapping', 'paramMapping') WHERE perms LIKE 'plugin:param_mapping%';

-- upgrade plugin_handle
/*update plugin_handle data for waf*/
DELETE FROM plugin_handle WHERE plugin_id = '2';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('200','2' ,'permission','permission','3', 2, 1, '2020-11-22 12:04:10', '2020-11-22 12:04:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('201','2' ,'statusCode','statusCode','2', 2, 2, '2020-11-22 12:04:10', '2020-11-22 12:04:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('202', '2', 'model', 'model', 2, 3, 1, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');

/*update plugin_handle data for rewrite*/
DELETE FROM plugin_handle WHERE plugin_id = '3';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('300', '3', 'regex', 'regex', 2, 2, 1, '2021-05-24 16:07:10', '2021-05-24 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('301', '3', 'replace', 'replace', 2, 2, 2, '2021-05-24 16:07:10', '2021-05-24 16:07:10');

/*update plugin_handle data for rateLimiter*/
DELETE FROM plugin_handle WHERE plugin_id = '4';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('400', '4', 'replenishRate','replenishRate', 2, 2, 2, '{"required":"1","defaultValue":"10","rule":""}', '2020-11-24 00:17:10', '2020-11-24 00:17:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('401', '4', 'burstCapacity','burstCapacity', 2, 2, 3, '{"required":"1","defaultValue":"100","rule":""}', '2020-11-24 00:17:10', '2020-11-24 00:17:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('402', '4', 'algorithmName','algorithmName','3', 2, 1, '{"required":"1","defaultValue":"slidingWindow","rule":""}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('403', '4', 'keyResolverName','keyResolverName','3', 2, 4, '{"required":"1","defaultValue":"WHOLE_KEY_RESOLVER","rule":""}', '2021-06-12 19:17:10', '2021-06-12 19:17:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('404', '4', 'mode', 'mode', 3, 3, 1, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('405', '4', 'master', 'master', 2, 3, 2, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('406', '4', 'url', 'url', 2, 3, 3, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('407', '4', 'password', 'password', 2, 3, 4, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');

/*update plugin_handle data for divide*/
DELETE FROM plugin_handle WHERE plugin_id = '5';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('500', '5', 'upstreamHost', 'host', 2, 1, 0, null, '2021-03-06 21:23:41', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('501', '5', 'protocol', 'protocol', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"http://","rule":""}', '2021-03-06 21:25:37', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('502', '5', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}', '2021-03-06 21:25:55', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('503', '5', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}', '2021-03-06 21:26:35', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('504', '5', 'timestamp', 'startupTime', 1, 1, 3, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}', '2021-03-06 21:27:11', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('505', '5', 'warmup', 'warmupTime', 1, 1, 5, '{"defaultValue":"0","placeholder":"warmup time (ms)","rule":""}', '2021-03-06 21:27:34', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('506', '5', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}', '2021-03-06 21:29:16', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('507', '5', 'loadBalance', 'loadStrategy', 3, 2, 0, null, '2021-03-06 21:30:32', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('508', '5', 'retry', 'retryCount', 1, 2, 1, null, '2021-03-06 21:31:00', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('509', '5', 'timeout', 'timeout', 1, 2, 2, '{"defaultValue":"3000","rule":""}', '2021-03-07 21:13:50', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('510', '5', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, null, '2021-03-08 13:18:44', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('511', '5', 'multiRuleHandle', 'multiRuleHandle', 3, 3, 1, null, '2021-03-08 13:37:12', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('512', '5', 'headerMaxSize', 'headerMaxSize', 1, 2, 3, '{"defaultValue":"10240","rule":""}', '2021-04-29 12:28:45', '2021-04-29 12:28:52');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('513', '5', 'requestMaxSize', 'requestMaxSize', 1, 2, 4, '{"defaultValue":"102400","rule":""}', '2021-04-29 14:24:13', '2021-04-29 14:24:16');

/*update plugin_handle data for dubbo*/
DELETE FROM plugin_handle WHERE plugin_id = '6';
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('600', '6', 'gray', 'gray', '3', '1', '9', '{"required":"0","defaultValue":"false","placeholder":"gray","rule":""}', '2021-03-06 21:29:16', '2021-09-23 14:45:16');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('601', '6', 'group', 'group', '2', '1', '3', '{"required":"0","placeholder":"group","rule":""}', '2021-03-06 21:25:55', '2021-09-23 14:43:19');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('602', '6', 'loadbalance', 'loadbalance', '2', '2', '0', '{"required":"0","placeholder":"loadbalance","rule":""}', '2021-09-20 20:36:10', '2021-09-20 21:25:12');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('603', '6', 'multiSelectorHandle', 'multiSelectorHandle', '3', '3', '0', NULL, '2021-03-08 13:18:44', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('604', '6', 'protocol', 'protocol', '2', '1', '2', '{"required":"0","defaultValue":"","placeholder":"http://","rule":""}', '2021-03-06 21:25:37', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('605', '6', 'status', 'status', '3', '1', '8', '{"defaultValue":"true","rule":""}', '2021-03-06 21:29:16', '2021-09-23 14:45:14');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('606', '6', 'timestamp', 'startupTime', '1', '1', '7', '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}', '2021-03-06 21:27:11', '2021-09-23 14:45:10');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('607', '6', 'upstreamHost', 'host', '2', '1', '0', NULL, '2021-03-06 21:23:41', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('608', '6', 'upstreamUrl', 'ip:port', '2', '1', '1', '{"required":"1","placeholder":"","rule":""}', '2021-03-06 21:25:55', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('609', '6', 'version', 'version', '2', '1', '4', '{"required":"0","placeholder":"version","rule":""}', '2021-03-06 21:25:55', '2021-09-23 14:43:39');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('610', '6', 'warmup', 'warmupTime', '1', '1', '6', '{"defaultValue":"0","placeholder":"warmup time (ms)","rule":""}', '2021-03-06 21:27:34', '2021-09-23 14:45:08');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('611', '6', 'weight', 'weight', '1', '1', '5', '{"defaultValue":"50","rule":""}', '2021-03-06 21:26:35', '2021-09-23 14:45:03');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('612', '6', 'register', 'register', 2, 3, 1, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');

/*update plugin_handle data for plugin monitor*/
DELETE FROM plugin_handle WHERE plugin_id = '7';
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('700', '7', 'metricsName', 'metricsName', 2, 3, 1, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('701', '7', 'host', 'host', 2, 3, 2, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('702', '7', 'port', 'port', 2, 3, 3, '{"rule":"/^[0-9]*$/"}', '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('703', '7', 'async', 'async', 2, 3, 4, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');

/*update plugin_handle data for springCloud*/
DELETE FROM plugin_handle WHERE plugin_id = '8';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('800', '8' ,'path','path', 2, 2, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('801', '8' ,'timeout','timeout (ms)', 1, 2, 2, '2020-11-29 16:07:10', '2020-11-29 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('802', '8' ,'serviceId','serviceId', 2, 1, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');

/*update plugin_handle data for sentinel*/
DELETE FROM plugin_handle WHERE plugin_id = '10';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1000','10' ,'flowRuleGrade','flowRuleGrade','3', 2, 8, '{"required":"1","defaultValue":"1","rule":""}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1001','10' ,'flowRuleControlBehavior','flowRuleControlBehavior','3', 2, 5, '{"required":"1","defaultValue":"0","rule":""}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1002','10' ,'flowRuleEnable','flowRuleEnable (1 or 0)', '1', 2, 7, '{"required":"1","defaultValue":"1","rule":"/^[01]$/"}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1003','10' ,'flowRuleCount','flowRuleCount','1', 2, 6, '{"required":"1","defaultValue":"0","rule":""}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1004','10' ,'degradeRuleEnable','degradeRuleEnable (1 or 0)', '1', 2, 2, '{"required":"1","defaultValue":"1","rule":"/^[01]$/"}', '2020-11-09 01:19:10', '2020-11-09 01:19:10') ;
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1005','10' ,'degradeRuleGrade','degradeRuleGrade','3', 2, 3, '{"required":"1","defaultValue":"0","rule":""}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1006','10' ,'degradeRuleCount','degradeRuleCount','1', 2, 1, '{"required":"1","defaultValue":"0","rule":""}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1007','10' ,'degradeRuleTimeWindow','degradeRuleTimeWindow','1', 2, 4, '{"required":"1","defaultValue":"0","rule":""}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1008','10' ,'degradeRuleMinRequestAmount','degradeRuleMinRequestAmount','1', 2, 3, '{"required":"1","defaultValue":"5","rule":""}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1009','10' ,'degradeRuleStatIntervals','degradeRuleStatIntervals','1', 2, 3, '{"required":"1","defaultValue":"1","rule":""}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1010','10' ,'degradeRuleSlowRatioThreshold','degradeRuleSlowRatioThreshold','1', 2, 3, '{"required":"1","defaultValue":"0.5","rule":""}', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1011', '10', 'fallbackUri', 'fallbackUri', 2, 2, 9, '{"required":"0","rule":""}', '2021-07-07 21:24:18', '2021-07-07 21:35:00');

/*update plugin_handle data for plugin sofa*/
DELETE FROM plugin_handle WHERE plugin_id = '11';
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('1100', '11', 'protocol', 'protocol', 2, 3, 1, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('1101', '11', 'register', 'register', 2, 3, 2, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');

/*update plugin_handle data for resilience4j*/
DELETE FROM plugin_handle WHERE plugin_id = '12';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1200', '12' ,'timeoutDurationRate','timeoutDurationRate (ms)', 1, 2, 1, '{"required":"1","defaultValue":"5000","rule":""}', '2020-11-28 11:08:14', '2020-11-28 11:19:12');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1201', '12' ,'limitRefreshPeriod','limitRefreshPeriod (ms)', 1, 2, 0, '{"required":"1","defaultValue":"500","rule":""}', '2020-11-28 11:18:54', '2020-11-28 11:22:40');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1202', '12' ,'limitForPeriod','limitForPeriod', 1, 2, 0, '{"required":"1","defaultValue":"50","rule":""}', '2020-11-28 11:20:11', '2020-11-28 11:20:11');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1203', '12' ,'circuitEnable','circuitEnable', 1, 2, 2, '{"required":"1","defaultValue":"0","rule":"/^[01]$/"}', '2020-11-28 11:23:09', '2020-11-28 11:24:12');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1204', '12' ,'timeoutDuration','timeoutDuration (ms)', 1, 2, 2, '{"required":"1","defaultValue":"30000","rule":""}', '2020-11-28 11:25:56', '2020-11-28 11:25:56');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('1205', '12' ,'fallbackUri','fallbackUri', 2, 2, 2, '2020-11-28 11:26:44', '2020-11-28 11:26:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1206', '12' ,'slidingWindowSize','slidingWindowSize', 1, 2, 2, '{"required":"1","defaultValue":"100","rule":""}', '2020-11-28 11:27:34', '2020-11-28 11:27:34');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1207', '12' ,'slidingWindowType','slidingWindowType', 1, 2, 2, '{"required":"1","defaultValue":"0","rule":"/^[01]$/"}', '2020-11-28 11:28:05', '2020-11-28 11:28:05');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1208', '12' ,'minimumNumberOfCalls','minimumNumberOfCalls', 1, 2, 2, '{"required":"1","defaultValue":"100","rule":""}', '2020-11-28 11:28:34', '2020-11-28 11:28:34');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1209', '12' ,'waitIntervalFunctionInOpenState','waitIntervalInOpen', 1, 2, 2, '{"required":"1","defaultValue":"60000","rule":""}', '2020-11-28 11:29:01', '2020-11-28 11:29:01');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1210', '12' ,'permittedNumberOfCallsInHalfOpenState','bufferSizeInHalfOpen', 1, 2, 2, '{"required":"1","defaultValue":"10","rule":""}', '2020-11-28 11:29:55', '2020-11-28 11:29:55');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1211', '12' ,'failureRateThreshold','failureRateThreshold', 1, 2, 2, '{"required":"1","defaultValue":"50","rule":""}', '2020-11-28 11:30:40', '2020-11-28 11:30:40');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1212', '12', 'automaticTransitionFromOpenToHalfOpenEnabled', 'automaticHalfOpen', 3, 2, 1, '{"required":"1","defaultValue":"true","rule":""}', '2021-07-18 22:52:20', '2021-07-18 22:59:57');

/*update plugin_handle data for tars*/
DELETE FROM plugin_handle WHERE plugin_id = '13';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1300', '13', 'upstreamHost', 'host', 2, 1, 0, null, '2021-03-06 21:23:41', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1301', '13', 'protocol', 'protocol', 2, 1, 2, '{"defaultValue":"","rule":""}', '2021-03-06 21:25:37', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1302', '13', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}', '2021-03-06 21:25:55', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1303', '13', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}', '2021-03-06 21:26:35', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1304', '13', 'timestamp', 'startupTime', 1, 1, 3, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}', '2021-03-06 21:27:11', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1305', '13', 'warmup', 'warmupTime', 1, 1, 5, '{"defaultValue":"0","placeholder":"warmup time (ms)","rule":""}', '2021-03-06 21:27:34', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1306', '13', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}', '2021-03-06 21:29:16', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1307', '13', 'loadBalance', 'loadStrategy', 3, 2, 0, null, '2021-03-06 21:30:32', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1308', '13', 'retry', 'retryCount', 1, 2, 1, null, '2021-03-06 21:31:00', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1309', '13', 'timeout', 'timeout', 1, 2, 2, '{"defaultValue":"3000","rule":""}', '2021-03-07 21:13:50', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1310', '13', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, null, '2021-03-08 13:18:44', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1311', '13', 'multiRuleHandle', 'multiRuleHandle', 3, 3, 1, null, '2021-03-08 13:37:12', '2021-03-09 10:32:51');

/*update plugin_handle data for context path*/
DELETE FROM plugin_handle WHERE plugin_id = '14';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('1400', '14', 'contextPath', 'contextPath', 2, 2, 0, '2020-12-25 16:13:09', '2020-12-25 16:13:09');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('1401', '14', 'addPrefix', 'addPrefix', 2, 2, 0, '2020-12-25 16:13:09', '2020-12-25 16:13:09');

/*update plugin_handle data for grpc*/
DELETE FROM plugin_handle WHERE plugin_id = '15';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1500', '15', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}', '2021-03-06 21:25:55', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1501', '15', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}', '2021-03-06 21:26:35', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1502', '15', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}', '2021-03-06 21:29:16', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1503', '15', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, null, '2021-03-08 13:18:44', '2021-03-09 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1504', '15', 'multiRuleHandle', 'multiRuleHandle', 3, 3, 1, null, '2021-03-08 13:37:12', '2021-03-09 10:32:51');

/*update plugin_handle data for redirect*/
DELETE FROM plugin_handle WHERE plugin_id = '16';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('1600', '16' ,'redirectURI','redirectURI', 2, 2, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');

/*update plugin_handle data for plugin motan*/
DELETE FROM plugin_handle WHERE plugin_id = '17';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1700', '17', 'register', 'register', 2, 3, 0, null, '2021-09-25 13:19:10', '2021-09-25 13:19:10');

/*update plugin_handle data for plugin jwt*/
DELETE FROM plugin_handle WHERE plugin_id = '19';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1900','19' ,'secretKey','secretKey',2, 3, 0, null, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('1901','19' ,'filterPath','filterPath',2, 3, 1, null, '2021-06-12 19:17:10', '2021-06-12 19:17:10');

/*update plugin_handle data for request*/
DELETE FROM plugin_handle WHERE plugin_id = '20';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2000', '20', 'ruleHandlePageType', 'ruleHandlePageType', 3, 3, 0, '{"required":"0","rule":""}', '2021-05-27 23:43:53', '2021-05-30 19:58:28');

/*update plugin_handle data for plugin cryptor request*/
DELETE FROM plugin_handle WHERE plugin_id = '24';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2400', '24', 'strategyName', 'strategyName', 3, 2, 1, NULL, '2021-08-06 14:35:50', '2021-08-06 14:35:50');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2401', '24', 'fieldNames', 'fieldNames', 2, 2, 3, NULL, '2021-08-06 14:37:13', '2021-08-06 14:37:46');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2402', '24', 'decryptKey', 'decryptKey', 2, 2, 3, NULL, '2021-08-06 14:37:22', '2021-08-06 14:37:48');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2403', '24', 'encryptKey', 'encryptKey', 2, 2, 3, NULL, '2021-08-06 14:37:22', '2021-08-06 14:37:48');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2404', '24', 'way', 'way', 3, 2, 3, NULL, '2021-08-06 14:37:22', '2021-08-06 14:37:48');

/*update plugin_handle data for plugin cryptor response*/
DELETE FROM plugin_handle WHERE plugin_id = '25';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2500', '25', 'strategyName', 'strategyName', 3, 2, 2, NULL, '2021-08-13 15:10:53', '2021-08-13 15:11:11');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2501', '25', 'decryptKey', 'decryptKey', 2, 2, 3, NULL, '2021-08-13 15:14:07', '2021-08-13 15:14:36');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2502', '25', 'encryptKey', 'encryptKey', 2, 2, 3, NULL, '2021-08-13 15:14:07', '2021-08-13 15:14:36');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2503', '25', 'fieldNames', 'fieldNames', 2, 2, 4, NULL, '2021-08-13 15:16:30', '2021-08-13 15:16:45');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2504', '25', 'way', 'way', 3, 2, 3, NULL, '2021-08-06 14:37:22', '2021-08-06 14:37:48');

/*update plugin_handle data for websocket*/
DELETE FROM plugin_handle WHERE plugin_id = '26';
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2600', '26', 'host', 'host', 2, 1, 0, null, '2021-08-27 21:23:41', '2021-08-27 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2601', '26', 'protocol', 'protocol', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"ws://","rule":""}', '2021-08-27 21:25:37', '2021-08-27 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2602', '26', 'url', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}', '2021-08-27 21:25:55', '2021-08-27 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2603', '26', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}', '2021-08-27 21:26:35', '2021-08-27 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2604', '26', 'timestamp', 'startupTime', 1, 1, 3, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}', '2021-08-27 21:27:11', '2021-08-27 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2605', '26', 'warmup', 'warmupTime', 1, 1, 5, '{"defaultValue":"0","placeholder":"warmup time (ms)","rule":""}', '2021-08-27 21:27:34', '2021-08-27 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2606', '26', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}', '2021-08-27 21:29:16', '2021-08-27 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2607', '26', 'loadBalance', 'loadStrategy', 3, 2, 0, null, '2021-08-27 21:30:32', '2021-08-27 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2608', '26', 'retry', 'retryCount', 1, 2, 1, null, '2021-08-27 21:31:00', '2021-08-27 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2609', '26', 'timeout', 'timeout', 1, 2, 2, '{"defaultValue":"3000","rule":""}', '2021-08-27 21:13:50', '2021-08-27 10:32:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`,`date_created`,`date_updated`) VALUES ('2610', '26', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, null, '2021-08-27 21:30:32', '2021-08-27 10:32:51');
