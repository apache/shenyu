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

-- update admin password
UPDATE dashboard_user SET password='ba3253876aed6bc22d4a6ff53d8406c6ad864195ed144ab5c87621b6c233b548baeae6956df346ec8c17f5ea10f35ee3cbc514797ed7ddd3145464e2a0bab413' WHERE user_name = 'admin';

-- Note: it doesn't matter if you don't execute this SQL, the default configuration will be compatible with the old version
-- Note: because most users have changed ZK configuration, this SQL is annotated to prevent erroneous execution
-- UPDATE plugin SET config='{"register":"zookeeper://localhost:2181","multiSelectorHandle":"1","threadpool":"cached","corethreads":0,"threads":2147483647,"queues":0}' WHERE `name` = 'dubbo';
-- if you want to execute this SQL, please replace it with your ZK configuration

-- insert plugin_handle data for dubbo
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1509837592619499565', '6', 'threadpool', 'threadpool', '3', '3', '0', '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1509837592619499566', '6', 'corethreads', 'corethreads', '1', '3', '0', '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1509837592619499567', '6', 'threads', 'threads', '1', '3', '0', '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1509837592619499568', '6', 'queues', 'queues', '1', '3', '0', '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}');

-- insert dict for dubbo plugin
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1509837592611110924', 'threadpool', 'THREADPOOL', 'shared', 'shared', '', '4', '1');
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1509837592611110925', 'threadpool', 'THREADPOOL', 'fixed', 'fixed', '', '3', '1');
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1509837592611110926', 'threadpool', 'THREADPOOL', 'eager', 'eager', '', '2', '1');
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1509837592611110927', 'threadpool', 'THREADPOOL', 'cached', 'cached', '', '0', '1');
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1509837592611110928', 'threadpool', 'THREADPOOL', 'limited', 'limited', '', '1', '1');

-- remove monitor plugin
DELETE FROM plugin WHERE `id` = '7';
DELETE FROM plugin_handle WHERE `plugin_id` = '7';

-- remove plugin_handle shenyu_dict trigger
DROP TRIGGER IF EXISTS `plugin_handle_before_trigger`;
DROP TRIGGER IF EXISTS `shenyu_dict_before_trigger`;

-- insert plugin_handle data for divide
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1509837592619499520', '5', 'retryStrategy', 'retryStrategy', '3', '2', '0', '{"required":"0","defaultValue":"current","placeholder":"retryStrategy","rule":""}');

-- insert dict for divide plugin
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1509837592611110929', 'retryStrategy', 'RETRY_STRATEGY', 'current', 'current', 'current', '0', '1');
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1509837592611110930', 'retryStrategy', 'RETRY_STRATEGY', 'failover', 'failover', 'failover', '1', '1');

-- Note: it doesn't matter if you don't execute this SQL, the default configuration will be compatible with the old version
-- Note: because users may have changed configuration, this SQL is annotated to prevent erroneous execution
-- UPDATE plugin SET config='{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"cached"}' WHERE `name` = 'grpc';
-- if you want to execute this SQL, please replace these with your own configuration

-- insert plugin_handle data for grpc
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1509843898293887014', '15', 'threadpool', 'threadpool', '3', '3', '0', '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}');

/* insert dict for compress algorithm  */
INSERT IGNORE INTO `shenyu_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1509837592611110932', 'compressAlg', 'COMPRESS_ALG', 'none', 'none', '', 0, 1);
INSERT IGNORE INTO `shenyu_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1509837592611110933', 'compressAlg', 'COMPRESS_ALG', 'LZ4', 'LZ4', '', 1, 1);

/* insert plugin for loggingRocketMQ  */
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `sort`, `config`, `enabled`, `date_created`, `date_updated`) VALUES ('29', 'loggingRocketMQ', 'Logging', 170,'{"topic":"shenyu-access-logging", "namesrvAddr": "localhost:9876","producerGroup":"shenyu-plugin-logging-rocketmq"}', '0', '2022-03-19 09:00:00', '2022-03-19 09:00:00');

/*insert plugin_handle data for plugin loggingRocketMQ*/
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`) VALUES ('1509837592623693841', '29', 'topic', 'topic', 2, 3, 1, '{"required":"1","defaultValue":"shenyu-access-logging"}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`) VALUES ('1509837592623693842', '29', 'namesrvAddr', 'namesrvAddr', 2, 3, 2, '{"required":"1","defaultValue":"localhost:9876"}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`) VALUES ('1509837592623693843', '29', 'producerGroup', 'producerGroup', 2, 3, 3, '{"required":"1","defaultValue":"shenyu-plugin-logging-rocketmq"}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`) VALUES ('1509837592623693844', '29', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`) VALUES ('1509837592623693845', '29', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`) VALUES ('1509837592623693846', '29', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`) VALUES ('1509837592623693847', '29', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}');

INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`) VALUES ('1509837592623693848', '29', 'topic', 'topic', 2, 1, 1, '{"required":"0","defaultValue":"","placeholder":"optional"}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`ext_obj`) VALUES ('1509837592623693849', '29', 'sampleRate', 'sampleRate', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"optional,0,0.01~1"}');

-- update dict flag
UPDATE shenyu_dict SET dict_value = 'false' WHERE dict_code = 'INIT_FLAG';

-- insert plugin data for cache
INSERT IGNORE INTO `plugin` (`id`, `name`, `config`, `role`, `sort`, `enabled`) VALUES ('30', 'cache', '{"cacheType":"memory"}', 'Cache', 10, 0);

-- insert plugin_handle data for cache
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1505397799121616896', '30', 'cacheType', 'cacheType', 3, 3, 1, '{"required":"1","defaultValue":"memory","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1505398085349310464', '30', 'database', 'database', 1, 3, 2, '{"required":"0","defaultValue":"0","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1505398320372940800', '30', 'master', 'master', 2, 3, 3, '{"required":"0","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1505398496462405632', '30', 'mode', 'mode', 2, 3, 4, '{"required":"0","defaultValue":"standalone","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1505398640738074624', '30', 'url', 'url', 2, 3, 5, '{"required":"0","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1505398799370846208', '30', 'password', 'password', 2, 3, 6, '{"required":"0","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1505399120822304768', '30', 'maxIdle', 'maxIdle', 1, 3, 7, '{"required":"0","defaultValue":"8","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1505399444706459648', '30', 'minIdle', 'minIdle', 1, 3, 8, '{"required":"0","defaultValue":"0","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1505399837066821632', '30', 'maxActive', 'maxActive', 1, 3, 9, '{"required":"0","defaultValue":"8","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1505400882341904384', '30', 'maxWait', 'maxWait', 3, 3, 10, '{"required":"0","defaultValue":"-1","rule":""}');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('1508820967919169536', '30', 'timeoutSeconds', 'timeoutSeconds', 1, 2, 0, '{"required":"0","defaultValue":"60","rule":""}');

-- insert dict for cacheType
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1508821695466483712', 'cacheType', 'CACHE_TYPE_MEMORY', 'memory', 'memory', 'use memory to cache data', 0, 1);
INSERT IGNORE INTO shenyu_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('1508821813744058368' ,'cacheType', 'CACHE_TYPE_REDIS', 'redis', 'redis', 'use redis to cache data', 1, 1);

-- alert plugin_handle id properties
ALTER TABLE plugin_handle MODIFY COLUMN id VARCHAR(128) NOT NULL COMMENT 'primary key id';
ALTER TABLE shenyu_dict MODIFY COLUMN id VARCHAR(128) NOT NULL COMMENT 'primary key id';