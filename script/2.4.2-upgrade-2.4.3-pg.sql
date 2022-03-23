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

-- update admin password
UPDATE dashboard_user SET password='ba3253876aed6bc22d4a6ff53d8406c6ad864195ed144ab5c87621b6c233b548baeae6956df346ec8c17f5ea10f35ee3cbc514797ed7ddd3145464e2a0bab413' WHERE user_name = 'admin';

-- Note: it doesn't matter if you don't execute this SQL, the default configuration will be compatible with the old version
-- Note: because most users have changed ZK configuration, this SQL is annotated to prevent erroneous execution
-- UPDATE plugin SET config='{"register":"zookeeper://localhost:2181","multiSelectorHandle":"1","threadpool":"cached","corethreads":0,"threads":2147483647,"queues":0}' WHERE `name` = 'dubbo';
--                                                    ^^^^^^^^^ if you want to execute this SQL, please replace it with your ZK configuration

-- insert plugin_handle data for dubbo
INSERT IGNORE INTO plugin_handle (`plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('6', 'threadpool', 'threadpool', '3', '3', '0', '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}');
INSERT IGNORE INTO plugin_handle (`plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('6', 'corethreads', 'corethreads', '1', '3', '0', '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}');
INSERT IGNORE INTO plugin_handle (`plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('6', 'threads', 'threads', '1', '3', '0', '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}');
INSERT IGNORE INTO plugin_handle (`plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('6', 'queues', 'queues', '1', '3', '0', '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}');

-- insert dict for dubbo plugin
INSERT IGNORE INTO shenyu_dict (`type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('threadpool', 'THREADPOOL', 'shared', 'shared', '', '4', '1');
INSERT IGNORE INTO shenyu_dict (`type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('threadpool', 'THREADPOOL', 'fixed', 'fixed', '', '3', '1');
INSERT IGNORE INTO shenyu_dict (`type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('threadpool', 'THREADPOOL', 'eager', 'eager', '', '2', '1');
INSERT IGNORE INTO shenyu_dict (`type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('threadpool', 'THREADPOOL', 'cached', 'cached', '', '0', '1');
INSERT IGNORE INTO shenyu_dict (`type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('threadpool', 'THREADPOOL', 'limited', 'limited', '', '1', '1');

-- remove monitor plugin
DELETE FROM plugin WHERE `id` = '7';
DELETE FROM plugin_handle WHERE `plugin_id` = '7';

-- remove plugin_handle shenyu_dict trigger
DROP TRIGGER IF EXISTS `plugin_handle_check_insert` ON plugin_handle;
DROP TRIGGER IF EXISTS `shenyu_dict_check_insert` ON shenyu_dict;

-- insert plugin_handle data for divide
INSERT IGNORE INTO plugin_handle (`plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`) VALUES ('5', 'retryStrategy', 'retryStrategy', '3', '2', '0', '{"required":"0","defaultValue":"current","placeholder":"retryStrategy","rule":""}');

-- insert dict for divide plugin
INSERT IGNORE INTO shenyu_dict (`type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('retryStrategy', 'RETRY_STRATEGY', 'current', 'current', 'current', '0', '1');
INSERT IGNORE INTO shenyu_dict (`type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`) VALUES ('retryStrategy', 'RETRY_STRATEGY', 'failover', 'failover', 'failover', '1', '1');
