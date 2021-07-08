-- noinspection SqlNoDataSourceInspectionForFile

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

/** prepare rate limit **/
delete from plugin where id = '4';
INSERT INTO plugin (id, name, config, role, enabled, date_created, date_updated) VALUES ('4', 'rate_limiter', '{"mode":"standalone","master":"mymaster","url":"shenyu-redis:6379","password":"abc"}', 1, 1, '2018-06-23 10:26:37', '2021-06-12 13:10:09');

INSERT INTO selector (id, plugin_id, name, match_mode, type, sort, handle, enabled, loged, continued, date_created, date_updated) VALUES ('1403700859048841216', '4', 'http-ratelimit-slidingwindow', 0, 1, 1, null, 1, 1, 1, '2021-06-12 13:08:59', '2021-06-12 13:08:59');

INSERT INTO rule (id, selector_id, match_mode, name, enabled, loged, sort, handle, date_created, date_updated) VALUES ('1403701003857186816', '1403700859048841216', 0, 'http-ratelimit-slidingwindow', 1, 1, 1, '{"algorithmName":"slidingWindow","replenishRate":"1","burstCapacity":"1"}', '2021-06-12 13:09:33', '2021-06-12 13:09:33');
INSERT INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1403701003865575424', '1403701003857186816', 'uri', '=', '/', '/http/test/path/123', '2021-06-12 13:09:33', '2021-06-12 13:09:33');
INSERT INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1403700859057229824', '1403700859048841216', 'uri', '=', '/', '/http/test/path/123', '2021-06-12 13:08:59', '2021-06-12 13:08:59');

