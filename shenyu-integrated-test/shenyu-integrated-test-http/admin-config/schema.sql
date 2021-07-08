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
DELETE FROM plugin WHERE id = '4';
INSERT INTO plugin (id, name, config, role, enabled, date_created, date_updated) VALUES ('4', 'rate_limiter', '{"mode":"standalone","master":"mymaster","url":"shenyu-redis:6379","password":"abc"}', 1, 1, '2018-06-23 10:26:37', '2021-06-12 13:10:09');

INSERT INTO selector (id, plugin_id, name, match_mode, type, sort, handle, enabled, loged, continued, date_created, date_updated) VALUES ('1403700859048841216', '4', 'http-ratelimit-slidingwindow', 0, 1, 1, null, 1, 1, 1, '2021-06-12 13:08:59', '2021-06-12 13:08:59');

INSERT INTO rule (id, selector_id, match_mode, name, enabled, loged, sort, handle, date_created, date_updated) VALUES ('1403701003857186816', '1403700859048841216', 0, 'http-ratelimit-slidingwindow', 1, 1, 1, '{"algorithmName":"slidingWindow","replenishRate":"1","burstCapacity":"1"}', '2021-06-12 13:09:33', '2021-06-12 13:09:33');
INSERT INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1403701003865575424', '1403701003857186816', 'uri', '=', '/', '/http/test/path/123', '2021-06-12 13:09:33', '2021-06-12 13:09:33');
INSERT INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1403700859057229824', '1403700859048841216', 'uri', '=', '/', '/http/test/path/123', '2021-06-12 13:08:59', '2021-06-12 13:08:59');

/** prepare sign **/
UPDATE plugin SET enabled = 1 WHERE id = '1';
INSERT INTO selector (id, plugin_id, name, match_mode, type, sort, handle, enabled, loged, continued,date_created, date_updated) VALUES ('1412234272995270656', '1', 'http-sign', 0, 1, 1, null, 1, 1, 1,'2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT INTO rule (id, selector_id, match_mode, name, enabled, loged, sort,date_created, date_updated) VALUES ('1412234425227534336', '1412234272995270656', 0, 'http-sign', 1, 1, 1,'2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value,date_created, date_updated) VALUES ('1412234425256894464', '1412234425227534336', 'uri', '=', '/', '/http/test/path/456','2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value,date_created, date_updated) VALUES ('1412234273020436480', '1412234272995270656', 'uri', '=', '/', '/http/test/path/456','2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT INTO app_auth(id,app_key,app_secret,open,enabled,date_created, date_updated) VALUES ('1412239238157819904','108C27175A2C43C1BC29B1E483D57E3D','061521A73DD94A3FA873C25D050685BB',1,1,'2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT INTO auth_param(id,auth_id,app_name,date_created, date_updated) VALUES ('1412246190984257536','1412239238157819904','http-sign','2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT INTO auth_path(id,auth_id,app_name,path,enabled,date_created, date_updated) VALUES ('1412255231079411712','1412239238157819904','http-sign','/http/test/path/456',1,'2021-07-06 13:09:3','2021-07-06 13:09:33');
