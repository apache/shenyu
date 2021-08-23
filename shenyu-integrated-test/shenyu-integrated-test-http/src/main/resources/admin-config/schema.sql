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
INSERT IGNORE INTO plugin (id, name, config, role, enabled, date_created, date_updated) VALUES ('4', 'rate_limiter', '{"mode":"standalone","master":"mymaster","url":"shenyu-redis:6379","password":"abc"}', 1, 1, '2018-06-23 10:26:37', '2021-06-12 13:10:09');

INSERT IGNORE INTO selector (id, plugin_id, name, match_mode, type, sort, handle, enabled, loged, continued, date_created, date_updated) VALUES ('1403700859048841216', '4', 'http-ratelimit-slidingwindow', 0, 1, 1, null, 1, 1, 1, '2021-06-12 13:08:59', '2021-06-12 13:08:59');

INSERT IGNORE INTO rule (id, selector_id, match_mode, name, enabled, loged, sort, handle, date_created, date_updated) VALUES ('1403701003857186816', '1403700859048841216', 0, 'http-ratelimit-slidingwindow', 1, 1, 1, '{"algorithmName":"slidingWindow","replenishRate":"1","burstCapacity":"1"}', '2021-06-12 13:09:33', '2021-06-12 13:09:33');
INSERT IGNORE INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1403701003865575424', '1403701003857186816', 'uri', '=', '/', '/http/test/path/123', '2021-06-12 13:09:33', '2021-06-12 13:09:33');
INSERT IGNORE INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1403700859057229824', '1403700859048841216', 'uri', '=', '/', '/http/test/path/123', '2021-06-12 13:08:59', '2021-06-12 13:08:59');

/** prepare sign **/
UPDATE plugin SET enabled = 1 WHERE id = '1';
INSERT IGNORE INTO selector (id, plugin_id, name, match_mode, type, sort, handle, enabled, loged, continued,date_created, date_updated) VALUES ('1412234272995270656', '1', 'http-sign', 0, 1, 1, null, 1, 1, 1,'2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT IGNORE INTO rule (id, selector_id, match_mode, name, enabled, loged, sort,date_created, date_updated) VALUES ('1412234425227534336', '1412234272995270656', 0, 'http-sign', 1, 1, 1,'2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT IGNORE INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value,date_created, date_updated) VALUES ('1412234425256894464', '1412234425227534336', 'uri', '=', '/', '/http/test/path/456','2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT IGNORE INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value,date_created, date_updated) VALUES ('1412234273020436480', '1412234272995270656', 'uri', '=', '/', '/http/test/path/456','2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT IGNORE INTO app_auth(id,app_key,app_secret,open,enabled,date_created, date_updated) VALUES ('1412239238157819904','108C27175A2C43C1BC29B1E483D57E3D','061521A73DD94A3FA873C25D050685BB',1,1,'2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT IGNORE INTO auth_param(id,auth_id,app_name,date_created, date_updated) VALUES ('1412246190984257536','1412239238157819904','http-sign','2021-07-06 13:09:3','2021-07-06 13:09:33');
INSERT IGNORE INTO auth_path(id,auth_id,app_name,path,enabled,date_created, date_updated) VALUES ('1412255231079411712','1412239238157819904','http-sign','/http/test/path/456',1,'2021-07-06 13:09:3','2021-07-06 13:09:33');

/** prepare waf **/
UPDATE plugin SET enabled = 1 WHERE id = '2';
INSERT IGNORE INTO selector (id, plugin_id, name, match_mode, type, sort, handle, enabled, loged, continued, date_created, date_updated) VALUES ('1422826261990641664', '2', '/http/test/waf', 0, 1, 1, null, 1, 1, 1,'2021-08-04 13:09:32','2021-08-04 13:09:32');
INSERT IGNORE INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1422826261999030272', '1422826261990641664', 'uri', 'match', '/', '/http/test/waf/**','2021-08-04 13:09:32','2021-08-04 13:09:32');
INSERT IGNORE INTO rule (id, selector_id, match_mode, name, enabled, loged, sort, handle, date_created, date_updated) VALUES ('1422826330114527232', '1422826261990641664', 0, '/http/test/waf/pass', 1, 1, 1, '{"permission":"allow","statusCode":"200"}', '2021-08-04 13:09:32','2021-08-04 13:09:32');
INSERT IGNORE INTO rule (id, selector_id, match_mode, name, enabled, loged, sort, handle, date_created, date_updated) VALUES ('1422826385177350144', '1422826261990641664', 0, '/http/test/waf/deny', 1, 1, 1, '{"permission":"reject","statusCode":"403"}', '2021-08-04 13:09:32','2021-08-04 13:09:32');
INSERT IGNORE INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1422826330122915840', '1422826330114527232', 'uri', '=', '/', '/http/test/waf/pass','2021-08-04 13:09:32','2021-08-04 13:09:32');
INSERT IGNORE INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1422826385185738752', '1422826385177350144', 'uri', '=', '/', '/http/test/waf/deny','2021-08-04 13:09:32','2021-08-04 13:09:32');

/** prepare redirect **/
UPDATE plugin SET enabled = 1 WHERE id = '16';
INSERT IGNORE INTO selector (id, plugin_id, name, match_mode, type, sort, handle, enabled, loged, continued, date_created, date_updated) VALUES ('1424403439680131072', '16', 'http-redirect', 0, 1, 1, null, 1, 1, 1, '2021-08-09 00:13:39', '2021-08-09 11:47:23');
INSERT IGNORE INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1424403439684325376', '1424403439680131072', 'uri', '=', '/', '/http/test/path/111', '2021-08-09 11:47:23', '2021-08-09 11:47:23');
INSERT IGNORE INTO rule (id, selector_id, match_mode, name, enabled, loged, sort, handle, date_created, date_updated) VALUES ('1424403555619082240', '1424403439680131072', 0, 'http-redirect-outer-url', 1, 1, 1, '{"redirectURI":"http://localhost:9195/http/test/path/222?name=redirectToOuterPath"}', '2021-08-09 00:14:06', '2021-08-09 00:22:01');
INSERT IGNORE INTO rule (id, selector_id, match_mode, name, enabled, loged, sort, handle, date_created, date_updated) VALUES ('1424405341335621632', '1424403439680131072', 0, 'http-redirect-inner-path', 1, 1, 2, '{"redirectURI":"/actuator/health"}', '2021-08-09 00:21:12', '2021-08-09 00:30:22');
INSERT IGNORE INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1424403555627470848', '1424403555619082240', 'uri', '=', '/', '/http/test/path/111', '2021-08-09 00:22:01', '2021-08-09 00:22:01');
INSERT IGNORE INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1424404768645353472', '1424403555619082240', 'query', '=', 'name', 'redirectToOuterPath', '2021-08-09 00:22:01', '2021-08-09 00:22:01');
INSERT IGNORE INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1424405341339815936', '1424405341335621632', 'uri', '=', '/', '/http/test/path/111', '2021-08-09 00:30:21', '2021-08-09 00:30:22');
INSERT IGNORE INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1424405341344010240', '1424405341335621632', 'query', '=', 'name', 'redirectToInnerPath', '2021-08-09 00:30:21', '2021-08-09 00:30:22');

/** prepare jwt **/
UPDATE plugin SET config = '{"secretKey":"key00000","filterPath":"/http/test/path/1111/name"}', enabled = 1 WHERE id = '19';
INSERT IGNORE INTO selector (id, plugin_id, name, match_mode, type, sort, handle, enabled, loged, continued, date_created, date_updated) VALUES ('1425390877916254208', '19', 'http-jwt', 0, 1, 1, null, 1, 1, 1,'2021-08-11 17:37:22','2021-08-11 21:08:25');
INSERT IGNORE INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1425390877920448512', '1425390877916254208', 'uri', 'match', '/', '/http/**','2021-08-11 17:37:22','2021-08-11 17:37:22');
INSERT IGNORE INTO rule (id, selector_id, match_mode, name, enabled, loged, sort, date_created, date_updated) VALUES ('1425390970652315648', '1425390877916254208', 1, 'http-jwt', 1, 1, 1,'2021-08-11 17:37:45','2021-08-11 21:07:26');
INSERT IGNORE INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1425450010986262528', '1425390970652315648', 'uri', '=', '/', '/http/test/findByUserId','2021-08-04 13:09:32','2021-08-04 13:09:32');
INSERT IGNORE INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1425479564999307264', '1425390970652315648', 'uri', '=', '/', '/http/test/path/1111/name','2021-08-04 13:09:32','2021-08-04 13:09:32');

/** prepare context-path **/
INSERT IGNORE INTO selector (id, plugin_id, name, match_mode, type, sort, handle, enabled, loged, continued, date_created, date_updated) VALUES ('1429645721775689728', '14', '/context-path-test/error', 0, 1, 1, NULL, 1, 1, 1, '2021-08-23 11:24:36', '2021-08-23 11:24:36');
INSERT IGNORE INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value, date_created, date_updated) VALUES ('1429645721788272640', '1429645721775689728', 'uri', '=', '/', '/http/order/findById', '2021-08-23 11:24:36', '2021-08-23 11:24:36');
INSERT IGNORE INTO rule (id, selector_id, match_mode, name, enabled, loged, sort, handle, date_created, date_updated) VALUES ('1429645883050872832', '1429645721775689728', 0, '/context-path-test/error', 1, 1, 1, '{"contextPath":"/http","addPrefix":"/error"}', '2021-08-23 11:25:15', '2021-08-23 11:25:15');
