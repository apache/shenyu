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
INSERT INTO "public"."plugin" VALUES ('67', 'sensitiveWord', NULL, 'Ai', 197, 0, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000', NULL);
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684609', '67', 'url', 'url', 2, 3, 0, '{"required":"0","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684610', '67', 'password', 'password', 2, 3, 1, '{"required":"0","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684611', '67', 'database', 'database', 1, 3, 2, '{"required":"0","defaultValue":"0","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684612', '67', 'mode', 'mode', 2, 3, 3, '{"required":"0","defaultValue":"standalone","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684613', '67', 'master', 'master', 2, 3, 4, '{"required":"0","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684614', '67', 'maxIdle', 'maxIdle', 1, 3, 5, '{"required":"0","defaultValue":"8","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684615', '67', 'minIdle', 'minIdle', 1, 3, 6, '{"required":"0","defaultValue":"0","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684616', '67', 'maxActive', 'maxActive', 1, 3, 7, '{"required":"0","defaultValue":"8","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684617', '67', 'maxWait', 'maxWait', 1, 3, 8, '{"required":"0","defaultValue":"-1","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684618', '67', 'redisKey', 'redisKey', 2, 2, 0, '{"required":"0","defaultValue":"shenyu:sensitive:words","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684619', '67', 'refreshIntervalSeconds', 'refreshIntervalSeconds', 1, 2, 1, '{"required":"0","defaultValue":"300","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684620', '67', 'failClosed', 'failClosed', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684621', '67', 'words', 'words', 2, 2, 3, '{"required":"0","placeholder":"words separated by commas or by new lines","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."plugin_handle" VALUES ('1942847622591684622', '67', 'maxBodySize', 'maxBodySize', 1, 2, 4, '{"required":"0","defaultValue":"0","placeholder":"the largest body that is scanned, in bytes, 0 means unlimited","rule":""}', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."resource" VALUES ('1953048313980116905', '1346775491550474240', 'sensitiveWord', 'sensitiveWord', '/plug/sensitiveWord', 'sensitiveWord', 1, 0, 'pic-center', 0, 0, '', 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."resource" VALUES ('1953048313980116906', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordSelector:add', 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."resource" VALUES ('1953048313980116907', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordSelector:query', 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."resource" VALUES ('1953048313980116908', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordSelector:edit', 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."resource" VALUES ('1953048313980116909', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordSelector:delete', 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."resource" VALUES ('1953048313980116910', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordRule:add', 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."resource" VALUES ('1953048313980116911', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordRule:query', 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."resource" VALUES ('1953048313980116912', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordRule:edit', 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."resource" VALUES ('1953048313980116913', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWordRule:delete', 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."resource" VALUES ('1953048313980116914', '1953048313980116905', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:sensitiveWord:modify', 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737584', 'failClosed', 'FAIL_CLOSED', 'open', 'true', '', 1, 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737585', 'failClosed', 'FAIL_CLOSED', 'close', 'false', '', 2, 1, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."permission" VALUES ('1953049887387303965', '1346358560427216896', '1953048313980116905', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."permission" VALUES ('1953049887387303966', '1346358560427216896', '1953048313980116906', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."permission" VALUES ('1953049887387303967', '1346358560427216896', '1953048313980116907', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."permission" VALUES ('1953049887387303968', '1346358560427216896', '1953048313980116908', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."permission" VALUES ('1953049887387303969', '1346358560427216896', '1953048313980116909', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."permission" VALUES ('1953049887387303970', '1346358560427216896', '1953048313980116910', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."permission" VALUES ('1953049887387303971', '1346358560427216896', '1953048313980116911', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."permission" VALUES ('1953049887387303972', '1346358560427216896', '1953048313980116912', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."permission" VALUES ('1953049887387303973', '1346358560427216896', '1953048313980116913', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."permission" VALUES ('1953049887387303974', '1346358560427216896', '1953048313980116914', '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1907261515594055681', '649330b6-c2d7-4edc-be8e-8a54df9eb385', '67', NULL, 197, 0, '2026-09-21 00:00:00.000', '2026-09-21 00:00:00.000');
