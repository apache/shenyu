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

/* insert plugin for loggingPulsar */
INSERT INTO "public"."plugin" VALUES ('35', 'loggingPulsar', '{"topic":"shenyu-access-logging", "serviceUrl": "pulsar://localhost:6650"}', 'Logging', 185, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');

/* insert plugin_handle data for plugin loggingPulsar */
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524972', '35', 'topic', 'topic', 2, 3, 1, '{"required":"1","defaultValue":"shenyu-access-logging"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524973', '35', 'serviceUrl', 'serviceUrl', 2, 3, 2, '{"required":"1","defaultValue":"pulsar://localhost:6650"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524974', '35', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524975', '35', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524976', '35', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524977', '35', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
