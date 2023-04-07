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

ALTER TABLE selector ADD COLUMN match_restful int2 NOT NULL;
COMMENT ON COLUMN "public"."rule"."match_restful" IS 'whether to match restful(0 cache, 1 not cache)';
COMMIT;

ALTER TABLE rule ADD COLUMN match_restful int2 NOT NULL;
COMMENT ON COLUMN "public"."rule"."match_restful" IS 'whether to match restful(0 cache, 1 not cache)';
COMMIT;


/* insert plugin_handle data for plugin CryptorRequest */
INSERT INTO "public"."plugin_handle" VALUES ('1630760188111376384', '24', 'mapType', 'mapType', 3, 2, 3, '{\"required\":\"0\",\"defaultValue\":\"all\",\"rule\":\"\"}', '2023-03-01 10:41:41', '2023-03-01 10:42:21');
/* insert plugin_handle data for plugin cryptorResponse */
INSERT INTO "public"."plugin_handle" VALUES ('1630768384280514560', '25', 'mapType', 'mapType', 3, 2, 4, '{\"required\":\"0\",\"defaultValue\":\"all\",\"rule\":\"\"}', '2023-03-01 11:14:15', '2023-03-01 11:15:14');

/* insert plugin_handle data for plugin_handle mapType */
INSERT INTO "public"."shenyu_dict" VALUES ('1630761573833920512', 'mapType', 'mapType', 'all', 'all', '', 0, 1, '2023-03-01 10:47:11', '2023-03-01 10:47:11');
INSERT INTO "public"."shenyu_dict" VALUES ('1630761984393367552', 'mapType', 'mapType', 'field', 'field', '', 1, 1, '2023-03-01 10:48:49', '2023-03-01 10:48:49');
INSERT INTO "public"."shenyu_dict" VALUES ('1572621976689762308', 'loadBalance', 'LOAD_BALANCE', 'p2c', 'p2c', 'p2c', 4, 1, '2023-03-07 22:15:16.846', '2023-03-07 22:15:16.846');
INSERT INTO "public"."shenyu_dict" VALUES ('1572621976689762309', 'loadBalance', 'LOAD_BALANCE', 'shortestResponse', 'shortestResponse', 'shortestResponse', 5, 1, '2023-03-17 10:15:16.846', '2023-03-07 10:15:16.846');

/* add column into plugin table */
ALTER TABLE "public"."plugin" ADD COLUMN plugin_jar bytea NULL;
COMMENT ON COLUMN "public"."plugin".plugin_jar IS 'plugin jar';
