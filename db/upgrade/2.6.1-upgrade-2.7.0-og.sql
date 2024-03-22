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

-- this file works for og.
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507020', '14', 'rewriteContextPath', 'rewriteContextPath', 2, 2, 2, '{"required":"0","defaultValue":""}', '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507021', '14', 'percentage', 'percentage', 1, 2, 3, '{"required":"1","defaultValue":"100"}', '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507022', '3', 'rewriteMetaData', 'rewriteMetaData', 3, 2, 3, '{"required":"1","defaultValue":"false"}', '2024-02-07 14:31:49', '2024-02-07 14:31:49');

INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737478', 'rewriteMetaData', 'REWRITE_META_DATA', 'true', 'true', '', 4, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737479', 'rewriteMetaData', 'REWRITE_META_DATA', 'false', 'false', '', 4, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');

DROP TABLE IF EXISTS "public"."int_lock";
CREATE TABLE "public"."int_lock" (
    "lock_key" CHAR(36) NOT NULL,
    "region" VARCHAR(100) NOT NULL,
    "client_id" CHAR(36),
    "created_date" TIMESTAMP WITH TIME ZONE NOT NULL,
    CONSTRAINT INT_LOCK_PK PRIMARY KEY ("lock_key", "region")
);
COMMENT ON COLUMN "public"."int_lock"."lock_key" IS 'lock_key';
COMMENT ON COLUMN "public"."int_lock"."region" IS 'region';
COMMENT ON COLUMN "public"."int_lock"."client_id" IS 'client_id';
COMMENT ON COLUMN "public"."int_lock"."created_date" IS 'created_date';

INSERT INTO "public"."resource" VALUES ('1347048240677269503', '1346777766301888512', 'SHENYU.PLUGIN.BATCH.OPENED', '', '', '', 2, 3, '', 1, 0, 'system:authen:open', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708748849151', '1346358560427216896', '1347048240677269503', '2022-05-25 18:08:01', '2022-05-25 18:08:01');

INSERT INTO "public"."resource" VALUES ('1386680049203195915', '1346777157943259136', 'SHENYU.COMMON.EXPORT', '', '', '', 2, 0, '', 1, 0, 'system:manager:exportConfig', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1386680049203195916', '1346777157943259136', 'SHENYU.COMMON.IMPORT', '', '', '', 2, 0, '', 1, 0, 'system:manager:importConfig', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1386680049203195906', '1346358560427216896', '1386680049203195915', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1386680049203195907', '1346358560427216896', '1386680049203195916', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
