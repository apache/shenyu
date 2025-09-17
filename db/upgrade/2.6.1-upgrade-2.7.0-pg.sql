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
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507020', '14', 'rewriteContextPath', 'rewriteContextPath', 2, 2, 2, '{"required":"0","defaultValue":""}', '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507021', '14', 'percentage', 'percentage', 1, 2, 3, '{"required":"1","defaultValue":"100"}', '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507022', '3', 'rewriteMetaData', 'rewriteMetaData', 3, 2, 3, '{"required":"1","defaultValue":"false"}', '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507024', '8', 'registerType', 'registerType', 2, 3, 1, NULL, '2024-08-24 09:40:03.293', '2024-08-24 21:52:27.920');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507025', '8', 'serverLists', 'serverLists', 2, 3, 2, NULL, '2024-08-24 21:52:51.179', '2024-08-24 21:53:27.483');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507026', '8', 'props', 'props', 4, 3, 3, NULL, '2024-08-24 21:53:25.764', '2024-08-24 21:53:30.255');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507027', '20', 'preserveHost', 'preserveHost', 3, 2, 0, '{"required":"0","defaultValue":"false","rule":""}', '2024-12-05 22:00:02.251', '2024-12-05 22:00:02.251');

INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737478', 'rewriteMetaData', 'REWRITE_META_DATA', 'true', 'true', '', 4, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737479', 'rewriteMetaData', 'REWRITE_META_DATA', 'false', 'false', '', 4, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737480', 'preserveHost', 'PRESERVE_HOST', 'true', 'true', '', 0, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737481', 'preserveHost', 'PRESERVE_HOST', 'false', 'false', '', 1, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507028', '20', 'requestHeaderUniqueStrategy', 'requestHeaderUniqueStrategy', 2, 2, 1, '{"required":"0","rule":""}', '2024-12-13 22:36:54.299', '2024-12-13 22:36:54.299');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507029', '20', 'requestUniqueHeaders', 'requestUniqueHeaders', 2, 2, 2, '{"required":"0","rule":""}', '2024-12-13 22:37:29.959', '2024-12-13 22:37:29.959');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507030', '20', 'respHeaderUniqueStrategy', 'respHeaderUniqueStrategy', 2, 2, 3, '{"required":"0","rule":""}', '2024-12-13 22:37:48.239', '2024-12-13 22:37:48.239');
INSERT INTO "public"."plugin_handle" VALUES ('1722804548510507031', '20', 'respUniqueHeaders', 'respUniqueHeaders', 2, 2, 4, '{"required":"0","rule":""}', '2024-12-13 22:38:05.726', '2024-12-13 22:38:05.726');

-- ----------------------------
-- Table structure for shenyu_lock
-- ----------------------------
DROP TABLE IF EXISTS "public"."shenyu_lock";
CREATE TABLE "public"."shenyu_lock" (
    "lock_key" CHAR(36) NOT NULL,
    "region" VARCHAR(100) NOT NULL,
    "client_id" CHAR(36),
    "created_date" TIMESTAMP WITH TIME ZONE NOT NULL,
    CONSTRAINT shenyu_lock_pk PRIMARY KEY ("lock_key", "region")
);
COMMENT ON COLUMN "public"."shenyu_lock"."lock_key" IS 'lock_key';
COMMENT ON COLUMN "public"."shenyu_lock"."region" IS 'region';
COMMENT ON COLUMN "public"."shenyu_lock"."client_id" IS 'client_id';
COMMENT ON COLUMN "public"."shenyu_lock"."created_date" IS 'created_date';


INSERT INTO "public"."resource" VALUES ('1347048240677269503', '1346777766301888512', 'SHENYU.PLUGIN.BATCH.OPENED', '', '', '', 2, 3, '', 1, 0, 'system:authen:open', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708748849151', '1346358560427216896', '1347048240677269503', '2022-05-25 18:08:01', '2022-05-25 18:08:01');

INSERT INTO "public"."resource" VALUES ('1386680049203195915', '1346777157943259136', 'SHENYU.COMMON.EXPORT', '', '', '', 2, 0, '', 1, 0, 'system:manager:exportConfig', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1386680049203195916', '1346777157943259136', 'SHENYU.COMMON.IMPORT', '', '', '', 2, 0, '', 1, 0, 'system:manager:importConfig', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1386680049203195906', '1346358560427216896', '1386680049203195915', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1386680049203195907', '1346358560427216896', '1386680049203195916', '2022-05-25 18:08:01', '2022-05-25 18:08:01');


-- ----------------------------
-- Table structure for cluster_master
-- ----------------------------
DROP TABLE IF EXISTS "public"."cluster_master";
CREATE TABLE "public"."cluster_master"
(
    "id"            varchar(128) COLLATE "pg_catalog"."default" NOT NULL PRIMARY KEY,
    "master_host"   varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "master_port"   varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "context_path"  varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "date_created"  timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated"  timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."cluster_master"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."cluster_master"."master_host" IS 'master host';
COMMENT ON COLUMN "public"."cluster_master"."master_port" IS 'master port';
COMMENT ON COLUMN "public"."cluster_master"."context_path" IS 'master context_path';
COMMENT ON COLUMN "public"."cluster_master"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."cluster_master"."date_updated" IS 'update time';

INSERT INTO "public"."resource" VALUES ('1792749362445840474', '1357956838021890048', 'SHENYU.MENU.SYSTEM.MANAGMENT.NAMESPACE', 'namespace', '/system/namespace', 'namespace', 1, 0, 'appstore', 0, 0, '', 1, '2024-06-22 17:00:00.000', '2024-06-22 17:00:00.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840475', '1792749362445840474', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 0, '', 1, 0, 'system:namespace:add', 1, '2024-06-22 17:00:00.000', '2024-06-22 17:00:00.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840476', '1792749362445840474', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 1, '', 1, 0, 'system:namespace:list', 1, '2024-06-22 17:00:00.000', '2024-06-22 17:00:00.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840477', '1792749362445840474', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 2, '', 1, 0, 'system:namespace:delete', 1,'2024-06-22 17:00:00.000', '2024-06-22 17:00:00.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840478', '1792749362445840474', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 3, '', 1, 0, 'system:namespace:edit', 1, '2024-06-22 17:00:00.000', '2024-06-22 17:00:00.000');

INSERT INTO "public"."permission" VALUES ('1792779493541343252', '1346358560427216896', '1792749362445840474', '2024-06-22 17:00:00.000', '2024-06-22 17:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343253', '1346358560427216896', '1792749362445840475', '2024-06-22 17:00:00.000', '2024-06-22 17:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343254', '1346358560427216896', '1792749362445840476', '2024-06-22 17:00:00.000', '2024-06-22 17:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343255', '1346358560427216896', '1792749362445840477', '2024-06-22 17:00:00.000', '2024-06-22 17:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343256', '1346358560427216896', '1792749362445840478', '2024-06-22 17:00:00.000', '2024-06-22 17:00:00.000');



DROP TABLE IF EXISTS "public"."namespace";
CREATE TABLE "public"."namespace" (
                                      "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
                                      "namespace_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
                                      "name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
                                      "description" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
                                      "date_created" timestamp(3) NOT NULL DEFAULT now(),
                                      "date_updated" timestamp(3) NOT NULL DEFAULT now(),
                                      PRIMARY KEY ("id")
);
COMMENT ON COLUMN "public"."namespace"."id" IS 'Namespace primary key';
COMMENT ON COLUMN "public"."namespace"."namespace_id" IS 'Namespace ID';
COMMENT ON COLUMN "public"."namespace"."name" IS 'Namespace name';
COMMENT ON COLUMN "public"."namespace"."description" IS 'Namespace description';
COMMENT ON COLUMN "public"."namespace"."date_created" IS 'Creation time';
COMMENT ON COLUMN "public"."namespace"."date_updated" IS 'Update time';


INSERT INTO "public"."namespace" VALUES ('1', '649330b6-c2d7-4edc-be8e-8a54df9eb385', 'default', 'default-namespace', '2024-06-22 20:25:14.359', '2024-06-22 23:27:40.778');

CREATE TABLE "public"."namespace_plugin_rel" (
                                                 "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
                                                 "namespace_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
                                                 "plugin_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
                                                 "config" text ,
                                                 "sort" int,
                                                 "enabled" smallint NOT NULL DEFAULT 0 CHECK (enabled IN (0, 1)),
                                                 "date_created" timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
                                                 "date_updated" timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
                                                 PRIMARY KEY (id)
);

COMMENT ON COLUMN "public"."namespace_plugin_rel"."id" IS 'Primary key ID';
COMMENT ON COLUMN "public"."namespace_plugin_rel"."namespace_id" IS 'Namespace ID';
COMMENT ON COLUMN "public"."namespace_plugin_rel"."plugin_id" IS 'Plugin ID';
COMMENT ON COLUMN "public"."namespace_plugin_rel"."config" IS 'Plugin configuration';
COMMENT ON COLUMN "public"."namespace_plugin_rel"."sort" IS 'Sort order';
COMMENT ON COLUMN "public"."namespace_plugin_rel"."enabled" IS 'Whether the plugin is enabled (0 = not open, 1 = open)';
COMMENT ON COLUMN "public"."namespace_plugin_rel"."date_created" IS 'Creation time';
COMMENT ON COLUMN "public"."namespace_plugin_rel"."date_updated" IS 'Update time';


INSERT INTO "public"."resource" VALUES ('1792749362445840479', '1357956838021890048', 'SHENYU.MENU.SYSTEM.MANAGMENT.NAMESPACEPLUGIN', 'namespacePlugin', '/config/namespacePlugin', 'namespacePlugin', 1, 2, 'build', 0, 0, '', 1, '2024-06-25 18:02:53.000', '2024-06-25 18:02:53.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840480', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, '', 1, 0, 'system:namespacePlugin:list', 1, '2024-06-25 18:02:53.000', '2024-06-25 18:02:53.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840481', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 1, '', 1, 0, 'system:namespacePlugin:delete', 1, '2024-06-25 18:02:53.000', '2024-06-25 18:02:53.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840482', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 2, '', 1, 0, 'system:namespacePlugin:add', 1, '2024-06-25 18:02:53.000', '2024-06-25 18:02:53.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840483', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.SYNCHRONIZE', '', '', '', 2, 3, '', 1, 0, 'system:namespacePlugin:modify', 1, '2024-06-25 18:02:53.000', '2024-06-25 18:02:53.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840484', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.ENABLE', '', '', '', 2, 4, '', 1, 0, 'system:namespacePlugin:disable', 1, '2024-06-25 18:02:53.000', '2024-06-25 18:02:53.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840485', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 5, '', 1, 0, 'system:namespacePlugin:edit', 1,'2024-06-25 18:02:53.000', '2024-06-25 18:02:53.000');
INSERT INTO "public"."resource" VALUES ('1792749362445840486', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.RESOURCE', '', '', '', 2, 6, '', 1, 0, 'system:namespacePlugin:resource', 1,'2024-06-25 18:02:53.000', '2024-06-25 18:02:53.000');


INSERT INTO "public"."permission" VALUES ('1792779493541343260', '1346358560427216896', '1792749362445840479', '2024-06-25 20:00:00.000', '2024-06-25 20:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343261', '1346358560427216896', '1792749362445840480', '2024-06-25 20:00:00.000', '2024-06-25 20:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343262', '1346358560427216896', '1792749362445840481', '2024-06-25 20:00:00.000', '2024-06-25 20:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343263', '1346358560427216896', '1792749362445840482', '2024-06-25 20:00:00.000', '2024-06-25 20:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343264', '1346358560427216896', '1792749362445840483', '2024-06-25 20:00:00.000', '2024-06-25 20:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343265', '1346358560427216896', '1792749362445840484', '2024-06-25 20:00:00.000', '2024-06-25 20:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343266', '1346358560427216896', '1792749362445840485', '2024-06-25 20:00:00.000', '2024-06-25 20:00:00.000');
INSERT INTO "public"."permission" VALUES ('1792779493541343267', '1346358560427216896', '1792749362445840486', '2024-06-25 20:00:00.000', '2024-06-25 20:00:00.000');

INSERT INTO "public"."permission" VALUES ('1697141926281381720', '1346358560427216896', '1844015648095666176', '2023-08-31 06:59:01', '2023-08-31 06:59:01');
INSERT INTO "public"."permission" VALUES ('1697145808239621836', '1346358560427216896', '1844025735425183744', '2023-08-31 07:22:07', '2023-08-31 07:22:07');
INSERT INTO "public"."permission" VALUES ('1697146375754129471', '1346358560427216896', '1844025850382667776', '2023-08-31 07:14:26', '2023-08-31 07:14:26');
INSERT INTO "public"."permission" VALUES ('1697146617543248162', '1346358560427216896', '1844025989214130176', '2023-08-31 07:22:07', '2023-08-31 07:22:07');
INSERT INTO "public"."permission" VALUES ('1697146860569542740', '1346358560427216896', '1844026099075534848', '2023-08-31 07:18:37', '2023-08-31 07:18:37');

INSERT INTO "public"."resource" VALUES ('1844015648095666176', '1346776175553376256', 'SHENYU.MENU.SYSTEM.MANAGMENT.SCALE', '', '/system/scale', '', 1, 4, 'sliders', 0, 0, '', 1, '2024-10-09 22:02:45.317000', '2024-10-10 14:33:43.897017');
INSERT INTO "public"."resource" VALUES ('1844025735425183744', '1844015648095666176', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, 'unordered-list', 1, 0, 'system:scale:list', 1, '2024-10-09 22:42:50.322000', '2024-10-09 22:42:50.325462');
INSERT INTO "public"."resource" VALUES ('1844025850382667776', '1844015648095666176', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 0, 'plus', 1, 0, 'system:scale:add', 1, '2024-10-09 22:43:17.731000', '2024-10-09 22:43:17.731661');
INSERT INTO "public"."resource" VALUES ('1844025989214130176', '1844015648095666176', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 0, 'delete', 1, 0, 'system:scale:delete', 1, '2024-10-09 22:43:50.831000', '2024-10-09 22:43:50.831705');
INSERT INTO "public"."resource" VALUES ('1844026099075534848', '1844015648095666176', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 0, 'edit', 1, 0, 'system:scale:edit', 1, '2024-10-09 22:44:17.024000', '2024-10-09 22:44:17.024555');


/* add column into dashboard_user table */
ALTER TABLE "public"."dashboard_user" ADD COLUMN client_id VARCHAR(32) NULL;
COMMENT ON COLUMN "public"."dashboard_user".client_id IS 'client id';

ALTER TABLE "public"."selector" ADD COLUMN namespace_id VARCHAR(50) NOT NULL default '649330b6-c2d7-4edc-be8e-8a54df9eb385';
COMMENT ON COLUMN "public"."selector"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."rule" ADD COLUMN namespace_id VARCHAR(50) NOT NULL default '649330b6-c2d7-4edc-be8e-8a54df9eb385';
COMMENT ON COLUMN "public"."rule"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."meta_data" ADD COLUMN namespace_id VARCHAR(50) NOT NULL default '649330b6-c2d7-4edc-be8e-8a54df9eb385';
COMMENT ON COLUMN "public"."meta_data"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."app_auth" ADD COLUMN namespace_id VARCHAR(50) NOT NULL default '649330b6-c2d7-4edc-be8e-8a54df9eb385';
COMMENT ON COLUMN "public"."app_auth"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."discovery" ADD COLUMN namespace_id VARCHAR(50) NOT NULL default '649330b6-c2d7-4edc-be8e-8a54df9eb385';
COMMENT ON COLUMN "public"."discovery"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."discovery_upstream" ADD COLUMN namespace_id VARCHAR(50) NOT NULL default '649330b6-c2d7-4edc-be8e-8a54df9eb385';
COMMENT ON COLUMN "public"."discovery_upstream"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."proxy_selector" ADD COLUMN namespace_id VARCHAR(50) NOT NULL default '649330b6-c2d7-4edc-be8e-8a54df9eb385';
COMMENT ON COLUMN "public"."proxy_selector"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."alert_receiver" ADD COLUMN namespace_id VARCHAR(50) NOT NULL default '649330b6-c2d7-4edc-be8e-8a54df9eb385';
COMMENT ON COLUMN "public"."alert_receiver"."namespace_id" IS 'namespaceId';

UPDATE "public"."selector" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL OR LENGTH(namespace_id) = 0;

UPDATE "public"."rule" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL OR LENGTH(namespace_id) = 0;

UPDATE "public"."meta_data" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL OR LENGTH(namespace_id) = 0;

UPDATE "public"."app_auth" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL OR LENGTH(namespace_id) = 0;

UPDATE "public"."discovery" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL OR LENGTH(namespace_id) = 0;

UPDATE "public"."discovery_upstream" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL OR LENGTH(namespace_id) = 0;

UPDATE "public"."proxy_selector" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL OR LENGTH(namespace_id) = 0;

UPDATE "public"."alert_receiver" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL OR LENGTH(namespace_id) = 0;

-- ----------------------------
-- Table structure for scale
-- ----------------------------
DROP TABLE IF EXISTS "public"."scale_policy";
CREATE TABLE IF NOT EXISTS "public"."scale_policy"
(
    "id"             varchar(128)   COLLATE "pg_catalog"."default" NOT NULL,
    "sort"           int4           NOT NULL,
    "status"         int2           NOT NULL,
    "num"            int4           ,
    "begin_time"     timestamp(6)   ,
    "end_time"       timestamp(6)   ,
    "date_created"   timestamp(3)   NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated"   timestamp(3)   NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
    );
COMMENT ON COLUMN "public"."scale_policy"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."scale_policy"."sort" IS 'sort';
COMMENT ON COLUMN "public"."scale_policy"."status" IS 'status 1:enable 0:disable';
COMMENT ON COLUMN "public"."scale_policy"."num" IS 'number of bootstrap';
COMMENT ON COLUMN "public"."scale_policy"."begin_time" IS 'begin time';
COMMENT ON COLUMN "public"."scale_policy"."end_time" IS 'end time';
COMMENT ON COLUMN "public"."scale_policy"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."scale_policy"."date_updated" IS 'update time';

INSERT INTO "public"."scale_policy" VALUES ('1', 3, 0, 10, NULL, NULL, '2024-07-31 20:00:00.000', '2024-07-31 20:00:00.000');
INSERT INTO "public"."scale_policy" VALUES ('2', 2, 0, 10, '2024-07-31 20:00:00.000', '2024-08-01 20:00:00.000', '2024-07-31 20:00:00.000', '2024-07-31 20:00:00.000');
INSERT INTO "public"."scale_policy" VALUES ('3', 1, 0, NULL, NULL, NULL, '2024-07-31 20:00:00.000', '2024-07-31 20:00:00.000');

DROP TABLE IF EXISTS "public"."scale_rule";
CREATE TABLE "public"."scale_rule"
(
    "id"             varchar(128)   COLLATE "pg_catalog"."default" NOT NULL,
    "metric_name"    varchar(128)   COLLATE "pg_catalog"."default" NOT NULL,
    "type"           int4           NOT NULL,
    "sort"           int4           NOT NULL,
    "status"         int2           NOT NULL,
    "minimum"        varchar(128)   COLLATE "pg_catalog"."default",
    "maximum"        varchar(128)   COLLATE "pg_catalog"."default",
    "date_created"   timestamp(3)   NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated"   timestamp(3)   NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
);
COMMENT ON COLUMN "public"."scale_rule"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."scale_rule"."metric_name" IS 'metric name';
COMMENT ON COLUMN "public"."scale_rule"."type" IS 'type 0:shenyu 1:k8s 2:others';
COMMENT ON COLUMN "public"."scale_rule"."sort" IS 'sort';
COMMENT ON COLUMN "public"."scale_rule"."status" IS 'status 1:enable 0:disable';
COMMENT ON COLUMN "public"."scale_rule"."minimum" IS 'minimum of metric';
COMMENT ON COLUMN "public"."scale_rule"."maximum" IS 'maximum of metric';
COMMENT ON COLUMN "public"."scale_rule"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."scale_rule"."date_updated" IS 'update time';

DROP TABLE IF EXISTS "public"."scale_history";
CREATE TABLE "public"."scale_history"
(
    "id"             varchar(128)   COLLATE "pg_catalog"."default" NOT NULL,
    "config_id"      int4           NOT NULL,
    "num"            int4           NOT NULL,
    "action"         int4           NOT NULL,
    "msg"            text           COLLATE "pg_catalog"."default",
    "date_created"   timestamp(3)   NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated"   timestamp(3)   NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
);
COMMENT ON COLUMN "public"."scale_history"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."scale_history"."config_id" IS '0:manual 1:period 2:dynamic';
COMMENT ON COLUMN "public"."scale_history"."num" IS 'number of bootstrap';
COMMENT ON COLUMN "public"."scale_history"."action" IS 'status 1:enable 0:disable';
COMMENT ON COLUMN "public"."scale_history"."msg" IS 'message';
COMMENT ON COLUMN "public"."scale_history"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."scale_history"."date_updated" IS 'update time';


DROP TABLE IF EXISTS "public"."namespace_user_rel";
CREATE TABLE "public"."namespace_user_rel"
(
    "id"             varchar(128)   COLLATE "pg_catalog"."default" NOT NULL,
    "namespace_id"   varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
    "user_id"        varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "date_created"   timestamp(3)   NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated"   timestamp(3)   NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
);
COMMENT ON COLUMN "public"."namespace_user_rel"."id" IS 'primary key';
COMMENT ON COLUMN "public"."namespace_user_rel"."namespace_id" IS 'namespace_id';
COMMENT ON COLUMN "public"."namespace_user_rel"."user_id" IS 'user_id';
COMMENT ON COLUMN "public"."namespace_user_rel"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."namespace_user_rel"."date_updated" IS 'update time';

INSERT INTO "public"."namespace_plugin_rel" (
    id,
    namespace_id,
    plugin_id,
    config,
    sort,
    enabled,
    date_created,
    date_updated
)
SELECT
    CONCAT(EXTRACT(EPOCH FROM clock_timestamp()) * 1000::BIGINT, LPAD((RANDOM() * 1000000)::BIGINT::TEXT, 6, '0')),
    '649330b6-c2d7-4edc-be8e-8a54df9eb385',
    id,
    config,
    sort,
    enabled,
    date_created,
    date_updated
FROM "public"."plugin";


INSERT INTO "public"."namespace_user_rel" (
    id,
    namespace_id,
    user_id,
    date_created,
    date_updated
)
SELECT
    CONCAT(
            FLOOR(EXTRACT(EPOCH FROM NOW()) * 1000),
            LPAD(CAST(FLOOR(RANDOM() * 1000000) AS TEXT), 6, '0')
        ),
    '649330b6-c2d7-4edc-be8e-8a54df9eb385',
    id,
    date_created,
    date_updated
FROM dashboard_user;