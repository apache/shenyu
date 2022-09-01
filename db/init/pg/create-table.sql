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

-- ----------------------------
-- Sequence structure for operation_record_log_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "public"."operation_record_log_id_seq";
CREATE SEQUENCE "public"."operation_record_log_id_seq"
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for plugin_handle_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "public"."plugin_handle_id_seq";
CREATE SEQUENCE "public"."plugin_handle_id_seq"
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for shenyu_dict_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "public"."shenyu_dict_id_seq";
CREATE SEQUENCE "public"."shenyu_dict_id_seq"
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

-- ----------------------------
-- Table structure for app_auth
-- ----------------------------
DROP TABLE IF EXISTS "public"."app_auth";
CREATE TABLE "public"."app_auth" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "app_key" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "app_secret" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "user_id" varchar(128) COLLATE "pg_catalog"."default",
  "phone" varchar(255) COLLATE "pg_catalog"."default",
  "ext_info" varchar(1024) COLLATE "pg_catalog"."default",
  "open" int2 NOT NULL,
  "enabled" int2 NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."app_auth"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."app_auth"."app_key" IS 'application identification key';
COMMENT ON COLUMN "public"."app_auth"."app_secret" IS 'encryption algorithm secret';
COMMENT ON COLUMN "public"."app_auth"."user_id" IS 'user id';
COMMENT ON COLUMN "public"."app_auth"."phone" IS 'phone number when the user applies';
COMMENT ON COLUMN "public"."app_auth"."ext_info" IS 'extended parameter json';
COMMENT ON COLUMN "public"."app_auth"."open" IS 'open auth path or not (0 close, 1 open) ';
COMMENT ON COLUMN "public"."app_auth"."enabled" IS 'delete or not (0 close, 1 open) ';
COMMENT ON COLUMN "public"."app_auth"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."app_auth"."date_updated" IS 'update time';

-- ----------------------------
-- Records of app_auth
-- ----------------------------

-- ----------------------------
-- Table structure for auth_param
-- ----------------------------
DROP TABLE IF EXISTS "public"."auth_param";
CREATE TABLE "public"."auth_param" (
  "id" varchar(129) COLLATE "pg_catalog"."default" NOT NULL,
  "auth_id" varchar(129) COLLATE "pg_catalog"."default",
  "app_name" varchar(256) COLLATE "pg_catalog"."default" NOT NULL,
  "app_param" varchar(256) COLLATE "pg_catalog"."default",
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."auth_param"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."auth_param"."auth_id" IS 'Authentication table id';
COMMENT ON COLUMN "public"."auth_param"."app_name" IS 'business Module';
COMMENT ON COLUMN "public"."auth_param"."app_param" IS 'service module parameters (parameters that need to be passed by the gateway) json type';
COMMENT ON COLUMN "public"."auth_param"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."auth_param"."date_updated" IS 'update time';

-- ----------------------------
-- Records of auth_param
-- ----------------------------

-- ----------------------------
-- Table structure for auth_path
-- ----------------------------
DROP TABLE IF EXISTS "public"."auth_path";
CREATE TABLE "public"."auth_path" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "auth_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "app_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "path" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "enabled" int2 NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."auth_path"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."auth_path"."auth_id" IS 'auth table id';
COMMENT ON COLUMN "public"."auth_path"."app_name" IS 'module';
COMMENT ON COLUMN "public"."auth_path"."path" IS 'path';
COMMENT ON COLUMN "public"."auth_path"."enabled" IS 'whether pass 1 is (0 close, 1 open) ';
COMMENT ON COLUMN "public"."auth_path"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."auth_path"."date_updated" IS 'update time';

-- ----------------------------
-- Records of auth_path
-- ----------------------------

-- ----------------------------
-- Table structure for dashboard_user
-- ----------------------------
DROP TABLE IF EXISTS "public"."dashboard_user";
CREATE TABLE "public"."dashboard_user" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "user_name" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "password" varchar(128) COLLATE "pg_catalog"."default",
  "role" int4 NOT NULL,
  "enabled" int2 NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."dashboard_user"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."dashboard_user"."user_name" IS 'user name';
COMMENT ON COLUMN "public"."dashboard_user"."password" IS 'user password';
COMMENT ON COLUMN "public"."dashboard_user"."role" IS 'role';
COMMENT ON COLUMN "public"."dashboard_user"."enabled" IS 'delete or not (0 close, 1 open) ';
COMMENT ON COLUMN "public"."dashboard_user"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."dashboard_user"."date_updated" IS 'update time';

-- ----------------------------
-- Records of dashboard_user
-- ----------------------------
INSERT INTO "public"."dashboard_user" VALUES ('1', 'admin', 'ba3253876aed6bc22d4a6ff53d8406c6ad864195ed144ab5c87621b6c233b548baeae6956df346ec8c17f5ea10f35ee3cbc514797ed7ddd3145464e2a0bab413', 1, 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');

-- ----------------------------
-- Table structure for data_permission
-- ----------------------------
DROP TABLE IF EXISTS "public"."data_permission";
CREATE TABLE "public"."data_permission" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "user_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "data_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "data_type" int4 NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."data_permission"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."data_permission"."user_id" IS 'user primary key id';
COMMENT ON COLUMN "public"."data_permission"."data_id" IS 'data(selector,rule) primary key id';
COMMENT ON COLUMN "public"."data_permission"."data_type" IS '0 selector type , 1 rule type';
COMMENT ON COLUMN "public"."data_permission"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."data_permission"."date_updated" IS 'update time';
COMMENT ON TABLE "public"."data_permission" IS 'data permission table';

-- ----------------------------
-- Records of data_permission
-- ----------------------------

-- ----------------------------
-- Table structure for meta_data
-- ----------------------------
DROP TABLE IF EXISTS "public"."meta_data";
CREATE TABLE "public"."meta_data" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "app_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "path" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "path_desc" varchar(255) COLLATE "pg_catalog"."default",
  "rpc_type" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "service_name" varchar(255) COLLATE "pg_catalog"."default",
  "method_name" varchar(255) COLLATE "pg_catalog"."default",
  "parameter_types" varchar(255) COLLATE "pg_catalog"."default",
  "rpc_ext" varchar(512) COLLATE "pg_catalog"."default",
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "enabled" int2 NOT NULL
)
;
COMMENT ON COLUMN "public"."meta_data"."id" IS 'id';
COMMENT ON COLUMN "public"."meta_data"."app_name" IS 'application name';
COMMENT ON COLUMN "public"."meta_data"."path" IS 'path, cannot be repeated';
COMMENT ON COLUMN "public"."meta_data"."path_desc" IS 'path description';
COMMENT ON COLUMN "public"."meta_data"."rpc_type" IS 'rpc type';
COMMENT ON COLUMN "public"."meta_data"."service_name" IS 'service name';
COMMENT ON COLUMN "public"."meta_data"."method_name" IS 'method name';
COMMENT ON COLUMN "public"."meta_data"."parameter_types" IS 'parameter types are provided with multiple parameter types separated by commas';
COMMENT ON COLUMN "public"."meta_data"."rpc_ext" IS 'rpc extended information, json format';
COMMENT ON COLUMN "public"."meta_data"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."meta_data"."date_updated" IS 'update time';
COMMENT ON COLUMN "public"."meta_data"."enabled" IS 'enabled state (0 close, 1 open) ';

-- ----------------------------
-- Records of meta_data
-- ----------------------------

-- ----------------------------
-- Table structure for operation_record_log
-- ----------------------------
DROP TABLE IF EXISTS "public"."operation_record_log";
CREATE TABLE "public"."operation_record_log" (
  "id" int8 NOT NULL DEFAULT nextval('operation_record_log_id_seq'::regclass),
  "color" varchar(20) COLLATE "pg_catalog"."default" NOT NULL,
  "context" text COLLATE "pg_catalog"."default" NOT NULL,
  "operator" varchar(200) COLLATE "pg_catalog"."default" NOT NULL,
  "operation_time" timestamp(6) NOT NULL,
  "operation_type" varchar(60) COLLATE "pg_catalog"."default" NOT NULL
)
;
COMMENT ON COLUMN "public"."operation_record_log"."id" IS 'id';
COMMENT ON COLUMN "public"."operation_record_log"."color" IS 'log color';
COMMENT ON COLUMN "public"."operation_record_log"."context" IS 'log context';
COMMENT ON COLUMN "public"."operation_record_log"."operator" IS 'operator [user or app]]';
COMMENT ON COLUMN "public"."operation_record_log"."operation_time" IS 'operation time';
COMMENT ON COLUMN "public"."operation_record_log"."operation_type" IS 'operation type：create/update/delete/register...';
COMMENT ON TABLE "public"."operation_record_log" IS 'operation record log';

-- ----------------------------
-- Records of operation_record_log
-- ----------------------------

-- ----------------------------
-- Table structure for permission
-- ----------------------------
DROP TABLE IF EXISTS "public"."permission";
CREATE TABLE "public"."permission" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "object_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "resource_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."permission"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."permission"."object_id" IS 'user primary key id or role primary key id';
COMMENT ON COLUMN "public"."permission"."resource_id" IS 'resource primary key id';
COMMENT ON COLUMN "public"."permission"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."permission"."date_updated" IS 'update time';
COMMENT ON TABLE "public"."permission" IS 'permission table';

-- ----------------------------
-- Records of permission
-- ----------------------------
INSERT INTO "public"."permission" VALUES ('1351007708572688384', '1346358560427216896', '1346775491550474240', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1357956838021890049', '1346358560427216896', '1357956838021890048', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708597854208', '1346358560427216896', '1346777449787125760', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708702711808', '1346358560427216896', '1347034027070337024', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708706906112', '1346358560427216896', '1347039054925148160', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708711100416', '1346358560427216896', '1347041326749691904', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708715294720', '1346358560427216896', '1347046566244003840', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708719489024', '1346358560427216896', '1347047143350874112', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708723683328', '1346358560427216896', '1347047203220369408', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708606242816', '1346358560427216896', '1346777623011880960', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708727877632', '1346358560427216896', '1347047555588042752', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708732071936', '1346358560427216896', '1347047640145211392', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708732071937', '1346358560427216896', '1347047695002513408', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708736266240', '1346358560427216896', '1347047747305484288', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708610437120', '1346358560427216896', '1346777766301888512', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708740460544', '1346358560427216896', '1347048004105940992', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708614631424', '1346358560427216896', '1346777907096285184', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708744654848', '1346358560427216896', '1347048101875167232', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708744654849', '1346358560427216896', '1347048145877610496', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708748849152', '1346358560427216896', '1347048240677269504', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708753043456', '1346358560427216896', '1347048316216684544', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708757237760', '1346358560427216896', '1347048776029843456', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708757237761', '1346358560427216896', '1347048968414179328', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007709088587777', '1346358560427216896', '1350804501819195392', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708623020032', '1346358560427216896', '1346778036402483200', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708761432064', '1346358560427216896', '1347049029323862016', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708765626368', '1346358560427216896', '1347049092552994816', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708769820672', '1346358560427216896', '1347049251395481600', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708774014976', '1346358560427216896', '1347049317178945536', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708774014977', '1346358560427216896', '1347049370014593024', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708778209280', '1346358560427216896', '1347049542417264640', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708782403584', '1346358560427216896', '1347049598155370496', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708786597888', '1346358560427216896', '1347049659023110144', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708790792192', '1346358560427216896', '1347049731047698432', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708585271296', '1346358560427216896', '1346776175553376256', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708593659904', '1346358560427216896', '1346777157943259136', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708685934593', '1346358560427216896', '1347032308726902784', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708690128896', '1346358560427216896', '1347032395901317120', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708694323200', '1346358560427216896', '1347032453707214848', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708698517504', '1346358560427216896', '1347032509051056128', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007708794986496', '1346358560427216896', '1347049794008395776', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007709080199168', '1346358560427216896', '1350106119681622016', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007709080199169', '1346358560427216896', '1350107709494804480', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007709084393472', '1346358560427216896', '1350107842236137472', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007709084393473', '1346358560427216896', '1350112406754766848', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1351007709088587776', '1346358560427216896', '1350112481253994496', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1355167519859040256', '1346358560427216896', '1355163372527050752', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1355167519859040257', '1346358560427216896', '1355165158419750912', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1355167519859040258', '1346358560427216896', '1355165353534578688', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1355167519859040259', '1346358560427216896', '1355165475785957376', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1355167519859040260', '1346358560427216896', '1355165608565039104', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1357977745893326848', '1346358560427216896', '1357977745889132544', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1357977912126177281', '1346358560427216896', '1357977912126177280', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1357977971827900417', '1346358560427216896', '1357977971827900416', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1386680049203195905', '1346358560427216896', '1386680049203195904', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."permission" VALUES ('1529403932797964288', '1346358560427216896', '1529403932772798464', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964289', '1346358560427216896', '1529403932781187072', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964290', '1346358560427216896', '1529403932781187073', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964291', '1346358560427216896', '1529403932781187074', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964292', '1346358560427216896', '1529403932781187075', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964293', '1346358560427216896', '1529403932781187076', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964294', '1346358560427216896', '1529403932781187077', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964295', '1346358560427216896', '1529403932781187078', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964296', '1346358560427216896', '1529403932781187079', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964297', '1346358560427216896', '1529403932781187080', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964298', '1346358560427216896', '1529403932781187081', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964299', '1346358560427216896', '1529403932781187082', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964300', '1346358560427216896', '1529403932781187083', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964301', '1346358560427216896', '1529403932781187084', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964302', '1346358560427216896', '1529403932781187085', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964303', '1346358560427216896', '1529403932781187086', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964304', '1346358560427216896', '1529403932781187087', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964305', '1346358560427216896', '1529403932781187088', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964306', '1346358560427216896', '1529403932781187089', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964307', '1346358560427216896', '1529403932781187090', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964308', '1346358560427216896', '1529403932781187091', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964309', '1346358560427216896', '1529403932781187092', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964310', '1346358560427216896', '1529403932781187093', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964311', '1346358560427216896', '1529403932781187094', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964312', '1346358560427216896', '1529403932781187095', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964313', '1346358560427216896', '1529403932781187096', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964314', '1346358560427216896', '1529403932781187097', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964315', '1346358560427216896', '1529403932781187098', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932797964316', '1346358560427216896', '1529403932781187099', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850512', '1346358560427216896', '1529403932877656064', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850513', '1346358560427216896', '1529403932877656065', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850514', '1346358560427216896', '1529403932877656066', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850515', '1346358560427216896', '1529403932877656067', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850516', '1346358560427216896', '1529403932877656068', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850517', '1346358560427216896', '1529403932877656069', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850518', '1346358560427216896', '1529403932877656070', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850519', '1346358560427216896', '1529403932877656071', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850520', '1346358560427216896', '1529403932877656072', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850521', '1346358560427216896', '1529403932877656073', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850522', '1346358560427216896', '1529403932877656074', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850523', '1346358560427216896', '1529403932877656075', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850524', '1346358560427216896', '1529403932877656076', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850525', '1346358560427216896', '1529403932877656077', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850526', '1346358560427216896', '1529403932877656078', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850527', '1346358560427216896', '1529403932877656079', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850528', '1346358560427216896', '1529403932877656080', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850529', '1346358560427216896', '1529403932877656081', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850530', '1346358560427216896', '1529403932877656082', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850531', '1346358560427216896', '1529403932877656083', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850532', '1346358560427216896', '1529403932877656084', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850533', '1346358560427216896', '1529403932877656085', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850534', '1346358560427216896', '1529403932877656086', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850535', '1346358560427216896', '1529403932877656087', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850536', '1346358560427216896', '1529403932877656088', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850537', '1346358560427216896', '1529403932877656089', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850538', '1346358560427216896', '1529403932877656090', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850539', '1346358560427216896', '1529403932877656091', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850540', '1346358560427216896', '1529403932877656092', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850541', '1346358560427216896', '1529403932877656093', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850542', '1346358560427216896', '1529403932877656094', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850543', '1346358560427216896', '1529403932877656095', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850544', '1346358560427216896', '1529403932877656096', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850545', '1346358560427216896', '1529403932877656097', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850546', '1346358560427216896', '1529403932877656098', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850547', '1346358560427216896', '1529403932877656099', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850548', '1346358560427216896', '1529403932877656100', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850549', '1346358560427216896', '1529403932877656101', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850550', '1346358560427216896', '1529403932877656102', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850551', '1346358560427216896', '1529403932877656103', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850552', '1346358560427216896', '1529403932877656104', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850553', '1346358560427216896', '1529403932877656105', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850554', '1346358560427216896', '1529403932877656106', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850555', '1346358560427216896', '1529403932877656107', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850556', '1346358560427216896', '1529403932877656108', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850557', '1346358560427216896', '1529403932877656109', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850558', '1346358560427216896', '1529403932877656110', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850559', '1346358560427216896', '1529403932877656111', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850560', '1346358560427216896', '1529403932877656112', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850561', '1346358560427216896', '1529403932877656113', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850562', '1346358560427216896', '1529403932877656114', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850563', '1346358560427216896', '1529403932877656115', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850564', '1346358560427216896', '1529403932877656116', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850565', '1346358560427216896', '1529403932877656117', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850566', '1346358560427216896', '1529403932877656118', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850567', '1346358560427216896', '1529403932877656119', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850568', '1346358560427216896', '1529403932877656120', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850569', '1346358560427216896', '1529403932877656121', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850570', '1346358560427216896', '1529403932877656122', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850571', '1346358560427216896', '1529403932877656123', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850572', '1346358560427216896', '1529403932877656124', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850573', '1346358560427216896', '1529403932877656125', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850574', '1346358560427216896', '1529403932877656126', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850575', '1346358560427216896', '1529403932877656127', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850576', '1346358560427216896', '1529403932877656128', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850577', '1346358560427216896', '1529403932877656129', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850578', '1346358560427216896', '1529403932877656130', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850579', '1346358560427216896', '1529403932877656131', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850580', '1346358560427216896', '1529403932877656132', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850581', '1346358560427216896', '1529403932877656133', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850582', '1346358560427216896', '1529403932877656134', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850583', '1346358560427216896', '1529403932877656135', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850584', '1346358560427216896', '1529403932877656136', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850585', '1346358560427216896', '1529403932877656137', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850586', '1346358560427216896', '1529403932877656138', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850587', '1346358560427216896', '1529403932877656139', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850588', '1346358560427216896', '1529403932877656140', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850589', '1346358560427216896', '1529403932877656141', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850590', '1346358560427216896', '1529403932877656142', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850591', '1346358560427216896', '1529403932877656143', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850592', '1346358560427216896', '1529403932877656144', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850593', '1346358560427216896', '1529403932877656145', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850594', '1346358560427216896', '1529403932877656146', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850595', '1346358560427216896', '1529403932877656147', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850596', '1346358560427216896', '1529403932877656148', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850597', '1346358560427216896', '1529403932877656149', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850598', '1346358560427216896', '1529403932877656150', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850599', '1346358560427216896', '1529403932877656151', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850600', '1346358560427216896', '1529403932877656152', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850601', '1346358560427216896', '1529403932877656153', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850602', '1346358560427216896', '1529403932877656154', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850603', '1346358560427216896', '1529403932877656155', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850604', '1346358560427216896', '1529403932877656156', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850605', '1346358560427216896', '1529403932877656157', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850606', '1346358560427216896', '1529403932877656158', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850607', '1346358560427216896', '1529403932877656159', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850608', '1346358560427216896', '1529403932877656160', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850609', '1346358560427216896', '1529403932877656161', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850610', '1346358560427216896', '1529403932877656162', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850611', '1346358560427216896', '1529403932877656163', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850612', '1346358560427216896', '1529403932877656164', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850613', '1346358560427216896', '1529403932877656165', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850614', '1346358560427216896', '1529403932877656166', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850615', '1346358560427216896', '1529403932877656167', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850616', '1346358560427216896', '1529403932877656168', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850617', '1346358560427216896', '1529403932877656169', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850618', '1346358560427216896', '1529403932877656170', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850619', '1346358560427216896', '1529403932877656171', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850620', '1346358560427216896', '1529403932877656172', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850621', '1346358560427216896', '1529403932877656173', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850622', '1346358560427216896', '1529403932877656174', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850623', '1346358560427216896', '1529403932877656175', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850624', '1346358560427216896', '1529403932877656176', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850625', '1346358560427216896', '1529403932877656177', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850626', '1346358560427216896', '1529403932877656178', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850627', '1346358560427216896', '1529403932877656179', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850628', '1346358560427216896', '1529403932877656180', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850629', '1346358560427216896', '1529403932881850368', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850630', '1346358560427216896', '1529403932881850369', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850631', '1346358560427216896', '1529403932881850370', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850632', '1346358560427216896', '1529403932881850371', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850633', '1346358560427216896', '1529403932881850372', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850634', '1346358560427216896', '1529403932881850373', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850635', '1346358560427216896', '1529403932881850374', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850636', '1346358560427216896', '1529403932881850375', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850637', '1346358560427216896', '1529403932881850376', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850638', '1346358560427216896', '1529403932881850377', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850639', '1346358560427216896', '1529403932881850378', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850640', '1346358560427216896', '1529403932881850379', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850641', '1346358560427216896', '1529403932881850380', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850642', '1346358560427216896', '1529403932881850381', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850643', '1346358560427216896', '1529403932881850382', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850644', '1346358560427216896', '1529403932881850383', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850645', '1346358560427216896', '1529403932881850384', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850646', '1346358560427216896', '1529403932881850385', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850647', '1346358560427216896', '1529403932881850386', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850648', '1346358560427216896', '1529403932881850387', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850649', '1346358560427216896', '1529403932881850388', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850650', '1346358560427216896', '1529403932881850389', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850651', '1346358560427216896', '1529403932881850390', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850652', '1346358560427216896', '1529403932881850391', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850653', '1346358560427216896', '1529403932881850392', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850654', '1346358560427216896', '1529403932881850393', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850655', '1346358560427216896', '1529403932881850394', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850656', '1346358560427216896', '1529403932881850395', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850657', '1346358560427216896', '1529403932881850396', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850658', '1346358560427216896', '1529403932881850397', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850659', '1346358560427216896', '1529403932881850398', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850660', '1346358560427216896', '1529403932881850399', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850661', '1346358560427216896', '1529403932881850400', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850662', '1346358560427216896', '1529403932881850401', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850663', '1346358560427216896', '1529403932881850402', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850664', '1346358560427216896', '1529403932881850403', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850665', '1346358560427216896', '1529403932881850404', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850666', '1346358560427216896', '1529403932881850405', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850667', '1346358560427216896', '1529403932881850406', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850668', '1346358560427216896', '1529403932881850407', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850669', '1346358560427216896', '1529403932881850408', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850670', '1346358560427216896', '1529403932881850409', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850671', '1346358560427216896', '1529403932881850410', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850672', '1346358560427216896', '1529403932881850411', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850673', '1346358560427216896', '1529403932881850412', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850674', '1346358560427216896', '1529403932881850413', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850675', '1346358560427216896', '1529403932881850414', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850676', '1346358560427216896', '1529403932881850415', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932881850677', '1346358560427216896', '1529403932881850416', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044672', '1346358560427216896', '1529403932881850417', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044673', '1346358560427216896', '1529403932881850418', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044674', '1346358560427216896', '1529403932881850419', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044675', '1346358560427216896', '1529403932881850420', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044676', '1346358560427216896', '1529403932881850421', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044677', '1346358560427216896', '1529403932881850422', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044678', '1346358560427216896', '1529403932881850423', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044679', '1346358560427216896', '1529403932881850424', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044680', '1346358560427216896', '1529403932881850425', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044681', '1346358560427216896', '1529403932881850426', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044682', '1346358560427216896', '1529403932881850427', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044683', '1346358560427216896', '1529403932881850428', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044684', '1346358560427216896', '1529403932881850429', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044685', '1346358560427216896', '1529403932881850430', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044686', '1346358560427216896', '1529403932881850431', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044687', '1346358560427216896', '1529403932881850432', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044688', '1346358560427216896', '1529403932881850433', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044689', '1346358560427216896', '1529403932881850434', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044690', '1346358560427216896', '1529403932881850435', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044691', '1346358560427216896', '1529403932881850436', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044692', '1346358560427216896', '1529403932881850437', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044693', '1346358560427216896', '1529403932881850438', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044694', '1346358560427216896', '1529403932881850439', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044695', '1346358560427216896', '1529403932881850440', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044696', '1346358560427216896', '1529403932881850441', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044697', '1346358560427216896', '1529403932881850442', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044698', '1346358560427216896', '1529403932881850443', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044699', '1346358560427216896', '1529403932881850444', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044700', '1346358560427216896', '1529403932881850445', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044701', '1346358560427216896', '1529403932881850446', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044702', '1346358560427216896', '1529403932881850447', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044703', '1346358560427216896', '1529403932881850448', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044704', '1346358560427216896', '1529403932881850449', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044705', '1346358560427216896', '1529403932881850450', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044706', '1346358560427216896', '1529403932881850451', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044707', '1346358560427216896', '1529403932881850452', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044708', '1346358560427216896', '1529403932881850453', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044709', '1346358560427216896', '1529403932881850454', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044710', '1346358560427216896', '1529403932881850455', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044711', '1346358560427216896', '1529403932881850456', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044712', '1346358560427216896', '1529403932881850457', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044713', '1346358560427216896', '1529403932881850458', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044714', '1346358560427216896', '1529403932881850459', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044715', '1346358560427216896', '1529403932881850460', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044716', '1346358560427216896', '1529403932881850461', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044717', '1346358560427216896', '1529403932881850462', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044718', '1346358560427216896', '1529403932881850463', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044719', '1346358560427216896', '1529403932881850464', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044720', '1346358560427216896', '1529403932881850465', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044721', '1346358560427216896', '1529403932881850466', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044722', '1346358560427216896', '1529403932881850467', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044723', '1346358560427216896', '1529403932881850468', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044724', '1346358560427216896', '1529403932881850469', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044725', '1346358560427216896', '1529403932881850470', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044726', '1346358560427216896', '1529403932881850471', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044727', '1346358560427216896', '1529403932881850472', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044728', '1346358560427216896', '1529403932881850473', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044729', '1346358560427216896', '1529403932881850474', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044730', '1346358560427216896', '1529403932881850475', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044731', '1346358560427216896', '1529403932881850476', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044732', '1346358560427216896', '1529403932881850477', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044733', '1346358560427216896', '1529403932881850478', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044734', '1346358560427216896', '1529403932881850479', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044735', '1346358560427216896', '1529403932881850480', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044736', '1346358560427216896', '1529403932881850481', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044737', '1346358560427216896', '1529403932881850482', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044738', '1346358560427216896', '1529403932881850483', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044739', '1346358560427216896', '1529403932881850484', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044740', '1346358560427216896', '1529403932881850485', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044741', '1346358560427216896', '1529403932881850486', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044742', '1346358560427216896', '1529403932881850487', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044743', '1346358560427216896', '1529403932881850488', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044744', '1346358560427216896', '1529403932881850489', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044745', '1346358560427216896', '1529403932881850490', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044746', '1346358560427216896', '1529403932881850491', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044747', '1346358560427216896', '1529403932881850492', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044748', '1346358560427216896', '1529403932881850493', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044749', '1346358560427216896', '1529403932881850494', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044750', '1346358560427216896', '1529403932881850495', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044751', '1346358560427216896', '1529403932881850496', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044752', '1346358560427216896', '1529403932881850497', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044753', '1346358560427216896', '1529403932881850498', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044754', '1346358560427216896', '1529403932881850499', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044755', '1346358560427216896', '1529403932881850500', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044756', '1346358560427216896', '1529403932881850501', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044757', '1346358560427216896', '1529403932881850502', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044758', '1346358560427216896', '1529403932881850503', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044759', '1346358560427216896', '1529403932881850504', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044760', '1346358560427216896', '1529403932881850505', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044761', '1346358560427216896', '1529403932881850506', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044762', '1346358560427216896', '1529403932881850507', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044763', '1346358560427216896', '1529403932881850508', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044764', '1346358560427216896', '1529403932881850509', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044765', '1346358560427216896', '1529403932881850510', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044766', '1346358560427216896', '1529403932881850511', '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."permission" VALUES ('1529403932886044767', '1346358560427216896', '1534577121923309568', '2022-06-09 00:44:32', '2022-06-09 00:44:31');
INSERT INTO "public"."permission" VALUES ('1529403932886044768', '1346358560427216896', '1534585430311051264', '2022-06-09 01:17:33', '2022-06-09 01:17:32');
INSERT INTO "public"."permission" VALUES ('1529403932886044769', '1346358560427216896', '1534585531108564992', '2022-06-09 01:17:57', '2022-06-09 01:17:56');

-- ----------------------------
-- Table structure for plugin
-- ----------------------------
DROP TABLE IF EXISTS "public"."plugin";
CREATE TABLE "public"."plugin" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "name" varchar(62) COLLATE "pg_catalog"."default" NOT NULL,
  "config" text COLLATE "pg_catalog"."default",
  "role" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "sort" int4,
  "enabled" int2 NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."plugin"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."plugin"."name" IS 'plugin name';
COMMENT ON COLUMN "public"."plugin"."config" IS 'plugin configuration';
COMMENT ON COLUMN "public"."plugin"."role" IS 'plug-in role';
COMMENT ON COLUMN "public"."plugin"."sort" IS 'sort';
COMMENT ON COLUMN "public"."plugin"."enabled" IS 'whether to open (0, not open, 1 open)';
COMMENT ON COLUMN "public"."plugin"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."plugin"."date_updated" IS 'update time';

-- ----------------------------
-- Records of plugin
-- ----------------------------
INSERT INTO "public"."plugin" VALUES ('1', 'sign', NULL, 'Authentication', 20, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('10', 'sentinel', NULL, 'FaultTolerance', 140, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('11', 'sofa', '{"protocol":"zookeeper","register":"127.0.0.1:2181","threadpool":"shared"}', 'Proxy', 310, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('12', 'resilience4j', NULL, 'FaultTolerance', 310, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('13', 'tars', '{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"shared"}', 'Proxy', 310, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('14', 'contextPath', NULL, 'HttpProcess', 80, 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('15', 'grpc', '{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"shared"}', 'Proxy', 310, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('16', 'redirect', NULL, 'HttpProcess', 110, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('17', 'motan', '{"register":"127.0.0.1:2181","corethreads":0,"threads":2147483647,"queues":0,"threadpool":"shared"}', 'Proxy', 310, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('18', 'loggingConsole', NULL, 'Logging', 160, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('19', 'jwt', '{"secretKey":"key"}', 'Authentication', 30, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('2', 'waf', '{"model":"black"}', 'Authentication', 50, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('20', 'request', NULL, 'HttpProcess', 120, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('21', 'oauth2', NULL, 'Authentication', 40, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('22', 'paramMapping', '{"ruleHandlePageType":"custom"}', 'HttpProcess', 70, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('23', 'modifyResponse', '{"ruleHandlePageType":"custom"}', 'HttpProcess', 220, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('24', 'cryptorRequest', NULL, 'Cryptor', 100, 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('25', 'cryptorResponse', NULL, 'Cryptor', 410, 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('26', 'websocket', '{"multiSelectorHandle":"1"}', 'Proxy', 200, 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('3', 'rewrite', NULL, 'HttpProcess', 90, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('4', 'rateLimiter', '{"master":"mymaster","mode":"standalone","url":"192.168.1.1:6379","password":"abc"}', 'FaultTolerance', 60, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('5', 'divide', '{"multiSelectorHandle":"1","multiRuleHandle":"0"}', 'Proxy', 200, 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('6', 'dubbo', '{"register":"zookeeper://localhost:2181","multiSelectorHandle":"1","threadpool":"shared","corethreads":0,"threads":2147483647,"queues":0}', 'Proxy', 310, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('8', 'springCloud', NULL, 'Proxy', 200, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('9', 'hystrix', NULL, 'FaultTolerance', 130, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('27', 'generalContext', NULL, 'Common', 125, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('28', 'mqtt', '{"port": 9500,"bossGroupThreadCount": 1,"maxPayloadSize": 65536,"workerGroupThreadCount": 12,"userName": "shenyu","password": "shenyu","isEncryptPassword": false,"encryptMode": "","leakDetectorLevel": "DISABLED"}', 'Proxy', 125, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('29', 'loggingRocketMQ', '{"topic":"shenyu-access-logging", "namesrvAddr": "localhost:9876","producerGroup":"shenyu-plugin-logging-rocketmq"}', 'Logging', 170, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('30', 'cache', '{"cacheType":"memory"}', 'Cache', 10, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('31', 'mock', null, 'Mock', 1, 0, '2022-06-16 14:40:35', '2022-06-16 14:40:55');
INSERT INTO "public"."plugin" VALUES ('32', 'loggingElasticSearch', '{"host":"localhost", "port": "9200"}', 'Logging', 190, 0, '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin" VALUES ('33', 'loggingKafka', '{"topic":"shenyu-access-logging", "namesrvAddr": "localhost:9092"}', 'Logging', 180, 0, '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO "public"."plugin" VALUES ('34', 'loggingAliyunSls', '{"projectName": "shenyu", "logStoreName": "shenyu-logstore", "topic": "shenyu-topic"}', 'Logging', 175, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin" VALUES ('35', 'loggingPulsar', '{"topic":"shenyu-access-logging", "serviceUrl": "pulsar://localhost:6650"}', 'Logging', 185, 0, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin" VALUES ('36', 'loggingTencentCls', '{"endpoint": "ap-guangzhou.cls.tencentcs.com", "topic": "shenyu-topic"}', 'Logging', 176, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');

-- ----------------------------
-- Table structure for plugin_handle
-- ----------------------------
DROP TABLE IF EXISTS "public"."plugin_handle";
CREATE TABLE "public"."plugin_handle" (
  "id" varchar(128) NOT NULL DEFAULT nextval('plugin_handle_id_seq'::regclass),
  "plugin_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "field" varchar(100) COLLATE "pg_catalog"."default" NOT NULL,
  "label" varchar(100) COLLATE "pg_catalog"."default",
  "data_type" int2 NOT NULL,
  "type" int2,
  "sort" int4,
  "ext_obj" varchar(1024) COLLATE "pg_catalog"."default",
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."plugin_handle"."plugin_id" IS 'plugin id';
COMMENT ON COLUMN "public"."plugin_handle"."field" IS 'field';
COMMENT ON COLUMN "public"."plugin_handle"."label" IS 'label';
COMMENT ON COLUMN "public"."plugin_handle"."data_type" IS 'data type 1 number 2 string';
COMMENT ON COLUMN "public"."plugin_handle"."type" IS 'type, 1 means selector, 2 means rule, 3 means plugin';
COMMENT ON COLUMN "public"."plugin_handle"."sort" IS 'sort';
COMMENT ON COLUMN "public"."plugin_handle"."ext_obj" IS 'extra configuration (json format data)';
COMMENT ON COLUMN "public"."plugin_handle"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."plugin_handle"."date_updated" IS 'update time';

-- ----------------------------
-- Records of plugin_handle
-- ----------------------------
INSERT INTO "public"."plugin_handle" VALUES ('1529403902770941952', '10', 'flowRuleGrade', 'flowRuleGrade', 3, 2, 8, '{"required":"1","defaultValue":"1","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902770941953', '10', 'flowRuleControlBehavior', 'flowRuleControlBehavior', 3, 2, 5, '{"required":"1","defaultValue":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902770941954', '10', 'flowRuleEnable', 'flowRuleEnable 1 or 0)', 1, 2, 7, '{"required":"1","defaultValue":"1","rule":"/^[01]$/"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902770941955', '10', 'flowRuleCount', 'flowRuleCount', 1, 2, 6, '{"required":"1","defaultValue":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902770941956', '10', 'degradeRuleEnable', 'degradeRuleEnable 1 or 0)', 1, 2, 2, '{"required":"1","defaultValue":"1","rule":"/^[01]$/"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902770941957', '10', 'degradeRuleGrade', 'degradeRuleGrade', 3, 2, 3, '{"required":"1","defaultValue":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136256', '10', 'degradeRuleCount', 'degradeRuleCount', 1, 2, 1, '{"required":"1","defaultValue":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136257', '10', 'degradeRuleTimeWindow', 'degradeRuleTimeWindow', 1, 2, 4, '{"required":"1","defaultValue":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136258', '10', 'degradeRuleMinRequestAmount', 'degradeRuleMinRequestAmount', 1, 2, 3, '{"required":"1","defaultValue":"5","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136259', '10', 'degradeRuleStatIntervals', 'degradeRuleStatIntervals', 1, 2, 3, '{"required":"1","defaultValue":"1","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136260', '10', 'degradeRuleSlowRatioThreshold', 'degradeRuleSlowRatioThreshold', 1, 2, 3, '{"required":"1","defaultValue":"0.5","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136261', '10', 'fallbackUri', 'fallbackUri', 2, 2, 9, '{"required":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136262', '2', 'permission', 'permission', 3, 2, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136263', '2', 'statusCode', 'statusCode', 2, 2, 2, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136264', '4', 'replenishRate', 'replenishRate', 2, 2, 2, '{"required":"1","defaultValue":"10","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136265', '4', 'burstCapacity', 'burstCapacity', 2, 2, 3, '{"required":"1","defaultValue":"100","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136266', '3', 'regex', 'regex', 2, 2, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136267', '3', 'replace', 'replace', 2, 2, 2, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136268', '16', 'redirectURI', 'redirectURI', 2, 2, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136269', '8', 'path', 'path', 2, 2, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136270', '8', 'timeout', 'timeout ms)', 1, 2, 2, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136271', '8', 'serviceId', 'serviceId', 2, 1, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136272', '12', 'timeoutDurationRate', 'timeoutDurationRate ms)', 1, 2, 1, '{"required":"1","defaultValue":"5000","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136273', '12', 'limitRefreshPeriod', 'limitRefreshPeriod ms)', 1, 2, 0, '{"required":"1","defaultValue":"500","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136274', '12', 'limitForPeriod', 'limitForPeriod', 1, 2, 0, '{"required":"1","defaultValue":"50","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136275', '12', 'circuitEnable', 'circuitEnable', 1, 2, 2, '{"required":"1","defaultValue":"0","rule":"/^[01]$/"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136276', '12', 'timeoutDuration', 'timeoutDuration ms)', 1, 2, 2, '{"required":"1","defaultValue":"30000","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136277', '12', 'fallbackUri', 'fallbackUri', 2, 2, 2, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136278', '12', 'slidingWindowSize', 'slidingWindowSize', 1, 2, 2, '{"required":"1","defaultValue":"100","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136279', '12', 'slidingWindowType', 'slidingWindowType', 1, 2, 2, '{"required":"1","defaultValue":"0","rule":"/^[01]$/"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136280', '12', 'minimumNumberOfCalls', 'minimumNumberOfCalls', 1, 2, 2, '{"required":"1","defaultValue":"100","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136281', '12', 'waitIntervalFunctionInOpenState', 'waitIntervalInOpen', 1, 2, 2, '{"required":"1","defaultValue":"60000","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136282', '12', 'permittedNumberOfCallsInHalfOpenState', 'bufferSizeInHalfOpen', 1, 2, 2, '{"required":"1","defaultValue":"10","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136283', '12', 'failureRateThreshold', 'failureRateThreshold', 1, 2, 2, '{"required":"1","defaultValue":"50","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136284', '12', 'automaticTransitionFromOpenToHalfOpenEnabled', 'automaticHalfOpen', 3, 2, 1, '{"required":"1","defaultValue":"true","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136285', '4', 'mode', 'mode', 3, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136286', '4', 'master', 'master', 2, 3, 2, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136287', '4', 'url', 'url', 2, 3, 3, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136288', '4', 'password', 'password', 2, 3, 4, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136289', '11', 'protocol', 'protocol', 2, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136290', '11', 'register', 'register', 2, 3, 2, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136291', '2', 'model', 'model', 2, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136292', '6', 'register', 'register', 2, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136293', '4', 'algorithmName', 'algorithmName', 3, 2, 1, '{"required":"1","defaultValue":"slidingWindow","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136294', '4', 'keyResolverName', 'keyResolverName', 3, 2, 4, '{"required":"1","defaultValue":"WHOLE_KEY_RESOLVER","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136295', '5', 'upstreamHost', 'host', 2, 1, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136296', '5', 'protocol', 'protocol', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"http://","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136297', '5', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136298', '5', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136299', '5', 'timestamp', 'startupTime', 1, 1, 3, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136300', '5', 'warmup', 'warmupTime', 1, 1, 5, '{"defaultValue":"0","placeholder":"warmup time ms)","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902775136301', '5', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330560', '5', 'loadBalance', 'loadStrategy', 3, 2, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330561', '5', 'retry', 'retryCount', 1, 2, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330562', '5', 'timeout', 'timeout', 1, 2, 2, '{"defaultValue":"3000","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330563', '5', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330564', '5', 'multiRuleHandle', 'multiRuleHandle', 3, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330565', '5', 'headerMaxSize', 'headerMaxSize', 1, 2, 3, '{"defaultValue":"10240","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330566', '5', 'requestMaxSize', 'requestMaxSize', 1, 2, 4, '{"defaultValue":"102400","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330567', '5', 'retryStrategy', 'retryStrategy', 3, 2, 0, '{"required":"0","defaultValue":"current","placeholder":"retryStrategy","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330568', '13', 'upstreamHost', 'host', 2, 1, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330569', '13', 'protocol', 'protocol', 2, 1, 2, '{"defaultValue":"","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330570', '13', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330571', '13', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330572', '13', 'timestamp', 'startupTime', 1, 1, 3, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330573', '13', 'warmup', 'warmupTime', 1, 1, 5, '{"defaultValue":"0","placeholder":"warmup time ms)","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330574', '13', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330575', '13', 'loadBalance', 'loadStrategy', 3, 2, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330576', '13', 'retry', 'retryCount', 1, 2, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330577', '13', 'timeout', 'timeout', 1, 2, 2, '{"defaultValue":"3000","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330578', '13', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330579', '13', 'multiRuleHandle', 'multiRuleHandle', 3, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330580', '15', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330581', '15', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330582', '15', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330583', '15', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330584', '15', 'multiRuleHandle', 'multiRuleHandle', 3, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330585', '15', 'threadpool', 'threadpool', 3, 3, 0, '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330586', '14', 'contextPath', 'contextPath', 2, 2, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330587', '14', 'addPrefix', 'addPrefix', 2, 2, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330588', '20', 'ruleHandlePageType', 'ruleHandlePageType', 3, 3, 0, '{"required":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330589', '19', 'secretKey', 'secretKey', 2, 3, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330590', '24', 'strategyName', 'strategyName', 3, 2, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330591', '24', 'fieldNames', 'fieldNames', 2, 2, 3, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330592', '24', 'decryptKey', 'decryptKey', 2, 2, 3, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330593', '24', 'encryptKey', 'encryptKey', 2, 2, 3, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330594', '24', 'way', 'way', 3, 2, 3, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330595', '25', 'strategyName', 'strategyName', 3, 2, 2, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330596', '25', 'decryptKey', 'decryptKey', 2, 2, 3, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330597', '25', 'encryptKey', 'encryptKey', 2, 2, 3, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330598', '25', 'fieldNames', 'fieldNames', 2, 2, 4, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330599', '25', 'way', 'way', 3, 2, 3, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330600', '6', 'gray', 'gray', 3, 1, 9, '{"required":"0","defaultValue":"false","placeholder":"gray","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330601', '6', 'group', 'group', 2, 1, 3, '{"required":"0","placeholder":"group","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330602', '6', 'loadbalance', 'loadbalance', 3, 2, 0, '{"required":"0","placeholder":"loadbalance","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330603', '6', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330604', '6', 'protocol', 'protocol', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"http://","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330605', '6', 'status', 'status', 3, 1, 8, '{"defaultValue":"true","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330606', '6', 'timestamp', 'startupTime', 1, 1, 7, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330607', '6', 'upstreamHost', 'host', 2, 1, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330608', '6', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330609', '6', 'version', 'version', 2, 1, 4, '{"required":"0","placeholder":"version","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330610', '6', 'warmup', 'warmupTime', 1, 1, 6, '{"defaultValue":"0","placeholder":"warmup time ms)","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902779330611', '6', 'weight', 'weight', 1, 1, 5, '{"defaultValue":"50","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524864', '6', 'threadpool', 'threadpool', 3, 3, 0, '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524865', '6', 'corethreads', 'corethreads', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524866', '6', 'threads', 'threads', 1, 3, 0, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524867', '6', 'queues', 'queues', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524868', '26', 'host', 'host', 2, 1, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524869', '26', 'protocol', 'protocol', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"ws://","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524870', '26', 'url', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524871', '26', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524872', '26', 'timestamp', 'startupTime', 1, 1, 3, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524873', '26', 'warmup', 'warmupTime', 1, 1, 5, '{"defaultValue":"0","placeholder":"warmup time ms)","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524874', '26', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524875', '26', 'loadBalance', 'loadStrategy', 3, 2, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524876', '26', 'retry', 'retryCount', 1, 2, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524877', '26', 'timeout', 'timeout', 1, 2, 2, '{"defaultValue":"3000","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524878', '26', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524879', '17', 'register', 'register', 2, 3, 0, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524880', '17', 'corethreads', 'corethreads', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524881', '17', 'threads', 'threads', 1, 3, 0, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524882', '17', 'queues', 'queues', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524883', '17', 'threadpool', 'threadpool', 3, 3, 0, '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524884', '28', 'port', 'port', 1, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524885', '28', 'bossGroupThreadCount', 'bossGroupThreadCount', 1, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524886', '28', 'maxPayloadSize', 'maxPayloadSize', 1, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524887', '28', 'workerGroupThreadCount', 'workerGroupThreadCount', 1, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524888', '28', 'userName', 'userName', 2, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524889', '28', 'password', 'password', 2, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524890', '28', 'isEncryptPassword', 'isEncryptPassword', 2, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524891', '28', 'encryptMode', 'encryptMode', 2, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524892', '28', 'leakDetectorLevel', 'leakDetectorLevel', 2, 3, 1, NULL, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524893', '29', 'topic', 'topic', 2, 3, 1, '{"required":"1","defaultValue":"shenyu-access-logging"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524894', '29', 'namesrvAddr', 'namesrvAddr', 2, 3, 2, '{"required":"1","defaultValue":"localhost:9876"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524895', '29', 'producerGroup', 'producerGroup', 2, 3, 3, '{"required":"1","defaultValue":"shenyu-plugin-logging-rocketmq"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524896', '29', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524897', '29', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524898', '29', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524899', '29', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524900', '29', 'topic', 'topic', 2, 1, 1, '{"required":"0","defaultValue":"","placeholder":"optional"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524901', '29', 'sampleRate', 'sampleRate', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"optional,0,0.01~1"}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524902', '30', 'cacheType', 'cacheType', 3, 3, 1, '{"required":"1","defaultValue":"memory","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524903', '30', 'database', 'database', 1, 3, 2, '{"required":"0","defaultValue":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524904', '30', 'master', 'master', 2, 3, 3, '{"required":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524905', '30', 'mode', 'mode', 2, 3, 4, '{"required":"0","defaultValue":"standalone","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524906', '30', 'url', 'url', 2, 3, 5, '{"required":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524907', '30', 'password', 'password', 2, 3, 6, '{"required":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524908', '30', 'maxIdle', 'maxIdle', 1, 3, 7, '{"required":"0","defaultValue":"8","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524909', '30', 'minIdle', 'minIdle', 1, 3, 8, '{"required":"0","defaultValue":"0","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524910', '30', 'maxActive', 'maxActive', 1, 3, 9, '{"required":"0","defaultValue":"8","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524911', '30', 'maxWait', 'maxWait', 3, 3, 10, '{"required":"0","defaultValue":"-1","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524912', '30', 'timeoutSeconds', 'timeoutSeconds', 1, 2, 0, '{"required":"0","defaultValue":"60","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524913', '13', 'corethreads', 'corethreads', 1, 3, 3, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524914', '13', 'threads', 'threads', 1, 3, 4, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524915', '13', 'queues', 'queues', 1, 3, 5, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524916', '13', 'threadpool', 'threadpool', 3, 3, 2, '{"required":"0","defaultValue":"default","placeholder":"threadpool","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524917', '11', 'corethreads', 'corethreads', 1, 3, 4, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524918', '11', 'threads', 'threads', 1, 3, 5, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524919', '11', 'queues', 'queues', 1, 3, 6, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524920', '11', 'threadpool', 'threadpool', 3, 3, 3, '{"required":"0","defaultValue":"default","placeholder":"threadpool","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1537326008606343168', '31', 'responseContent', 'responseContent', 2, 2, 0, '{"required":"0","rule":""}', '2022-06-16 14:47:37', '2022-06-16 14:50:39');
INSERT INTO "public"."plugin_handle" VALUES ('1537325892176658432', '31', 'httpStatusCode', 'httpStatusCode', 1, 2, 0, '{"required":"0","defaultValue":"200","rule":""}', '2022-06-16 14:47:09', '2022-06-16 14:50:39');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524921', '32', 'host', 'host', 2, 3, 2, '{"required":"1","defaultValue":"localhost"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524922', '32', 'port', 'port', 2, 3, 3, '{"required":"1","defaultValue":"9200"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524923', '32', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524924', '32', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524925', '32', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524926', '32', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524927', '32', 'index', 'index', 2, 1, 1, '{"required":"0","defaultValue":"","placeholder":"optional"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524928', '32', 'sampleRate', 'sampleRate', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"optional,0,0.01~1"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524929', '1', 'signRequestBody', 'signRequestBody', 3, 2, 9, '{"required":"0","defaultValue":"false","placeholder":"signRequestBody","rule":""}', '2022-06-29 10:08:02', '2022-06-29 10:08:02');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524950', '33', 'topic', 'topic', 2, 3, 1, '{"required":"1","defaultValue":"shenyu-access-logging"}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524951', '33', 'namesrvAddr', 'namesrvAddr', 2, 3, 2, '{"required":"1","defaultValue":"localhost:9092"}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524952', '33', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524953', '33', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524954', '33', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524955', '33', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524956', '33', 'topic', 'topic', 2, 1, 1, '{"required":"0","defaultValue":"","placeholder":"optional"}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524957', '33', 'sampleRate', 'sampleRate', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"optional,0,0.01~1"}', '2022-07-04 22:00:00', '2022-07-04 22:00:00');

INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524958', '34', 'accessId', 'accessId', 2, 3, 0, '{"required":"1","defaultValue":"","placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524959', '34', 'accessKey', 'accessKey', 2, 3, 1, '{"required":"1","defaultValue":"","placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524960', '34', 'host', 'host', 2, 3, 2, '{"required":"1","defaultValue":"","placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524961', '34', 'projectName', 'projectName', 2, 3, 3, '{"required":"0","defaultValue":"shenyu","placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524962', '34', 'logStoreName', 'logStoreName', 2, 3, 4, '{"required":"0","defaultValue":"shenyu-logstore","placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524963', '34', 'topic', 'topic', 2, 3, 5, '{"required":"0","defaultValue":"shenyu-topic","placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524964', '34', 'ttlInDay', 'ttlInDay', 1, 3, 6, '{"required":"0","defaultValue":3,"placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524965', '34', 'shardCount', 'shardCount', 1, 3, 7, '{"required":"0","defaultValue":10,"placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524966', '34', 'sendThreadCount', 'sendThreadCount', 1, 3, 8, '{"required":"0","defaultValue":1,"placeholder":"1-500"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524967', '34', 'ioThreadCount', 'ioThreadCount', 1, 3, 9, '{"required":"0","defaultValue":1,"placeholder":"1-500"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524968', '34', 'sampleRate', 'sampleRate', 2, 3, 10, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524969', '34', 'maxRequestBody', 'maxRequestBody', 1, 3, 11, '{"required":"0","defaultValue":524288,"placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524970', '34', 'maxResponseBody', 'maxResponseBody', 1, 3, 12, '{"required":"0","defaultValue":524288,"placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524971', '34', 'bufferQueueSize', 'bufferQueueSize', 1, 3, 13, '{"required":"0","defaultValue":50000,"placeholder":""}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524972', '35', 'topic', 'topic', 2, 3, 1, '{"required":"1","defaultValue":"shenyu-access-logging"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524973', '35', 'serviceUrl', 'serviceUrl', 2, 3, 2, '{"required":"1","defaultValue":"pulsar://localhost:6650"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524974', '35', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524975', '35', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524976', '35', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524977', '35', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524978', '36', 'secretId', 'secretId', 2, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524979', '36', 'secretKey', 'secretKey', 2, 3, 2, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524980', '36', 'endpoint', 'endpoint', 2, 3, 3, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524981', '36', 'topic', 'topic', 2, 3, 4, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524982', '36', 'sendThreadCount', 'sendThreadCount', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":1,\"placeholder\":\"1-500\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524983', '36', 'totalSizeInBytes', 'totalSizeInBytes', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":104857600}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524984', '36', 'maxSendThreadCount', 'maxSendThreadCount', 1, 3, 7, '{\"required\":\"0\",\"defaultValue\":1,\"placeholder\":\"availableProcessors + 1\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524985', '36', 'maxBlockSec', 'maxBlockSec', 1, 3, 8, '{\"required\":\"0\",\"defaultValue\":60000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524986', '36', 'maxBatchSize', 'maxBatchSize', 1, 3, 9, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524987', '36', 'maxBatchCount', 'maxBatchCount', 1, 3, 10, '{\"required\":\"0\",\"defaultValue\":4096}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524988', '36', 'lingerMs', 'lingerMs', 1, 3, 11, '{\"required\":\"0\",\"defaultValue\":2000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524989', '36', 'retries', 'retries', 1, 3, 12, '{\"required\":\"0\",\"defaultValue\":10}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524990', '36', 'maxReservedAttempts', 'maxReservedAttempts', 1, 3, 13, '{\"required\":\"0\",\"defaultValue\":11}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524991', '36', 'baseRetryBackoffMs', 'baseRetryBackoffMs', 1, 3, 14, '{\"required\":\"0\",\"defaultValue\":100}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524992', '36', 'maxRetryBackoffMs', 'maxRetryBackoffMs', 1, 3, 15, '{\"required\":\"0\",\"defaultValue\":50000}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');

-- ----------------------------
-- Table structure for resource
-- ----------------------------
DROP TABLE IF EXISTS "public"."resource";
CREATE TABLE "public"."resource" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "parent_id" varchar(128) COLLATE "pg_catalog"."default" NULL,
  "title" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "name" varchar(32) COLLATE "pg_catalog"."default" NULL,
  "url" varchar(32) COLLATE "pg_catalog"."default" NULL,
  "component" varchar(32) COLLATE "pg_catalog"."default" NULL,
  "resource_type" int4 NOT NULL,
  "sort" int4 NOT NULL,
  "icon" varchar(32) COLLATE "pg_catalog"."default" NULL,
  "is_leaf" int2 NOT NULL,
  "is_route" int4 NOT NULL,
  "perms" varchar(64) COLLATE "pg_catalog"."default" NULL,
  "status" int4 NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."resource"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."resource"."parent_id" IS 'resource parent primary key id';
COMMENT ON COLUMN "public"."resource"."title" IS 'title';
COMMENT ON COLUMN "public"."resource"."name" IS 'route name';
COMMENT ON COLUMN "public"."resource"."url" IS 'route url';
COMMENT ON COLUMN "public"."resource"."component" IS 'component';
COMMENT ON COLUMN "public"."resource"."resource_type" IS 'resource type eg 0:main menu 1:child menu 2:function button';
COMMENT ON COLUMN "public"."resource"."sort" IS 'sort';
COMMENT ON COLUMN "public"."resource"."icon" IS 'icon';
COMMENT ON COLUMN "public"."resource"."is_leaf" IS 'leaf node 0:no 1:yes';
COMMENT ON COLUMN "public"."resource"."is_route" IS 'route 1:yes 0:no';
COMMENT ON COLUMN "public"."resource"."perms" IS 'button permission description sys:user:add(add)/sys:user:edit(edit)';
COMMENT ON COLUMN "public"."resource"."status" IS 'status 1:enable 0:disable';
COMMENT ON COLUMN "public"."resource"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."resource"."date_updated" IS 'update time';
COMMENT ON TABLE "public"."resource" IS 'resource table';

-- ----------------------------
-- Records of resource
-- ----------------------------
INSERT INTO "public"."resource" VALUES ('1346775491550474240', '', 'SHENYU.MENU.PLUGIN.LIST', 'plug', '/plug', 'PluginList', 0, 0, 'dashboard', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1346776175553376256', '', 'SHENYU.MENU.SYSTEM.MANAGMENT', 'system', '/system', 'system', 0, 2, 'setting', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1346777157943259136', '1346776175553376256', 'SHENYU.MENU.SYSTEM.MANAGMENT.USER', 'manage', '/system/manage', 'manage', 1, 1, 'user', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1346777449787125760', '1357956838021890048', 'SHENYU.MENU.SYSTEM.MANAGMENT.PLUGIN', 'plugin', '/config/plugin', 'plugin', 1, 2, 'book', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1346777623011880960', '1357956838021890048', 'SHENYU.PLUGIN.PLUGINHANDLE', 'pluginhandle', '/config/pluginhandle', 'pluginhandle', 1, 3, 'down-square', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1346777766301888512', '1357956838021890048', 'SHENYU.MENU.SYSTEM.MANAGMENT.AUTHEN', 'auth', '/config/auth', 'auth', 1, 4, 'audit', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1346777907096285184', '1357956838021890048', 'SHENYU.MENU.SYSTEM.MANAGMENT.METADATA', 'metadata', '/config/metadata', 'metadata', 1, 5, 'snippets', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1346778036402483200', '1357956838021890048', 'SHENYU.MENU.SYSTEM.MANAGMENT.DICTIONARY', 'dict', '/config/dict', 'dict', 1, 6, 'ordered-list', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347032308726902784', '1346777157943259136', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 0, '', 1, 0, 'system:manager:add', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347032395901317120', '1346777157943259136', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 1, '', 1, 0, 'system:manager:list', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347032453707214848', '1346777157943259136', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 2, '', 1, 0, 'system:manager:delete', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347032509051056128', '1346777157943259136', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 3, '', 1, 0, 'system:manager:edit', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347034027070337024', '1346777449787125760', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, '', 1, 0, 'system:plugin:list', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347039054925148160', '1346777449787125760', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 1, '', 1, 0, 'system:plugin:delete', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347041326749691904', '1346777449787125760', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 2, '', 1, 0, 'system:plugin:add', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347046566244003840', '1346777449787125760', 'SHENYU.BUTTON.SYSTEM.SYNCHRONIZE', '', '', '', 2, 3, '', 1, 0, 'system:plugin:modify', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347047143350874112', '1346777449787125760', 'SHENYU.BUTTON.SYSTEM.ENABLE', '', '', '', 2, 4, '', 1, 0, 'system:plugin:disable', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347047203220369408', '1346777449787125760', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 5, '', 1, 0, 'system:plugin:edit', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347047555588042752', '1346777623011880960', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, '', 1, 0, 'system:pluginHandler:list', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347047640145211392', '1346777623011880960', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 1, '', 1, 0, 'system:pluginHandler:delete', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347047695002513408', '1346777623011880960', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 2, '', 1, 0, 'system:pluginHandler:add', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347047747305484288', '1346777623011880960', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 3, '', 1, 0, 'system:pluginHandler:edit', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347048004105940992', '1346777766301888512', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, '', 1, 0, 'system:authen:list', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347048101875167232', '1346777766301888512', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 1, '', 1, 0, 'system:authen:delete', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347048145877610496', '1346777766301888512', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 2, '', 1, 0, 'system:authen:add', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347048240677269504', '1346777766301888512', 'SHENYU.BUTTON.SYSTEM.ENABLE', '', '', '', 2, 3, '', 1, 0, 'system:authen:disable', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347048316216684544', '1346777766301888512', 'SHENYU.BUTTON.SYSTEM.SYNCHRONIZE', '', '', '', 2, 4, '', 1, 0, 'system:authen:modify', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347048776029843456', '1346777766301888512', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 5, '', 1, 0, 'system:authen:edit', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347048968414179328', '1346777907096285184', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, '', 1, 0, 'system:meta:list', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347049029323862016', '1346777907096285184', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 1, '', 1, 0, 'system:meta:delete', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347049092552994816', '1346777907096285184', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 2, '', 1, 0, 'system:meta:add', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347049251395481600', '1346777907096285184', 'SHENYU.BUTTON.SYSTEM.ENABLE', '', '', '', 2, 3, '', 1, 0, 'system:meta:disable', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347049317178945536', '1346777907096285184', 'SHENYU.BUTTON.SYSTEM.SYNCHRONIZE', '', '', '', 2, 4, '', 1, 0, 'system:meta:modify', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347049370014593024', '1346777907096285184', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 5, '', 1, 0, 'system:meta:edit', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347049542417264640', '1346778036402483200', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, '', 1, 0, 'system:dict:list', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347049598155370496', '1346778036402483200', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 1, '', 1, 0, 'system:dict:delete', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347049659023110144', '1346778036402483200', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 2, '', 1, 0, 'system:dict:add', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347049731047698432', '1346778036402483200', 'SHENYU.BUTTON.SYSTEM.ENABLE', '', '', '', 2, 3, '', 1, 0, 'system:dict:disable', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1347049794008395776', '1346778036402483200', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 4, '', 1, 0, 'system:dict:edit', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1350106119681622016', '1346776175553376256', 'SHENYU.MENU.SYSTEM.MANAGMENT.ROLE', 'role', '/system/role', 'role', 1, 0, 'usergroup-add', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1350107709494804480', '1350106119681622016', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 0, '', 1, 0, 'system:role:add', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1350107842236137472', '1350106119681622016', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 1, '', 1, 0, 'system:role:list', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1350112406754766848', '1350106119681622016', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 2, '', 1, 0, 'system:role:delete', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1350112481253994496', '1350106119681622016', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 3, '', 1, 0, 'system:role:edit', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1350804501819195392', '1346777766301888512', 'SHENYU.BUTTON.SYSTEM.EDITRESOURCEDETAILS', '', '', '', 2, 6, '', 1, 0, 'system:authen:editResourceDetails', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1355163372527050752', '1346776175553376256', 'SHENYU.MENU.SYSTEM.MANAGMENT.RESOURCE', 'resource', '/system/resource', 'resource', 1, 2, 'menu', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1355165158419750912', '1355163372527050752', 'SHENYU.BUTTON.RESOURCE.MENU.ADD', '', '', '', 2, 1, '', 1, 0, 'system:resource:addMenu', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1355165353534578688', '1355163372527050752', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, '', 1, 0, 'system:resource:list', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1355165475785957376', '1355163372527050752', 'SHENYU.BUTTON.RESOURCE.MENU.DELETE', '', '', '', 2, 2, '', 1, 0, 'system:resource:deleteMenu', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1355165608565039104', '1355163372527050752', 'SHENYU.BUTTON.RESOURCE.MENU.EDIT', '', '', '', 2, 3, '', 1, 0, 'system:resource:editMenu', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1357956838021890048', '', 'SHENYU.MENU.CONFIG.MANAGMENT', 'config', '/config', 'config', 0, 1, 'api', 0, 0, '', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1357977745889132544', '1355163372527050752', 'SHENYU.BUTTON.RESOURCE.BUTTON.ADD', '', '', '', 2, 4, '', 1, 0, 'system:resource:addButton', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1357977912126177280', '1355163372527050752', 'SHENYU.SYSTEM.EDITOR', '', '', '', 2, 5, '', 1, 0, 'system:resource:editButton', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1357977971827900416', '1355163372527050752', 'SHENYU.SYSTEM.DELETEDATA', '', '', '', 2, 6, '', 1, 0, 'system:resource:deleteButton', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1386680049203195904', '1346777157943259136', 'SHENYU.BUTTON.DATA.PERMISSION.CONFIG', '', '', '', 2, 0, '', 1, 0, 'system:manager:configureDataPermission', 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."resource" VALUES ('1529403932772798464', '1346775491550474240', 'sign', 'sign', '/plug/sign', 'sign', 1, 0, 'pic-center', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187072', '1346775491550474240', 'sentinel', 'sentinel', '/plug/sentinel', 'sentinel', 1, 0, 'database', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187073', '1346775491550474240', 'sofa', 'sofa', '/plug/sofa', 'sofa', 1, 0, 'key', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187074', '1346775491550474240', 'resilience4j', 'resilience4j', '/plug/resilience4j', 'resilience4j', 1, 0, 'align-left', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187075', '1346775491550474240', 'tars', 'tars', '/plug/tars', 'tars', 1, 0, 'align-left', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187076', '1346775491550474240', 'contextPath', 'contextPath', '/plug/contextPath', 'contextPath', 1, 0, 'thunderbolt', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187077', '1346775491550474240', 'grpc', 'grpc', '/plug/grpc', 'grpc', 1, 0, 'highlight', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187078', '1346775491550474240', 'redirect', 'redirect', '/plug/redirect', 'redirect', 1, 0, 'align-left', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187079', '1346775491550474240', 'motan', 'motan', '/plug/motan', 'motan', 1, 0, 'highlight', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187080', '1346775491550474240', 'loggingConsole', 'loggingConsole', '/plug/loggingConsole', 'loggingConsole', 1, 0, 'block', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187081', '1346775491550474240', 'jwt', 'jwt', '/plug/jwt', 'jwt', 1, 0, 'pic-left', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187082', '1346775491550474240', 'waf', 'waf', '/plug/waf', 'waf', 1, 0, 'border-bottom', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187083', '1346775491550474240', 'request', 'request', '/plug/request', 'request', 1, 0, 'camera', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187084', '1346775491550474240', 'oauth2', 'oauth2', '/plug/oauth2', 'oauth2', 1, 0, 'retweet', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187085', '1346775491550474240', 'paramMapping', 'paramMapping', '/plug/paramMapping', 'paramMapping', 1, 0, 'pic-left', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187086', '1346775491550474240', 'modifyResponse', 'modifyResponse', '/plug/modifyResponse', 'modifyResponse', 1, 0, 'border-bottom', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187087', '1346775491550474240', 'cryptorRequest', 'cryptorRequest', '/plug/cryptorRequest', 'cryptorRequest', 1, 0, 'safety', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187088', '1346775491550474240', 'cryptorResponse', 'cryptorResponse', '/plug/cryptorResponse', 'cryptorResponse', 1, 0, 'database', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187089', '1346775491550474240', 'websocket', 'websocket', '/plug/websocket', 'websocket', 1, 0, 'border-bottom', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187090', '1346775491550474240', 'rewrite', 'rewrite', '/plug/rewrite', 'rewrite', 1, 0, 'redo', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187091', '1346775491550474240', 'rateLimiter', 'rateLimiter', '/plug/rateLimiter', 'rateLimiter', 1, 0, 'pic-center', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187092', '1346775491550474240', 'divide', 'divide', '/plug/divide', 'divide', 1, 0, 'block', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187093', '1346775491550474240', 'dubbo', 'dubbo', '/plug/dubbo', 'dubbo', 1, 0, 'align-left', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187094', '1346775491550474240', 'springCloud', 'springCloud', '/plug/springCloud', 'springCloud', 1, 0, 'stop', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187095', '1346775491550474240', 'hystrix', 'hystrix', '/plug/hystrix', 'hystrix', 1, 0, 'fire', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187096', '1346775491550474240', 'generalContext', 'generalContext', '/plug/generalContext', 'generalContext', 1, 0, 'highlight', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187097', '1346775491550474240', 'mqtt', 'mqtt', '/plug/mqtt', 'mqtt', 1, 0, 'database', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187098', '1346775491550474240', 'loggingRocketMQ', 'loggingRocketMQ', '/plug/loggingRocketMQ', 'loggingRocketMQ', 1, 0, 'stop', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932781187099', '1346775491550474240', 'cache', 'cache', '/plug/cache', 'cache', 1, 0, 'stop', 0, 0, '', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656064', '1529403932772798464', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:signSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656065', '1529403932772798464', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:signSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656066', '1529403932772798464', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:signSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656067', '1529403932772798464', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:signSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656068', '1529403932772798464', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:signRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656069', '1529403932772798464', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:signRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656070', '1529403932772798464', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:signRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656071', '1529403932772798464', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:signRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656072', '1529403932772798464', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:sign:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656073', '1529403932781187072', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:sentinelSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656074', '1529403932781187072', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:sentinelSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656075', '1529403932781187072', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:sentinelSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656076', '1529403932781187072', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:sentinelSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656077', '1529403932781187072', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:sentinelRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656078', '1529403932781187072', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:sentinelRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656079', '1529403932781187072', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:sentinelRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656080', '1529403932781187072', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:sentinelRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656081', '1529403932781187072', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:sentinel:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656082', '1529403932781187073', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:sofaSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656083', '1529403932781187073', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:sofaSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656084', '1529403932781187073', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:sofaSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656085', '1529403932781187073', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:sofaSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656086', '1529403932781187073', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:sofaRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656087', '1529403932781187073', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:sofaRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656088', '1529403932781187073', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:sofaRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656089', '1529403932781187073', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:sofaRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656090', '1529403932781187073', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:sofa:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656091', '1529403932781187074', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:resilience4jSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656092', '1529403932781187074', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:resilience4jSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656093', '1529403932781187074', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:resilience4jSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656094', '1529403932781187074', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:resilience4jSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656095', '1529403932781187074', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:resilience4jRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656096', '1529403932781187074', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:resilience4jRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656097', '1529403932781187074', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:resilience4jRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656098', '1529403932781187074', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:resilience4jRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656099', '1529403932781187074', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:resilience4j:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656100', '1529403932781187075', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:tarsSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656101', '1529403932781187075', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:tarsSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656102', '1529403932781187075', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:tarsSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656103', '1529403932781187075', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:tarsSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656104', '1529403932781187075', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:tarsRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656105', '1529403932781187075', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:tarsRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656106', '1529403932781187075', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:tarsRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656107', '1529403932781187075', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:tarsRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656108', '1529403932781187075', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:tars:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656109', '1529403932781187076', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:contextPathSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656110', '1529403932781187076', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:contextPathSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656111', '1529403932781187076', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:contextPathSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656112', '1529403932781187076', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:contextPathSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656113', '1529403932781187076', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:contextPathRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656114', '1529403932781187076', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:contextPathRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656115', '1529403932781187076', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:contextPathRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656116', '1529403932781187076', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:contextPathRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656117', '1529403932781187076', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:contextPath:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656118', '1529403932781187077', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:grpcSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656119', '1529403932781187077', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:grpcSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656120', '1529403932781187077', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:grpcSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656121', '1529403932781187077', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:grpcSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656122', '1529403932781187077', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:grpcRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656123', '1529403932781187077', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:grpcRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656124', '1529403932781187077', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:grpcRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656125', '1529403932781187077', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:grpcRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656126', '1529403932781187077', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:grpc:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656127', '1529403932781187078', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:redirectSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656128', '1529403932781187078', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:redirectSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656129', '1529403932781187078', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:redirectSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656130', '1529403932781187078', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:redirectSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656131', '1529403932781187078', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:redirectRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656132', '1529403932781187078', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:redirectRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656133', '1529403932781187078', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:redirectRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656134', '1529403932781187078', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:redirectRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656135', '1529403932781187078', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:redirect:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656136', '1529403932781187079', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:motanSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656137', '1529403932781187079', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:motanSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656138', '1529403932781187079', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:motanSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656139', '1529403932781187079', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:motanSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656140', '1529403932781187079', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:motanRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656141', '1529403932781187079', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:motanRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656142', '1529403932781187079', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:motanRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656143', '1529403932781187079', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:motanRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656144', '1529403932781187079', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:motan:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656145', '1529403932781187080', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingConsoleSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656146', '1529403932781187080', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingConsoleSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656147', '1529403932781187080', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingConsoleSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656148', '1529403932781187080', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingConsoleSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656149', '1529403932781187080', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingConsoleRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656150', '1529403932781187080', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingConsoleRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656151', '1529403932781187080', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingConsoleRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656152', '1529403932781187080', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingConsoleRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656153', '1529403932781187080', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingConsole:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656154', '1529403932781187081', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:jwtSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656155', '1529403932781187081', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:jwtSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656156', '1529403932781187081', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:jwtSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656157', '1529403932781187081', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:jwtSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656158', '1529403932781187081', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:jwtRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656159', '1529403932781187081', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:jwtRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656160', '1529403932781187081', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:jwtRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656161', '1529403932781187081', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:jwtRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656162', '1529403932781187081', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:jwt:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656163', '1529403932781187082', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:wafSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656164', '1529403932781187082', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:wafSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656165', '1529403932781187082', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:wafSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656166', '1529403932781187082', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:wafSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656167', '1529403932781187082', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:wafRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656168', '1529403932781187082', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:wafRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656169', '1529403932781187082', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:wafRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656170', '1529403932781187082', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:wafRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656171', '1529403932781187082', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:waf:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656172', '1529403932781187083', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:requestSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656173', '1529403932781187083', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:requestSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656174', '1529403932781187083', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:requestSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656175', '1529403932781187083', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:requestSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656176', '1529403932781187083', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:requestRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656177', '1529403932781187083', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:requestRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656178', '1529403932781187083', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:requestRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656179', '1529403932781187083', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:requestRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932877656180', '1529403932781187083', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:request:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850368', '1529403932781187084', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:oauth2Selector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850369', '1529403932781187084', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:oauth2Selector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850370', '1529403932781187084', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:oauth2Selector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850371', '1529403932781187084', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:oauth2Selector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850372', '1529403932781187084', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:oauth2Rule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850373', '1529403932781187084', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:oauth2Rule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850374', '1529403932781187084', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:oauth2Rule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850375', '1529403932781187084', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:oauth2Rule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850376', '1529403932781187084', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:oauth2:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850377', '1529403932781187085', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:paramMappingSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850378', '1529403932781187085', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:paramMappingSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850379', '1529403932781187085', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:paramMappingSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850380', '1529403932781187085', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:paramMappingSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850381', '1529403932781187085', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:paramMappingRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850382', '1529403932781187085', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:paramMappingRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850383', '1529403932781187085', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:paramMappingRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850384', '1529403932781187085', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:paramMappingRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850385', '1529403932781187085', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:paramMapping:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850386', '1529403932781187086', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:modifyResponseSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850387', '1529403932781187086', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:modifyResponseSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850388', '1529403932781187086', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:modifyResponseSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850389', '1529403932781187086', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:modifyResponseSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850390', '1529403932781187086', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:modifyResponseRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850391', '1529403932781187086', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:modifyResponseRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850392', '1529403932781187086', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:modifyResponseRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850393', '1529403932781187086', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:modifyResponseRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850394', '1529403932781187086', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:modifyResponse:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850395', '1529403932781187087', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorRequestSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850396', '1529403932781187087', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorRequestSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850397', '1529403932781187087', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorRequestSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850398', '1529403932781187087', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorRequestSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850399', '1529403932781187087', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorRequestRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850400', '1529403932781187087', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorRequestRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850401', '1529403932781187087', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorRequestRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850402', '1529403932781187087', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorRequestRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850403', '1529403932781187087', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorRequest:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850404', '1529403932781187088', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorResponseSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850405', '1529403932781187088', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorResponseSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850406', '1529403932781187088', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorResponseSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850407', '1529403932781187088', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorResponseSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850408', '1529403932781187088', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorResponseRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850409', '1529403932781187088', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorResponseRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850410', '1529403932781187088', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorResponseRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850411', '1529403932781187088', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorResponseRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850412', '1529403932781187088', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:cryptorResponse:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850413', '1529403932781187089', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:websocketSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850414', '1529403932781187089', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:websocketSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850415', '1529403932781187089', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:websocketSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850416', '1529403932781187089', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:websocketSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850417', '1529403932781187089', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:websocketRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850418', '1529403932781187089', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:websocketRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850419', '1529403932781187089', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:websocketRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850420', '1529403932781187089', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:websocketRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850421', '1529403932781187089', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:websocket:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850422', '1529403932781187090', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:rewriteSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850423', '1529403932781187090', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:rewriteSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850424', '1529403932781187090', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:rewriteSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850425', '1529403932781187090', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:rewriteSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850426', '1529403932781187090', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:rewriteRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850427', '1529403932781187090', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:rewriteRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850428', '1529403932781187090', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:rewriteRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850429', '1529403932781187090', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:rewriteRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850430', '1529403932781187090', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:rewrite:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850431', '1529403932781187091', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:rateLimiterSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850432', '1529403932781187091', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:rateLimiterSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850433', '1529403932781187091', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:rateLimiterSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850434', '1529403932781187091', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:rateLimiterSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850435', '1529403932781187091', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:rateLimiterRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850436', '1529403932781187091', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:rateLimiterRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850437', '1529403932781187091', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:rateLimiterRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850438', '1529403932781187091', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:rateLimiterRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850439', '1529403932781187091', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:rateLimiter:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850440', '1529403932781187092', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:divideSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850441', '1529403932781187092', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:divideSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850442', '1529403932781187092', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:divideSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850443', '1529403932781187092', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:divideSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850444', '1529403932781187092', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:divideRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850445', '1529403932781187092', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:divideRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850446', '1529403932781187092', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:divideRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850447', '1529403932781187092', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:divideRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850448', '1529403932781187092', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:divide:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850449', '1529403932781187093', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:dubboSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850450', '1529403932781187093', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:dubboSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850451', '1529403932781187093', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:dubboSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850452', '1529403932781187093', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:dubboSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850453', '1529403932781187093', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:dubboRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850454', '1529403932781187093', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:dubboRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850455', '1529403932781187093', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:dubboRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850456', '1529403932781187093', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:dubboRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850457', '1529403932781187093', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:dubbo:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850458', '1529403932781187094', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:springCloudSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850459', '1529403932781187094', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:springCloudSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850460', '1529403932781187094', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:springCloudSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850461', '1529403932781187094', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:springCloudSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850462', '1529403932781187094', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:springCloudRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850463', '1529403932781187094', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:springCloudRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850464', '1529403932781187094', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:springCloudRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850465', '1529403932781187094', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:springCloudRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850466', '1529403932781187094', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:springCloud:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850467', '1529403932781187095', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:hystrixSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850468', '1529403932781187095', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:hystrixSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850469', '1529403932781187095', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:hystrixSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850470', '1529403932781187095', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:hystrixSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850471', '1529403932781187095', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:hystrixRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850472', '1529403932781187095', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:hystrixRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850473', '1529403932781187095', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:hystrixRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850474', '1529403932781187095', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:hystrixRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850475', '1529403932781187095', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:hystrix:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850476', '1529403932781187096', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:generalContextSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850477', '1529403932781187096', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:generalContextSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850478', '1529403932781187096', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:generalContextSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850479', '1529403932781187096', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:generalContextSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850480', '1529403932781187096', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:generalContextRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850481', '1529403932781187096', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:generalContextRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850482', '1529403932781187096', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:generalContextRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850483', '1529403932781187096', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:generalContextRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850484', '1529403932781187096', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:generalContext:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850485', '1529403932781187097', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:mqttSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850486', '1529403932781187097', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:mqttSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850487', '1529403932781187097', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:mqttSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850488', '1529403932781187097', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:mqttSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850489', '1529403932781187097', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:mqttRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850490', '1529403932781187097', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:mqttRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850491', '1529403932781187097', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:mqttRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850492', '1529403932781187097', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:mqttRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850493', '1529403932781187097', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:mqtt:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850494', '1529403932781187098', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingRocketMQSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850495', '1529403932781187098', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingRocketMQSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850496', '1529403932781187098', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingRocketMQSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850497', '1529403932781187098', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingRocketMQSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850498', '1529403932781187098', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingRocketMQRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850499', '1529403932781187098', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingRocketMQRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850500', '1529403932781187098', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingRocketMQRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850501', '1529403932781187098', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingRocketMQRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850502', '1529403932781187098', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingRocketMQ:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850503', '1529403932781187099', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:cacheSelector:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850504', '1529403932781187099', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:cacheSelector:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850505', '1529403932781187099', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:cacheSelector:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850506', '1529403932781187099', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:cacheSelector:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850507', '1529403932781187099', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:cacheRule:add', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850508', '1529403932781187099', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:cacheRule:query', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850509', '1529403932781187099', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:cacheRule:edit', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850510', '1529403932781187099', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:cacheRule:delete', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1529403932881850511', '1529403932781187099', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:cache:modify', 1, '2022-05-25 18:08:07', '2022-05-25 18:08:07');
INSERT INTO "public"."resource" VALUES ('1534577121923309568', '', 'Document', '', '/document', '', 0, 3, 'file-text', 0, 0, '', 1, '2022-06-09 00:44:32', '2022-06-09 01:06:45');
INSERT INTO "public"."resource" VALUES ('1534585430311051264', '1534577121923309568', 'API document', '', '/document/apidoc', '', 1, 0, 'api', 0, 0, '', 1, '2022-06-09 01:17:32', '2022-06-09 01:17:32');
INSERT INTO "public"."resource" VALUES ('1534585531108564992', '1534585430311051264', 'send', '', '', '', 2, 0, '', 1, 0, 'document:apirun:send', 1, '2022-06-09 01:17:56', '2022-06-09 01:17:56');

-- ----------------------------
-- Table structure for role
-- ----------------------------
DROP TABLE IF EXISTS "public"."role";
CREATE TABLE "public"."role" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "role_name" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "description" varchar(255) COLLATE "pg_catalog"."default",
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."role"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."role"."role_name" IS 'role name';
COMMENT ON COLUMN "public"."role"."description" IS 'role describe';
COMMENT ON COLUMN "public"."role"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."role"."date_updated" IS 'update time';
COMMENT ON TABLE "public"."role" IS 'role table';

-- ----------------------------
-- Records of role
-- ----------------------------
INSERT INTO "public"."role" VALUES ('1346358560427216896', 'super', 'Administrator', '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."role" VALUES ('1385482862971723776', 'default', 'Standard', '2022-05-25 18:08:02', '2022-05-25 18:08:02');

-- ----------------------------
-- Table structure for rule
-- ----------------------------
DROP TABLE IF EXISTS "public"."rule";
CREATE TABLE "public"."rule" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "selector_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "match_mode" int4 NOT NULL,
  "name" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "enabled" int2 NOT NULL,
  "loged" int2 NOT NULL,
  "sort" int4 NOT NULL,
  "handle" varchar(1024) COLLATE "pg_catalog"."default",
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."rule"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."rule"."selector_id" IS 'selector id';
COMMENT ON COLUMN "public"."rule"."match_mode" IS 'matching mode (0 and 1 or)';
COMMENT ON COLUMN "public"."rule"."name" IS 'rule name';
COMMENT ON COLUMN "public"."rule"."enabled" IS 'whether to open (0 close, 1 open) ';
COMMENT ON COLUMN "public"."rule"."loged" IS 'whether to log or not (0 no print, 1 print) ';
COMMENT ON COLUMN "public"."rule"."sort" IS 'sort';
COMMENT ON COLUMN "public"."rule"."handle" IS 'processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)';
COMMENT ON COLUMN "public"."rule"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."rule"."date_updated" IS 'update time';

-- ----------------------------
-- Records of rule
-- ----------------------------

-- ----------------------------
-- Table structure for rule_condition
-- ----------------------------
DROP TABLE IF EXISTS "public"."rule_condition";
CREATE TABLE "public"."rule_condition" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "rule_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "param_type" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "operator" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "param_name" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "param_value" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."rule_condition"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."rule_condition"."rule_id" IS 'rule id';
COMMENT ON COLUMN "public"."rule_condition"."param_type" IS 'parameter type (post query uri, etc.)';
COMMENT ON COLUMN "public"."rule_condition"."operator" IS 'matching character (=> <like match)';
COMMENT ON COLUMN "public"."rule_condition"."param_name" IS 'parameter name';
COMMENT ON COLUMN "public"."rule_condition"."param_value" IS 'parameter value';
COMMENT ON COLUMN "public"."rule_condition"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."rule_condition"."date_updated" IS 'update time';

-- ----------------------------
-- Records of rule_condition
-- ----------------------------

-- ----------------------------
-- Table structure for selector
-- ----------------------------
DROP TABLE IF EXISTS "public"."selector";
CREATE TABLE "public"."selector" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "plugin_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "name" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "match_mode" int4 NOT NULL,
  "type" int4 NOT NULL,
  "sort" int4 NOT NULL,
  "handle" varchar(1024) COLLATE "pg_catalog"."default",
  "enabled" int2 NOT NULL,
  "loged" int2 NOT NULL,
  "continued" int2 NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."selector"."id" IS 'primary key id varchar';
COMMENT ON COLUMN "public"."selector"."plugin_id" IS 'plugin id';
COMMENT ON COLUMN "public"."selector"."name" IS 'selector name';
COMMENT ON COLUMN "public"."selector"."match_mode" IS 'matching mode (0 and 1 or)';
COMMENT ON COLUMN "public"."selector"."type" IS 'type (0, full flow, 1 custom flow)';
COMMENT ON COLUMN "public"."selector"."sort" IS 'sort';
COMMENT ON COLUMN "public"."selector"."handle" IS 'processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)';
COMMENT ON COLUMN "public"."selector"."enabled" IS 'whether to open (0 close, 1 open) ';
COMMENT ON COLUMN "public"."selector"."loged" IS 'whether to print the log (0 no print, 1 print) ';
COMMENT ON COLUMN "public"."selector"."continued" IS 'whether to continue execution';
COMMENT ON COLUMN "public"."selector"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."selector"."date_updated" IS 'update time';

-- ----------------------------
-- Records of selector
-- ----------------------------

-- ----------------------------
-- Table structure for selector_condition
-- ----------------------------
DROP TABLE IF EXISTS "public"."selector_condition";
CREATE TABLE "public"."selector_condition" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "selector_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "param_type" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "operator" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "param_name" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "param_value" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."selector_condition"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."selector_condition"."selector_id" IS 'selector id';
COMMENT ON COLUMN "public"."selector_condition"."param_type" IS 'parameter type (to query uri, etc.)';
COMMENT ON COLUMN "public"."selector_condition"."operator" IS 'matching character (=> <like matching)';
COMMENT ON COLUMN "public"."selector_condition"."param_name" IS 'parameter name';
COMMENT ON COLUMN "public"."selector_condition"."param_value" IS 'parameter value';
COMMENT ON COLUMN "public"."selector_condition"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."selector_condition"."date_updated" IS 'update time';

-- ----------------------------
-- Records of selector_condition
-- ----------------------------

-- ----------------------------
-- Table structure for shenyu_dict
-- ----------------------------
DROP TABLE IF EXISTS "public"."shenyu_dict";
CREATE TABLE "public"."shenyu_dict" (
  "id" varchar(128) NOT NULL DEFAULT nextval('shenyu_dict_id_seq'::regclass),
  "type" varchar(100) COLLATE "pg_catalog"."default" NOT NULL,
  "dict_code" varchar(100) COLLATE "pg_catalog"."default" NOT NULL,
  "dict_name" varchar(100) COLLATE "pg_catalog"."default" NOT NULL,
  "dict_value" varchar(100) COLLATE "pg_catalog"."default",
  "desc" varchar(255) COLLATE "pg_catalog"."default",
  "sort" int4 NOT NULL,
  "enabled" int2,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."shenyu_dict"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."shenyu_dict"."type" IS 'type';
COMMENT ON COLUMN "public"."shenyu_dict"."dict_code" IS 'dictionary encoding';
COMMENT ON COLUMN "public"."shenyu_dict"."dict_name" IS 'dictionary name';
COMMENT ON COLUMN "public"."shenyu_dict"."dict_value" IS 'dictionary value';
COMMENT ON COLUMN "public"."shenyu_dict"."desc" IS 'dictionary description or remarks';
COMMENT ON COLUMN "public"."shenyu_dict"."sort" IS 'sort';
COMMENT ON COLUMN "public"."shenyu_dict"."enabled" IS 'whether it is enabled (0 close, 1 open) ';
COMMENT ON COLUMN "public"."shenyu_dict"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."shenyu_dict"."date_updated" IS 'update time';

-- ----------------------------
-- Records of shenyu_dict
-- ----------------------------
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107776', 'degradeRuleGrade', 'DEGRADE_GRADE_RT', 'slow call ratio', '0', 'degrade type-slow call ratio', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107777', 'degradeRuleGrade', 'DEGRADE_GRADE_EXCEPTION_RATIO', 'exception ratio', '1', 'degrade type-abnormal ratio', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107778', 'degradeRuleGrade', 'DEGRADE_GRADE_EXCEPTION_COUNT', 'exception number strategy', '2', 'degrade type-abnormal number strategy', 2, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107779', 'flowRuleGrade', 'FLOW_GRADE_QPS', 'QPS', '1', 'grade type-QPS', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107780', 'flowRuleGrade', 'FLOW_GRADE_THREAD', 'number of concurrent threads', '0', 'degrade type-number of concurrent threads', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107781', 'flowRuleControlBehavior', 'CONTROL_BEHAVIOR_DEFAULT', 'direct rejection by default', '0', 'control behavior-direct rejection by default', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107782', 'flowRuleControlBehavior', 'CONTROL_BEHAVIOR_WARM_UP', 'warm up', '1', 'control behavior-warm up', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107783', 'flowRuleControlBehavior', 'CONTROL_BEHAVIOR_RATE_LIMITER', 'constant speed queuing', '2', 'control behavior-uniform speed queuing', 2, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107784', 'flowRuleControlBehavior', 'CONTROL_BEHAVIOR_WARM_UP_RATE_LIMITER', 'preheating uniformly queued', '3', 'control behavior-preheating uniformly queued', 3, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107785', 'permission', 'REJECT', 'reject', 'reject', 'reject', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107786', 'permission', 'ALLOW', 'allow', 'allow', 'allow', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107787', 'algorithmName', 'ALGORITHM_SLIDINGWINDOW', 'slidingWindow', 'slidingWindow', 'Sliding window algorithm', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107788', 'algorithmName', 'ALGORITHM_LEAKYBUCKET', 'leakyBucket', 'leakyBucket', 'Leaky bucket algorithm', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107789', 'algorithmName', 'ALGORITHM_CONCURRENT', 'concurrent', 'concurrent', 'Concurrent algorithm', 2, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107790', 'algorithmName', 'ALGORITHM_TOKENBUCKET', 'tokenBucket', 'tokenBucket', 'Token bucket algorithm', 3, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107791', 'loadBalance', 'LOAD_BALANCE', 'roundRobin', 'roundRobin', 'roundRobin', 2, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107792', 'loadBalance', 'LOAD_BALANCE', 'random', 'random', 'random', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107793', 'loadBalance', 'LOAD_BALANCE', 'hash', 'hash', 'hash', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107794', 'status', 'DIVIDE_STATUS', 'close', 'false', 'close', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107795', 'status', 'DIVIDE_STATUS', 'open', 'true', 'open', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107796', 'multiRuleHandle', 'MULTI_RULE_HANDLE', 'multiple rule', '1', 'multiple rule', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107797', 'multiRuleHandle', 'MULTI_RULE_HANDLE', 'single rule', '0', 'single rule', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107798', 'multiSelectorHandle', 'MULTI_SELECTOR_HANDLE', 'multiple handle', '1', 'multiple handle', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107799', 'multiSelectorHandle', 'MULTI_SELECTOR_HANDLE', 'single handle', '0', 'single handle', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107800', 'matchMode', 'MATCH_MODE', 'and', '0', 'and', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107801', 'matchMode', 'MATCH_MODE', 'or', '1', 'or', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107802', 'operator', 'OPERATOR', 'match', 'match', 'match', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107803', 'operator', 'OPERATOR', '=', '=', '=', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107804', 'operator', 'OPERATOR', 'regex', 'regex', 'regex', 2, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107805', 'operator', 'OPERATOR', 'contains', 'contains', 'contains', 3, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107806', 'operator', 'OPERATOR', 'TimeBefore', 'TimeBefore', 'TimeBefore', 4, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107807', 'operator', 'OPERATOR', 'TimeAfter', 'TimeAfter', 'TimeAfter', 5, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107808', 'operator', 'OPERATOR', 'exclude', 'exclude', 'exclude', 6, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107809', 'operator', 'OPERATOR', 'startsWith', 'startsWith', 'startsWith', 7, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107810', 'operator', 'OPERATOR', 'endsWith', 'endsWith', 'endWiths', 8, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1629403902796107810', 'operator', 'OPERATOR', 'pathPattern', 'pathPattern', 'pathPattern', 9, 1, '2022-07-19 18:08:02', '2022-07-19 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107811', 'paramType', 'PARAM_TYPE', 'post', 'post', 'post', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107812', 'paramType', 'PARAM_TYPE', 'uri', 'uri', 'uri', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107813', 'paramType', 'PARAM_TYPE', 'query', 'query', 'query', 2, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107814', 'paramType', 'PARAM_TYPE', 'host', 'host', 'host', 3, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107815', 'paramType', 'PARAM_TYPE', 'ip', 'ip', 'ip', 4, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107816', 'paramType', 'PARAM_TYPE', 'header', 'header', 'header', 5, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107817', 'paramType', 'PARAM_TYPE', 'cookie', 'cookie', 'cookie', 6, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107818', 'paramType', 'PARAM_TYPE', 'req_method', 'req_method', 'req_method', 7, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107819', 'keyResolverName', 'WHOLE_KEY_RESOLVER', 'whole', 'WHOLE_KEY_RESOLVER', 'Rate limit by all request', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107820', 'keyResolverName', 'REMOTE_ADDRESS_KEY_RESOLVER', 'remoteAddress', 'REMOTE_ADDRESS_KEY_RESOLVER', 'Rate limit by remote address', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107821', 'automaticTransitionFromOpenToHalfOpenEnabled', 'AUTOMATIC_HALF_OPEN', 'open', 'true', '', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107822', 'automaticTransitionFromOpenToHalfOpenEnabled', 'AUTOMATIC_HALF_OPEN', 'close', 'false', '', 2, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107823', 'paramType', 'PARAM_TYPE', 'domain', 'domain', 'domain', 8, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107824', 'strategyName', 'STRATEGY_NAME', 'rsa', 'rsa', 'rsa strategy', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902796107825', 'way', 'WAY', 'encrypt', 'encrypt', 'encrypt', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302080', 'way', 'WAY', 'decrypt', 'decrypt', 'decrypt', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302081', 'mode', 'MODE', 'cluster', 'cluster', 'cluster', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302082', 'mode', 'MODE', 'sentinel', 'sentinel', 'sentinel', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302083', 'mode', 'MODE', 'standalone', 'standalone', 'standalone', 2, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302084', 'gray', 'GRAY_STATUS', 'close', 'false', 'close', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302085', 'gray', 'GRAY_STATUS', 'open', 'true', 'open', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302086', 'threadpool', 'THREADPOOL', 'shared', 'shared', '', 4, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302087', 'threadpool', 'THREADPOOL', 'fixed', 'fixed', '', 3, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302088', 'threadpool', 'THREADPOOL', 'eager', 'eager', '', 2, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302089', 'threadpool', 'THREADPOOL', 'cached', 'cached', '', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302090', 'threadpool', 'THREADPOOL', 'limited', 'limited', '', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302091', 'retryStrategy', 'RETRY_STRATEGY', 'current', 'current', 'current', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302092', 'retryStrategy', 'RETRY_STRATEGY', 'failover', 'failover', 'failover', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302094', 'compressAlg', 'COMPRESS_ALG', 'none', 'none', '', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302095', 'compressAlg', 'COMPRESS_ALG', 'LZ4', 'LZ4', '', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302096', 'cacheType', 'CACHE_TYPE_MEMORY', 'memory', 'memory', 'use memory to cache data', 0, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302097', 'cacheType', 'CACHE_TYPE_REDIS', 'redis', 'redis', 'use redis to cache data', 1, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302093', 'table', 'INIT_FLAG', 'status', 'true', 'table(resource,permission) init status', 0, 0, '2022-05-25 18:08:02', '2022-05-25 18:08:07.275');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302098', 'threadpool', 'THREADPOOL', 'default', 'default', '', 5, 1, '2022-05-25 18:08:02', '2022-05-25 18:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302099', 'signRequestBody', 'SIGN_REQUEST_BODY', 'close', 'false', 'close', 1, 1, '2022-06-29 10:08:02', '2022-06-29 10:08:02');
INSERT INTO "public"."shenyu_dict" VALUES ('1529403902800302100', 'signRequestBody', 'SIGN_REQUEST_BODY', 'open', 'true', 'open', 0, 1, '2022-06-29 10:08:02', '2022-06-29 10:08:02');
INSERT INTO "public".`shenyu_dict` VALUES ('1545811989312315392', 'apidoc', 'API_DOC_GLOBAL_FLAG', 'status', 'true', 'Global switching (on or off) of API documents.', 0, 1, '2022-07-10 00:47:52', '2022-07-10 00:47:52');
INSERT INTO "public".`shenyu_dict` VALUES ('1545812101056962560', 'apidocEnv', 'ENV_LABEL_OFFLINE', 'Offline', 'http://127.0.0.1:9195', 'Offline environment', 0, 1, '2022-07-10 00:48:19', '2022-07-10 00:48:19');
INSERT INTO "public".`shenyu_dict` VALUES ('1545812228228259840', 'apidocEnv', 'ENV_LABEL_ONLINE', 'Online', 'http://127.0.0.1:9196', 'Online environment', 1, 1, '2022-07-10 00:48:49', '2022-07-10 00:48:49');
-- ----------------------------
-- Table structure for user_role
-- ----------------------------
DROP TABLE IF EXISTS "public"."user_role";
CREATE TABLE "public"."user_role" (
  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "user_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "role_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
  "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
  "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."user_role"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."user_role"."user_id" IS 'user primary key';
COMMENT ON COLUMN "public"."user_role"."role_id" IS 'role primary key';
COMMENT ON COLUMN "public"."user_role"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."user_role"."date_updated" IS 'update time';
COMMENT ON TABLE "public"."user_role" IS 'user and role bind table';

-- ----------------------------
-- Records of user_role
-- ----------------------------
INSERT INTO "public"."user_role" VALUES ('1351007709096976384', '1', '1346358560427216896', '2022-05-25 18:08:02', '2022-05-25 18:08:02');

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"public"."operation_record_log_id_seq"', 2, false);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "public"."plugin_handle_id_seq"
OWNED BY "public"."plugin_handle"."id";
SELECT setval('"public"."plugin_handle_id_seq"', 2, false);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "public"."shenyu_dict_id_seq"
OWNED BY "public"."shenyu_dict"."id";
SELECT setval('"public"."shenyu_dict_id_seq"', 2, false);

-- ----------------------------
-- Primary Key structure for table app_auth
-- ----------------------------
ALTER TABLE "public"."app_auth" ADD CONSTRAINT "app_auth_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table auth_param
-- ----------------------------
ALTER TABLE "public"."auth_param" ADD CONSTRAINT "auth_param_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table auth_path
-- ----------------------------
ALTER TABLE "public"."auth_path" ADD CONSTRAINT "auth_path_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Indexes structure for table dashboard_user
-- ----------------------------
CREATE INDEX "unique_user_name" ON "public"."dashboard_user" USING btree (
  "user_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Primary Key structure for table dashboard_user
-- ----------------------------
ALTER TABLE "public"."dashboard_user" ADD CONSTRAINT "dashboard_user_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table data_permission
-- ----------------------------
ALTER TABLE "public"."data_permission" ADD CONSTRAINT "data_permission_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table meta_data
-- ----------------------------
ALTER TABLE "public"."meta_data" ADD CONSTRAINT "meta_data_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table operation_record_log
-- ----------------------------
ALTER TABLE "public"."operation_record_log" ADD CONSTRAINT "operation_record_log_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table permission
-- ----------------------------
ALTER TABLE "public"."permission" ADD CONSTRAINT "permission_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table plugin
-- ----------------------------
ALTER TABLE "public"."plugin" ADD CONSTRAINT "plugin_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Indexes structure for table plugin_handle
-- ----------------------------
CREATE INDEX "plugin_id_field_type" ON "public"."plugin_handle" USING btree (
  "plugin_id" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "field" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "type" "pg_catalog"."int2_ops" ASC NULLS LAST
);

-- ----------------------------
-- Rules structure for table plugin_handle
-- ----------------------------
CREATE RULE "plugin_handle_insert_ignore" AS ON INSERT TO "public"."plugin_handle" WHERE (EXISTS ( SELECT 1
           FROM plugin_handle
          WHERE ((plugin_handle.id)::text = (new.id)::text))) DO INSTEAD NOTHING;;

-- ----------------------------
-- Primary Key structure for table plugin_handle
-- ----------------------------
ALTER TABLE "public"."plugin_handle" ADD CONSTRAINT "plugin_handle_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table resource
-- ----------------------------
ALTER TABLE "public"."resource" ADD CONSTRAINT "resource_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table role
-- ----------------------------
ALTER TABLE "public"."role" ADD CONSTRAINT "role_pkey" PRIMARY KEY ("id", "role_name");

-- ----------------------------
-- Primary Key structure for table rule
-- ----------------------------
ALTER TABLE "public"."rule" ADD CONSTRAINT "rule_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table rule_condition
-- ----------------------------
ALTER TABLE "public"."rule_condition" ADD CONSTRAINT "rule_condition_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table selector
-- ----------------------------
ALTER TABLE "public"."selector" ADD CONSTRAINT "selector_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Rules structure for table shenyu_dict
-- ----------------------------
CREATE RULE "shenyu_dict_insert_ignore" AS ON INSERT TO "public"."shenyu_dict" WHERE (EXISTS ( SELECT 1
           FROM shenyu_dict
          WHERE ((shenyu_dict.id)::text = (new.id)::text))) DO INSTEAD NOTHING;;

-- ----------------------------
-- Primary Key structure for table shenyu_dict
-- ----------------------------
ALTER TABLE "public"."shenyu_dict" ADD CONSTRAINT "shenyu_dict_pkey" PRIMARY KEY ("id");
