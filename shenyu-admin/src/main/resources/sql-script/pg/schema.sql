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

CREATE EXTENSION IF NOT EXISTS dblink;
DO
$do$
DECLARE
  _db TEXT := 'shenyu';
  _user TEXT := 'userName';
  _password TEXT := 'password';
  _tablelock INTEGER :=0;
BEGIN
  IF EXISTS (SELECT 1 FROM pg_database WHERE datname = _db) THEN
    RAISE NOTICE 'Database already exists';
  ELSE
    PERFORM public.dblink_connect('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||current_database());
    PERFORM public.dblink_exec('CREATE DATABASE ' || _db || ' template template0;');
  END IF;

    PERFORM public.dblink_connect('init_conn','host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db);


-- ----------------------------------------
-- create table app_auth if not exist ---
-- ---------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'app_auth' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'app_auth already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
		PERFORM public.dblink_exec('init_conn', 'CREATE TABLE "app_auth" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "app_key" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
	  "app_secret" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "user_id" varchar(128) COLLATE "pg_catalog"."default",
	  "phone" varchar(255) COLLATE "pg_catalog"."default",
	  "ext_info" varchar(1024) COLLATE "pg_catalog"."default",
	  "open" int2 NOT NULL,
	  "enabled" int2 NOT NULL,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  CONSTRAINT "app_auth_pkey" PRIMARY KEY ("id")
	)');

	PERFORM public.dblink_exec('init_conn','COMMENT ON COLUMN "app_auth"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn','COMMENT ON COLUMN "app_auth"."app_key" IS ''' || 'application identification key' || '''');
	PERFORM public.dblink_exec('init_conn','COMMENT ON COLUMN "app_auth"."app_secret" IS ''' || 'encryption algorithm secret' || '''');
	PERFORM public.dblink_exec('init_conn','COMMENT ON COLUMN "app_auth"."user_id" IS ''' || 'user id' || '''');
	PERFORM public.dblink_exec('init_conn','COMMENT ON COLUMN "app_auth"."phone" IS ''' || 'phone number when the user applies' || '''');
	PERFORM public.dblink_exec('init_conn','COMMENT ON COLUMN "app_auth"."ext_info" IS ''' || 'extended parameter json' || '''');
	PERFORM public.dblink_exec('init_conn','COMMENT ON COLUMN "app_auth"."open" IS ''' || 'open auth path or not' || '''');
	PERFORM public.dblink_exec('init_conn','COMMENT ON COLUMN "app_auth"."enabled" IS ''' || 'delete or not' || '''');
	PERFORM public.dblink_exec('init_conn','COMMENT ON COLUMN "app_auth"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn','COMMENT ON COLUMN "app_auth"."date_updated" IS ''' || 'update time' || '''');
	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;


-- ----------------------------------------------------
-- create table auth_param if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'auth_param' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'auth_param already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "auth_param" (
	  "id" varchar(129) COLLATE "pg_catalog"."default" NOT NULL,
	  "auth_id" varchar(129) COLLATE "pg_catalog"."default",
	  "app_name" varchar(256) COLLATE "pg_catalog"."default" NOT NULL,
	  "app_param" varchar(256) COLLATE "pg_catalog"."default",
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_param"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_param"."auth_id" IS ''' || 'Authentication table id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_param"."app_name" IS ''' || 'business Module' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_param"."app_param" IS ''' || 'service module parameters (parameters that need to be passed by the gateway) json type' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_param"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_param"."date_updated" IS ''' || 'update time' || '''');

	-- ----------------------------
	-- Primary Key structure for table auth_param
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  ' ALTER TABLE "auth_param" ADD CONSTRAINT "auth_param_pkey" PRIMARY KEY ("id");');
	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;


-- ----------------------------------------------------
-- create table auth_path if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'auth_path' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'auth_path already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "auth_path" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "auth_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "app_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
	  "path" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
	  "enabled" int2 NOT NULL,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_path"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_path"."auth_id" IS ''' || 'auth table id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_path"."app_name" IS ''' || 'module' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_path"."path" IS ''' || 'path' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_path"."enabled" IS ''' || 'whether pass 1 is' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_path"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "auth_path"."date_updated" IS ''' || 'update time' || '''');
	-- ----------------------------
	-- Primary Key structure for table auth_path
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "auth_path" ADD CONSTRAINT "auth_path_pkey" PRIMARY KEY ("id");');
	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;

-- ----------------------------------------------------
-- create table meta_data if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'meta_data' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'meta_data already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "meta_data" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "app_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
	  "path" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
	  "path_desc" varchar(255) COLLATE "pg_catalog"."default",
	  "rpc_type" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "service_name" varchar(255) COLLATE "pg_catalog"."default",
	  "method_name" varchar(255) COLLATE "pg_catalog"."default",
	  "parameter_types" varchar(255) COLLATE "pg_catalog"."default",
	  "rpc_ext" varchar(512) COLLATE "pg_catalog"."default",
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "enabled" int2 NOT NULL
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."id" IS ''' || 'id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."app_name" IS ''' || 'application name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."path" IS ''' || 'path, cannot be repeated' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."path_desc" IS ''' || 'path description' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."rpc_type" IS ''' || 'rpc type' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."service_name" IS ''' || 'service name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."method_name" IS ''' || 'method name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."parameter_types" IS ''' || 'parameter types are provided with multiple parameter types separated by commas' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."rpc_ext" IS ''' || 'rpc extended information, json format' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."date_updated" IS ''' || 'update time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "meta_data"."enabled" IS ''' || 'enabled state' || '''');
	-- ----------------------------
	-- Primary Key structure for table meta_data
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "meta_data" ADD CONSTRAINT "meta_data_pkey" PRIMARY KEY ("id");');
	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;


-- ----------------------------------------------------
-- create table dashboard_user if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'dashboard_user' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'dashboard_user already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "dashboard_user" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "user_name" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "password" varchar(128) COLLATE "pg_catalog"."default",
	  "role" int4 NOT NULL,
	  "enabled" int2 NOT NULL,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "dashboard_user"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "dashboard_user"."user_name" IS ''' || 'user name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "dashboard_user"."password" IS ''' || 'user password' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "dashboard_user"."role" IS ''' || 'role' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "dashboard_user"."enabled" IS ''' || 'delete or not' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "dashboard_user"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "dashboard_user"."date_updated" IS ''' || 'update time' || '''');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "dashboard_user" VALUES (''' || '1' || ''', ''' || 'admin' || ''', ''' || 'ba3253876aed6bc22d4a6ff53d8406c6ad864195ed144ab5c87621b6c233b548baeae6956df346ec8c17f5ea10f35ee3cbc514797ed7ddd3145464e2a0bab413' || ''', 1, 1);');

	-- ----------------------------
	-- Indexes structure for table dashboard_user
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'CREATE INDEX "unique_user_name" ON "dashboard_user" USING btree (
	  "user_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
	);');

	-- ----------------------------
	-- Primary Key structure for table dashboard_user
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "dashboard_user" ADD CONSTRAINT "dashboard_user_pkey" PRIMARY KEY ("id");');

	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;


-- ----------------------------------------------------
-- create table data_permission if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'data_permission' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'data_permission already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "data_permission" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "user_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "data_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "data_type" int4 NOT NULL,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "data_permission"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "data_permission"."user_id" IS ''' || 'user primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "data_permission"."data_id" IS ''' || 'data(selector,rule) primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "data_permission"."data_type" IS ''' || '0 selector type , 1 rule type' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "data_permission"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "data_permission"."date_updated" IS ''' || 'update time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON TABLE "data_permission" IS ''' || 'data permission table' || '''');
	-- ----------------------------
	-- Primary Key structure for table data_permission
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "data_permission" ADD CONSTRAINT "data_permission_pkey" PRIMARY KEY ("id");');
	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;

-- ----------------------------------------------------
-- create table permission if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'permission' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'permission already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "permission" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "object_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "resource_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "permission"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "permission"."object_id" IS ''' || 'user primary key id or role primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "permission"."resource_id" IS ''' || 'resource primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "permission"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "permission"."date_updated" IS ''' || 'update time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON TABLE "permission" IS ''' || 'permission table' || '''');
	-- ----------------------------
	-- Primary Key structure for table permission
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "permission" ADD CONSTRAINT "permission_pkey" PRIMARY KEY ("id");');

	----------------------------
	-- Records of permission
	-- ---------------------------
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708572688384' || ''', ''' || '1346358560427216896' || ''', ''' || '1346775491550474240' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1357956838021890049' || ''', ''' || '1346358560427216896' || ''', ''' || '1357956838021890048' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708597854208' || ''', ''' || '1346358560427216896' || ''', ''' || '1346777449787125760' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708702711808' || ''', ''' || '1346358560427216896' || ''', ''' || '1347034027070337024' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708706906112' || ''', ''' || '1346358560427216896' || ''', ''' || '1347039054925148160' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708711100416' || ''', ''' || '1346358560427216896' || ''', ''' || '1347041326749691904' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708715294720' || ''', ''' || '1346358560427216896' || ''', ''' || '1347046566244003840' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708719489024' || ''', ''' || '1346358560427216896' || ''', ''' || '1347047143350874112' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708723683328' || ''', ''' || '1346358560427216896' || ''', ''' || '1347047203220369408' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708606242816' || ''', ''' || '1346358560427216896' || ''', ''' || '1346777623011880960' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708727877632' || ''', ''' || '1346358560427216896' || ''', ''' || '1347047555588042752' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708732071936' || ''', ''' || '1346358560427216896' || ''', ''' || '1347047640145211392' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708732071937' || ''', ''' || '1346358560427216896' || ''', ''' || '1347047695002513408' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708736266240' || ''', ''' || '1346358560427216896' || ''', ''' || '1347047747305484288' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708610437120' || ''', ''' || '1346358560427216896' || ''', ''' || '1346777766301888512' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708740460544' || ''', ''' || '1346358560427216896' || ''', ''' || '1347048004105940992' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708614631424' || ''', ''' || '1346358560427216896' || ''', ''' || '1346777907096285184' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708744654848' || ''', ''' || '1346358560427216896' || ''', ''' || '1347048101875167232' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708744654849' || ''', ''' || '1346358560427216896' || ''', ''' || '1347048145877610496' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708748849152' || ''', ''' || '1346358560427216896' || ''', ''' || '1347048240677269504' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708753043456' || ''', ''' || '1346358560427216896' || ''', ''' || '1347048316216684544' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708757237760' || ''', ''' || '1346358560427216896' || ''', ''' || '1347048776029843456' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708757237761' || ''', ''' || '1346358560427216896' || ''', ''' || '1347048968414179328' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007709088587777' || ''', ''' || '1346358560427216896' || ''', ''' || '1350804501819195392' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708623020032' || ''', ''' || '1346358560427216896' || ''', ''' || '1346778036402483200' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708761432064' || ''', ''' || '1346358560427216896' || ''', ''' || '1347049029323862016' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708765626368' || ''', ''' || '1346358560427216896' || ''', ''' || '1347049092552994816' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708769820672' || ''', ''' || '1346358560427216896' || ''', ''' || '1347049251395481600' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708774014976' || ''', ''' || '1346358560427216896' || ''', ''' || '1347049317178945536' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708774014977' || ''', ''' || '1346358560427216896' || ''', ''' || '1347049370014593024' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708778209280' || ''', ''' || '1346358560427216896' || ''', ''' || '1347049542417264640' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708782403584' || ''', ''' || '1346358560427216896' || ''', ''' || '1347049598155370496' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708786597888' || ''', ''' || '1346358560427216896' || ''', ''' || '1347049659023110144' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708790792192' || ''', ''' || '1346358560427216896' || ''', ''' || '1347049731047698432' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708585271296' || ''', ''' || '1346358560427216896' || ''', ''' || '1346776175553376256' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708593659904' || ''', ''' || '1346358560427216896' || ''', ''' || '1346777157943259136' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708685934593' || ''', ''' || '1346358560427216896' || ''', ''' || '1347032308726902784' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708690128896' || ''', ''' || '1346358560427216896' || ''', ''' || '1347032395901317120' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708694323200' || ''', ''' || '1346358560427216896' || ''', ''' || '1347032453707214848' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708698517504' || ''', ''' || '1346358560427216896' || ''', ''' || '1347032509051056128' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007708794986496' || ''', ''' || '1346358560427216896' || ''', ''' || '1347049794008395776' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007709080199168' || ''', ''' || '1346358560427216896' || ''', ''' || '1350106119681622016' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007709080199169' || ''', ''' || '1346358560427216896' || ''', ''' || '1350107709494804480' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007709084393472' || ''', ''' || '1346358560427216896' || ''', ''' || '1350107842236137472' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007709084393473' || ''', ''' || '1346358560427216896' || ''', ''' || '1350112406754766848' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1351007709088587776' || ''', ''' || '1346358560427216896' || ''', ''' || '1350112481253994496' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1355167519859040256' || ''', ''' || '1346358560427216896' || ''', ''' || '1355163372527050752' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1355167519859040257' || ''', ''' || '1346358560427216896' || ''', ''' || '1355165158419750912' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1355167519859040258' || ''', ''' || '1346358560427216896' || ''', ''' || '1355165353534578688' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1355167519859040259' || ''', ''' || '1346358560427216896' || ''', ''' || '1355165475785957376' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1355167519859040260' || ''', ''' || '1346358560427216896' || ''', ''' || '1355165608565039104' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1357977745893326848' || ''', ''' || '1346358560427216896' || ''', ''' || '1357977745889132544' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1357977912126177281' || ''', ''' || '1346358560427216896' || ''', ''' || '1357977912126177280' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1357977971827900417' || ''', ''' || '1346358560427216896' || ''', ''' || '1357977971827900416' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "permission" VALUES (''' || '1386680049203195905' || ''', ''' || '1346358560427216896' || ''', ''' || '1386680049203195904' || ''');');
	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;

-- ----------------------------------------------------
-- create table plugin if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'plugin' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'plugin already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "plugin" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "name" varchar(62) COLLATE "pg_catalog"."default" NOT NULL,
	  "config" text COLLATE "pg_catalog"."default",
	  "role" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "sort" int4,
	  "enabled" int2 NOT NULL,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin"."name" IS ''' || 'plugin name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin"."config" IS ''' || 'plugin configuration' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin"."role" IS ''' || 'plug-in role' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin"."sort" IS ''' || 'sort' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin"."enabled" IS ''' || 'whether to open (0, not open, 1 open)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin"."date_updated" IS ''' || 'update time' || '''');
	-- ----------------------------
	-- Primary Key structure for table plugin
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "plugin" ADD CONSTRAINT "plugin_pkey" PRIMARY KEY ("id");');

	-- ----------------------------
	-- Records of plugin
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '1' || ''', ''' || 'sign' || ''', NULL, ''' || 'Authentication' || ''', 20, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '10' || ''', ''' || 'sentinel' || ''', NULL, ''' || 'FaultTolerance' || ''', 140, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '11' || ''', ''' || 'sofa' || ''', ''' || '{"protocol":"zookeeper","register":"127.0.0.1:2181"}' || ''', ''' || 'Proxy' || ''', 310, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '12' || ''', ''' || 'resilience4j' || ''', NULL, ''' || 'FaultTolerance' || ''', 310, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '13' || ''', ''' || 'tars' || ''', ''' || '{"multiSelectorHandle":"1","multiRuleHandle":"0"}' || ''', ''' || 'Proxy' || ''', 310, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '14' || ''', ''' || 'contextPath' || ''', NULL, ''' || 'HttpProcess' || ''', 80, 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '15' || ''', ''' || 'grpc' || ''', ''' || '{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"cached"}' || ''', ''' || 'Proxy' || ''', 310, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '16' || ''', ''' || 'redirect' || ''', NULL, ''' || 'HttpProcess' || ''', 110, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '17' || ''', ''' || 'motan' || ''', ''' || '{"register":"127.0.0.1:2181"}' || ''', ''' || 'Proxy' || ''', 310, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '18' || ''', ''' || 'logging' || ''', NULL, ''' || 'Logging' || ''', 160, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '19' || ''', ''' || 'jwt' || ''', ''' || '{"secretKey":"key"}' || ''', ''' || 'Authentication' || ''', 30, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '2' || ''', ''' || 'waf' || ''', ''' || '{"model":"black"}' || ''', ''' || 'Authentication' || ''', 50, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '20' || ''', ''' || 'request' || ''', NULL, ''' || 'HttpProcess' || ''', 120, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '21' || ''', ''' || 'oauth2' || ''', NULL, ''' || 'Authentication' || ''', 40, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '22' || ''', ''' || 'paramMapping' || ''', ''' || '{"ruleHandlePageType":"custom"}' || ''', ''' || 'HttpProcess' || ''', 70, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '23' || ''', ''' || 'modifyResponse' || ''', ''' || '{"ruleHandlePageType":"custom"}' || ''', ''' || 'HttpProcess' || ''', 220, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '24' || ''', ''' || 'cryptorRequest' || ''', NULL, ''' || 'Cryptor' || ''', 100, 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '25' || ''', ''' || 'cryptorResponse' || ''', NULL, ''' || 'Cryptor' || ''', 410, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '26' || ''', ''' || 'websocket' || ''', ''' || '{"multiSelectorHandle":"1"}' || ''', ''' || 'Proxy' || ''', 200, 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '3' || ''', ''' || 'rewrite' || ''', NULL, ''' || 'HttpProcess' || ''', 90, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '4' || ''', ''' || 'rateLimiter' || ''', ''' || '{"master":"mymaster","mode":"standalone","url":"192.168.1.1:6379","password":"abc"}' || ''', ''' || 'FaultTolerance' || ''', 60, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '5' || ''', ''' || 'divide' || ''', ''' || '{"multiSelectorHandle":"1","multiRuleHandle":"0"}' || ''', ''' || 'Proxy' || ''', 200, 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '6' || ''', ''' || 'dubbo' || ''', ''' || '{"register":"zookeeper://localhost:2181","multiSelectorHandle":"1","threadpool":"cached","corethreads":0,"threads":2147483647,"queues":0}' || ''', ''' || 'Proxy' || ''', 310, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '8' || ''', ''' || 'springCloud' || ''', NULL, ''' || 'Proxy' || ''', 200, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '9' || ''', ''' || 'hystrix' || ''', NULL, ''' || 'FaultTolerance' || ''', 130, 0);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '27' || ''', ''' || 'generalContext' || ''', NULL, ''' || 'Common' || ''', 125, 0);');
    PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '28' || ''', ''' || 'mqtt' || ''', ''' || '{"port": 9500,"bossGroupThreadCount": 1,"maxPayloadSize": 65536,"workerGroupThreadCount": 12,"userName": "shenyu","password": "shenyu","isEncryptPassword": false,"encryptMode": "","leakDetectorLevel": "DISABLED"}' || ''', ''' || 'Proxy' || ''', 125, 0);');
    PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '29' || ''', ''' || 'loggingRocketMQ' || ''', ''' || '{"topic":"shenyu-access-logging", "namesrvAddr": "localhost:9876","producerGroup":"shenyu-plugin-logging-rocketmq"}' || ''', ''' || 'Logging' || ''', 170, 0);');
    PERFORM public.dblink_exec('init_conn',  'INSERT INTO "plugin" VALUES (''' || '30' || ''', ''' || 'cache' || ''', ''' || '{"cacheType":"memory"}' || ''', ''' || 'Cache' || ''', 10, 0);');



PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;

-- ----------------------------------------------------
-- create table plugin_handle if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'plugin_handle' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'plugin_handle already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "plugin_handle" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "plugin_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "field" varchar(100) COLLATE "pg_catalog"."default" NOT NULL,
	  "label" varchar(100) COLLATE "pg_catalog"."default",
	  "data_type" int2 NOT NULL,
	  "type" int2,
	  "sort" int4,
	  "ext_obj" varchar(1024) COLLATE "pg_catalog"."default",
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin_handle"."plugin_id" IS ''' || 'plugin id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin_handle"."field" IS ''' || 'field' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin_handle"."label" IS ''' || 'label' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin_handle"."data_type" IS ''' || 'data type 1 number 2 string' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin_handle"."type" IS ''' || 'type, 1 means selector, 2 means rule, 3 means plugin' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin_handle"."sort" IS ''' || 'sort' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin_handle"."ext_obj" IS ''' || 'extra configuration (json format data)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin_handle"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "plugin_handle"."date_updated" IS ''' || 'update time' || '''');

    ----------------------------
	-- Create Rule for table shenyu_dict
	-- ----------------------------
    PERFORM public.dblink_exec('init_conn',  'create rule plugin_handle_insert_ignore as on insert to plugin_handle where exists (select 1 from plugin_handle where id = new.id) do instead nothing;');

	-- ----------------------------
	-- Primary Key structure for table plugin_handle
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "plugin_handle" ADD CONSTRAINT "plugin_handle_pkey" PRIMARY KEY ("id");');

	-- ----------------------------
	-- Create Sequence for table plugin_handle
	-- ----------------------------
    PERFORM public.dblink_exec('init_conn',  'CREATE SEQUENCE plugin_handle_ID_seq;	');
	PERFORM public.dblink_exec('init_conn',  'ALTER SEQUENCE plugin_handle_ID_seq OWNED BY plugin_handle.ID;');

	-- ----------------------------
	-- Indexes structure for table plugin_handle
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'CREATE INDEX "plugin_id_field_type" ON "plugin_handle" USING btree (
	  "plugin_id" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
	  "field" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
	  "type" "pg_catalog"."int2_ops" ASC NULLS LAST
	);');

    ----------------------------
	-- Records of plugin_handle
	-- ----------------------------
	/*insert "plugin_handle" data for sentinel*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'flowRuleGrade' || ''',''' || 'flowRuleGrade' || ''',''' || '3' || ''', 2, 8, ''' || '{"required":"1","defaultValue":"1","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'flowRuleControlBehavior' || ''',''' || 'flowRuleControlBehavior' || ''',''' || '3' || ''', 2, 5, ''' || '{"required":"1","defaultValue":"0","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'flowRuleEnable' || ''',''' || 'flowRuleEnable (1 or 0)' || ''', ''' || '1' || ''', 2, 7, ''' || '{"required":"1","defaultValue":"1","rule":"/^[01]$/"}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'flowRuleCount' || ''',''' || 'flowRuleCount' || ''',''' || '1' || ''', 2, 6, ''' || '{"required":"1","defaultValue":"0","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'degradeRuleEnable' || ''',''' || 'degradeRuleEnable (1 or 0)' || ''', ''' || '1' || ''', 2, 2, ''' || '{"required":"1","defaultValue":"1","rule":"/^[01]$/"}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'degradeRuleGrade' || ''',''' || 'degradeRuleGrade' || ''',''' || '3' || ''', 2, 3, ''' || '{"required":"1","defaultValue":"0","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'degradeRuleCount' || ''',''' || 'degradeRuleCount' || ''',''' || '1' || ''', 2, 1, ''' || '{"required":"1","defaultValue":"0","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'degradeRuleTimeWindow' || ''',''' || 'degradeRuleTimeWindow' || ''',''' || '1' || ''', 2, 4, ''' || '{"required":"1","defaultValue":"0","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'degradeRuleMinRequestAmount' || ''',''' || 'degradeRuleMinRequestAmount' || ''',''' || '1' || ''', 2, 3, ''' || '{"required":"1","defaultValue":"5","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'degradeRuleStatIntervals' || ''',''' || 'degradeRuleStatIntervals' || ''',''' || '1' || ''', 2, 3, ''' || '{"required":"1","defaultValue":"1","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''' ,''' || 'degradeRuleSlowRatioThreshold' || ''',''' || 'degradeRuleSlowRatioThreshold' || ''',''' || '1' || ''', 2, 3, ''' || '{"required":"1","defaultValue":"0.5","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '10' || ''', ''' || 'fallbackUri' || ''', ''' || 'fallbackUri' || ''', 2, 2, 9, ''' || '{"required":"0","rule":""}' || ''');');

    /*insert "plugin_handle" data for waf*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '2' || ''' ,''' || 'permission' || ''',''' || 'permission' || ''',''' || '3' || ''', 2, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '2' || ''' ,''' || 'statusCode' || ''',''' || 'statusCode' || ''',''' || '2' || ''', 2, 2);');

    /*insert "plugin_handle" data for rateLimiter*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '4' || ''' ,''' || 'replenishRate' || ''',''' || 'replenishRate' || ''', 2, 2, 2, ''' || '{"required":"1","defaultValue":"10","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '4' || ''' ,''' || 'burstCapacity' || ''',''' || 'burstCapacity' || ''', 2, 2, 3, ''' || '{"required":"1","defaultValue":"100","rule":""}' || ''');');

    /*insert "plugin_handle" data for rewrite*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '3' || ''', ''' || 'regex' || ''', ''' || 'regex' || ''', 2, 2, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '3' || ''', ''' || 'replace' || ''', ''' || 'replace' || ''', 2, 2, 2);');

    /*insert "plugin_handle" data for redirect*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '16' || ''' ,''' || 'redirectURI' || ''',''' || 'redirectURI' || ''', 2, 2, 1);');

    /*insert "plugin_handle" data for springCloud*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '8' || ''' ,''' || 'path' || ''',''' || 'path' || ''', 2, 2, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '8' || ''' ,''' || 'timeout' || ''',''' || 'timeout (ms)' || ''', 1, 2, 2);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '8' || ''' ,''' || 'serviceId' || ''',''' || 'serviceId' || ''', 2, 1, 1);');

    /*insert "plugin_handle" data for resilience4j*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'timeoutDurationRate' || ''',''' || 'timeoutDurationRate (ms)' || ''', 1, 2, 1, ''' || '{"required":"1","defaultValue":"5000","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'limitRefreshPeriod' || ''',''' || 'limitRefreshPeriod (ms)' || ''', 1, 2, 0, ''' || '{"required":"1","defaultValue":"500","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'limitForPeriod' || ''',''' || 'limitForPeriod' || ''', 1, 2, 0, ''' || '{"required":"1","defaultValue":"50","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'circuitEnable' || ''',''' || 'circuitEnable' || ''', 1, 2, 2, ''' || '{"required":"1","defaultValue":"0","rule":"/^[01]$/"}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'timeoutDuration' || ''',''' || 'timeoutDuration (ms)' || ''', 1, 2, 2, ''' || '{"required":"1","defaultValue":"30000","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '12' || ''' ,''' || 'fallbackUri' || ''',''' || 'fallbackUri' || ''', 2, 2, 2);');

    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'slidingWindowSize' || ''',''' || 'slidingWindowSize' || ''', 1, 2, 2, ''' || '{"required":"1","defaultValue":"100","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'slidingWindowType' || ''',''' || 'slidingWindowType' || ''', 1, 2, 2, ''' || '{"required":"1","defaultValue":"0","rule":"/^[01]$/"}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'minimumNumberOfCalls' || ''',''' || 'minimumNumberOfCalls' || ''', 1, 2, 2, ''' || '{"required":"1","defaultValue":"100","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'waitIntervalFunctionInOpenState' || ''',''' || 'waitIntervalInOpen' || ''', 1, 2, 2, ''' || '{"required":"1","defaultValue":"60000","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'permittedNumberOfCallsInHalfOpenState' || ''',''' || 'bufferSizeInHalfOpen' || ''', 1, 2, 2, ''' || '{"required":"1","defaultValue":"10","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''' ,''' || 'failureRateThreshold' || ''',''' || 'failureRateThreshold' || ''', 1, 2, 2, ''' || '{"required":"1","defaultValue":"50","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '12' || ''', ''' || 'automaticTransitionFromOpenToHalfOpenEnabled' || ''', ''' || 'automaticHalfOpen' || ''', 3, 2, 1, ''' || '{"required":"1","defaultValue":"true","rule":""}' || ''');');

    /*insert "plugin_handle" data for plugin*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '4' || ''', ''' || 'mode' || ''', ''' || 'mode' || ''', 3, 3, 1, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '4' || ''', ''' || 'master' || ''', ''' || 'master' || ''', 2, 3, 2, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '4' || ''', ''' || 'url' || ''', ''' || 'url' || ''', 2, 3, 3, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '4' || ''', ''' || 'password' || ''', ''' || 'password' || ''', 2, 3, 4, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '11' || ''', ''' || 'protocol' || ''', ''' || 'protocol' || ''', 2, 3, 1, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '11' || ''', ''' || 'register' || ''', ''' || 'register' || ''', 2, 3, 2, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '2' || ''', ''' || 'model' || ''', ''' || 'model' || ''', 2, 3, 1, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'register' || ''', ''' || 'register' || ''', 2, 3, 1, NULL);');

    /*insert "plugin_handle" data for plugin rateLimiter*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '4' || ''' ,''' || 'algorithmName' || ''',''' || 'algorithmName' || ''',''' || '3' || ''', 2, 1, ''' || '{"required":"1","defaultValue":"slidingWindow","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '4' || ''' ,''' || 'keyResolverName' || ''',''' || 'keyResolverName' || ''',''' || '3' || ''', 2, 4, ''' || '{"required":"1","defaultValue":"WHOLE_KEY_RESOLVER","rule":""}' || ''');');

    /*insert "plugin_handle" data for divide*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'upstreamHost' || ''', ''' || 'host' || ''', 2, 1, 0, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'protocol' || ''', ''' || 'protocol' || ''', 2, 1, 2, ''' || '{"required":"0","defaultValue":"","placeholder":"http://","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'upstreamUrl' || ''', ''' || 'ip:port' || ''', 2, 1, 1, ''' || '{"required":"1","placeholder":"","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'weight' || ''', ''' || 'weight' || ''', 1, 1, 3, ''' || '{"defaultValue":"50","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'timestamp' || ''', ''' || 'startupTime' || ''', 1, 1, 3, ''' || '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'warmup' || ''', ''' || 'warmupTime' || ''', 1, 1, 5, ''' || '{"defaultValue":"0","placeholder":"warmup time (ms)","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'status' || ''', ''' || 'status' || ''', 3, 1, 6, ''' || '{"defaultValue":"true","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'loadBalance' || ''', ''' || 'loadStrategy' || ''', 3, 2, 0, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'retry' || ''', ''' || 'retryCount' || ''', 1, 2, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'timeout' || ''', ''' || 'timeout' || ''', 1, 2, 2, ''' || '{"defaultValue":"3000","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'multiSelectorHandle' || ''', ''' || 'multiSelectorHandle' || ''', 3, 3, 0, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'multiRuleHandle' || ''', ''' || 'multiRuleHandle' || ''', 3, 3, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'headerMaxSize' || ''', ''' || 'headerMaxSize' || ''', 1, 2, 3, ''' || '{"defaultValue":"10240","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'requestMaxSize' || ''', ''' || 'requestMaxSize' || ''', 1, 2, 4, ''' || '{"defaultValue":"102400","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '5' || ''', ''' || 'retryStrategy' || ''', ''' || 'retryStrategy' || ''', 3, 2, 0, ''' || '{"required":"0","defaultValue":"current","placeholder":"retryStrategy","rule":""}' || ''');');

    /*insert "plugin_handle" data for tars*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'upstreamHost' || ''', ''' || 'host' || ''', 2, 1, 0, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'protocol' || ''', ''' || 'protocol' || ''', 2, 1, 2, ''' || '{"defaultValue":"","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'upstreamUrl' || ''', ''' || 'ip:port' || ''', 2, 1, 1, ''' || '{"required":"1","placeholder":"","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'weight' || ''', ''' || 'weight' || ''', 1, 1, 3, ''' || '{"defaultValue":"50","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'timestamp' || ''', ''' || 'startupTime' || ''', 1, 1, 3, ''' || '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'warmup' || ''', ''' || 'warmupTime' || ''', 1, 1, 5, ''' || '{"defaultValue":"0","placeholder":"warmup time (ms)","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'status' || ''', ''' || 'status' || ''', 3, 1, 6, ''' || '{"defaultValue":"true","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'loadBalance' || ''', ''' || 'loadStrategy' || ''', 3, 2, 0, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'retry' || ''', ''' || 'retryCount' || ''', 1, 2, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'timeout' || ''', ''' || 'timeout' || ''', 1, 2, 2, ''' || '{"defaultValue":"3000","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'multiSelectorHandle' || ''', ''' || 'multiSelectorHandle' || ''', 3, 3, 0, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '13' || ''', ''' || 'multiRuleHandle' || ''', ''' || 'multiRuleHandle' || ''', 3, 3, 1, null);');

    /*insert "plugin_handle" data for grpc*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '15' || ''', ''' || 'upstreamUrl' || ''', ''' || 'ip:port' || ''', 2, 1, 1, ''' || '{"required":"1","placeholder":"","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '15' || ''', ''' || 'weight' || ''', ''' || 'weight' || ''', 1, 1, 3, ''' || '{"defaultValue":"50","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '15' || ''', ''' || 'status' || ''', ''' || 'status' || ''', 3, 1, 6, ''' || '{"defaultValue":"true","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '15' || ''', ''' || 'multiSelectorHandle' || ''', ''' || 'multiSelectorHandle' || ''', 3, 3, 0, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '15' || ''', ''' || 'multiRuleHandle' || ''', ''' || 'multiRuleHandle' || ''', 3, 3, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '15' || ''', ''' || 'threadpool' || ''', ''' || 'threadpool' || ''', 3, 3, 0, ''' || '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}' || ''');');

    /*insert "plugin_handle" data for context path*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '14' || ''', ''' || 'contextPath' || ''', ''' || 'contextPath' || ''', 2, 2, 0);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort ) VALUES (''' || '14' || ''', ''' || 'addPrefix' || ''', ''' || 'addPrefix' || ''', 2, 2, 0);');

    /*insert "plugin_handle" data for request*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '20' || ''', ''' || 'ruleHandlePageType' || ''', ''' || 'ruleHandlePageType' || ''', 3, 3, 0, ''' || '{"required":"0","rule":""}' || ''');');

    /*insert "plugin_handle" data for plugin jwt*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '19' || ''' ,''' || 'secretKey' || ''',''' || 'secretKey' || ''',2, 3, 0, null);');

    /*insert "plugin_handle" data for plugin Cryptor*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '24' || ''', ''' || 'strategyName' || ''', ''' || 'strategyName' || ''', 3, 2, 1, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '24' || ''', ''' || 'fieldNames' || ''', ''' || 'fieldNames' || ''', 2, 2, 3, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '24' || ''', ''' || 'decryptKey' || ''', ''' || 'decryptKey' || ''', 2, 2, 3, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '24' || ''', ''' || 'encryptKey' || ''', ''' || 'encryptKey' || ''', 2, 2, 3, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '24' || ''', ''' || 'way' || ''', ''' || 'way' || ''', 3, 2, 3, NULL);');

    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '25' || ''', ''' || 'strategyName' || ''', ''' || 'strategyName' || ''', 3, 2, 2, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '25' || ''', ''' || 'decryptKey' || ''', ''' || 'decryptKey' || ''', 2, 2, 3, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '25' || ''', ''' || 'encryptKey' || ''', ''' || 'encryptKey' || ''', 2, 2, 3, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '25' || ''', ''' || 'fieldNames' || ''', ''' || 'fieldNames' || ''', 2, 2, 4, NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '25' || ''', ''' || 'way' || ''', ''' || 'way' || ''', 3, 2, 3, NULL);');

    /*insert "plugin_handle" data for dubbo*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'gray' || ''', ''' || 'gray' || ''', ''' || '3' || ''', ''' || '1' || ''', ''' || '9' || ''', ''' || '{"required":"0","defaultValue":"false","placeholder":"gray","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'group' || ''', ''' || 'group' || ''', ''' || '2' || ''', ''' || '1' || ''', ''' || '3' || ''', ''' || '{"required":"0","placeholder":"group","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'loadbalance' || ''', ''' || 'loadbalance' || ''', ''' || '3' || ''', ''' || '2' || ''', ''' || '0' || ''', ''' || '{"required":"0","placeholder":"loadbalance","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'multiSelectorHandle' || ''', ''' || 'multiSelectorHandle' || ''', ''' || '3' || ''', ''' || '3' || ''', ''' || '0' || ''', NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'protocol' || ''', ''' || 'protocol' || ''', ''' || '2' || ''', ''' || '1' || ''', ''' || '2' || ''', ''' || '{"required":"0","defaultValue":"","placeholder":"http://","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'status' || ''', ''' || 'status' || ''', ''' || '3' || ''', ''' || '1' || ''', ''' || '8' || ''', ''' || '{"defaultValue":"true","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'timestamp' || ''', ''' || 'startupTime' || ''', ''' || '1' || ''', ''' || '1' || ''', ''' || '7' || ''', ''' || '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'upstreamHost' || ''', ''' || 'host' || ''', ''' || '2' || ''', ''' || '1' || ''', ''' || '0' || ''', NULL);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'upstreamUrl' || ''', ''' || 'ip:port' || ''', ''' || '2' || ''', ''' || '1' || ''', ''' || '1' || ''', ''' || '{"required":"1","placeholder":"","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'version' || ''', ''' || 'version' || ''', ''' || '2' || ''', ''' || '1' || ''', ''' || '4' || ''', ''' || '{"required":"0","placeholder":"version","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'warmup' || ''', ''' || 'warmupTime' || ''', ''' || '1' || ''', ''' || '1' || ''', ''' || '6' || ''', ''' || '{"defaultValue":"0","placeholder":"warmup time (ms)","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'weight' || ''', ''' || 'weight' || ''', ''' || '1' || ''', ''' || '1' || ''', ''' || '5' || ''', ''' || '{"defaultValue":"50","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'threadpool' || ''', ''' || 'threadpool' || ''', ''' || '3' || ''', ''' || '3' || ''', ''' || '0' || ''', ''' || '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'corethreads' || ''', ''' || 'corethreads' || ''', ''' || '1' || ''', ''' || '3' || ''', ''' || '0' || ''', ''' || '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'threads' || ''', ''' || 'threads' || ''', ''' || '1' || ''', ''' || '3' || ''', ''' || '0' || ''', ''' || '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id ,  field ,  label ,  data_type ,  type ,  sort ,  ext_obj ) VALUES (''' || '6' || ''', ''' || 'queues' || ''', ''' || 'queues' || ''', ''' || '1' || ''', ''' || '3' || ''', ''' || '0' || ''', ''' || '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}' || ''');');

    /*insert "plugin_handle" data for websocket*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'host' || ''', ''' || 'host' || ''', 2, 1, 0, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'protocol' || ''', ''' || 'protocol' || ''', 2, 1, 2, ''' || '{"required":"0","defaultValue":"","placeholder":"ws://","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'url' || ''', ''' || 'ip:port' || ''', 2, 1, 1, ''' || '{"required":"1","placeholder":"","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'weight' || ''', ''' || 'weight' || ''', 1, 1, 3, ''' || '{"defaultValue":"50","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'timestamp' || ''', ''' || 'startupTime' || ''', 1, 1, 3, ''' || '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'warmup' || ''', ''' || 'warmupTime' || ''', 1, 1, 5, ''' || '{"defaultValue":"0","placeholder":"warmup time (ms)","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'status' || ''', ''' || 'status' || ''', 3, 1, 6, ''' || '{"defaultValue":"true","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'loadBalance' || ''', ''' || 'loadStrategy' || ''', 3, 2, 0, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'retry' || ''', ''' || 'retryCount' || ''', 1, 2, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'timeout' || ''', ''' || 'timeout' || ''', 1, 2, 2, ''' || '{"defaultValue":"3000","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '26' || ''', ''' || 'multiSelectorHandle' || ''', ''' || 'multiSelectorHandle' || ''', 3, 3, 0, null);');

    /*insert "plugin_handle" data for plugin motan*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '17' || ''', ''' || 'register' || ''', ''' || 'register' || ''', 2, 3, 0, null);');

    /*insert plugin_handle data for plugin mqtt*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '28' || ''', ''' || 'port' || ''', ''' || 'port' || ''', 1, 3, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '28' || ''', ''' || 'bossGroupThreadCount' || ''', ''' || 'bossGroupThreadCount' || ''', 1, 3, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '28' || ''', ''' || 'maxPayloadSize' || ''', ''' || 'maxPayloadSize' || ''', 1, 3, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '28' || ''', ''' || 'workerGroupThreadCount' || ''', ''' || 'workerGroupThreadCount' || ''', 1, 3, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '28' || ''', ''' || 'userName' || ''', ''' || 'userName' || ''', 2, 3, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '28' || ''', ''' || 'password' || ''', ''' || 'password' || ''', 2, 3, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '28' || ''', ''' || 'isEncryptPassword' || ''', ''' || 'isEncryptPassword' || ''', 2, 3, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '28' || ''', ''' || 'encryptMode' || ''', ''' || 'encryptMode' || ''', 2, 3, 1, null);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '28' || ''', ''' || 'leakDetectorLevel' || ''', ''' || 'leakDetectorLevel' || ''', 2, 3, 1, null);');

    /*insert plugin_handle data for plugin loggingRocketMQ*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '29' || ''', ''' || 'topic' || ''', ''' || 'topic' || ''', 2, 3, 1, ''' || '{"required":"1","defaultValue":"shenyu-access-logging"}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '29' || ''', ''' || 'namesrvAddr' || ''', ''' || 'namesrvAddr' || ''', 2, 3, 2, ''' || '{"required":"1","defaultValue":"localhost:9876"}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '29' || ''', ''' || 'producerGroup' || ''', ''' || 'producerGroup' || ''', 2, 3, 3, ''' || '{"required":"1","defaultValue":"shenyu-plugin-logging-rocketmq"}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '29' || ''', ''' || 'sampleRate' || ''', ''' || 'sampleRate' || ''', 2, 3, 4, ''' || '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '29' || ''', ''' || 'maxResponseBody' || ''', ''' || 'maxResponseBody' || ''', 1, 3, 5, ''' || '{"required":"0","defaultValue":524288}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '29' || ''', ''' || 'maxRequestBody' || ''', ''' || 'maxRequestBody' || ''', 1, 3, 6, ''' || '{"required":"0","defaultValue":524288}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '29' || ''', ''' || 'compressAlg' || ''', ''' || 'compressAlg' || ''', 3, 3, 7, ''' || '{"required":"0","defaultValue":"none"}' || ''');');

    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '29' || ''', ''' || 'topic' || ''', ''' || 'topic' || ''', 2, 1, 1, ''' || '{"required":"0","defaultValue":"","placeholder":"optional"}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '29' || ''', ''' || 'sampleRate' || ''', ''' || 'sampleRate' || ''', 2, 1, 2, ''' || '{"required":"0","defaultValue":"","placeholder":"optional,0,0.01~1"}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'cacheType' || ''', ''' || 'cacheType' || ''', 3, 3, 1, ''' || '{"required":"1","defaultValue":"memory","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'database' || ''', ''' || 'database' || ''', 1, 3, 2, ''' || '{"required":"0","defaultValue":"0","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'master' || ''', ''' || 'master' || ''', 2, 3, 3, ''' || '{"required":"0","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'mode' || ''', ''' || 'mode' || ''', 2, 3, 4, ''' || '{"required":"0","defaultValue":"standalone","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'url' || ''', ''' || 'url' || ''', 2, 3, 5, ''' || '{"required":"0","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'password' || ''', ''' || 'password' || ''', 2, 3, 6, ''' || '{"required":"0","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'maxIdle' || ''', ''' || 'maxIdle' || ''', 1, 3, 7, ''' || '{"required":"0","defaultValue":"8","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'minIdle' || ''', ''' || 'minIdle' || ''', 1, 3, 8, ''' || '{"required":"0","defaultValue":"0","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'maxActive' || ''', ''' || 'maxActive' || ''', 1, 3, 9, ''' || '{"required":"0","defaultValue":"8","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'maxWait' || ''', ''' || 'maxWait' || ''', 3, 3, 10, ''' || '{"required":"0","defaultValue":"-1","rule":""}' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO "plugin_handle" ( plugin_id , field , label , data_type , type , sort , ext_obj ) VALUES (''' || '30' || ''', ''' || 'timeoutSeconds' || ''', ''' || 'timeoutSeconds' || ''', 1, 2, 0, ''' || '{"required":"0","defaultValue":"60","rule":""}' || ''');');


PERFORM public.dblink_exec('init_conn', 'COMMIT');

END IF;


-- ----------------------------------------------------
-- create table resource if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'resource' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'resource already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "resource" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "parent_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "title" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "name" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
	  "url" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
	  "component" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
	  "resource_type" int4 NOT NULL,
	  "sort" int4 NOT NULL,
	  "icon" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
	  "is_leaf" int2 NOT NULL,
	  "is_route" int4 NOT NULL,
	  "perms" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "status" int4 NOT NULL,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');

	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."parent_id" IS ''' || 'resource parent primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."title" IS ''' || 'title' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."name" IS ''' || 'route name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."url" IS ''' || 'route url' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."component" IS ''' || 'component' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."resource_type" IS ''' || 'resource type eg 0:main menu 1:child menu 2:function button' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."sort" IS ''' || 'sort' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."icon" IS ''' || 'icon' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."is_leaf" IS ''' || 'leaf node 0:no 1:yes' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."is_route" IS ''' || 'route 1:yes 0:no' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."perms" IS ''' || 'button permission description sys:user:add(add)/sys:user:edit(edit)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."status" IS ''' || 'status 1:enable 0:disable' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "resource"."date_updated" IS ''' || 'update time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON TABLE "resource" IS ''' || 'resource table' || '''');
	-- ----------------------------
	-- Primary Key structure for table resource
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "resource" ADD CONSTRAINT "resource_pkey" PRIMARY KEY ("id");');

	-- ----------------------------
	-- Records of resource
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1346775491550474240' || ''', ''' || '' || ''', ''' || 'SHENYU.MENU.PLUGIN.LIST' || ''', ''' || 'plug' || ''', ''' || '/plug' || ''', ''' || 'PluginList' || ''', 0, 0, ''' || 'dashboard' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1346776175553376256' || ''', ''' || '' || ''', ''' || 'SHENYU.MENU.SYSTEM.MANAGMENT' || ''', ''' || 'system' || ''', ''' || '/system' || ''', ''' || 'system' || ''', 0, 2, ''' || 'setting' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1346777157943259136' || ''', ''' || '1346776175553376256' || ''', ''' || 'SHENYU.MENU.SYSTEM.MANAGMENT.USER' || ''', ''' || 'manage' || ''', ''' || '/system/manage' || ''', ''' || 'manage' || ''', 1, 1, ''' || 'user' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1346777449787125760' || ''', ''' || '1357956838021890048' || ''', ''' || 'SHENYU.MENU.SYSTEM.MANAGMENT.PLUGIN' || ''', ''' || 'plugin' || ''', ''' || '/config/plugin' || ''', ''' || 'plugin' || ''', 1, 2, ''' || 'book' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1346777623011880960' || ''', ''' || '1357956838021890048' || ''', ''' || 'SHENYU.PLUGIN.PLUGINHANDLE' || ''', ''' || 'pluginhandle' || ''', ''' || '/config/pluginhandle' || ''', ''' || 'pluginhandle' || ''', 1, 3, ''' || 'down-square' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1346777766301888512' || ''', ''' || '1357956838021890048' || ''', ''' || 'SHENYU.MENU.SYSTEM.MANAGMENT.AUTHEN' || ''', ''' || 'auth' || ''', ''' || '/config/auth' || ''', ''' || 'auth' || ''', 1, 4, ''' || 'audit' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1346777907096285184' || ''', ''' || '1357956838021890048' || ''', ''' || 'SHENYU.MENU.SYSTEM.MANAGMENT.METADATA' || ''', ''' || 'metadata' || ''', ''' || '/config/metadata' || ''', ''' || 'metadata' || ''', 1, 5, ''' || 'snippets' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1346778036402483200' || ''', ''' || '1357956838021890048' || ''', ''' || 'SHENYU.MENU.SYSTEM.MANAGMENT.DICTIONARY' || ''', ''' || 'dict' || ''', ''' || '/config/dict' || ''', ''' || 'dict' || ''', 1, 6, ''' || 'ordered-list' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347032308726902784' || ''', ''' || '1346777157943259136' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ADD' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 0, ''' || '' || ''', 1, 0, ''' || 'system:manager:add' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347032395901317120' || ''', ''' || '1346777157943259136' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.LIST' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 1, ''' || '' || ''', 1, 0, ''' || 'system:manager:list' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347032453707214848' || ''', ''' || '1346777157943259136' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.DELETE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 2, ''' || '' || ''', 1, 0, ''' || 'system:manager:delete' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347032509051056128' || ''', ''' || '1346777157943259136' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.EDIT' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 3, ''' || '' || ''', 1, 0, ''' || 'system:manager:edit' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347034027070337024' || ''', ''' || '1346777449787125760' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.LIST' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 0, ''' || '' || ''', 1, 0, ''' || 'system:plugin:list' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347039054925148160' || ''', ''' || '1346777449787125760' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.DELETE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 1, ''' || '' || ''', 1, 0, ''' || 'system:plugin:delete' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347041326749691904' || ''', ''' || '1346777449787125760' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ADD' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 2, ''' || '' || ''', 1, 0, ''' || 'system:plugin:add' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347046566244003840' || ''', ''' || '1346777449787125760' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.SYNCHRONIZE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 3, ''' || '' || ''', 1, 0, ''' || 'system:plugin:modify' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347047143350874112' || ''', ''' || '1346777449787125760' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ENABLE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 4, ''' || '' || ''', 1, 0, ''' || 'system:plugin:disable' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347047203220369408' || ''', ''' || '1346777449787125760' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.EDIT' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 5, ''' || '' || ''', 1, 0, ''' || 'system:plugin:edit' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347047555588042752' || ''', ''' || '1346777623011880960' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.LIST' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 0, ''' || '' || ''', 1, 0, ''' || 'system:pluginHandler:list' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347047640145211392' || ''', ''' || '1346777623011880960' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.DELETE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 1, ''' || '' || ''', 1, 0, ''' || 'system:pluginHandler:delete' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347047695002513408' || ''', ''' || '1346777623011880960' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ADD' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 2, ''' || '' || ''', 1, 0, ''' || 'system:pluginHandler:add' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347047747305484288' || ''', ''' || '1346777623011880960' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.EDIT' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 3, ''' || '' || ''', 1, 0, ''' || 'system:pluginHandler:edit' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347048004105940992' || ''', ''' || '1346777766301888512' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.LIST' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 0, ''' || '' || ''', 1, 0, ''' || 'system:authen:list' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347048101875167232' || ''', ''' || '1346777766301888512' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.DELETE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 1, ''' || '' || ''', 1, 0, ''' || 'system:authen:delete' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347048145877610496' || ''', ''' || '1346777766301888512' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ADD' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 2, ''' || '' || ''', 1, 0, ''' || 'system:authen:add' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347048240677269504' || ''', ''' || '1346777766301888512' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ENABLE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 3, ''' || '' || ''', 1, 0, ''' || 'system:authen:disable' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347048316216684544' || ''', ''' || '1346777766301888512' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.SYNCHRONIZE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 4, ''' || '' || ''', 1, 0, ''' || 'system:authen:modify' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347048776029843456' || ''', ''' || '1346777766301888512' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.EDIT' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 5, ''' || '' || ''', 1, 0, ''' || 'system:authen:edit' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347048968414179328' || ''', ''' || '1346777907096285184' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.LIST' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 0, ''' || '' || ''', 1, 0, ''' || 'system:meta:list' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347049029323862016' || ''', ''' || '1346777907096285184' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.DELETE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 1, ''' || '' || ''', 1, 0, ''' || 'system:meta:delete' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347049092552994816' || ''', ''' || '1346777907096285184' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ADD' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 2, ''' || '' || ''', 1, 0, ''' || 'system:meta:add' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347049251395481600' || ''', ''' || '1346777907096285184' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ENABLE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 3, ''' || '' || ''', 1, 0, ''' || 'system:meta:disable' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347049317178945536' || ''', ''' || '1346777907096285184' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.SYNCHRONIZE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 4, ''' || '' || ''', 1, 0, ''' || 'system:meta:modify' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347049370014593024' || ''', ''' || '1346777907096285184' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.EDIT' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 5, ''' || '' || ''', 1, 0, ''' || 'system:meta:edit' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347049542417264640' || ''', ''' || '1346778036402483200' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.LIST' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 0, ''' || '' || ''', 1, 0, ''' || 'system:dict:list' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347049598155370496' || ''', ''' || '1346778036402483200' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.DELETE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 1, ''' || '' || ''', 1, 0, ''' || 'system:dict:delete' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347049659023110144' || ''', ''' || '1346778036402483200' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ADD' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 2, ''' || '' || ''', 1, 0, ''' || 'system:dict:add' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347049731047698432' || ''', ''' || '1346778036402483200' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ENABLE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 3, ''' || '' || ''', 1, 0, ''' || 'system:dict:disable' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1347049794008395776' || ''', ''' || '1346778036402483200' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.EDIT' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 4, ''' || '' || ''', 1, 0, ''' || 'system:dict:edit' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1350106119681622016' || ''', ''' || '1346776175553376256' || ''', ''' || 'SHENYU.MENU.SYSTEM.MANAGMENT.ROLE' || ''', ''' || 'role' || ''', ''' || '/system/role' || ''', ''' || 'role' || ''', 1, 0, ''' || 'usergroup-add' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1350107709494804480' || ''', ''' || '1350106119681622016' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.ADD' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 0, ''' || '' || ''', 1, 0, ''' || 'system:role:add' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1350107842236137472' || ''', ''' || '1350106119681622016' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.LIST' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 1, ''' || '' || ''', 1, 0, ''' || 'system:role:list' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1350112406754766848' || ''', ''' || '1350106119681622016' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.DELETE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 2, ''' || '' || ''', 1, 0, ''' || 'system:role:delete' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1350112481253994496' || ''', ''' || '1350106119681622016' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.EDIT' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 3, ''' || '' || ''', 1, 0, ''' || 'system:role:edit' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1350804501819195392' || ''', ''' || '1346777766301888512' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.EDITRESOURCEDETAILS' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 6, ''' || '' || ''', 1, 0, ''' || 'system:authen:editResourceDetails' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1355163372527050752' || ''', ''' || '1346776175553376256' || ''', ''' || 'SHENYU.MENU.SYSTEM.MANAGMENT.RESOURCE' || ''', ''' || 'resource' || ''', ''' || '/system/resource' || ''', ''' || 'resource' || ''', 1, 2, ''' || 'menu' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1355165158419750912' || ''', ''' || '1355163372527050752' || ''', ''' || 'SHENYU.BUTTON.RESOURCE.MENU.ADD' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 1, ''' || '' || ''', 1, 0, ''' || 'system:resource:addMenu' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1355165353534578688' || ''', ''' || '1355163372527050752' || ''', ''' || 'SHENYU.BUTTON.SYSTEM.LIST' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 0, ''' || '' || ''', 1, 0, ''' || 'system:resource:list' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1355165475785957376' || ''', ''' || '1355163372527050752' || ''', ''' || 'SHENYU.BUTTON.RESOURCE.MENU.DELETE' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 2, ''' || '' || ''', 1, 0, ''' || 'system:resource:deleteMenu' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1355165608565039104' || ''', ''' || '1355163372527050752' || ''', ''' || 'SHENYU.BUTTON.RESOURCE.MENU.EDIT' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 3, ''' || '' || ''', 1, 0, ''' || 'system:resource:editMenu' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1357956838021890048' || ''', ''' || '' || ''', ''' || 'SHENYU.MENU.CONFIG.MANAGMENT' || ''', ''' || 'config' || ''', ''' || '/config' || ''', ''' || 'config' || ''', 0, 1, ''' || 'api' || ''', 0, 0, ''' || '' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1357977745889132544' || ''', ''' || '1355163372527050752' || ''', ''' || 'SHENYU.BUTTON.RESOURCE.BUTTON.ADD' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 4, ''' || '' || ''', 1, 0, ''' || 'system:resource:addButton' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1357977912126177280' || ''', ''' || '1355163372527050752' || ''', ''' || 'SHENYU.SYSTEM.EDITOR' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 5, ''' || '' || ''', 1, 0, ''' || 'system:resource:editButton' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1357977971827900416' || ''', ''' || '1355163372527050752' || ''', ''' || 'SHENYU.SYSTEM.DELETEDATA' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 6, ''' || '' || ''', 1, 0, ''' || 'system:resource:deleteButton' || ''', 1);');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "resource" VALUES (''' || '1386680049203195904' || ''', ''' || '1346777157943259136' || ''', ''' || 'SHENYU.BUTTON.DATA.PERMISSION.CONFIG' || ''', ''' || '' || ''', ''' || '' || ''', ''' || '' || ''', 2, 0, ''' || '' || ''', 1, 0, ''' || 'system:manager:configureDataPermission' || ''', 1);');

	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;

-- ----------------------------------------------------
-- create table role if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'role' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'role already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "role" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "role_name" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
	  "description" varchar(255) COLLATE "pg_catalog"."default",
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "role"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "role"."role_name" IS ''' || 'role name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "role"."description" IS ''' || 'role describe' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "role"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "role"."date_updated" IS ''' || 'update time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON TABLE "role" IS ''' || 'role table' || '''');
	-- ----------------------------
	-- Primary Key structure for table role
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "role" ADD CONSTRAINT "role_pkey" PRIMARY KEY ("id", "role_name");');
	-- ----------------------------
	-- Records of role
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "role" VALUES (''' || '1346358560427216896' || ''', ''' || 'super' || ''', ''' || 'Administrator' || ''');');
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "role" VALUES (''' || '1385482862971723776' || ''', ''' || 'default' || ''', ''' || 'Standard' || ''');');

	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;


-- ----------------------------------------------------
-- create table rule if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'rule' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'rule already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "rule" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "selector_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "match_mode" int4 NOT NULL,
	  "name" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "enabled" int2 NOT NULL,
	  "loged" int2 NOT NULL,
	  "sort" int4 NOT NULL,
	  "handle" varchar(1024) COLLATE "pg_catalog"."default",
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule"."selector_id" IS ''' || 'selector id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule"."match_mode" IS ''' || 'matching mode (0 and 1 or)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule"."name" IS ''' || 'rule name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule"."enabled" IS ''' || 'whether to open' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule"."loged" IS ''' || 'whether to log or not' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule"."sort" IS ''' || 'sort' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule"."handle" IS ''' || 'processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule"."date_updated" IS ''' || 'update time' || '''');
	-- ----------------------------
	-- Primary Key structure for table rule
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "rule" ADD CONSTRAINT "rule_pkey" PRIMARY KEY ("id");');
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;

-- ----------------------------------------------------
-- create table rule_condition if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'rule_condition' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'rule_condition already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "rule_condition" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "rule_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "param_type" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "operator" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "param_name" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "param_value" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule_condition"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule_condition"."rule_id" IS ''' || 'rule id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule_condition"."param_type" IS ''' || 'parameter type (post query uri, etc.)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule_condition"."operator" IS ''' || 'matching character (=> <like match)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule_condition"."param_name" IS ''' || 'parameter name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule_condition"."param_value" IS ''' || 'parameter value' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule_condition"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "rule_condition"."date_updated" IS ''' || 'update time' || '''');
	-- ----------------------------
	-- Primary Key structure for table rule_condition
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "rule_condition" ADD CONSTRAINT "rule_condition_pkey" PRIMARY KEY ("id");');
	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;


-- ----------------------------------------------------
-- create table selector if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'selector' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'selector already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "selector" (
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
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."id" IS ''' || 'primary key id varchar' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."plugin_id" IS ''' || 'plugin id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."name" IS ''' || 'selector name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."match_mode" IS ''' || 'matching mode (0 and 1 or)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."type" IS ''' || 'type (0, full flow, 1 custom flow)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."sort" IS ''' || 'sort' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."handle" IS ''' || 'processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."enabled" IS ''' || 'whether to open' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."loged" IS ''' || 'whether to print the log' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."continued" IS ''' || 'whether to continue execution' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector"."date_updated" IS ''' || 'update time' || '''');
	-- Primary Key structure for table selector
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'ALTER TABLE "selector" ADD CONSTRAINT "selector_pkey" PRIMARY KEY ("id");');
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;


-- ----------------------------------------------------
-- create table selector_condition if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'selector_condition' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'selector_condition already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "selector_condition" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "selector_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "param_type" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "operator" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "param_name" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "param_value" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');

	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector_condition"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector_condition"."selector_id" IS ''' || 'selector id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector_condition"."param_type" IS ''' || 'parameter type (to query uri, etc.)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector_condition"."operator" IS ''' || 'matching character (=> <like matching)' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector_condition"."param_name" IS ''' || 'parameter name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector_condition"."param_value" IS ''' || 'parameter value' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector_condition"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "selector_condition"."date_updated" IS ''' || 'update time' || '''');
	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;


-- ----------------------------------------------------
-- create table shenyu_dict if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'shenyu_dict' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'shenyu_dict already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "shenyu_dict" (
	  "id" varchar(128) primary key,
	  "type" varchar(100) COLLATE "pg_catalog"."default" NOT NULL,
	  "dict_code" varchar(100) COLLATE "pg_catalog"."default" NOT NULL,
	  "dict_name" varchar(100) COLLATE "pg_catalog"."default" NOT NULL,
	  "dict_value" varchar(100) COLLATE "pg_catalog"."default",
	  "desc" varchar(255) COLLATE "pg_catalog"."default",
	  "sort" int4 NOT NULL,
	  "enabled" int2,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "shenyu_dict"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "shenyu_dict"."type" IS ''' || 'type' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "shenyu_dict"."dict_code" IS ''' || 'dictionary encoding' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "shenyu_dict"."dict_name" IS ''' || 'dictionary name' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "shenyu_dict"."dict_value" IS ''' || 'dictionary value' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "shenyu_dict"."desc" IS ''' || 'dictionary description or remarks' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "shenyu_dict"."sort" IS ''' || 'sort' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "shenyu_dict"."enabled" IS ''' || 'whether it is enabled' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "shenyu_dict"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "shenyu_dict"."date_updated" IS ''' || 'update time' || '''');
    ----------------------------
	-- Create Rule for table shenyu_dict
	-- ----------------------------
    PERFORM public.dblink_exec('init_conn',  'create rule shenyu_dict_insert_ignore as on insert to shenyu_dict where exists (select 1 from shenyu_dict where id = new.id) do instead nothing;');

    ----------------------------
	-- Create Sequence for table shenyu_dict
	-- ----------------------------
    PERFORM public.dblink_exec('init_conn',  'CREATE SEQUENCE shenyu_dict_ID_seq;	');
	PERFORM public.dblink_exec('init_conn',  'ALTER SEQUENCE shenyu_dict_ID_seq OWNED BY shenyu_dict.ID;');

	-- ----------------------------
	-- Records of shenyu_dict
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'degradeRuleGrade' || ''',''' || 'DEGRADE_GRADE_RT' || ''',''' || 'slow call ratio' || ''',''' || '0' || ''',''' || 'degrade type-slow call ratio' || ''',1,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'degradeRuleGrade' || ''',''' || 'DEGRADE_GRADE_EXCEPTION_RATIO' || ''',''' || 'exception ratio' || ''',''' || '1' || ''',''' || 'degrade type-abnormal ratio' || ''',0,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'degradeRuleGrade' || ''',''' || 'DEGRADE_GRADE_EXCEPTION_COUNT' || ''',''' || 'exception number strategy' || ''',''' || '2' || ''',''' || 'degrade type-abnormal number strategy' || ''',2,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'flowRuleGrade' || ''',''' || 'FLOW_GRADE_QPS' || ''',''' || 'QPS' || ''',''' || '1' || ''',''' || 'grade type-QPS' || ''',0,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'flowRuleGrade' || ''',''' || 'FLOW_GRADE_THREAD' || ''',''' || 'number of concurrent threads' || ''',''' || '0' || ''',''' || 'degrade type-number of concurrent threads' || ''',1,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'flowRuleControlBehavior' || ''',''' || 'CONTROL_BEHAVIOR_DEFAULT' || ''',''' || 'direct rejection by default' || ''',''' || '0' || ''',''' || 'control behavior-direct rejection by default' || ''',0,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'flowRuleControlBehavior' || ''',''' || 'CONTROL_BEHAVIOR_WARM_UP' || ''',''' || 'warm up' || ''',''' || '1' || ''',''' || 'control behavior-warm up' || ''',1,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'flowRuleControlBehavior' || ''',''' || 'CONTROL_BEHAVIOR_RATE_LIMITER' || ''',''' || 'constant speed queuing' || ''',''' || '2' || ''',''' || 'control behavior-uniform speed queuing' || ''',2,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'flowRuleControlBehavior' || ''',''' || 'CONTROL_BEHAVIOR_WARM_UP_RATE_LIMITER' || ''',''' || 'preheating uniformly queued' || ''',''' || '3' || ''',''' || 'control behavior-preheating uniformly queued' || ''',3,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'permission' || ''',''' || 'REJECT' || ''',''' || 'reject' || ''',''' || 'reject' || ''',''' || 'reject' || ''',0,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'permission' || ''',''' || 'ALLOW' || ''',''' || 'allow' || ''',''' || 'allow' || ''',''' || 'allow' || ''',1,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'algorithmName' || ''',''' || 'ALGORITHM_SLIDINGWINDOW' || ''',''' || 'slidingWindow' || ''',''' || 'slidingWindow' || ''',''' || 'Sliding window algorithm' || ''',0,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'algorithmName' || ''',''' || 'ALGORITHM_LEAKYBUCKET' || ''',''' || 'leakyBucket' || ''',''' || 'leakyBucket' || ''',''' || 'Leaky bucket algorithm' || ''',1,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'algorithmName' || ''',''' || 'ALGORITHM_CONCURRENT' || ''',''' || 'concurrent' || ''',''' || 'concurrent' || ''',''' || 'Concurrent algorithm' || ''',2,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'algorithmName' || ''',''' || 'ALGORITHM_TOKENBUCKET' || ''',''' || 'tokenBucket' || ''',''' || 'tokenBucket' || ''',''' || 'Token bucket algorithm' || ''',3,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'loadBalance' || ''', ''' || 'LOAD_BALANCE' || ''', ''' || 'roundRobin' || ''', ''' || 'roundRobin' || ''', ''' || 'roundRobin' || ''', 2, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'loadBalance' || ''', ''' || 'LOAD_BALANCE' || ''', ''' || 'random' || ''', ''' || 'random' || ''', ''' || 'random' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'loadBalance' || ''', ''' || 'LOAD_BALANCE' || ''', ''' || 'hash' || ''', ''' || 'hash' || ''', ''' || 'hash' || ''', 0, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'status' || ''', ''' || 'DIVIDE_STATUS' || ''', ''' || 'close' || ''', ''' || 'false' || ''', ''' || 'close' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'status' || ''', ''' || 'DIVIDE_STATUS' || ''', ''' || 'open' || ''', ''' || 'true' || ''', ''' || 'open' || ''', 0, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'multiRuleHandle' || ''', ''' || 'MULTI_RULE_HANDLE' || ''', ''' || 'multiple rule' || ''', ''' || '1' || ''', ''' || 'multiple rule' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'multiRuleHandle' || ''', ''' || 'MULTI_RULE_HANDLE' || ''', ''' || 'single rule' || ''', ''' || '0' || ''', ''' || 'single rule' || ''', 0, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'multiSelectorHandle' || ''', ''' || 'MULTI_SELECTOR_HANDLE' || ''', ''' || 'multiple handle' || ''', ''' || '1' || ''', ''' || 'multiple handle' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'multiSelectorHandle' || ''', ''' || 'MULTI_SELECTOR_HANDLE' || ''', ''' || 'single handle' || ''', ''' || '0' || ''', ''' || 'single handle' || ''', 0, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'matchMode' || ''', ''' || 'MATCH_MODE' || ''', ''' || 'and' || ''', ''' || '0' || ''', ''' || 'and' || ''', 0, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'matchMode' || ''', ''' || 'MATCH_MODE' || ''', ''' || 'or' || ''', ''' || '1' || ''', ''' || 'or' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'operator' || ''', ''' || 'OPERATOR' || ''', ''' || 'match' || ''', ''' || 'match' || ''', ''' || 'match' || ''', 0, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'operator' || ''', ''' || 'OPERATOR' || ''', ''' || '=' || ''', ''' || '=' || ''', ''' || '=' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'operator' || ''', ''' || 'OPERATOR' || ''', ''' || 'regex' || ''', ''' || 'regex' || ''', ''' || 'regex' || ''', 2, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'operator' || ''', ''' || 'OPERATOR' || ''', ''' || 'contains' || ''', ''' || 'contains' || ''', ''' || 'contains' || ''', 3, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'operator' || ''', ''' || 'OPERATOR' || ''', ''' || 'TimeBefore' || ''', ''' || 'TimeBefore' || ''', ''' || 'TimeBefore' || ''', 4, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'operator' || ''', ''' || 'OPERATOR' || ''', ''' || 'TimeAfter' || ''', ''' || 'TimeAfter' || ''', ''' || 'TimeAfter' || ''', 5, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'operator' || ''', ''' || 'OPERATOR' || ''', ''' || 'exclude' || ''', ''' || 'exclude' || ''', ''' || 'exclude' || ''', 6, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'paramType' || ''', ''' || 'PARAM_TYPE' || ''', ''' || 'post' || ''', ''' || 'post' || ''', ''' || 'post' || ''', 0, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'paramType' || ''', ''' || 'PARAM_TYPE' || ''', ''' || 'uri' || ''', ''' || 'uri' || ''', ''' || 'uri' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'paramType' || ''', ''' || 'PARAM_TYPE' || ''', ''' || 'query' || ''', ''' || 'query' || ''', ''' || 'query' || ''', 2, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'paramType' || ''', ''' || 'PARAM_TYPE' || ''', ''' || 'host' || ''', ''' || 'host' || ''', ''' || 'host' || ''', 3, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'paramType' || ''', ''' || 'PARAM_TYPE' || ''', ''' || 'ip' || ''', ''' || 'ip' || ''', ''' || 'ip' || ''', 4, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'paramType' || ''', ''' || 'PARAM_TYPE' || ''', ''' || 'header' || ''', ''' || 'header' || ''', ''' || 'header' || ''', 5, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'paramType' || ''', ''' || 'PARAM_TYPE' || ''', ''' || 'cookie' || ''', ''' || 'cookie' || ''', ''' || 'cookie' || ''', 6, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'paramType' || ''', ''' || 'PARAM_TYPE' || ''', ''' || 'req_method' || ''', ''' || 'req_method' || ''', ''' || 'req_method' || ''', 7, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'keyResolverName' || ''',''' || 'WHOLE_KEY_RESOLVER' || ''',''' || 'whole' || ''',''' || 'WHOLE_KEY_RESOLVER' || ''',''' || 'Rate limit by all request' || ''',0,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'keyResolverName' || ''',''' || 'REMOTE_ADDRESS_KEY_RESOLVER' || ''',''' || 'remoteAddress' || ''',''' || 'REMOTE_ADDRESS_KEY_RESOLVER' || ''',''' || 'Rate limit by remote address' || ''',1,1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'automaticTransitionFromOpenToHalfOpenEnabled' || ''', ''' || 'AUTOMATIC_HALF_OPEN' || ''', ''' || 'open' || ''', ''' || 'true' || ''', ''' || '' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'automaticTransitionFromOpenToHalfOpenEnabled' || ''', ''' || 'AUTOMATIC_HALF_OPEN' || ''', ''' || 'close' || ''', ''' || 'false' || ''', ''' || '' || ''', 2, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'paramType' || ''', ''' || 'PARAM_TYPE' || ''', ''' || 'domain' || ''', ''' || 'domain' || ''', ''' || 'domain' || ''', 8, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'strategyName' || ''', ''' || 'STRATEGY_NAME' || ''', ''' || 'rsa' || ''', ''' || 'rsa' || ''', ''' || 'rsa strategy' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'way' || ''', ''' || 'WAY' || ''', ''' || 'encrypt' || ''', ''' || 'encrypt' || ''', ''' || 'encrypt' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO  shenyu_dict  ( type , dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'way' || ''', ''' || 'WAY' || ''', ''' || 'decrypt' || ''', ''' || 'decrypt' || ''', ''' || 'decrypt' || ''', 1, 1);');

    /*insert mode data for rateLimiter plugin*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'mode' || ''', ''' || 'MODE' || ''', ''' || 'cluster' || ''', ''' || 'cluster' || ''', ''' || 'cluster' || ''', 0, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'mode' || ''', ''' || 'MODE' || ''', ''' || 'sentinel' || ''', ''' || 'sentinel' || ''', ''' || 'sentinel' || ''', 1, 1);');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'mode' || ''', ''' || 'MODE' || ''', ''' || 'standalone' || ''', ''' || 'standalone' || ''', ''' || 'standalone' || ''', 2, 1);');

    /*insert dict for dubbo plugin*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'gray' || ''', ''' || 'GRAY_STATUS' || ''', ''' || 'close' || ''', ''' || 'false' || ''', ''' || 'close' || ''', ''' || '1' || ''', ''' || '1' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'gray' || ''', ''' || 'GRAY_STATUS' || ''', ''' || 'open' || ''', ''' || 'true' || ''', ''' || 'open' || ''', ''' || '0' || ''', ''' || '1' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'threadpool' || ''', ''' || 'THREADPOOL' || ''', ''' || 'shared' || ''', ''' || 'shared' || ''', ''' || '' || ''', ''' || '4' || ''', ''' || '1' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'threadpool' || ''', ''' || 'THREADPOOL' || ''', ''' || 'fixed' || ''', ''' || 'fixed' || ''', ''' || '' || ''', ''' || '3' || ''', ''' || '1' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'threadpool' || ''', ''' || 'THREADPOOL' || ''', ''' || 'eager' || ''', ''' || 'eager' || ''', ''' || '' || ''', ''' || '2' || ''', ''' || '1' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'threadpool' || ''', ''' || 'THREADPOOL' || ''', ''' || 'cached' || ''', ''' || 'cached' || ''', ''' || '' || ''', ''' || '0' || ''', ''' || '1' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'threadpool' || ''', ''' || 'THREADPOOL' || ''', ''' || 'limited' || ''', ''' || 'limited' || ''', ''' || '' || ''', ''' || '1' || ''', ''' || '1' || ''');');

    /*insert dict for divide plugin*/
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'retryStrategy' || ''', ''' || 'RETRY_STRATEGY' || ''', ''' || 'current' || ''', ''' || 'current' || ''', ''' || 'current' || ''', ''' || '0' || ''', ''' || '1' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'retryStrategy' || ''', ''' || 'RETRY_STRATEGY' || ''', ''' || 'failover' || ''', ''' || 'failover' || ''', ''' || 'failover' || ''', ''' || '1' || ''', ''' || '1' || ''');');

    /* insert dict for init resource,permission table */
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'table'|| ''', ''' || 'INIT_FLAG' || ''', ''' || 'status' || ''',''' ||  'false' ||''',''' || 'table(resource,permission) init status' ||''',''' || '0' || ''',''' || '1' || ''');');

    /* insert dict for compress algorithm  */
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'compressAlg'|| ''', ''' || 'COMPRESS_ALG' || ''', ''' || 'none' || ''',''' ||  'none' ||''',''' || '' ||''',''' || '0' || ''',''' || '1' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'compressAlg'|| ''', ''' || 'COMPRESS_ALG' || ''', ''' || 'LZ4' || ''',''' ||  'LZ4' ||''',''' || '' ||''',''' || '1' || ''',''' || '1' || ''');');

    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'cacheType'|| ''', ''' || 'CACHE_TYPE_MEMORY' || ''', ''' || 'memory' || ''',''' ||  'memory' ||''',''' || 'use memory to cache data' ||''',''' || '0' || ''',''' || '1' || ''');');
    PERFORM public.dblink_exec('init_conn',  'INSERT  INTO shenyu_dict ( type ,  dict_code ,  dict_name ,  dict_value ,  "desc" ,  sort ,  enabled ) VALUES (''' || 'cacheType'|| ''', ''' || 'CACHE_TYPE_REDIS'  || ''', ''' || 'redis'  || ''',''' ||  'redis' ||''','''  || 'use redis to cache data' ||''',''' || '1' || ''',''' || '1' || ''');');


	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;

-- ----------------------------------------------------
-- create table user_role if not exist ---
-- ----------------------------------------------------
IF (SELECT * FROM dblink('host=localhost user=' || _user || ' password=' || _password || ' dbname=' ||_db,'SELECT COUNT(1) FROM pg_class  WHERE relname  = ''' ||'user_role' || '''')AS t(count BIGINT) )> 0 THEN
    RAISE NOTICE 'user_role already exists';
ELSE
    PERFORM public.dblink_exec('init_conn', 'BEGIN');
    PERFORM public.dblink_exec('init_conn', ' CREATE TABLE "user_role" (
	  "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "user_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "role_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
	  "date_created" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE),
	  "date_updated" TIMESTAMP NOT NULL DEFAULT TIMEZONE(''UTC-8''::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE)
	)');

	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "user_role"."id" IS ''' || 'primary key id' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "user_role"."user_id" IS ''' || 'user primary key' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "user_role"."role_id" IS ''' || 'role primary key' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "user_role"."date_created" IS ''' || 'create time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON COLUMN "user_role"."date_updated" IS ''' || 'update time' || '''');
	PERFORM public.dblink_exec('init_conn', ' COMMENT ON TABLE "user_role" IS ''' || 'user and role bind table' || '''');
	-- ----------------------------
	-- Records of user_role
	-- ----------------------------
	PERFORM public.dblink_exec('init_conn',  'INSERT INTO "user_role" VALUES (''' || '1351007709096976384' || ''', ''' || '1' || ''', ''' || '1346358560427216896' || ''');');

	PERFORM public.dblink_exec('init_conn', 'COMMIT');
END IF;
	PERFORM public.dblink_disconnect('init_conn');
END
$do$;
