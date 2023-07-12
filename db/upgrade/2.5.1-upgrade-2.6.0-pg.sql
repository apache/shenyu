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

/* create new tables discovery,discovery_handler,discovery_rel,discovery_upstream,proxy_selector for discovery */
CREATE TABLE "public"."discovery" (
    "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "level" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
    "plugin_name" varchar(255) COLLATE "pg_catalog"."default",
    "type" varchar(64) COLLATE "pg_catalog"."default" NOT NULL,
    "server_list" varchar(255) COLLATE "pg_catalog"."default",
    "props" text COLLATE "pg_catalog"."default",
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."discovery"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."discovery"."name" IS 'the discovery name';
COMMENT ON COLUMN "public"."discovery"."level" IS '0 selector,1 plugin  2 global';
COMMENT ON COLUMN "public"."discovery"."plugin_name" IS 'the plugin name';
COMMENT ON COLUMN "public"."discovery"."type" IS 'local,zookeeper,etcd,consul,nacos';
COMMENT ON COLUMN "public"."discovery"."server_list" IS 'register server url (,)';
COMMENT ON COLUMN "public"."discovery"."props" IS 'the discovery pops (json) ';
COMMENT ON COLUMN "public"."discovery"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."discovery"."date_updated" IS 'update time';


CREATE TABLE "public"."discovery_handler" (
    "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "discovery_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "handler" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "listener_node" varchar(255) COLLATE "pg_catalog"."default",
    "props" text COLLATE "pg_catalog"."default",
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."discovery_handler"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."discovery_handler"."discovery_id" IS 'the discovery id';
COMMENT ON COLUMN "public"."discovery_handler"."handler" IS 'the handler';
COMMENT ON COLUMN "public"."discovery_handler"."listener_node" IS 'register server listener to node';
COMMENT ON COLUMN "public"."discovery_handler"."props" IS 'the discovery pops (json) ';
COMMENT ON COLUMN "public"."discovery_handler"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."discovery_handler"."date_updated" IS 'update time';

CREATE TABLE "public"."discovery_rel" (
    "id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "plugin_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
    "discovery_handler_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "selector_id" varchar(128) COLLATE "pg_catalog"."default",
    "proxy_selector_id" varchar(128) COLLATE "pg_catalog"."default",
    "date_created" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone),
    "date_updated" timestamp(6) NOT NULL DEFAULT timezone('UTC-8'::text, (now())::timestamp(0) without time zone)
)
;
COMMENT ON COLUMN "public"."discovery_rel"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."discovery_rel"."plugin_name" IS 'the plugin name';
COMMENT ON COLUMN "public"."discovery_rel"."discovery_handler_id" IS 'the discovery handler id';
COMMENT ON COLUMN "public"."discovery_rel"."selector_id" IS 'the selector id';
COMMENT ON COLUMN "public"."discovery_rel"."proxy_selector_id" IS 'the proxy selector id';
COMMENT ON COLUMN "public"."discovery_rel"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."discovery_rel"."date_updated" IS 'update time';

CREATE TABLE "public"."proxy_selector"
(
    "id"           varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "name"         varchar(128) COLLATE "pg_catalog"."default",
    "plugin_name"  varchar(128) COLLATE "pg_catalog"."default",
    "type"         varchar(128) COLLATE "pg_catalog"."default",
    "forward_port" int4 NOT NULL,
    "props"        text COLLATE "pg_catalog"."default",
    "date_created" timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "date_updated" timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP
)
;
COMMENT ON COLUMN "public"."proxy_selector"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."proxy_selector"."name" IS 'the proxy_selector name';
COMMENT ON COLUMN "public"."proxy_selector"."plugin_name" IS 'the plugin name';
COMMENT ON COLUMN "public"."proxy_selector"."type" IS 'the type ';
COMMENT ON COLUMN "public"."proxy_selector"."forward_port" IS 'the forward port';
COMMENT ON COLUMN "public"."proxy_selector"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."proxy_selector"."date_updated" IS 'update time';


CREATE TABLE "public"."discovery_upstream"
(
    "id"           varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "discovery_handler_id" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "protocol" varchar(128) COLLATE "pg_catalog"."default" NOT NULL,
    "url"         varchar(128) COLLATE "pg_catalog"."default",
    "status"      int2 COLLATE "pg_catalog"."default" NOT NULL,
    "weight"      int2 COLLATE "pg_catalog"."default" NOT NULL,
    "props"        text COLLATE "pg_catalog"."default",
    "date_created" timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "date_updated" timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP
)
;
COMMENT ON COLUMN "public"."discovery_upstream"."id" IS 'primary key id';
COMMENT ON COLUMN "public"."discovery_upstream"."discovery_handler_id" IS 'the discovery handler id';
COMMENT ON COLUMN "public"."discovery_upstream"."protocol" IS 'for http, https, tcp, ws';
COMMENT ON COLUMN "public"."discovery_upstream"."url" IS 'ip:port';
COMMENT ON COLUMN "public"."discovery_upstream"."status" IS 'type (0, healthy, 1 unhealthy)';
COMMENT ON COLUMN "public"."discovery_upstream"."weight" IS 'the weight for lists';
COMMENT ON COLUMN "public"."discovery_upstream"."props" IS 'the other field (json)';
COMMENT ON COLUMN "public"."discovery_upstream"."date_created" IS 'create time';
COMMENT ON COLUMN "public"."discovery_upstream"."date_updated" IS 'update time';

INSERT INTO "public"."plugin" VALUES ('42', 'tcp', null, 'Proxy', 320, 1, '2022-05-25 18:08:01', '2022-05-25 18:08:01', null);

ALTER TABLE shenyu_dict ALTER COLUMN dict_value TYPE  varchar(2048) COLLATE "pg_catalog"."default";

INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737472', 'discoveryMode', 'DISCOVERY_MODE', 'zookeeper', '{"baseSleepTimeMilliseconds":"1000","maxRetries":"3","maxSleepTimeMilliseconds":"1000","connectionTimeoutMilliseconds":"1000","sessionTimeoutMilliseconds":"1000","namespace":"","digest":null}', 'discoery mode to link zookeeper', 0, 1,'2023-03-17 10:15:16.846', '2023-03-07 10:15:16.846');

INSERT INTO "public"."plugin_handle"
(id, plugin_id, field, "label", data_type, "type", sort, ext_obj, date_created, date_updated)
VALUES('1678293231840038912', '42', 'discoveryZookeeper', 'discovery zk init props', 2, 3, 0, '{"required":"0","defaultValue":"{\"baseSleepTimeMilliseconds\":\"1000\",\"maxRetries\":\"3\",\"maxSleepTimeMilliseconds\":\"1000\",\"connectionTimeoutMilliseconds\":\"1000\",\"sessionTimeoutMilliseconds\":\"1000\",\"namespace\":\"\",\"digest\":null}","rule":""}', '2023-07-10 14:41:02.000', '2023-07-10 14:41:40.643');
INSERT INTO "public"."plugin_handle"
(id, plugin_id, field, "label", data_type, "type", sort, ext_obj, date_created, date_updated)
VALUES('1678293333363167232', '42', 'discoveryhandler', 'discoveryhandler', 2, 1, 0, '{"required":"0","defaultValue":"url,protocol,status,weight","rule":""}', '2023-07-10 14:41:27.000', '2023-07-12 13:16:55.067');
INSERT INTO "public"."plugin_handle"
(id, plugin_id, field, "label", data_type, "type", sort, ext_obj, date_created, date_updated)
VALUES('1678997037438107648', '42', 'bossGroupThreadCount', 'bossGroupThreadCount', 2, 1, 1, '{"required":"0","defaultValue":"1","rule":""}', '2023-07-12 13:17:43.000', '2023-07-12 13:22:24.662');
INSERT INTO"public"."plugin_handle"
(id, plugin_id, field, "label", data_type, "type", sort, ext_obj, date_created, date_updated)
VALUES('1678997142656417792', '42', 'workerGroupThreadCount', 'workerGroupThreadCount', 2, 1, 2, '{"required":"0","defaultValue":"12","rule":""}', '2023-07-12 13:18:08.000', '2023-07-12 13:22:54.357');
INSERT INTO "public"."plugin_handle"
(id, plugin_id, field, "label", data_type, "type", sort, ext_obj, date_created, date_updated)
VALUES('1678997399104552960', '42', 'clientMaxIdleTimeMs', 'clientMaxIdleTimeMs', 2, 1, 7, '{"required":"0","defaultValue":"30000","rule":""}', '2023-07-12 13:19:09.000', '2023-07-12 13:23:54.147');
INSERT INTO "public"."plugin_handle"
(id, plugin_id, field, "label", data_type, "type", sort, ext_obj, date_created, date_updated)
VALUES('1678997479614218240', '42', 'clientPendingAcquireMaxCount', 'clientPendingAcquireMaxCount', 2, 1, 4, '{"required":"0","defaultValue":"5","rule":""}', '2023-07-12 13:19:28.000', '2023-07-12 13:23:20.131');
INSERT INTO "public"."plugin_handle"
(id, plugin_id, field, "label", data_type, "type", sort, ext_obj, date_created, date_updated)
VALUES('1678996921914392576', '42', 'loadBalance', 'loadBalance', 3, 1, 3, '{"required":"0","defaultValue":"random","rule":""}', '2023-07-12 13:17:15.000', '2023-07-12 13:23:10.235');
INSERT INTO "public"."plugin_handle"
(id, plugin_id, field, "label", data_type, "type", sort, ext_obj, date_created, date_updated)
VALUES('1678997769998467072', '42', 'clientMaxLifeTimeMs', 'clientMaxLifeTimeMs', 2, 1, 8, '{"required":"0","defaultValue":"60000","rule":""}', '2023-07-12 13:20:37.000', '2023-07-12 13:24:07.104');
INSERT INTO "public"."plugin_handle"
(id, plugin_id, field, "label", data_type, "type", sort, ext_obj, date_created, date_updated)
VALUES('1678997277012557824', '42', 'clientMaxConnections', 'clientMaxConnections', 2, 1, 6, '{"required":"0","defaultValue":"20","rule":""}', '2023-07-12 13:18:40.000', '2023-07-12 13:23:44.428');
INSERT INTO "public"."plugin_handle"
(id, plugin_id, field, "label", data_type, "type", sort, ext_obj, date_created, date_updated)
VALUES('1678997557628272640', '42', 'clientPendingAcquireTimeout', 'clientPendingAcquireTimeout', 2, 1, 5, '{"required":"0","defaultValue":"5","rule":""}', '2023-07-12 13:19:47.000', '2023-07-12 13:23:33.253');
