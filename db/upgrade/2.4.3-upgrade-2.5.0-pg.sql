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

-- Note: it doesn't matter if you don't execute this SQL
-- Note: because most users have changed ZK configuration, this SQL is annotated to prevent erroneous execution
-- UPDATE plugin SET config='{"register":"127.0.0.1:2181","corethreads":0,"threads":2147483647,"queues":0}' WHERE "name" = 'motan';
-- if you want to execute this SQL, please replace it with your ZK configuration

-- insert plugin_handle data for motan
INSERT INTO plugin_handle ("id", "plugin_id", "field", "label", "data_type", "type", "sort", "ext_obj") VALUES ('1510270286164094976', '17', 'corethreads', 'corethreads', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}');
INSERT INTO plugin_handle ("id", "plugin_id", "field", "label", "data_type", "type", "sort", "ext_obj") VALUES ('1510270476329644032', '17', 'threads', 'threads', 1, 3, 0, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}');
INSERT INTO plugin_handle ("id", "plugin_id", "field", "label", "data_type", "type", "sort", "ext_obj") VALUES ('1510270555383885824', '17', 'queues', 'queues', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}');
INSERT INTO plugin_handle ("id", "plugin_id", "field", "label", "data_type", "type", "sort", "ext_obj") VALUES ('1515116191850078208', '17', 'threadpool', 'threadpool', 3, 3, 0, '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}');

-- Note: it doesn't matter if you don't execute this SQL
-- Note: because most users have changed ZK configuration, this SQL is annotated to prevent erroneous execution
-- UPDATE plugin SET config='{"register":"zookeeper://localhost:2181","corethreads":0,"threads":2147483647,"queues":0,"threadpool":"cached"}' WHERE "name" = 'motan';
--                                                    ^^^^^^^^^^^^^^ if you want to execute this SQL, please replace it with your own ZK configuration

-- insert dict for all plugin
INSERT INTO shenyu_dict ("id", "type", "dict_code", "dict_name", "dict_value", "desc", "sort", "enabled") VALUES ('1516043399649357824', 'operator', 'OPERATOR', 'startsWith', 'startsWith', 'startsWith', 7, 1);
INSERT INTO shenyu_dict ("id", "type", "dict_code", "dict_name", "dict_value", "desc", "sort", "enabled") VALUES ('1516043495265869824', 'operator', 'OPERATOR', 'endsWith', 'endsWith', 'endsWith', 8, 1);
INSERT INTO shenyu_dict ("id", "type", "dict_code", "dict_name", "dict_value", "desc", "sort", "enabled") VALUES ('1529403902800302098', 'threadpool', 'THREADPOOL', 'default', 'default', '', 5, 1);

-- refactor logging name
UPDATE plugin SET name = "LoggingConsole" WHERE name = "logging";

-- new table operation_record_log
-- ----------------------------
-- Table structure for operation_record_log
-- ----------------------------
CREATE TABLE "operation_record_log"
(
    "id"             int8                                        NOT NULL,
    "color"          varchar(20) COLLATE "pg_catalog"."default"  NOT NULL,
    "context"        text COLLATE "pg_catalog"."default"         NOT NULL,
    "operator"       varchar(200) COLLATE "pg_catalog"."default" NOT NULL,
    "operation_time" timestamp(6)                                NOT NULL,
    "operation_type" varchar(60) COLLATE "pg_catalog"."default"  NOT NULL,
    CONSTRAINT "operation_record_log_pkey" PRIMARY KEY ("id")
)
;

COMMENT ON COLUMN "operation_record_log"."id" IS 'id';
COMMENT ON COLUMN "operation_record_log"."color" IS 'log color';
COMMENT ON COLUMN "operation_record_log"."context" IS 'log context';
COMMENT ON COLUMN "operation_record_log"."operator" IS 'operator [user or app]]';
COMMENT ON COLUMN "operation_record_log"."operation_time" IS 'operation time';
COMMENT ON COLUMN "operation_record_log"."operation_type" IS 'operation type：create/update/delete/register...';
COMMENT ON TABLE "operation_record_log" IS 'operation record log';

-- insert plugin_handle data for tars
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524913', '13', 'corethreads', 'corethreads', 1, 3, 3, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524914', '13', 'threads', 'threads', 1, 3, 4, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524915', '13', 'queues', 'queues', 1, 3, 5, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524916', '13', 'threadpool', 'threadpool', 3, 3, 2, '{"required":"0","defaultValue":"default","placeholder":"threadpool","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
-- insert plugin_handle data for sofa
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524917', '11', 'corethreads', 'corethreads', 1, 3, 4, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524918', '11', 'threads', 'threads', 1, 3, 5, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524919', '11', 'queues', 'queues', 1, 3, 6, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524920', '11', 'threadpool', 'threadpool', 3, 3, 3, '{"required":"0","defaultValue":"default","placeholder":"threadpool","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');

INSERT INTO "public"."resource" VALUES ('1534577121923309568', '', 'Document', '', '/document', '', 0, 3, 'file-text', 0, 0, '', 1, '2022-06-09 00:44:32', '2022-06-09 01:06:45');
INSERT INTO "public"."resource" VALUES ('1534585430311051264', '1534577121923309568', 'API document', '', '/document/apidoc', '', 1, 0, 'api', 0, 0, '', 1, '2022-06-09 01:17:32', '2022-06-09 01:17:32');
INSERT INTO "public"."resource" VALUES ('1534585531108564992', '1534585430311051264', 'send', '', '', '', 2, 0, '', 1, 0, 'document:apirun:send', 1, '2022-06-09 01:17:56', '2022-06-09 01:17:56');

INSERT INTO "public"."permission" VALUES ('1529403932886044767', '1346358560427216896', '1534577121923309568', '2022-06-09 00:44:32', '2022-06-09 00:44:31');
INSERT INTO "public"."permission" VALUES ('1529403932886044768', '1346358560427216896', '1534585430311051264', '2022-06-09 01:17:33', '2022-06-09 01:17:32');
INSERT INTO "public"."permission" VALUES ('1529403932886044769', '1346358560427216896', '1534585531108564992', '2022-06-09 01:17:57', '2022-06-09 01:17:56');

/* insert plugin for loggingElasticSearch  */
INSERT INTO "public"."plugin" VALUES ('32', 'loggingElasticSearch','{"host":"localhost", "port": "9200"}', 'Logging', 190, 0, '2022-06-19 22:00:00', '2022-06-19 22:00:00');

/*insert plugin_handle data for plugin loggingElasticSearch*/
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524921', '32', 'host', 'host', 2, 3, 2, '{"required":"1","defaultValue":"localhost"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524922', '32', 'port', 'port', 2, 3, 3, '{"required":"1","defaultValue":"9200"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524923', '32', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524924', '32', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524925', '32', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524926', '32', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524927', '32', 'index', 'index', 2, 1, 1, '{"required":"0","defaultValue":"","placeholder":"optional"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO "public"."plugin_handle" VALUES ('1529403902783524928', '32', 'sampleRate', 'sampleRate', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"optional,0,0.01~1"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');