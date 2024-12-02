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

INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737478', 'rewriteMetaData', 'REWRITE_META_DATA', 'true', 'true', '', 4, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');
INSERT INTO "public"."shenyu_dict" VALUES ('1679002911061737479', 'rewriteMetaData', 'REWRITE_META_DATA', 'false', 'false', '', 4, 1, '2024-02-07 14:31:49', '2024-02-07 14:31:49');

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

INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822145','649330b6-c2d7-4edc-be8e-8a54df9eb385','1', NULL, 20, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822146','649330b6-c2d7-4edc-be8e-8a54df9eb385','10', NULL, 140, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822147','649330b6-c2d7-4edc-be8e-8a54df9eb385','11', '{"protocol":"zookeeper","register":"127.0.0.1:2181","threadpool":"shared"}', 310, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822148','649330b6-c2d7-4edc-be8e-8a54df9eb385','12', NULL, 310, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822149','649330b6-c2d7-4edc-be8e-8a54df9eb385','13', '{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"shared"}', 310, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822150','649330b6-c2d7-4edc-be8e-8a54df9eb385','14', NULL, 80, 1, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822151','649330b6-c2d7-4edc-be8e-8a54df9eb385','15', '{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"shared"}', 310, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822152','649330b6-c2d7-4edc-be8e-8a54df9eb385','16', NULL, 110, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822153','649330b6-c2d7-4edc-be8e-8a54df9eb385','17', '{"registerProtocol":"direct","registerAddress":"127.0.0.1:2181","corethreads":0,"threads":2147483647,"queues":0,"threadpool":"shared"}', 310, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822154','649330b6-c2d7-4edc-be8e-8a54df9eb385','18', NULL, 160, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822155','649330b6-c2d7-4edc-be8e-8a54df9eb385','19', '{"secretKey":"key"}', 30, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822156','649330b6-c2d7-4edc-be8e-8a54df9eb385','2', '{"model":"black"}', 50, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822157','649330b6-c2d7-4edc-be8e-8a54df9eb385','20', NULL, 120, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822158','649330b6-c2d7-4edc-be8e-8a54df9eb385','21', NULL, 40, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822159','649330b6-c2d7-4edc-be8e-8a54df9eb385','22', NULL, 70, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822160','649330b6-c2d7-4edc-be8e-8a54df9eb385','23', NULL, 220, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822161','649330b6-c2d7-4edc-be8e-8a54df9eb385','24', NULL, 100, 1, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822162','649330b6-c2d7-4edc-be8e-8a54df9eb385','25', NULL, 410, 1, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822163','649330b6-c2d7-4edc-be8e-8a54df9eb385','26', '{"multiSelectorHandle":"1"}', 200, 1, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822164','649330b6-c2d7-4edc-be8e-8a54df9eb385','27', NULL, 125, 1, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822165','649330b6-c2d7-4edc-be8e-8a54df9eb385','28', '{"port": 9500,"bossGroupThreadCount": 1,"maxPayloadSize": 65536,"workerGroupThreadCount": 12,"userName": "shenyu","password": "shenyu","isEncryptPassword": false,"encryptMode": "","leakDetectorLevel": "DISABLED"}', 125, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822166','649330b6-c2d7-4edc-be8e-8a54df9eb385','29', '{"topic":"shenyu-access-logging", "namesrvAddr": "localhost:9876","producerGroup":"shenyu-plugin-logging-rocketmq"}', 170, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822167','649330b6-c2d7-4edc-be8e-8a54df9eb385','3', NULL, 90, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822168','649330b6-c2d7-4edc-be8e-8a54df9eb385','30', '{"cacheType":"memory"}', 10, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822169','649330b6-c2d7-4edc-be8e-8a54df9eb385','31', NULL, 1, 0, '2022-06-16 14:40:35.000', '2022-06-16 14:40:55.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822170','649330b6-c2d7-4edc-be8e-8a54df9eb385','32', '{"host":"localhost", "port": "9200"}', 190, 0, '2022-06-19 22:00:00.000', '2022-06-19 22:00:00.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822171','649330b6-c2d7-4edc-be8e-8a54df9eb385','33', '{"host":"localhost", "port": "9092"}', 180, 0, '2022-07-04 22:00:00.000', '2022-07-02 22:00:00.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822172','649330b6-c2d7-4edc-be8e-8a54df9eb385','34', '{"projectName": "shenyu", "logStoreName": "shenyu-logstore", "topic": "shenyu-topic"}', 175, 0, '2022-06-30 21:00:00.000', '2022-06-30 21:00:00.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822173','649330b6-c2d7-4edc-be8e-8a54df9eb385','35', '{"topic":"shenyu-access-logging", "serviceUrl": "pulsar://localhost:6650"}', 185, 0, '2022-06-30 21:00:00.000', '2022-06-30 21:00:00.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822174','649330b6-c2d7-4edc-be8e-8a54df9eb385','36', '{"endpoint": "ap-guangzhou.cls.tencentcs.com", "topic": "shenyu-topic"}', 176, 0, '2022-06-30 21:00:00.000', '2022-06-30 21:00:00.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822175','649330b6-c2d7-4edc-be8e-8a54df9eb385','38', '{"host":"127.0.0.1","port":"8123","databse":"shenyu-gateway","username":"foo","password":"bar"}', 195, 0, '2022-06-30 21:00:00.000', '2022-06-30 21:00:00.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822176','649330b6-c2d7-4edc-be8e-8a54df9eb385','39', '{"endpoint":"http://localhost:8000"}', 40, 0, '2022-09-11 12:00:00.000', '2022-09-11 12:00:00.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822177','649330b6-c2d7-4edc-be8e-8a54df9eb385','4', '{"master":"mymaster","mode":"standalone","url":"192.168.1.1:6379","password":"abc"}', 60, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822178','649330b6-c2d7-4edc-be8e-8a54df9eb385','40', NULL, 150, 0, '2022-07-24 19:00:00.000', '2022-07-24 19:00:00.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822179','649330b6-c2d7-4edc-be8e-8a54df9eb385','42', NULL, 320, 1, '2023-05-30 18:02:53.000', '2022-05-30 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822180','649330b6-c2d7-4edc-be8e-8a54df9eb385','43', '{"totalSizeInBytes":"104857600","maxBlockMs":"0","ioThreadCount":"1","batchSizeThresholdInBytes":"524288","batchCountThreshold":"4096","lingerMs":"2000","retries":"100","baseRetryBackoffMs":"100","maxRetryBackoffMs":"100","enableLocalTest":"true","setGiveUpExtraLongSingleLog":"false"}', 177, 0, '2023-07-05 14:03:53.686', '2023-07-06 12:42:07.234');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822181','649330b6-c2d7-4edc-be8e-8a54df9eb385','44', '{"defaultHandleJson":"{\"authorization\":\"test:test123\"}"}', 150, 0, '2022-07-24 19:00:00.000', '2022-07-24 19:00:00.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822182','649330b6-c2d7-4edc-be8e-8a54df9eb385','45', '{"host":"127.0.0.1","port":5672,"password":"admin","username":"admin","exchangeName":"exchange.logging.plugin","queueName":"queue.logging.plugin","routingKey":"topic.logging","virtualHost":"/","exchangeType":"direct","durable":"true","exclusive":"false","autoDelete":"false"}', 171, 0, '2023-11-06 15:49:56.454', '2023-11-10 10:40:58.447');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822183','649330b6-c2d7-4edc-be8e-8a54df9eb385','5', '{"multiSelectorHandle":"1","multiRuleHandle":"0"}', 200, 1, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822184','649330b6-c2d7-4edc-be8e-8a54df9eb385','6', '{"register":"zookeeper://localhost:2181","multiSelectorHandle":"1","threadpool":"shared","corethreads":0,"threads":2147483647,"queues":0}', 310, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822185','649330b6-c2d7-4edc-be8e-8a54df9eb385','8', '{"enabled":false,"registerType":"eureka","serverLists":"http://localhost:8761/eureka","props":{}}', 200, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');
INSERT INTO "public"."namespace_plugin_rel" VALUES ('1801816010882822186','649330b6-c2d7-4edc-be8e-8a54df9eb385','9', NULL, 130, 0, '2022-05-25 18:02:53.000', '2022-05-25 18:02:53.000');


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

ALTER TABLE "public"."selector" ADD COLUMN namespace_id VARCHAR(50) NOT NULL;
COMMENT ON COLUMN "public"."selector"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."rule" ADD COLUMN namespace_id VARCHAR(50) NOT NULL;
COMMENT ON COLUMN "public"."rule"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."meta_data" ADD COLUMN namespace_id VARCHAR(50) NOT NULL;
COMMENT ON COLUMN "public"."meta_data"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."app_auth" ADD COLUMN namespace_id VARCHAR(50) NOT NULL;
COMMENT ON COLUMN "public"."app_auth"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."discovery" ADD COLUMN namespace_id VARCHAR(50) NOT NULL;
COMMENT ON COLUMN "public"."discovery"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."discovery_upstream" ADD COLUMN namespace_id VARCHAR(50) NOT NULL;
COMMENT ON COLUMN "public"."discovery_upstream"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."proxy_selector" ADD COLUMN namespace_id VARCHAR(50) NOT NULL;
COMMENT ON COLUMN "public"."proxy_selector"."namespace_id" IS 'namespaceId';

ALTER TABLE "public"."alert_receiver" ADD COLUMN namespace_id VARCHAR(50) NOT NULL;
COMMENT ON COLUMN "public"."alert_receiver"."namespace_id" IS 'namespaceId';

UPDATE "public"."selector" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE "public"."rule" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE "public"."meta_data" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE "public"."app_auth" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE "public"."discovery" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE "public"."discovery_upstream" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE "public"."proxy_selector" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE "public"."alert_receiver" SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

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

