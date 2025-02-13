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

-- this file works for Oracle, can not use "`" syntax.
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1679002911061737478', 'rewriteMetaData', 'REWRITE_META_DATA', 'true', 'true', '', 4, 1);

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1679002911061737479', 'rewriteMetaData', 'REWRITE_META_DATA', 'false', 'false', '', 4, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1679002911061737480', 'preserveHost', 'PRESERVE_HOST', 'true', 'true', '', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES('1679002911061737481', 'preserveHost', 'PRESERVE_HOST', 'false', 'false', '', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507020', '14', 'rewriteContextPath', 'rewriteContextPath', 2, 2, 2, '{"required":"0","defaultValue":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507021', '14', 'percentage', 'percentage', 1, 2, 3, '{"required":"1","defaultValue":"100"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507022', '3', 'rewriteMetaData', 'rewriteMetaData', 3, 2, 3, '{"required":"1","defaultValue":"false"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507024', '8', 'registerType', 'registerType', 2, 3, 1, NULL);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507025', '8', 'serverLists', 'serverLists', 2, 3, 2, NULL);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507026', '8', 'props', 'props', 4, 3, 3, NULL);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507027', '20', 'preserveHost', 'preserveHost', 3, 2, 0, '{"required":"0","defaultValue":"false","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507028', '20', 'requestHeaderUniqueStrategy', 'requestHeaderUniqueStrategy', 2, 2, 1, '{"required":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507029', '20', 'requestUniqueHeaders', 'requestUniqueHeaders', 2, 2, 2, '{"required":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507030', '20', 'respHeaderUniqueStrategy', 'respHeaderUniqueStrategy', 2, 2, 3, '{"required":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1722804548510507031', '20', 'respUniqueHeaders', 'respUniqueHeaders', 2, 2, 4, '{"required":"0","rule":""}');


-- ----------------------------
-- Table structure for SHENYU_LOCK
-- ----------------------------
CREATE TABLE SHENYU_LOCK  (
   LOCK_KEY CHAR(36),
   REGION VARCHAR(100),
   CLIENT_ID CHAR(36),
   CREATED_DATE TIMESTAMP NOT NULL,
   constraint SHENYU_LOCK_PK primary key (LOCK_KEY, REGION)
);
-- Add comments to the columns
comment on column INT_LOCK.LOCK_KEY
  is 'LOCK_KEY';
comment on column SHENYU_LOCK.REGION
  is 'REGION';
comment on column SHENYU_LOCK.CLIENT_ID
  is 'CLIENT_ID';
comment on column SHENYU_LOCK.CREATED_DATE
  is 'CREATED_DATE';


INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347048240677269503','1346777766301888512','SHENYU.PLUGIN.BATCH.OPENED','','','','2','3','','1','0','system:authen:open','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708748849151', '1346358560427216896', '1347048240677269503');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1386680049203195915','1346777157943259136','SHENYU.COMMON.EXPORT', '', '', '', 2, 0, '', 1, 0, 'system:manager:exportConfig', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1386680049203195916','1346777157943259136','SHENYU.COMMON.IMPORT', '', '', '', 2, 0, '', 1, 0, 'system:manager:importConfig', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1386680049203195906', '1346358560427216896', '1386680049203195915');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1386680049203195907', '1346358560427216896', '1386680049203195916');


-- ----------------------------
-- Table structure for cluster_master
-- ----------------------------
create table cluster_master
(
    id                   varchar(128)   not null,
    master_host          varchar(255)   not null,
    master_port          varchar(255)   not null,
    context_path          varchar(255)   not null,
    date_created      timestamp(3) default SYSDATE not null,
    date_updated      timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
)
;
-- Add comments to the columns
comment
on column alert_receiver.id
  is 'primary key id';
comment
on column alert_receiver.master_host
  is 'master host';
comment
on column alert_receiver.master_port
  is 'master port';
comment
on column alert_receiver.context_path
  is 'master context_path';
comment
on column alert_receiver.date_created
  is 'create time';
comment
on column alert_receiver.date_updated
  is 'update time';



INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1792749362445840474', '1357956838021890048', 'SHENYU.MENU.SYSTEM.MANAGMENT.NAMESPACE', 'namespace', '/config/namespace', 'namespace', 1, 0, 'appstore', 0, 0, '', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1792749362445840475', '1792749362445840474', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 0, '', 1, 0, 'system:namespace:add', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1792749362445840476', '1792749362445840474', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 1, '', 1, 0, 'system:namespace:list', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1792749362445840477', '1792749362445840474', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 2, '', 1, 0, 'system:namespace:delete', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1792749362445840478', '1792749362445840474', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 3, '', 1, 0, 'system:namespace:edit', 1);


INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343252', '1346358560427216896', '1792749362445840474');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343253', '1346358560427216896', '1792749362445840475');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343254', '1346358560427216896', '1792749362445840476');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343255', '1346358560427216896', '1792749362445840477');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343256', '1346358560427216896', '1792749362445840478');

CREATE TABLE namespace (
                             id VARCHAR2(128) NOT NULL,
                             namespace_id VARCHAR2(50) NOT NULL,
                             name VARCHAR2(255) NOT NULL,
                             description VARCHAR2(255),
                             date_created      timestamp(3) default SYSDATE not null,
                             date_updated      timestamp(3) default SYSDATE not null,
                             CONSTRAINT pk_namespace PRIMARY KEY (id)
);


COMMENT ON COLUMN namespace.id IS 'namespace primary key';
COMMENT ON COLUMN namespace.namespace_id IS 'namespace id';
COMMENT ON COLUMN namespace.name IS 'namespace name';
COMMENT ON COLUMN namespace.description IS 'namespace desc';
COMMENT ON COLUMN namespace.date_created IS 'create time';
COMMENT ON COLUMN namespace.date_updated IS 'update time';

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace(id)) */ INTO `namespace` (`id`, `namespace_id`, `name`, `description`) VALUES ('1', '649330b6-c2d7-4edc-be8e-8a54df9eb385', 'default', 'default-namespace');


CREATE TABLE namespace_plugin_rel (
                                      id VARCHAR2(128) COLLATE utf8mb4_unicode_ci NOT NULL,
                                      namespace_id VARCHAR2(50) COLLATE utf8mb4_unicode_ci NOT NULL,
                                      plugin_id VARCHAR2(128) COLLATE utf8mb4_unicode_ci NOT NULL,
                                      config CLOB COLLATE utf8mb4_unicode_ci,
                                      sort NUMBER(11),
                                      enabled NUMBER(4,0) NOT NULL DEFAULT 0 CHECK (enabled IN (0, 1)),
                                      date_created      timestamp(3) default SYSDATE not null,
                                      date_updated      timestamp(3) default SYSDATE not null,
                                      CONSTRAINT pk_namespace_plugin_rel PRIMARY KEY (id)
);

COMMENT ON COLUMN namespace_plugin_rel.id IS 'Primary key ID';
COMMENT ON COLUMN namespace_plugin_rel.namespace_id IS 'Namespace ID';
COMMENT ON COLUMN namespace_plugin_rel.plugin_id IS 'Plugin ID';
COMMENT ON COLUMN namespace_plugin_rel.config IS 'Plugin configuration';
COMMENT ON COLUMN namespace_plugin_rel.sort IS 'Sort order';
COMMENT ON COLUMN namespace_plugin_rel.enabled IS 'Whether the plugin is enabled (0 = not open, 1 = open)';
COMMENT ON COLUMN namespace_plugin_rel.date_created IS 'Creation time';
COMMENT ON COLUMN namespace_plugin_rel.date_updated IS 'Update time';

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822145','649330b6-c2d7-4edc-be8e-8a54df9eb385','1', NULL, 20, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822146','649330b6-c2d7-4edc-be8e-8a54df9eb385','10', NULL, 140, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822147','649330b6-c2d7-4edc-be8e-8a54df9eb385','11', '{"protocol":"zookeeper","register":"127.0.0.1:2181","threadpool":"shared"}', 310, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822148','649330b6-c2d7-4edc-be8e-8a54df9eb385','12', NULL, 310, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822149','649330b6-c2d7-4edc-be8e-8a54df9eb385','13', '{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"shared"}', 310, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822150','649330b6-c2d7-4edc-be8e-8a54df9eb385','14', NULL, 80, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822151','649330b6-c2d7-4edc-be8e-8a54df9eb385','15', '{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"shared"}', 310, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822152','649330b6-c2d7-4edc-be8e-8a54df9eb385','16', NULL, 110, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822153','649330b6-c2d7-4edc-be8e-8a54df9eb385','17', '{"registerProtocol":"direct","registerAddress":"127.0.0.1:2181","corethreads":0,"threads":2147483647,"queues":0,"threadpool":"shared"}', 310, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822154','649330b6-c2d7-4edc-be8e-8a54df9eb385','18', NULL, 160, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822155','649330b6-c2d7-4edc-be8e-8a54df9eb385','19', '{"secretKey":"key"}', 30, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822156','649330b6-c2d7-4edc-be8e-8a54df9eb385','2', '{"model":"black"}', 50, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822157','649330b6-c2d7-4edc-be8e-8a54df9eb385','20', NULL, 120, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822158','649330b6-c2d7-4edc-be8e-8a54df9eb385','21', NULL, 40, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822159','649330b6-c2d7-4edc-be8e-8a54df9eb385','22', NULL, 70, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822160','649330b6-c2d7-4edc-be8e-8a54df9eb385','23', NULL, 220, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822161','649330b6-c2d7-4edc-be8e-8a54df9eb385','24', NULL, 100, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822162','649330b6-c2d7-4edc-be8e-8a54df9eb385','25', NULL, 410, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822163','649330b6-c2d7-4edc-be8e-8a54df9eb385','26', '{"multiSelectorHandle":"1"}', 200, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822164','649330b6-c2d7-4edc-be8e-8a54df9eb385','27', NULL, 125, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822165','649330b6-c2d7-4edc-be8e-8a54df9eb385','28', '{"port": 9500,"bossGroupThreadCount": 1,"maxPayloadSize": 65536,"workerGroupThreadCount": 12,"userName": "shenyu","password": "shenyu","isEncryptPassword": false,"encryptMode": "","leakDetectorLevel": "DISABLED"}', 125, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822166','649330b6-c2d7-4edc-be8e-8a54df9eb385','29', '{"topic":"shenyu-access-logging", "namesrvAddr": "localhost:9876","producerGroup":"shenyu-plugin-logging-rocketmq"}', 170, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822167','649330b6-c2d7-4edc-be8e-8a54df9eb385','3', NULL, 90, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822168','649330b6-c2d7-4edc-be8e-8a54df9eb385','30', '{"cacheType":"memory"}', 10, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822169','649330b6-c2d7-4edc-be8e-8a54df9eb385','31', NULL, 1, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822170','649330b6-c2d7-4edc-be8e-8a54df9eb385','32', '{"host":"localhost", "port": "9200"}', 190, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822171','649330b6-c2d7-4edc-be8e-8a54df9eb385','33', '{"host":"localhost", "port": "9092"}', 180, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822172','649330b6-c2d7-4edc-be8e-8a54df9eb385','34', '{"projectName": "shenyu", "logStoreName": "shenyu-logstore", "topic": "shenyu-topic"}', 175, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822173','649330b6-c2d7-4edc-be8e-8a54df9eb385','35', '{"topic":"shenyu-access-logging", "serviceUrl": "pulsar://localhost:6650"}', 185, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822174','649330b6-c2d7-4edc-be8e-8a54df9eb385','36', '{"endpoint": "ap-guangzhou.cls.tencentcs.com", "topic": "shenyu-topic"}', 176, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822175','649330b6-c2d7-4edc-be8e-8a54df9eb385','38', '{"host":"127.0.0.1","port":"8123","databse":"shenyu-gateway","username":"foo","password":"bar"}', 195, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822176','649330b6-c2d7-4edc-be8e-8a54df9eb385','39', '{"endpoint":"http://localhost:8000"}', 40, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822177','649330b6-c2d7-4edc-be8e-8a54df9eb385','4', '{"master":"mymaster","mode":"standalone","url":"192.168.1.1:6379","password":"abc"}', 60, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822178','649330b6-c2d7-4edc-be8e-8a54df9eb385','40', NULL, 150, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822179','649330b6-c2d7-4edc-be8e-8a54df9eb385','42', NULL, 320, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822180','649330b6-c2d7-4edc-be8e-8a54df9eb385','43', '{"totalSizeInBytes":"104857600","maxBlockMs":"0","ioThreadCount":"1","batchSizeThresholdInBytes":"524288","batchCountThreshold":"4096","lingerMs":"2000","retries":"100","baseRetryBackoffMs":"100","maxRetryBackoffMs":"100","enableLocalTest":"true","setGiveUpExtraLongSingleLog":"false"}', 177, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822181','649330b6-c2d7-4edc-be8e-8a54df9eb385','44', '{"defaultHandleJson":"{\"authorization\":\"test:test123\"}"}', 150, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822182','649330b6-c2d7-4edc-be8e-8a54df9eb385','45', '{"host":"127.0.0.1","port":5672,"password":"admin","username":"admin","exchangeName":"exchange.logging.plugin","queueName":"queue.logging.plugin","routingKey":"topic.logging","virtualHost":"/","exchangeType":"direct","durable":"true","exclusive":"false","autoDelete":"false"}', 171, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822183','649330b6-c2d7-4edc-be8e-8a54df9eb385','5', '{"multiSelectorHandle":"1","multiRuleHandle":"0"}', 200, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822184','649330b6-c2d7-4edc-be8e-8a54df9eb385','6', '{"register":"zookeeper://localhost:2181","multiSelectorHandle":"1","threadpool":"shared","corethreads":0,"threads":2147483647,"queues":0}', 310, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822185','649330b6-c2d7-4edc-be8e-8a54df9eb385','8', '{"enabled":false,"registerType":"eureka","serverLists":"http://localhost:8761/eureka","props":{}}', 200, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (namespace_plugin_rel(id)) */ INTO namespace_plugin_rel (id,namespace_id,plugin_id, config, sort, enabled) VALUES ('1801816010882822186','649330b6-c2d7-4edc-be8e-8a54df9eb385','9', NULL, 130, 0);



INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status)VALUES ('1792749362445840479', '1357956838021890048', 'SHENYU.MENU.SYSTEM.MANAGMENT.NAMESPACEPLUGIN', 'namespacePlugin', '/config/namespacePlugin', 'namespacePlugin', 1, 2, 'build', 0, 0, '', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status)VALUES ('1792749362445840480', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, '', 1, 0, 'system:namespacePlugin:list', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status)VALUES ('1792749362445840481', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 1, '', 1, 0, 'system:namespacePlugin:delete', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status)VALUES ('1792749362445840482', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 2, '', 1, 0, 'system:namespacePlugin:add', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status)VALUES ('1792749362445840483', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.SYNCHRONIZE', '', '', '', 2, 3, '', 1, 0, 'system:namespacePlugin:modify', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status)VALUES ('1792749362445840484', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.ENABLE', '', '', '', 2, 4, '', 1, 0, 'system:namespacePlugin:disable', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status)VALUES ('1792749362445840485', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 5, '', 1, 0, 'system:namespacePlugin:edit', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status)VALUES ('1792749362445840486', '1792749362445840479', 'SHENYU.BUTTON.SYSTEM.RESOURCE', '', '', '', 2, 6, '', 1, 0, 'system:namespacePlugin:resource', 1);


INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343260', '1346358560427216896', '1792749362445840479');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343261', '1346358560427216896', '1792749362445840480');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343262', '1346358560427216896', '1792749362445840481');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343263', '1346358560427216896', '1792749362445840482');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343264', '1346358560427216896', '1792749362445840483');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343265', '1346358560427216896', '1792749362445840484');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343266', '1346358560427216896', '1792749362445840485');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1792779493541343267', '1346358560427216896', '1792749362445840486');


INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1844015648095666176', '1346776175553376256', 'SHENYU.MENU.SYSTEM.MANAGMENT.SCALE', '', '/system/scale', '', 1, 4, 'sliders', 0, 0, '', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1844025735425183744', '1844015648095666176', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, 'unordered-list', 1, 0, 'system:scale:list', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1844025850382667776', '1844015648095666176', 'SHENYU.BUTTON.SYSTEM.ADD', '', '', '', 2, 0, 'plus', 1, 0, 'system:scale:add', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1844025989214130176', '1844015648095666176', 'SHENYU.BUTTON.SYSTEM.DELETE', '', '', '', 2, 0, 'delete', 1, 0, 'system:scale:delete', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1844026099075534848', '1844015648095666176', 'SHENYU.BUTTON.SYSTEM.EDIT', '', '', '', 2, 0, 'edit', 1, 0, 'system:scale:edit', 1);

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1697141926281381720', '1346358560427216896', '1844015648095666176');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1697145808239621836', '1346358560427216896', '1844025735425183744');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1697146375754129471', '1346358560427216896', '1844025850382667776');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1697146617543248162', '1346358560427216896', '1844025989214130176');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1697146860569542740', '1346358560427216896', '1844026099075534848');

/* add column into dashboard_user table */
ALTER TABLE dashboard_user ADD client_id VARCHAR(32) NULL;
COMMENT ON COLUMN dashboard_user.client_id IS 'client id';

ALTER TABLE selector ADD namespace_id VARCHAR2(50) NOT NULL;
COMMENT ON COLUMN selector.namespace_id IS 'namespaceId';

ALTER TABLE rule ADD namespace_id VARCHAR2(50) NOT NULL;
COMMENT ON COLUMN rule.namespace_id IS 'namespaceId';

ALTER TABLE meta_data ADD namespace_id VARCHAR2(50) NOT NULL;
COMMENT ON COLUMN meta_data.namespace_id IS 'namespaceId';

ALTER TABLE app_auth ADD namespace_id VARCHAR2(50) NOT NULL;
COMMENT ON COLUMN app_auth.namespace_id IS 'namespaceId';

ALTER TABLE discovery ADD namespace_id VARCHAR2(50) NOT NULL;
COMMENT ON COLUMN discovery.namespace_id IS 'namespaceId';

ALTER TABLE discovery_upstream ADD namespace_id VARCHAR2(50) NOT NULL;
COMMENT ON COLUMN discovery_upstream.namespace_id IS 'namespaceId';

ALTER TABLE proxy_selector ADD namespace_id VARCHAR2(50) NOT NULL;
COMMENT ON COLUMN proxy_selector.namespace_id IS 'namespaceId';

ALTER TABLE alert_receiver ADD namespace_id VARCHAR2(50) NOT NULL;
COMMENT ON COLUMN alert_receiver.namespace_id IS 'namespaceId';

UPDATE selector SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE rule SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE meta_data SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE app_auth SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE discovery SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE discovery_upstream SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE proxy_selector SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

UPDATE alert_receiver SET namespace_id = '649330b6-c2d7-4edc-be8e-8a54df9eb385' WHERE namespace_id IS NULL;

-- ----------------------------
-- Table structure for scale
-- ----------------------------
create table scale_policy
(
    id             varchar(128) not null,
    sort           number not null,
    status         number not null,
    num            number,
    begin_time     timestamp(3),
    end_time       timestamp(3),
    date_created   timestamp(3) default SYSDATE not null,
    date_updated   timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
comment on column SCALE_POLICY.id
    is 'primary key id';
comment on column SCALE_POLICY.sort
    is 'sort';
comment on column SCALE_POLICY.status
    is 'status 1:enable 0:disable';
comment on column SCALE_POLICY.num
    is 'number of bootstrap';
comment on column SCALE_POLICY.begin_time
    is 'begin time';
comment on column SCALE_POLICY.end_time
    is 'end time';
comment on column SCALE_POLICY.date_created
    is 'create time';
comment on column SCALE_POLICY.date_updated
    is 'update time';

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (scale_policy(id)) */ INTO scale_policy (id, sort, status, num, begin_time, end_time, date_created, date_updated) VALUES ('1', 3, 0, 10, NULL, NULL, '2024-07-31 20:00:00.000', '2024-07-31 20:00:00.000');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (scale_policy(id)) */ INTO scale_policy (id, sort, status, num, begin_time, end_time, date_created, date_updated) VALUES ('2', 2, 0, 10, '2024-07-31 20:00:00.000', '2024-08-01 20:00:00.000', '2024-07-31 20:00:00.000', '2024-07-31 20:00:00.000');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (scale_policy(id)) */ INTO scale_policy (id, sort, status, num, begin_time, end_time, date_created, date_updated) VALUES ('3', 1, 0, NULL, NULL, NULL, '2024-07-31 20:00:00.000', '2024-07-31 20:00:00.000');

create table scale_rule
(
    id             varchar(128) not null,
    metric_name    varchar(128) not null,
    type           number not null,
    sort           number not null,
    status         number not null,
    minimum        varchar(128),
    maximum        varchar(128),
    date_created   timestamp(3) default SYSDATE not null,
    date_updated   timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
comment on column SCALE_RULE.id
    is 'primary key id';
comment on column SCALE_RULE.metric_name
    is 'metric name';
comment on column SCALE_RULE.type
    is 'type 0:shenyu 1:k8s 2:others';
comment on column SCALE_RULE.sort
    is 'sort';
comment on column SCALE_RULE.status
    is 'status 1:enable 0:disable';
comment on column SCALE_RULE.minimum
    is 'minimum of metric';
comment on column SCALE_RULE.maximum
    is 'maximum of metric';
comment on column SCALE_RULE.date_created
    is 'create time';
comment on column SCALE_RULE.date_updated
    is 'update time';

create table scale_history
(
    id             varchar(128) not null,
    config_id      number not null,
    num            number not null,
    action         number not null,
    msg            clob,
    date_created   timestamp(3) default SYSDATE not null,
    date_updated   timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
comment on column SCALE_HISTORY.id
    is 'primary key id';
comment on column SCALE_HISTORY.config_id
    is '0:manual 1:period 2:dynamic';
comment on column SCALE_HISTORY.num
    is 'number of bootstrap';
comment on column SCALE_HISTORY.action
    is 'status 1:enable 0:disable';
comment on column SCALE_HISTORY.msg
    is 'message';
comment on column SCALE_HISTORY.date_created
    is 'create time';
comment on column SCALE_HISTORY.date_updated
    is 'update time';


create table namespace_user_rel
(
    id             varchar(128) not null,
    namespace_id   varchar(50) NOT NULL,
    user_id        varchar(128) NOT NULL,
    date_created   timestamp(3) default SYSDATE not null,
    date_updated   timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
comment on column NAMESPACE_USER_REL.id
    is 'primary key id';
comment on column NAMESPACE_USER_REL.namespace_id
    is 'namespace_id';
comment on column NAMESPACE_USER_REL.user_id
    is 'user_id';
comment on column NAMESPACE_USER_REL.date_created
    is 'create time';
comment on column NAMESPACE_USER_REL.date_updated
    is 'update time';

