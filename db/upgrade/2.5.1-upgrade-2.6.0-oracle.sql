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

ALTER TABLE selector ADD match_restful NUMBER(3) NOT NULL DEFAULT 0;
COMMENT ON COLUMN SELECTOR.match_restful IS 'whether to match restful(0 cache, 1 not cache)';
COMMIT;

ALTER TABLE rule ADD match_restful NUMBER(3) NOT NULL DEFAULT 0;
COMMENT ON COLUMN RULE.match_restful IS 'whether to match restful(0 cache, 1 not cache)';
COMMIT;

/* insert plugin_handle data for plugin CryptorRequest */
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1630760188111376384', '24', 'mapType', 'mapType', 3, 2, 3, '{\"required\":\"0\",\"defaultValue\":\"all\",\"rule\":\"\"}');
/* insert plugin_handle data for plugin cryptorResponse */
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1630768384280514560', '25', 'mapType', 'mapType', 3, 2, 4, '{\"required\":\"0\",\"defaultValue\":\"all\",\"rule\":\"\"}');

/* insert plugin_handle data for plugin_handle mapType */
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1630761573833920512', 'mapType', 'mapType', 'all', 'all', '', 0, 1);
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1630761984393367552', 'mapType', 'mapType', 'field', 'field', '', 1, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1545812228228259844', 'loadBalance', 'LOAD_BALANCE', 'p2c', 'p2c', 'p2c', 4, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1545812228228259845', 'loadBalance', 'LOAD_BALANCE', 'shortestResponse', 'shortestResponse', 'shortestResponse', 5, 1);

/* add column into plugin table */
ALTER TABLE plugin ADD plugin_jar BLOB NULL;
COMMENT ON COLUMN plugin.plugin_jar IS 'plugin jar';



/* create new tables discovery,discovery_handler,discovery_rel,discovery_upstream,proxy_selector for discovery */
create table discovery
(
    id                VARCHAR2(128) not null,
    name            VARCHAR2(255) not null,
    level            VARCHAR2(64) not null,
    plugin_name      VARCHAR2(255),
    type            VARCHAR2(64) not null,
    server_list      VARCHAR2(255),
    props            CLOB,
    date_created      timestamp(3) default SYSDATE not null,
    date_updated      timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column DISCOVERY.id
  is 'primary key id';
comment on column DISCOVERY.name
  is 'the discovery name';
comment on column DISCOVERY.level
  is '0 selector,1 plugin  2 global';
comment on column DISCOVERY.plugin_name
  is 'the plugin name';
comment on column DISCOVERY.type
  is 'local,zookeeper,etcd,consul,nacos';
comment on column DISCOVERY.server_list
  is 'register server url (,)';
comment on column DISCOVERY.props
  is 'the discovery pops (json)';
comment on column DISCOVERY.date_created
  is 'create time';
comment on column DISCOVERY.date_updated
  is 'update time';


create table discovery_handler
(
    id                VARCHAR2(128) not null,
    discovery_id            VARCHAR2(128) not null,
    handler            VARCHAR2(255) not null,
    listener_node      VARCHAR2(255),
    props            CLOB,
    date_created      timestamp(3) default SYSDATE not null,
    date_updated      timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column DISCOVERY_HANDLER.id
  is 'primary key id';
comment on column DISCOVERY_HANDLER.discovery_id
  is 'the discovery id';
comment on column DISCOVERY_HANDLER.handler
  is 'the handler';
comment on column DISCOVERY_HANDLER.listener_node
  is 'register server listener to node';
comment on column DISCOVERY_HANDLER.props
  is 'the discovery pops (json)';
comment on column DISCOVERY_HANDLER.date_created
  is 'create time';
comment on column DISCOVERY_HANDLER.date_updated
  is 'update time';


create table discovery_rel
(
    id                VARCHAR2(128) not null,
    plugin_name      VARCHAR2(255) not null,
    discovery_handler_id            VARCHAR2(128) not null,
    selector_id      VARCHAR2(128),
    proxy_selector_id      VARCHAR2(128),
    date_created      timestamp(3) default SYSDATE not null,
    date_updated      timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column DISCOVERY_REL.id
  is 'primary key id';
comment on column DISCOVERY_REL.plugin_name
  is 'the plugin name';
comment on column DISCOVERY_REL.discovery_handler_id
  is 'the discovery handler id';
comment on column DISCOVERY_REL.selector_id
  is 'the selector id';
comment on column DISCOVERY_REL.proxy_selector_id
  is 'the proxy selector id';
comment on column DISCOVERY_REL.date_created
  is 'create time';
comment on column DISCOVERY_REL.date_updated
  is 'update time';


create table discovery_upstream
(
    id                VARCHAR2(128) not null,
    discovery_handler_id   VARCHAR2(128) not null,
    protocol            VARCHAR2(64),
    url      VARCHAR2(64) not null,
    status      NUMBER(10) not null,
    weight      NUMBER(10)  not null,
    props       CLOB,
    date_created      timestamp(3) default SYSDATE not null,
    date_updated      timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column DISCOVERY_UPSTREAM.id
  is 'primary key id';
comment on column DISCOVERY_UPSTREAM.discovery_handler_id
  is 'the discovery handler id';
comment on column DISCOVERY_UPSTREAM.protocol
  is 'for http, https, tcp, ws';
comment on column DISCOVERY_UPSTREAM.url
  is 'ip:port';
comment on column DISCOVERY_UPSTREAM.status
  is 'type (0, healthy, 1 unhealthy)';
comment on column DISCOVERY_UPSTREAM.weight
  is 'the weight for lists';
comment on column DISCOVERY_UPSTREAM.props
  is 'the discovery pops (json)';
comment on column DISCOVERY_UPSTREAM.date_created
  is 'create time';
comment on column DISCOVERY_UPSTREAM.date_updated
  is 'update time';


create table proxy_selector
(
    id                VARCHAR2(128) not null,
    name   VARCHAR2(255) not null,
    plugin_name            VARCHAR2(255) not null,
    type      VARCHAR2(64) not null,
    forward_port      NUMBER(10) not null,
    props       CLOB,
    date_created      timestamp(3) default SYSDATE not null,
    date_updated      timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column PROXY_SELECTOR.id
  is 'primary key id';
comment on column PROXY_SELECTOR.name
  is 'the proxy name';
comment on column PROXY_SELECTOR.plugin_name
  is 'the plugin name';
comment on column PROXY_SELECTOR.type
  is 'proxy type for tcp, upd, ws';
comment on column PROXY_SELECTOR.forward_port
  is 'the proxy forward port';
comment on column PROXY_SELECTOR.props
  is 'the discovery pops (json)';
comment on column PROXY_SELECTOR.date_created
  is 'create time';
comment on column PROXY_SELECTOR.date_updated
  is 'update time';

ALTER TABLE shenyu_dict MODIFY dict_value varchar2(2048);

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1679002911061737472', 'discoveryMode', 'DISCOVERY_MODE', 'zookeeper', '{"baseSleepTimeMilliseconds":"1000","maxRetries":"3","maxSleepTimeMilliseconds":"1000","connectionTimeoutMilliseconds":"1000","sessionTimeoutMilliseconds":"1000","namespace":"","digest":null}', 'discoery mode to link zookeeper', 0, 1);


INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled, plugin_jar) VALUES ('42', 'tcp', 'Proxy', 320, null, '1', null);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('43', 'loggingHuaweiLts', 'Logging', 177, '{"totalSizeInBytes":"104857600","maxBlockMs":"0","ioThreadCount":"1","batchSizeThresholdInBytes":"524288","batchCountThreshold":"4096","lingerMs":"2000","retries":"100","baseRetryBackoffMs":"100","maxRetryBackoffMs":"100","enableLocalTest":"true","setGiveUpExtraLongSingleLog":"false"}','0');


insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172863', '43', 'projectId', 'projectId', 2, 3, 0, '{"required":"1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172864', '43', 'logGroupId', 'logGroupId', 2, 3, 1, '{"required":"1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172865', '43', 'logStreamId', 'logStreamId', 2, 3, 2, '{"required":"1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172866', '43', 'accessKeyId', 'AccessKey', 2, 3, 4, '{"required":"1","placeholder":"accessKeyId","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172867', '43','accessKeySecret', 'accessKey', 2, 3, 5, '{"required":"1","placeholder":"accessKeySecret","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172868', '43', 'regionName', 'regionName', 2, 3, 6, '{"required":"1","placeholder":"regionName","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172869', '43', 'totalSizeInBytes', 'totalSizeInBytes', 1, 3, 8, '{"required":"0","defaultValue":"104857600","placeholder":"totalSizeInBytes","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172870', '43', 'maxBlockMs', 'maxBlockMs', 1, 3, 9, '{"required":"0","defaultValue":"0","placeholder":"maxBlockMs","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172871', '43', 'ioThreadCount', 'ioThreadCount', 1, 3, 10, '{"required":"0","defaultValue":"1","placeholder":"ioThreadCount","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172872', '43', 'batchSizeThresholdInBytes', 'batchSizeThresholdInBytes', 1, 3, 11, '{"required":"0","defaultValue":"524288","placeholder":" batchSizeThresholdInBytes","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172873', '43', 'batchCountThreshold', 'batchCountThreshold', 1, 3, 12, '{"required":"0","defaultValue":"4096","placeholder":" batchCountThreshold","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172874', '43', 'lingerMs', 'lingerMs', 1, 3, 12, '{"required":"0","defaultValue":"2000","placeholder":"lingerMs","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172875', '43', 'retries', 'retries', 1, 3, 13, '{"required":"0","defaultValue":"100","placeholder":"retries","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172876', '43', 'baseRetryBackoffMs', 'baseRetryBackoffMs', 1, 3, 14, '{"required":"0","defaultValue":"100","placeholder":"baseRetryBackoffMs","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172877', '43', 'maxRetryBackoffMs', 'maxRetryBackoffMs', 1, 3, 15, '{"required":"0","defaultValue":"100","placeholder":"maxRetryBackoffMs","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172878', '43', 'enableLocalTest', 'enableLocalTest', 2, 3, 15, '{"required":"0","defaultValue":"false","placeholder":"enableLocalTest","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1529402613204172879', '43', 'setGiveUpExtraLongSingleLog', 'setGiveUpExtraLongSingleLog', 2, 3, 16, '{"required":"0","defaultValue":"false","placeholder":"setGiveUpExtraLongSingleLog","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204172880', '43', 'maskStatus', 'maskStatus', 3, 2, 2, '{"required":"0","defaultValue":"false","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204172881', '43', 'keyword', 'keyword', 2, 2, 0, '{"required":"0","placeholder":"please use ‘;’ to split keyword","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204172882', '43', 'maskType', 'maskType', 3, 2, 1, '{"required":"0","defaultValue":"dataMaskByMD5","rule":""}');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1678293333363167232', '42', 'discoveryHandler', 'discoveryHandler', 2, 1, 0, '{"required":"0","defaultValue":"url,protocol,status,weight","rule":""}');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1678997037438107648', '42', 'bossGroupThreadCount', 'bossGroupThreadCount', 2, 1, 1, '{"required":"0","defaultValue":"1","rule":""}');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1678997142656417792', '42', 'workerGroupThreadCount', 'workerGroupThreadCount', 2, 1, 2, '{"required":"0","defaultValue":"12","rule":""}');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1678997399104552960', '42', 'clientMaxIdleTimeMs', 'clientMaxIdleTimeMs', 2, 1, 7, '{"required":"0","defaultValue":"30000","rule":""}');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1678997479614218240', '42', 'clientPendingAcquireMaxCount', 'clientPendingAcquireMaxCount', 2, 1, 4, '{"required":"0","defaultValue":"5","rule":""}');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1678996921914392576', '42', 'loadBalance', 'loadBalance', 3, 1, 3, '{"required":"0","defaultValue":"random","rule":""}');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1678997769998467072', '42', 'clientMaxLifeTimeMs', 'clientMaxLifeTimeMs', 2, 1, 8, '{"required":"0","defaultValue":"60000","rule":""}');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1678997277012557824', '42', 'clientMaxConnections', 'clientMaxConnections', 2, 1, 6, '{"required":"0","defaultValue":"20","rule":""}');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ INTO plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
VALUES ('1678997557628272640', '42', 'clientPendingAcquireTimeout', 'clientPendingAcquireTimeout', 2, 1, 5, '{"required":"0","defaultValue":"5","rule":""}');

UPDATE "public"."plugin" set config = '{"registerProtocol":"zk",registerAddress":"127.0.0.1:2181","corethreads":0,"threads":2147483647,"queues":0,"threadpool":"shared"}' WHERE id = '17';
UPDATE "public"."plugin_handle" set field = 'registerProtocol', label = 'registerProtocol', sort = 0, ext_obj = '{"required":"0","defaultValue":"direct","placeholder":"registerProtocol","rule":""}' where id = '1518229897214468142';

INSERT INTO "public"."plugin_handle" VALUES ('1678997557628272641', '17', 'registerAddress', 'registerAddress', 2, 3, 1, '{"required":"0","defaultValue":"127.0.0.1:2181","placeholder":"registerAddress","rule":""}', '2022-05-25 18:08:01', '2022-05-25 18:08:01');
