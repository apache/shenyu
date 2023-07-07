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

ALTER TABLE selector ADD match_restful NUMBER(3) not null;
COMMENT ON COLUMN SELECTOR.match_restful IS 'whether to match restful(0 cache, 1 not cache)';
COMMIT;

ALTER TABLE rule ADD match_restful NUMBER(3) not null;
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

  INSERT INTO `plugin` VALUES ('42', 'tcp', NULL, 'Proxy', 320, 1, '2023-05-30 18:02:53', '2022-05-30 18:02:53',null);
 INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled, plugin_jar) VALUES ('42', 'tcp', 'Proxy', 320, null, '1', null);
