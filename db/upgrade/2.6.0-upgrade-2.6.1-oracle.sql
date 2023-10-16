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

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1618229897206079499', 'operator', 'OPERATOR', 'isBlank', 'isBlank', 'isBlank', 8, 1);

--- clickhouse plugin
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204172777', '38', 'ttl', 'ttl', 3, 3, 10,  '{\"required\":\"0\",\"defaultValue\":\"30\"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1572621976689762310', 'engine', 'engine', 'ReplicatedReplicatedMergeTree', 'ReplicatedReplicatedMergeTree', '', 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1572621976689762311', 'engine', 'engine', 'ReplicatedMergeTree', 'ReplicatedMergeTree', '', 3, 1);
---- clickhouse plugin end

--elasticsearch plugin
update plugin_handle set plugin_id = '32', field = 'indexName', label = 'indexName', data_type = 2, type = 3, sort = 10, ext_obj = '{\"required\":\"0\",\"defaultValue\":\"shenyu-access-logging\"}'
where plugin_id = '32' and field = 'index';
--elasticsearch plugin end

--dubbo plugin
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204173923', '6', 'timeout', 'timeout', 3, 2, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204173924', '6', 'retries', 'retries', 3, 2, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1529402613204173925', '6', 'loadBalance', 'loadStrategy', 3, 2, 0, null);
--dubbo plugin end

--alert notice menu
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1697141926247763968','1346776175553376256','SHENYU.MENU.SYSTEM.MANAGMENT.ALERT','','/system/alert','',1,3,'alert',0,0,'',1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1697146375729025024', '1697141926247763968', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, 'unordered-list', 1, 0, 'system:alert:list', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1697145808210333696','1697141926247763968','SHENYU.BUTTON.SYSTEM.ADD','','','',2,0,'plus',1,0,'system:alert:add',1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1697146617513873408','1697141926247763968','SHENYU.BUTTON.SYSTEM.DELETE','','','',2,0,'delete',1,0,'system:alert:delete',1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES ('1697146860540235776','1697141926247763968','SHENYU.BUTTON.SYSTEM.EDIT','','','',2,0,'edit',1,0,'system:alert:edit',1);

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1697141926281318400', '1346358560427216896', '1697141926247763968');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1697145808239693824', '1346358560427216896', '1697145808210333696');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1697146375754190848', '1346358560427216896', '1697146375729025024');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1697146617543233536', '1346358560427216896', '1697146617513873408');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1697146860569595904', '1346358560427216896', '1697146860540235776');
--alert notice menu end

-- add percentage field for rewrite plugin
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ INTO plugin_handle (id, plugin_id, field, label, data_type, type, sort, ext_obj) VALUES ('1697146860569596304', '3', 'percentage', 'percentage', 1, 2, 3, null);

-- ----------------------------
-- Table structure for alert_receiver
-- ----------------------------
create table alert_receiver
(
    id                   varchar(128)   not null,
    name                 varchar(255)   not null,
    enable               NUMBER(10)     not null,
    type                 NUMBER(10)     not null,
    phone                varchar(255),
    email                varchar(255),
    hook_url             varchar(255),
    wechat_id            varchar(255),
    access_token         varchar(255),
    tg_bot_token         varchar(255),
    tg_user_id           varchar(255),
    slack_web_hook_url   varchar(255),
    corp_id              varchar(255),
    agent_id             varchar(255),
    app_secret           varchar(255),
    discord_channel_id   varchar(255),
    discord_bot_token    varchar(255),
    smn_ak               varchar(255),
    smn_sk               varchar(255),
    smn_project_id       varchar(255),
    smn_region           varchar(255),
    smn_topic_urn        varchar(255),
    match_all            NUMBER(10),
    labels               varchar(255),
    levels               varchar(255),
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
on column alert_receiver.name
  is 'alert receiver name';
comment
on column alert_receiver.enable
  is 'enable or not';
comment
on column alert_receiver.type
  is 'notice type 0-SMS 1-Email 2-webhook 3-WeChat Official Account 4-Enterprise WeChat Robot 5-DingTalk Robot 6-FeiShu Robot 7-Telegram Bot 8-SlackWebHook 9-Discord Bot 10-Enterprise WeChat';
comment
on column alert_receiver.match_all
  is 'enable or not';
comment
on column alert_receiver.labels
  is 'alarm labels';
comment
on column alert_receiver.levels
  is 'alarm levels';
comment
on column alert_receiver.date_created
  is 'create time';
comment
on column alert_receiver.date_updated
  is 'update time';
