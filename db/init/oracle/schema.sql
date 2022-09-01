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

create table dashboard_user
(
    id           VARCHAR2(128) not null,
    user_name    VARCHAR2(64) not null,
    password     VARCHAR2(128),
    role         NUMBER(10) not null,
    enabled      NUMBER(3) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id),
    constraint unique_user_name unique (user_name)
);
-- Add comments to the columns
comment on column DASHBOARD_USER.id
  is 'primary key id';
comment on column DASHBOARD_USER.user_name
  is 'user name';
comment on column DASHBOARD_USER.password
  is 'user password';
comment on column DASHBOARD_USER.role
  is 'role';
comment on column DASHBOARD_USER.enabled
  is 'delete or not';
comment on column DASHBOARD_USER.date_created
  is 'create time';
comment on column DASHBOARD_USER.date_updated
  is 'update time';

create table plugin
(
    id           VARCHAR2(128) not null,
    name         VARCHAR2(62) not null,
    config       CLOB,
    role         VARCHAR2(64) not null,
    sort         NUMBER(10),
    enabled      NUMBER(3) default '0' not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column PLUGIN.id
  is 'primary key id';
comment on column PLUGIN.name
  is 'plugin name';
comment on column PLUGIN.config
  is 'plugin configuration';
comment on column PLUGIN.role
  is 'plugin role';
comment on column PLUGIN.sort
  is 'sort';
comment on column PLUGIN.enabled
  is 'plugin whether to open (0 not open, 1 open)';
comment on column PLUGIN.date_created
  is 'create time';
comment on column PLUGIN.date_updated
  is 'update time';

create table plugin_handle
(
    id           VARCHAR2(128) not null,
    plugin_id    VARCHAR2(128) not null,
    field        VARCHAR2(100) not null,
    label        VARCHAR2(100),
    data_type    NUMBER(5) default '1' not null,
    type         NUMBER(5),
    sort         NUMBER(10),
    ext_obj      clob,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id),
    constraint plugin_id_field_type unique (plugin_id,field,type)
);
-- Add comments to the columns
comment on column PLUGIN_HANDLE.plugin_id
  is 'plugin id';
comment on column PLUGIN_HANDLE.field
  is 'field';
comment on column PLUGIN_HANDLE.label
  is 'label';
comment on column PLUGIN_HANDLE.data_type
  is 'data type 1 number 2 string';
comment on column PLUGIN_HANDLE.type
  is 'type, 1 means selector, 2 means rule, 3 means plugin';
comment on column PLUGIN_HANDLE.sort
  is 'sort';
comment on column PLUGIN_HANDLE.ext_obj
  is 'extra configuration (json format data)';
comment on column PLUGIN_HANDLE.date_created
  is 'create time';
comment on column PLUGIN_HANDLE.date_updated
  is 'update time';

create table selector
(
    id           VARCHAR2(128) not null primary key,
    plugin_id    VARCHAR2(128) not null,
    name         VARCHAR2(64) not null,
    match_mode   NUMBER(10) not null,
    type         NUMBER(10) not null,
    sort         NUMBER(10) not null,
    handle       VARCHAR2(1024),
    enabled      NUMBER(3) not null,
    loged        NUMBER(3) not null,
    continued    NUMBER(3) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null
);
-- Add comments to the columns
comment on column SELECTOR.id
  is 'primary key id varchar';
comment on column SELECTOR.plugin_id
  is 'plugin id';
comment on column SELECTOR.name
  is 'selector name';
comment on column SELECTOR.match_mode
  is 'matching mode (0 and 1 or)';
comment on column SELECTOR.type
  is 'type (0 full flow, 1 custom flow)';
comment on column SELECTOR.sort
  is 'sort';
comment on column SELECTOR.handle
  is 'processing logic (here for different plugins, there will be different fields to identify different processes, all data in JSON format is stored)';
comment on column SELECTOR.enabled
  is 'whether to open (0 not open, 1 open)';
comment on column SELECTOR.loged
  is 'whether to print the log (0 not print, 1 print)';
comment on column SELECTOR.continued
  is 'whether to continue execution';
comment on column SELECTOR.date_created
  is 'create time';
comment on column SELECTOR.date_updated
  is 'update time';

create table selector_condition
(
    id           VARCHAR2(128) not null,
    selector_id  VARCHAR2(128) not null,
    param_type   VARCHAR2(64) not null,
    operator     VARCHAR2(64) not null,
    param_name   VARCHAR2(64) not null,
    param_value  VARCHAR2(64) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column SELECTOR_CONDITION.id
  is 'primary key id';
comment on column SELECTOR_CONDITION.selector_id
  is 'selector id';
comment on column SELECTOR_CONDITION.param_type
  is 'parameter type (to query uri, etc.)';
comment on column SELECTOR_CONDITION.operator
  is 'matching character (=> <like matching)';
comment on column SELECTOR_CONDITION.param_name
  is 'parameter name';
comment on column SELECTOR_CONDITION.param_value
  is 'parameter value';
comment on column SELECTOR_CONDITION.date_created
  is 'create time';
comment on column SELECTOR_CONDITION.date_updated
  is 'update time';

create table rule
(
    id           VARCHAR2(128) not null PRIMARY KEY,
    selector_id  VARCHAR2(128) not null,
    match_mode   NUMBER(10) not null,
    name         VARCHAR2(128) not null,
    enabled      NUMBER(3) not null,
    loged        NUMBER(3) not null,
    sort         NUMBER(10) not null,
    handle       VARCHAR2(1024),
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null
);
-- Add comments to the columns
comment on column RULE.id
  is 'primary key id';
comment on column RULE.selector_id
  is 'selector id';
comment on column RULE.match_mode
  is 'matching mode (0 and 1 or)';
comment on column RULE.name
  is 'rule name';
comment on column RULE.enabled
  is 'whether to open (0 not open, 1 open)';
comment on column RULE.loged
  is 'whether to log or not (0 not print, 1 print)';
comment on column RULE.sort
  is 'sort';
comment on column RULE.handle
  is 'processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)';
comment on column RULE.date_created
  is 'create time';
comment on column RULE.date_updated
  is 'update time';

create table rule_condition
(
    id           VARCHAR2(128) not null PRIMARY KEY,
    rule_id      VARCHAR2(128) not null,
    param_type   VARCHAR2(64) not null,
    operator     VARCHAR2(64) not null,
    param_name   VARCHAR2(64) not null,
    param_value  VARCHAR2(64) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null
);
-- Add comments to the columns
comment on column RULE_CONDITION.id
  is 'primary key id';
comment on column RULE_CONDITION.rule_id
  is 'rule id';
comment on column RULE_CONDITION.param_type
  is 'parameter type (post query uri, etc.)';
comment on column RULE_CONDITION.operator
  is 'matching character (=> <like match)';
comment on column RULE_CONDITION.param_name
  is 'parameter name';
comment on column RULE_CONDITION.param_value
  is 'parameter value';
comment on column RULE_CONDITION.date_created
  is 'create time';
comment on column RULE_CONDITION.date_updated
  is 'update time';

create table meta_data
(
    id              VARCHAR2(128) not null,
    app_name        VARCHAR2(255) not null,
    path            VARCHAR2(255) not null,
    path_desc       VARCHAR2(255),
    rpc_type        VARCHAR2(64) not null,
    service_name    VARCHAR2(255),
    method_name     VARCHAR2(255),
    parameter_types VARCHAR2(255),
    rpc_ext         VARCHAR2(512),
    date_created    timestamp(3) default SYSDATE not null,
    date_updated    timestamp(3) default SYSDATE not null,
    enabled         NUMBER(3) default '0' not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column META_DATA.id
  is 'id';
comment on column META_DATA.app_name
  is 'application name';
comment on column META_DATA.path
  is 'path, cannot be repeated';
comment on column META_DATA.path_desc
  is 'path description';
comment on column META_DATA.rpc_type
  is 'rpc type';
comment on column META_DATA.service_name
  is 'service name';
comment on column META_DATA.method_name
  is 'method name';
comment on column META_DATA.parameter_types
  is 'parameter types are provided with multiple parameter types separated by commas';
comment on column META_DATA.rpc_ext
  is 'rpc extended information, json format';
comment on column META_DATA.date_created
  is 'create time';
comment on column META_DATA.date_updated
  is 'update time';
comment on column META_DATA.enabled
  is 'enabled state (0 close, 1 enabled) ';

create table operation_record_log
(
    id                NUMBER(20) not null PRIMARY KEY,
    color             VARCHAR2(20) not null,
    context           CLOB not null,
    operator          VARCHAR2(200) not null,
    operation_time    timestamp(3) not null,
    operation_type    VARCHAR2(60) DEFAULT 'update' not null
);
-- Add comments to the columns
comment on column OPERATION_RECORD_LOG.id
  is 'id';
comment on column OPERATION_RECORD_LOG.color
  is 'log color';
comment on column OPERATION_RECORD_LOG.context
  is 'log context';
comment on column OPERATION_RECORD_LOG.operator
  is 'operator [user or app]]';
comment on column OPERATION_RECORD_LOG.operation_time
  is 'operation time';
comment on column OPERATION_RECORD_LOG.operation_type
  is 'operation type：create/update/delete/register...';

create sequence operation_record_log_seq
    increment by 1
    START WITH 1
    NOMAXVALUE
    NOCYCLE
    NOCACHE;

create table app_auth
(
    id           VARCHAR2(128) not null,
    app_key      VARCHAR2(32) not null,
    app_secret   VARCHAR2(128) not null,
    user_id      VARCHAR2(128),
    phone        VARCHAR2(255),
    ext_info     VARCHAR2(1024),
    open         NUMBER(3) not null,
    enabled      NUMBER(3) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column APP_AUTH.id
  is 'primary key id';
comment on column APP_AUTH.app_key
  is 'application identification key';
comment on column APP_AUTH.app_secret
  is 'encryption algorithm secret';
comment on column APP_AUTH.user_id
  is 'user id';
comment on column APP_AUTH.phone
  is 'phone number when the user applies';
comment on column APP_AUTH.ext_info
  is 'extended parameter json';
comment on column APP_AUTH.open
  is 'open auth path or not (0 not open, 1 open) ';
comment on column APP_AUTH.enabled
  is 'delete or not (0 close, 1 open) ';
comment on column APP_AUTH.date_created
  is 'create time';
comment on column APP_AUTH.date_updated
  is 'update time';

create table auth_param
(
    id           VARCHAR2(128) not null,
    auth_id      VARCHAR2(128),
    app_name     VARCHAR2(255) not null,
    app_param    VARCHAR2(255),
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column AUTH_PARAM.id
  is 'primary key id';
comment on column AUTH_PARAM.auth_id
  is 'authentication table id';
comment on column AUTH_PARAM.app_name
  is 'business Module';
comment on column AUTH_PARAM.app_param
  is 'service module parameters (parameters that need to be passed by the gateway) json type';
comment on column AUTH_PARAM.date_created
  is 'create time';
comment on column AUTH_PARAM.date_updated
  is 'update time';

create table auth_path
(
    id           VARCHAR2(128) not null,
    auth_id      VARCHAR2(128) not null,
    app_name     VARCHAR2(255) not null,
    path         VARCHAR2(255) not null,
    enabled      NUMBER(3) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the columns
comment on column AUTH_PATH.id
  is 'primary key id';
comment on column AUTH_PATH.auth_id
  is 'auth table id';
comment on column AUTH_PATH.app_name
  is 'module';
comment on column AUTH_PATH.path
  is 'path';
comment on column AUTH_PATH.enabled
  is 'whether pass 1 is (0 close, 1 open) ';
comment on column AUTH_PATH.date_created
  is 'create time';
comment on column AUTH_PATH.date_updated
  is 'update time';

create table shenyu_dict
(
    id           VARCHAR2(128) not null,
    type         VARCHAR2(100) not null,
    dict_code    VARCHAR2(100) not null,
    dict_name    VARCHAR2(100) not null,
    dict_value   VARCHAR2(100),
    "desc"       VARCHAR2(255),
    sort         NUMBER(10) not null,
    enabled      NUMBER(3),
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id),
    constraint dict_type_dict_code_dict_name unique (type,dict_code,dict_name)
);
-- Add comments to the columns
comment on column SHENYU_DICT.id
  is 'primary key id';
comment on column SHENYU_DICT.type
  is 'type';
comment on column SHENYU_DICT.dict_code
  is 'dictionary encoding';
comment on column SHENYU_DICT.dict_name
  is 'dictionary name';
comment on column SHENYU_DICT.dict_value
  is 'dictionary value';
comment on column SHENYU_DICT."desc"
  is 'dictionary description or remarks';
comment on column SHENYU_DICT.sort
  is 'sort';
comment on column SHENYU_DICT.enabled
  is 'whether it is enabled (0 close, 1 open) ';
comment on column SHENYU_DICT.date_created
  is 'create time';
comment on column SHENYU_DICT.date_updated
  is 'update time';

create table role
(
    id           VARCHAR2(128) not null,
    role_name    VARCHAR2(32) not null,
    description  VARCHAR2(255),
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id,role_name)
);
-- Add comments to the table
comment on table ROLE
  is 'role table';
-- Add comments to the columns
comment on column ROLE.id
  is 'primary key id';
comment on column ROLE.role_name
  is 'role name';
comment on column ROLE.description
  is 'role describe';
comment on column ROLE.date_created
  is 'create time';
comment on column ROLE.date_updated
  is 'update time';

create table user_role
(
    id           VARCHAR2(128) not null,
    user_id      VARCHAR2(128) not null,
    role_id      VARCHAR2(128) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the table
comment on table USER_ROLE
  is 'user and role bind table';
-- Add comments to the columns
comment on column USER_ROLE.id
  is 'primary key id';
comment on column USER_ROLE.user_id
  is 'user primary key';
comment on column USER_ROLE.role_id
  is 'role primary key';
comment on column USER_ROLE.date_created
  is 'create time';
comment on column USER_ROLE.date_updated
  is 'update time';

create table permission
(
    id           VARCHAR2(128) not null,
    object_id    VARCHAR2(128) not null,
    resource_id  VARCHAR2(128) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the table
comment on table PERMISSION
  is 'permission table';
-- Add comments to the columns
comment on column PERMISSION.id
  is 'primary key id';
comment on column PERMISSION.object_id
  is 'user primary key id or role primary key id';
comment on column PERMISSION.resource_id
  is 'resource primary key id';
comment on column PERMISSION.date_created
  is 'create time';
comment on column PERMISSION.date_updated
  is 'update time';

create table "resource"
(
    id            VARCHAR2(128) not null,
    parent_id     VARCHAR2(128) null,
    title         VARCHAR2(128) not null,
    name          VARCHAR2(32) null,
    url           VARCHAR2(32) null,
    component     VARCHAR2(32) null,
    resource_type NUMBER(10) not null,
    sort          NUMBER(10) not null,
    icon          VARCHAR2(32) null,
    is_leaf       NUMBER(3) not null,
    is_route      NUMBER(10) not null,
    perms         VARCHAR2(64) null,
    status        NUMBER(10) not null,
    date_created  timestamp(3) default SYSDATE not null,
    date_updated  timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the table
comment on table "resource"
  is 'resource table';
-- Add comments to the columns
comment on column "resource".id
  is 'primary key id';
comment on column "resource".parent_id
  is 'resource parent primary key id';
comment on column "resource".title
  is 'title';
comment on column "resource".name
  is 'route name';
comment on column "resource".url
  is 'route url';
comment on column "resource".component
  is 'component';
comment on column "resource".resource_type
  is 'resource type eg 0:main menu 1:child menu 2:function button';
comment on column "resource".sort
  is 'sort';
comment on column "resource".icon
  is 'icon';
comment on column "resource".is_leaf
  is 'leaf node 0:no 1:yes';
comment on column "resource".is_route
  is 'route 1:yes 0:no';
comment on column "resource".perms
  is 'button permission description sys:user:add(add);sys:user:edit(edit)';
comment on column "resource".status
  is 'status 1:enable 0:disable';
comment on column "resource".date_created
  is 'create time';
comment on column "resource".date_updated
  is 'update time';

create table data_permission
(
    id           VARCHAR2(128) not null,
    user_id      VARCHAR2(128) not null,
    data_id      VARCHAR2(128) not null,
    data_type    NUMBER(10) not null,
    date_created timestamp(3) default SYSDATE not null,
    date_updated timestamp(3) default SYSDATE not null,
    PRIMARY KEY (id)
);
-- Add comments to the table
comment on table DATA_PERMISSION
  is 'data permission table';
-- Add comments to the columns
comment on column DATA_PERMISSION.id
  is 'primary key id';
comment on column DATA_PERMISSION.user_id
  is 'user primary key id';
comment on column DATA_PERMISSION.data_id
  is 'data(selector,rule) primary key id';
comment on column DATA_PERMISSION.data_type
  is '0 selector type , 1 rule type';
comment on column DATA_PERMISSION.date_created
  is 'create time';
comment on column DATA_PERMISSION.date_updated
  is 'update time';

/**default admin user**/
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(dashboard_user(id)) */ INTO dashboard_user (id, user_name, password, role, enabled) VALUES ('1','admin','ba3253876aed6bc22d4a6ff53d8406c6ad864195ed144ab5c87621b6c233b548baeae6956df346ec8c17f5ea10f35ee3cbc514797ed7ddd3145464e2a0bab413', '1', '1');

/** insert admin role */
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(user_role(id)) */ INTO user_role (id, user_id, role_id) VALUES ('1351007709096976384', '1', '1346358560427216896');
/** insert permission role for role */
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(role(id)) */ INTO role (id,role_name,description) VALUES ('1346358560427216896', 'super', 'Administrator');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(role(id)) */ INTO role (id,role_name,description) VALUES ('1385482862971723776', 'default', 'Standard');


insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885184', 'degradeRuleGrade', 'DEGRADE_GRADE_RT', 'slow call ratio', '0', 'degrade type-slow call ratio', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885185', 'degradeRuleGrade', 'DEGRADE_GRADE_EXCEPTION_RATIO', 'exception ratio', '1', 'degrade type-abnormal ratio', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885186', 'degradeRuleGrade', 'DEGRADE_GRADE_EXCEPTION_COUNT', 'exception number strategy', '2', 'degrade type-abnormal number strategy', 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885187', 'flowRuleGrade', 'FLOW_GRADE_QPS', 'QPS', '1', 'grade type-QPS', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885188', 'flowRuleGrade', 'FLOW_GRADE_THREAD', 'number of concurrent threads', '0', 'degrade type-number of concurrent threads', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885189', 'flowRuleControlBehavior', 'CONTROL_BEHAVIOR_DEFAULT', 'direct rejection by default', '0', 'control behavior-direct rejection by default', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885190', 'flowRuleControlBehavior', 'CONTROL_BEHAVIOR_WARM_UP', 'warm up', '1', 'control behavior-warm up', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885191', 'flowRuleControlBehavior', 'CONTROL_BEHAVIOR_RATE_LIMITER', 'constant speed queuing', '2', 'control behavior-uniform speed queuing', 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885192', 'flowRuleControlBehavior', 'CONTROL_BEHAVIOR_WARM_UP_RATE_LIMITER', 'preheating uniformly queued', '3', 'control behavior-preheating uniformly queued', 3, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885193', 'permission', 'REJECT', 'reject', 'reject', 'reject', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885194', 'permission', 'ALLOW', 'allow', 'allow', 'allow', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885195', 'algorithmName', 'ALGORITHM_SLIDINGWINDOW', 'slidingWindow', 'slidingWindow', 'Sliding window algorithm', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885196', 'algorithmName', 'ALGORITHM_LEAKYBUCKET', 'leakyBucket', 'leakyBucket', 'Leaky bucket algorithm', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885197', 'algorithmName', 'ALGORITHM_CONCURRENT', 'concurrent', 'concurrent', 'Concurrent algorithm', 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885198', 'algorithmName', 'ALGORITHM_TOKENBUCKET', 'tokenBucket', 'tokenBucket', 'Token bucket algorithm', 3, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885199', 'loadBalance', 'LOAD_BALANCE', 'roundRobin', 'roundRobin', 'roundRobin', 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885200', 'loadBalance', 'LOAD_BALANCE', 'random', 'random', 'random', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885201', 'loadBalance', 'LOAD_BALANCE', 'hash', 'hash', 'hash', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885202', 'status', 'DIVIDE_STATUS', 'close', 'false', 'close', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885203', 'status', 'DIVIDE_STATUS', 'open', 'true', 'open', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885204', 'multiRuleHandle', 'MULTI_RULE_HANDLE', 'multiple rule', '1', 'multiple rule', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897201885205', 'multiRuleHandle', 'MULTI_RULE_HANDLE', 'single rule', '0', 'single rule', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079488', 'multiSelectorHandle', 'MULTI_SELECTOR_HANDLE', 'multiple handle', '1', 'multiple handle', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079489', 'multiSelectorHandle', 'MULTI_SELECTOR_HANDLE', 'single handle', '0', 'single handle', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079490', 'matchMode', 'MATCH_MODE', 'and', '0', 'and', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079491', 'matchMode', 'MATCH_MODE', 'or', '1', 'or', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079492', 'operator', 'OPERATOR', 'match', 'match', 'match', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079493', 'operator', 'OPERATOR', '=', '=', '=', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079494', 'operator', 'OPERATOR', 'regex', 'regex', 'regex', 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079495', 'operator', 'OPERATOR', 'contains', 'contains', 'contains', 3, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079496', 'operator', 'OPERATOR', 'TimeBefore', 'TimeBefore', 'TimeBefore', 4, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079497', 'operator', 'OPERATOR', 'TimeAfter', 'TimeAfter', 'TimeAfter', 5, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079498', 'operator', 'OPERATOR', 'exclude', 'exclude', 'exclude', 6, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1618229897206079498', 'operator', 'OPERATOR', 'pathPattern', 'pathPattern', 'pathPattern', 7, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079499', 'paramType', 'PARAM_TYPE', 'post', 'post', 'post', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079500', 'paramType', 'PARAM_TYPE', 'uri', 'uri', 'uri', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079501', 'paramType', 'PARAM_TYPE', 'query', 'query', 'query', 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079502', 'paramType', 'PARAM_TYPE', 'host', 'host', 'host', 3, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079503', 'paramType', 'PARAM_TYPE', 'ip', 'ip', 'ip', 4, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079504', 'paramType', 'PARAM_TYPE', 'header', 'header', 'header', 5, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079505', 'paramType', 'PARAM_TYPE', 'cookie', 'cookie', 'cookie', 6, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079506', 'paramType', 'PARAM_TYPE', 'req_method', 'req_method', 'req_method', 7, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079507', 'keyResolverName', 'WHOLE_KEY_RESOLVER', 'whole', 'WHOLE_KEY_RESOLVER', 'Rate limit by all request', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079508', 'keyResolverName', 'REMOTE_ADDRESS_KEY_RESOLVER', 'remoteAddress', 'REMOTE_ADDRESS_KEY_RESOLVER', 'Rate limit by remote address', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079509', 'automaticTransitionFromOpenToHalfOpenEnabled', 'AUTOMATIC_HALF_OPEN', 'open', 'true', null, 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079510', 'automaticTransitionFromOpenToHalfOpenEnabled', 'AUTOMATIC_HALF_OPEN', 'close', 'false', null, 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079511', 'paramType', 'PARAM_TYPE', 'domain', 'domain', 'domain', 8, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079512', 'strategyName', 'STRATEGY_NAME', 'rsa', 'rsa', 'rsa strategy', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079513', 'way', 'WAY', 'encrypt', 'encrypt', 'encrypt', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079514', 'way', 'WAY', 'decrypt', 'decrypt', 'decrypt', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079515', 'mode', 'MODE', 'cluster', 'cluster', 'cluster', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079516', 'mode', 'MODE', 'sentinel', 'sentinel', 'sentinel', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079517', 'mode', 'MODE', 'standalone', 'standalone', 'standalone', 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079518', 'gray', 'GRAY_STATUS', 'close', 'false', 'close', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079519', 'gray', 'GRAY_STATUS', 'open', 'true', 'open', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079520', 'threadpool', 'THREADPOOL', 'shared', 'shared', null, 4, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079521', 'threadpool', 'THREADPOOL', 'fixed', 'fixed', null, 3, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079522', 'threadpool', 'THREADPOOL', 'eager', 'eager', null, 2, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079523', 'threadpool', 'THREADPOOL', 'cached', 'cached', null, 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079524', 'threadpool', 'THREADPOOL', 'limited', 'limited', null, 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079525', 'retryStrategy', 'RETRY_STRATEGY', 'current', 'current', 'current', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079526', 'retryStrategy', 'RETRY_STRATEGY', 'failover', 'failover', 'failover', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079527', 'table', 'INIT_FLAG', 'status', 'false', 'table(resource,permission) init status', 0, 0);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079528', 'compressAlg', 'COMPRESS_ALG', 'none', 'none', null, 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079529', 'compressAlg', 'COMPRESS_ALG', 'LZ4', 'LZ4', null, 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079530', 'cacheType', 'CACHE_TYPE_MEMORY', 'memory', 'memory', 'use memory to cache data', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079531', 'cacheType', 'CACHE_TYPE_REDIS', 'redis', 'redis', 'use redis to cache data', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079532', 'threadpool', 'THREADPOOL', 'default', 'default', null, 5, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079533', 'signRequestBody', 'SIGN_REQUEST_BODY', 'close', 'false', 'close', 1, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1518229897206079534', 'signRequestBody', 'SIGN_REQUEST_BODY', 'open', 'true', 'open', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1545811989312315392', 'apidoc', 'API_DOC_GLOBAL_FLAG', 'status', 'true', 'Global switching (on or off) of API documents.', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1545812101056962560', 'apidocEnv', 'ENV_LABEL_OFFLINE', 'Offline', 'http://127.0.0.1:9195', 'Offline environment', 0, 1);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1545812228228259840', 'apidocEnv', 'ENV_LABEL_ONLINE', 'Online', 'http://127.0.0.1:9196', 'Online environment', 1, 1);

/*plugin*/
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('1','sign','Authentication',  20, '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort,config,enabled) VALUES ('2','waf', 'Authentication', 50,'{"model":"black"}','0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('3','rewrite', 'HttpProcess', 90,'0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config,enabled) VALUES ('4','rateLimiter','FaultTolerance', 60,'{"master":"mymaster","mode":"standalone","url":"192.168.1.1:6379","password":"abc"}', '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config,enabled) VALUES ('5','divide', 'Proxy', 200,'{"multiSelectorHandle":"1","multiRuleHandle":"0"}','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config,enabled) VALUES ('6','dubbo','Proxy', 310,'{"register":"zookeeper://localhost:2181","multiSelectorHandle":"1","threadpool":"shared","corethreads":0,"threads":2147483647,"queues":0}', '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('8','springCloud','Proxy', 200, '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('9','hystrix', 'FaultTolerance', 130,'0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('10','sentinel', 'FaultTolerance', 140,'0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('11','sofa', 'Proxy', 310, '{"protocol":"zookeeper","register":"127.0.0.1:2181","threadpool":"shared"}', '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('12','resilience4j', 'FaultTolerance', 310,'0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('13', 'tars', 'Proxy', 310,'{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"shared"}','0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('14', 'contextPath', 'HttpProcess', 80,'1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('15', 'grpc', 'Proxy', 310,'{"multiSelectorHandle":"1","multiRuleHandle":"0","threadpool":"shared"}','0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('16', 'redirect', 'HttpProcess', 110,'0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('17', 'motan', 'Proxy', 310,'{"register":"127.0.0.1:2181","corethreads":0,"threads":2147483647,"queues":0,"threadpool":"shared"}','0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('18', 'loggingConsole', 'Logging', 160, '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('19', 'jwt', 'Authentication', 30, '{"secretKey":"key"}', '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('20', 'request', 'HttpProcess', 120, '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('21', 'oauth2', 'Authentication', 40, '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('22', 'paramMapping','HttpProcess', 70,'{"ruleHandlePageType":"custom"}', '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('23', 'modifyResponse', 'HttpProcess', 220, '{"ruleHandlePageType":"custom"}', '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('24', 'cryptorRequest', 'Cryptor', 100, '1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('25', 'cryptorResponse', 'Cryptor', 410, '1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('26', 'websocket', 'Proxy', 200, '{"multiSelectorHandle":"1"}', '1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('27', 'generalContext', 'Common', 125, '1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('28', 'mqtt', 'Proxy', 125, '{"port": 9500,"bossGroupThreadCount": 1,"maxPayloadSize": 65536,"workerGroupThreadCount": 12,"userName": "shenyu","password": "shenyu","isEncryptPassword": false,"encryptMode": "","leakDetectorLevel": "DISABLED"}', '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, config, enabled) VALUES ('29', 'loggingRocketMQ', 'Logging', 170,'{"topic":"shenyu-access-logging", "namesrvAddr": "localhost:9876","producerGroup":"shenyu-plugin-logging-rocketmq"}', '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled) VALUES ('30', 'cache', '{"cacheType":"memory"}', 'Cache', 10, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, role, sort, enabled) VALUES ('31', 'mock', 'Mock', 1, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled) VALUES ('32', 'loggingElasticSearch', '{"host":"localhost", "port": "9200"}', 'Logging', 190, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled) VALUES ('33', 'loggingKafka', '{"topic":"shenyu-access-logging", "namesrvAddr": "localhost:9092"}', 'Logging', 180, 0);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled) VALUES ('34', 'loggingAliyunSls', '{"projectName": "shenyu", "logStoreName": "shenyu-logstore", "topic": "shenyu-topic"}', 'Logging', 175, '0');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled) VALUES ('35', 'loggingPulsar', '{"topic":"shenyu-access-logging", "serviceUrl": "pulsar://localhost:6650"}', 'Logging', 185, '0')
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */ INTO plugin (id, name, config, role, sort, enabled) VALUES ('36', 'loggingTencentCls', '{"endpoint": "ap-guangzhou.cls.tencentcs.com", "topic": "shenyu-topic"}', 'Logging', 176, '0');




/*insert plugin_handle data for sentinel*/
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468120', '6', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468121', '6', 'protocol', 'protocol', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"http://","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468122', '6', 'status', 'status', 3, 1, 8, '{"defaultValue":"true","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468123', '6', 'timestamp', 'startupTime', 1, 1, 7, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468124', '6', 'upstreamHost', 'host', 2, 1, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468125', '6', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468126', '6', 'version', 'version', 2, 1, 4, '{"required":"0","placeholder":"version","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468127', '6', 'warmup', 'warmupTime', 1, 1, 6, '{"defaultValue":"0","placeholder":"warmup time ms)","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468128', '6', 'weight', 'weight', 1, 1, 5, '{"defaultValue":"50","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468129', '6', 'threadpool', 'threadpool', 3, 3, 0, '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468130', '6', 'corethreads', 'corethreads', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468131', '6', 'threads', 'threads', 1, 3, 0, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468132', '6', 'queues', 'queues', 1, 3, 0, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468133', '26', 'host', 'host', 2, 1, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468134', '26', 'protocol', 'protocol', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"ws://","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468135', '26', 'url', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468136', '26', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468137', '26', 'timestamp', 'startupTime', 1, 1, 3, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468138', '26', 'warmup', 'warmupTime', 1, 1, 5, '{"defaultValue":"0","placeholder":"warmup time ms)","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468139', '26', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468140', '26', 'loadBalance', 'loadStrategy', 3, 2, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468141', '26', 'retry', 'retryCount', 1, 2, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468142', '26', 'timeout', 'timeout', 1, 2, 2, '{"defaultValue":"3000","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468143', '26', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468144', '17', 'register', 'register', 2, 3, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468145', '28', 'port', 'port', 1, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468146', '28', 'bossGroupThreadCount', 'bossGroupThreadCount', 1, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468147', '28', 'maxPayloadSize', 'maxPayloadSize', 1, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468148', '28', 'workerGroupThreadCount', 'workerGroupThreadCount', 1, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468149', '28', 'userName', 'userName', 2, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468150', '28', 'password', 'password', 2, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468151', '28', 'isEncryptPassword', 'isEncryptPassword', 2, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468152', '28', 'encryptMode', 'encryptMode', 2, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468153', '28', 'leakDetectorLevel', 'leakDetectorLevel', 2, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468154', '29', 'topic', 'topic', 2, 3, 1, '{"required":"1","defaultValue":"shenyu-access-logging"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468155', '29', 'namesrvAddr', 'namesrvAddr', 2, 3, 2, '{"required":"1","defaultValue":"localhost:9876"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468156', '29', 'producerGroup', 'producerGroup', 2, 3, 3, '{"required":"1","defaultValue":"shenyu-plugin-logging-rocketmq"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468157', '29', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468158', '29', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468159', '29', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468160', '29', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468161', '29', 'topic', 'topic', 2, 1, 1, '{"required":"0","defaultValue":"","placeholder":"optional"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468162', '29', 'sampleRate', 'sampleRate', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"optional,0,0.01~1"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468163', '30', 'cacheType', 'cacheType', 3, 3, 1, '{"required":"1","defaultValue":"memory","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897218662400', '30', 'database', 'database', 1, 3, 2, '{"required":"0","defaultValue":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897218662401', '30', 'master', 'master', 2, 3, 3, '{"required":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897218662402', '30', 'mode', 'mode', 2, 3, 4, '{"required":"0","defaultValue":"standalone","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897218662403', '30', 'url', 'url', 2, 3, 5, '{"required":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897218662404', '30', 'password', 'password', 2, 3, 6, '{"required":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897218662405', '30', 'maxIdle', 'maxIdle', 1, 3, 7, '{"required":"0","defaultValue":"8","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897218662406', '30', 'minIdle', 'minIdle', 1, 3, 8, '{"required":"0","defaultValue":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897218662407', '30', 'maxActive', 'maxActive', 1, 3, 9, '{"required":"0","defaultValue":"8","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897218662408', '30', 'maxWait', 'maxWait', 3, 3, 10, '{"required":"0","defaultValue":"-1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897218662409', '30', 'timeoutSeconds', 'timeoutSeconds', 1, 2, 0, '{"required":"0","defaultValue":"60","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897206079532', '10', 'flowRuleGrade', 'flowRuleGrade', 3, 2, 8, '{"required":"1","defaultValue":"1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897206079533', '10', 'flowRuleControlBehavior', 'flowRuleControlBehavior', 3, 2, 5, '{"required":"1","defaultValue":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897206079534', '10', 'flowRuleEnable', 'flowRuleEnable 1 or 0)', 1, 2, 7, '{"required":"1","defaultValue":"1","rule":"/^[01]$/"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897206079535', '10', 'flowRuleCount', 'flowRuleCount', 1, 2, 6, '{"required":"1","defaultValue":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897206079536', '10', 'degradeRuleEnable', 'degradeRuleEnable 1 or 0)', 1, 2, 2, '{"required":"1","defaultValue":"1","rule":"/^[01]$/"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897206079537', '10', 'degradeRuleGrade', 'degradeRuleGrade', 3, 2, 3, '{"required":"1","defaultValue":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273792', '10', 'degradeRuleCount', 'degradeRuleCount', 1, 2, 1, '{"required":"1","defaultValue":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273793', '10', 'degradeRuleTimeWindow', 'degradeRuleTimeWindow', 1, 2, 4, '{"required":"1","defaultValue":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273794', '10', 'degradeRuleMinRequestAmount', 'degradeRuleMinRequestAmount', 1, 2, 3, '{"required":"1","defaultValue":"5","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273795', '10', 'degradeRuleStatIntervals', 'degradeRuleStatIntervals', 1, 2, 3, '{"required":"1","defaultValue":"1","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273796', '10', 'degradeRuleSlowRatioThreshold', 'degradeRuleSlowRatioThreshold', 1, 2, 3, '{"required":"1","defaultValue":"0.5","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273797', '10', 'fallbackUri', 'fallbackUri', 2, 2, 9, '{"required":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273798', '2', 'permission', 'permission', 3, 2, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273799', '2', 'statusCode', 'statusCode', 2, 2, 2, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273800', '4', 'replenishRate', 'replenishRate', 2, 2, 2, '{"required":"1","defaultValue":"10","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273801', '4', 'burstCapacity', 'burstCapacity', 2, 2, 3, '{"required":"1","defaultValue":"100","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273802', '3', 'regex', 'regex', 2, 2, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273803', '3', 'replace', 'replace', 2, 2, 2, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273804', '16', 'redirectURI', 'redirectURI', 2, 2, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273805', '8', 'path', 'path', 2, 2, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273806', '8', 'timeout', 'timeout ms)', 1, 2, 2, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273807', '8', 'serviceId', 'serviceId', 2, 1, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273808', '12', 'timeoutDurationRate', 'timeoutDurationRate ms)', 1, 2, 1, '{"required":"1","defaultValue":"5000","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273809', '12', 'limitRefreshPeriod', 'limitRefreshPeriod ms)', 1, 2, 0, '{"required":"1","defaultValue":"500","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273810', '12', 'limitForPeriod', 'limitForPeriod', 1, 2, 0, '{"required":"1","defaultValue":"50","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273811', '12', 'circuitEnable', 'circuitEnable', 1, 2, 2, '{"required":"1","defaultValue":"0","rule":"/^[01]$/"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273812', '12', 'timeoutDuration', 'timeoutDuration ms)', 1, 2, 2, '{"required":"1","defaultValue":"30000","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273813', '12', 'fallbackUri', 'fallbackUri', 2, 2, 2, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273814', '12', 'slidingWindowSize', 'slidingWindowSize', 1, 2, 2, '{"required":"1","defaultValue":"100","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273815', '12', 'slidingWindowType', 'slidingWindowType', 1, 2, 2, '{"required":"1","defaultValue":"0","rule":"/^[01]$/"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273816', '12', 'minimumNumberOfCalls', 'minimumNumberOfCalls', 1, 2, 2, '{"required":"1","defaultValue":"100","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273817', '12', 'waitIntervalFunctionInOpenState', 'waitIntervalInOpen', 1, 2, 2, '{"required":"1","defaultValue":"60000","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273818', '12', 'permittedNumberOfCallsInHalfOpenState', 'bufferSizeInHalfOpen', 1, 2, 2, '{"required":"1","defaultValue":"10","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273819', '12', 'failureRateThreshold', 'failureRateThreshold', 1, 2, 2, '{"required":"1","defaultValue":"50","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273820', '12', 'automaticTransitionFromOpenToHalfOpenEnabled', 'automaticHalfOpen', 3, 2, 1, '{"required":"1","defaultValue":"true","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273821', '4', 'mode', 'mode', 3, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273822', '4', 'master', 'master', 2, 3, 2, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273823', '4', 'url', 'url', 2, 3, 3, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273824', '4', 'password', 'password', 2, 3, 4, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273825', '11', 'protocol', 'protocol', 2, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273826', '11', 'register', 'register', 2, 3, 2, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273827', '2', 'model', 'model', 2, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273828', '6', 'register', 'register', 2, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273829', '4', 'algorithmName', 'algorithmName', 3, 2, 1, '{"required":"1","defaultValue":"slidingWindow","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273830', '4', 'keyResolverName', 'keyResolverName', 3, 2, 4, '{"required":"1","defaultValue":"WHOLE_KEY_RESOLVER","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273831', '5', 'upstreamHost', 'host', 2, 1, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273832', '5', 'protocol', 'protocol', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"http://","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273833', '5', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273834', '5', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273835', '5', 'timestamp', 'startupTime', 1, 1, 3, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273836', '5', 'warmup', 'warmupTime', 1, 1, 5, '{"defaultValue":"0","placeholder":"warmup time ms)","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273837', '5', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273838', '5', 'loadBalance', 'loadStrategy', 3, 2, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273839', '5', 'retry', 'retryCount', 1, 2, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273840', '5', 'timeout', 'timeout', 1, 2, 2, '{"defaultValue":"3000","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273841', '5', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273842', '5', 'multiRuleHandle', 'multiRuleHandle', 3, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273843', '5', 'headerMaxSize', 'headerMaxSize', 1, 2, 3, '{"defaultValue":"10240","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273844', '5', 'requestMaxSize', 'requestMaxSize', 1, 2, 4, '{"defaultValue":"102400","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273845', '5', 'retryStrategy', 'retryStrategy', 3, 2, 0, '{"required":"0","defaultValue":"current","placeholder":"retryStrategy","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273846', '13', 'upstreamHost', 'host', 2, 1, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273847', '13', 'protocol', 'protocol', 2, 1, 2, '{"defaultValue":"","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273848', '13', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273849', '13', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273850', '13', 'timestamp', 'startupTime', 1, 1, 3, '{"defaultValue":"0","placeholder":"startup timestamp","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273851', '13', 'warmup', 'warmupTime', 1, 1, 5, '{"defaultValue":"0","placeholder":"warmup time ms)","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273852', '13', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273853', '13', 'loadBalance', 'loadStrategy', 3, 2, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273854', '13', 'retry', 'retryCount', 1, 2, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273855', '13', 'timeout', 'timeout', 1, 2, 2, '{"defaultValue":"3000","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273856', '13', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468096', '13', 'multiRuleHandle', 'multiRuleHandle', 3, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273858', '13', 'corethreads', 'corethreads', 1, 3, 3, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273859', '13', 'threads', 'threads', 1, 3, 4, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273860', '13', 'queues', 'queues', 1, 3, 5, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897210273861', '13', 'threadpool', 'threadpool', 3, 3, 2, '{"required":"0","defaultValue":"default","placeholder":"threadpool","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468097', '15', 'upstreamUrl', 'ip:port', 2, 1, 1, '{"required":"1","placeholder":"","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468098', '15', 'weight', 'weight', 1, 1, 3, '{"defaultValue":"50","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468099', '15', 'status', 'status', 3, 1, 6, '{"defaultValue":"true","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468100', '15', 'multiSelectorHandle', 'multiSelectorHandle', 3, 3, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468101', '15', 'multiRuleHandle', 'multiRuleHandle', 3, 3, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468102', '15', 'threadpool', 'threadpool', 3, 3, 0, '{"required":"0","defaultValue":"cached","placeholder":"threadpool","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468103', '14', 'contextPath', 'contextPath', 2, 2, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468104', '14', 'addPrefix', 'addPrefix', 2, 2, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468105', '20', 'ruleHandlePageType', 'ruleHandlePageType', 3, 3, 0, '{"required":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468106', '19', 'secretKey', 'secretKey', 2, 3, 0, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468107', '24', 'strategyName', 'strategyName', 3, 2, 1, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468108', '24', 'fieldNames', 'fieldNames', 2, 2, 3, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468109', '24', 'decryptKey', 'decryptKey', 2, 2, 3, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468110', '24', 'encryptKey', 'encryptKey', 2, 2, 3, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468111', '24', 'way', 'way', 3, 2, 3, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468112', '25', 'strategyName', 'strategyName', 3, 2, 2, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468113', '25', 'decryptKey', 'decryptKey', 2, 2, 3, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468114', '25', 'encryptKey', 'encryptKey', 2, 2, 3, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468115', '25', 'fieldNames', 'fieldNames', 2, 2, 4, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468116', '25', 'way', 'way', 3, 2, 3, null);

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468117', '6', 'gray', 'gray', 3, 1, 9, '{"required":"0","defaultValue":"false","placeholder":"gray","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468118', '6', 'group', 'group', 2, 1, 3, '{"required":"0","placeholder":"group","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468119', '6', 'loadbalance', 'loadbalance', 3, 2, 0, '{"required":"0","placeholder":"loadbalance","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468164', '11', 'corethreads', 'corethreads', 1, 3, 4, '{"required":"0","defaultValue":"0","placeholder":"corethreads","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468165', '11', 'threads', 'threads', 1, 3, 5, '{"required":"0","defaultValue":"2147483647","placeholder":"threads","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468166', '11', 'queues', 'queues', 1, 3, 6, '{"required":"0","defaultValue":"0","placeholder":"queues","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468167', '11', 'threadpool', 'threadpool', 3, 3, 3, '{"required":"0","defaultValue":"default","placeholder":"threadpool","rule":""}');


insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1537326008606343168', '31', 'responseContent', 'responseContent', 2, 2, 0, '{"required":"0","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1537325892176658432', '31', 'httpStatusCode', 'httpStatusCode', 1, 2, 0, '{"required":"0","defaultValue":"200","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468168', '32', 'host', 'host', 2, 3, 1, '{"required":"1","defaultValue":"localhost"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468169', '32', 'port', 'port', 2, 3, 2, '{"required":"1","defaultValue":"9200"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468170', '32', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468171', '32', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468172', '32', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468173', '32', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468174', '32', 'index', 'index', 2, 1, 1, '{"required":"0","defaultValue":"","placeholder":"optional"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468175', '32', 'sampleRate', 'sampleRate', 2, 1, 2, '{"required":"0","defaultValue":"","placeholder":"optional,0,0.01~1"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468176', '1', 'signRequestBody', 'signRequestBody', 3, 2, 9, '{"required":"0","defaultValue":"false","placeholder":"signRequestBody","rule":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468177', '33', 'topic', 'topic', 2, 3, 1, '{"required":"1","defaultValue":"shenyu-access-logging"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468178', '33', 'namesrvAddr', 'namesrvAddr', 2, 3, 2, '{"required":"1","defaultValue":"localhost:9092"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468179', '33', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468180', '33', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468181', '33', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468182', '33', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468183', '33', 'topic', 'topic', 2, 1, 1, '{"required":"0","defaultValue":"","placeholder":"optional"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468184', '34', 'accessId', 'accessId', 2, 3, 0, '{"required":"1","defaultValue":"","placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468185', '34', 'accessKey', 'accessKey', 2, 3, 1, '{"required":"1","defaultValue":"","placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468186', '34', 'host', 'host', 2, 3, 2, '{"required":"1","defaultValue":"","placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468187', '34', 'projectName', 'projectName', 2, 3, 3, '{"required":"0","defaultValue":"shenyu","placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468188', '34', 'logStoreName', 'logStoreName', 2, 3, 4, '{"required":"0","defaultValue":"shenyu-logstore","placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468189', '34', 'topic', 'topic', 2, 3, 5, '{"required":"0","defaultValue":"shenyu-topic","placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468190', '34', 'ttlInDay', 'ttlInDay', 1, 3, 6, '{"required":"0","defaultValue":3,"placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468191', '34', 'shardCount', 'shardCount', 1, 3, 7, '{"required":"0","defaultValue":10,"placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468192', '34', 'sendThreadCount', 'sendThreadCount', 1, 3, 8, '{"required":"0","defaultValue":1,"placeholder":"1-500"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468193', '34', 'ioThreadCount', 'ioThreadCount', 1, 3, 9, '{"required":"0","defaultValue":1,"placeholder":"1-500"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468194', '34', 'sampleRate', 'sampleRate', 2, 3, 10, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468195', '34', 'maxRequestBody', 'maxRequestBody', 1, 3, 11, '{"required":"0","defaultValue":524288,"placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468196', '34', 'maxResponseBody', 'maxResponseBody', 1, 3, 12, '{"required":"0","defaultValue":524288,"placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468197', '34', 'bufferQueueSize', 'bufferQueueSize', 1, 3, 13, '{"required":"0","defaultValue":524288,"placeholder":""}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468198', '35', 'topic', 'topic', 2, 3, 1, '{"required":"1","defaultValue":"shenyu-access-logging"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468199', '35', 'serviceUrl', 'serviceUrl', 2, 3, 2, '{"required":"1","defaultValue":"pulsar://localhost:6650"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468200', '35', 'sampleRate', 'sampleRate', 2, 3, 4, '{"required":"0","defaultValue":"1","placeholder":"optional,0,0.01~1"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468201', '35', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468202', '35', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{"required":"0","defaultValue":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468203', '35', 'compressAlg', 'compressAlg', 3, 3, 7, '{"required":"0","defaultValue":"none"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468204', '36', 'secretId', 'secretId', 2, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468205', '36', 'secretKey', 'secretKey', 2, 3, 2, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468206', '36', 'endpoint', 'endpoint', 2, 3, 3, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468207', '36', 'topic', 'topic', 2, 3, 4, '{\"required\":\"1\",\"defaultValue\":\"\",\"placeholder\":\"\"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468208', '36', 'sendThreadCount', 'sendThreadCount', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":1,\"placeholder\":\"1-500\"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468209', '36', 'totalSizeInBytes', 'totalSizeInBytes', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":104857600}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468210', '36', 'maxSendThreadCount', 'maxSendThreadCount', 1, 3, 7, '{\"required\":\"0\",\"defaultValue\":1,\"placeholder\":\"availableProcessors + 1\"}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468211', '36', 'maxBlockSec', 'maxBlockSec', 1, 3, 8, '{\"required\":\"0\",\"defaultValue\":60000}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468206', '36', 'maxBatchSize', 'maxBatchSize', 1, 3, 9, '{\"required\":\"0\",\"defaultValue\":524288}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468212', '36', 'maxBatchCount', 'maxBatchCount', 1, 3, 10, '{\"required\":\"0\",\"defaultValue\":4096}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468213', '36', 'lingerMs', 'lingerMs', 1, 3, 11, '{\"required\":\"0\",\"defaultValue\":2000}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468214', '36', 'retries', 'retries', 1, 3, 12, '{\"required\":\"0\",\"defaultValue\":10}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468215', '36', 'maxReservedAttempts', 'maxReservedAttempts', 1, 3, 13, '{\"required\":\"0\",\"defaultValue\":11}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468216', '36', 'baseRetryBackoffMs', 'baseRetryBackoffMs', 1, 3, 14, '{\"required\":\"0\",\"defaultValue\":100}');

insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1518229897214468217', '36', 'maxRetryBackoffMs', 'maxRetryBackoffMs', 1, 3, 15, '{\"required\":\"0\",\"defaultValue\":50000}');

/** insert resource for resource */
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"   (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1346775491550474240','','SHENYU.MENU.PLUGIN.LIST','plug','/plug','PluginList','0','0','dashboard','0','0','','1');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1357956838021890048','','SHENYU.MENU.CONFIG.MANAGMENT','config','/config','config','0','1','api','0','0','','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1346777449787125760','1357956838021890048','SHENYU.MENU.SYSTEM.MANAGMENT.PLUGIN','plugin','/config/plugin','plugin','1','2','book','0','0','','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347034027070337024','1346777449787125760','SHENYU.BUTTON.SYSTEM.LIST','','','','2','0','','1','0','system:plugin:list','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347039054925148160','1346777449787125760','SHENYU.BUTTON.SYSTEM.DELETE','','','','2','1','','1','0','system:plugin:delete','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347041326749691904','1346777449787125760','SHENYU.BUTTON.SYSTEM.ADD','','','','2','2','','1','0','system:plugin:add','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347046566244003840','1346777449787125760','SHENYU.BUTTON.SYSTEM.SYNCHRONIZE','','','','2','3','','1','0','system:plugin:modify','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347047143350874112','1346777449787125760','SHENYU.BUTTON.SYSTEM.ENABLE','','','','2','4','','1','0','system:plugin:disable','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347047203220369408','1346777449787125760','SHENYU.BUTTON.SYSTEM.EDIT','','','','2','5','','1','0','system:plugin:edit','1');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1346777623011880960','1357956838021890048','SHENYU.PLUGIN.PLUGINHANDLE','pluginhandle','/config/pluginhandle','pluginhandle','1','3','down-square','0','0','','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347047555588042752','1346777623011880960','SHENYU.BUTTON.SYSTEM.LIST','','','','2','0','','1','0','system:pluginHandler:list','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347047640145211392','1346777623011880960','SHENYU.BUTTON.SYSTEM.DELETE','','','','2','1','','1','0','system:pluginHandler:delete','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347047695002513408','1346777623011880960','SHENYU.BUTTON.SYSTEM.ADD','','','','2','2','','1','0','system:pluginHandler:add','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347047747305484288','1346777623011880960','SHENYU.BUTTON.SYSTEM.EDIT','','','','2','3','','1','0','system:pluginHandler:edit','1');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1346777766301888512','1357956838021890048','SHENYU.MENU.SYSTEM.MANAGMENT.AUTHEN','auth','/config/auth','auth','1','4','audit','0','0','','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347048004105940992','1346777766301888512','SHENYU.BUTTON.SYSTEM.LIST','','','','2','0','','1','0','system:authen:list','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347048101875167232','1346777766301888512','SHENYU.BUTTON.SYSTEM.DELETE','','','','2','1','','1','0','system:authen:delete','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347048145877610496','1346777766301888512','SHENYU.BUTTON.SYSTEM.ADD','','','','2','2','','1','0','system:authen:add','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347048240677269504','1346777766301888512','SHENYU.BUTTON.SYSTEM.ENABLE','','','','2','3','','1','0','system:authen:disable','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347048316216684544','1346777766301888512','SHENYU.BUTTON.SYSTEM.SYNCHRONIZE','','','','2','4','','1','0','system:authen:modify','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347048776029843456','1346777766301888512','SHENYU.BUTTON.SYSTEM.EDIT','','','','2','5','','1','0','system:authen:edit','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1350804501819195392','1346777766301888512','SHENYU.BUTTON.SYSTEM.EDITRESOURCEDETAILS','','','','2','6','','1','0','system:authen:editResourceDetails','1');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1346777907096285184','1357956838021890048','SHENYU.MENU.SYSTEM.MANAGMENT.METADATA','metadata','/config/metadata','metadata','1','5','snippets','0','0','','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347048968414179328','1346777907096285184','SHENYU.BUTTON.SYSTEM.LIST','','','','2','0','','1','0','system:meta:list','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347049029323862016','1346777907096285184','SHENYU.BUTTON.SYSTEM.DELETE','','','','2','1','','1','0','system:meta:delete','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347049092552994816','1346777907096285184','SHENYU.BUTTON.SYSTEM.ADD','','','','2','2','','1','0','system:meta:add','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347049251395481600','1346777907096285184','SHENYU.BUTTON.SYSTEM.ENABLE','','','','2','3','','1','0','system:meta:disable','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347049317178945536','1346777907096285184','SHENYU.BUTTON.SYSTEM.SYNCHRONIZE','','','','2','4','','1','0','system:meta:modify','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347049370014593024','1346777907096285184','SHENYU.BUTTON.SYSTEM.EDIT','','','','2','5','','1','0','system:meta:edit','1');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1346778036402483200','1357956838021890048','SHENYU.MENU.SYSTEM.MANAGMENT.DICTIONARY','dict','/config/dict','dict','1','6','ordered-list','0','0','','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347049542417264640','1346778036402483200','SHENYU.BUTTON.SYSTEM.LIST','','','','2','0','','1','0','system:dict:list','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347049598155370496','1346778036402483200','SHENYU.BUTTON.SYSTEM.DELETE','','','','2','1','','1','0','system:dict:delete','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347049659023110144','1346778036402483200','SHENYU.BUTTON.SYSTEM.ADD','','','','2','2','','1','0','system:dict:add','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347049731047698432','1346778036402483200','SHENYU.BUTTON.SYSTEM.ENABLE','','','','2','3','','1','0','system:dict:disable','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347049794008395776','1346778036402483200','SHENYU.BUTTON.SYSTEM.EDIT','','','','2','4','','1','0','system:dict:edit','1');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1346776175553376256','','SHENYU.MENU.SYSTEM.MANAGMENT','system','/system','system','0','2','setting','0','0','','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1346777157943259136','1346776175553376256','SHENYU.MENU.SYSTEM.MANAGMENT.USER','manage','/system/manage','manage','1','1','user','0','0','','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347032308726902784','1346777157943259136','SHENYU.BUTTON.SYSTEM.ADD','','','','2','0','','1','0','system:manager:add','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347032395901317120','1346777157943259136','SHENYU.BUTTON.SYSTEM.LIST','','','','2','1','','1','0','system:manager:list','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347032453707214848','1346777157943259136','SHENYU.BUTTON.SYSTEM.DELETE','','','','2','2','','1','0','system:manager:delete','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1347032509051056128','1346777157943259136','SHENYU.BUTTON.SYSTEM.EDIT','','','','2','3','','1','0','system:manager:edit','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1386680049203195904','1346777157943259136','SHENYU.BUTTON.DATA.PERMISSION.CONFIG', '', '', '', 2, 0, '', 1, 0, 'system:manager:configureDataPermission', 1);

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1350106119681622016','1346776175553376256','SHENYU.MENU.SYSTEM.MANAGMENT.ROLE','role','/system/role','role','1','0','usergroup-add','0','0','','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1350107709494804480','1350106119681622016','SHENYU.BUTTON.SYSTEM.ADD','','','','2','0','','1','0','system:role:add','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1350107842236137472','1350106119681622016','SHENYU.BUTTON.SYSTEM.LIST','','','','2','1','','1','0','system:role:list','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1350112406754766848','1350106119681622016','SHENYU.BUTTON.SYSTEM.DELETE','','','','2','2','','1','0','system:role:delete','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1350112481253994496','1350106119681622016','SHENYU.BUTTON.SYSTEM.EDIT','','','','2','3','','1','0','system:role:edit','1');

INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1355163372527050752','1346776175553376256','SHENYU.MENU.SYSTEM.MANAGMENT.RESOURCE','resource','/system/resource','resource','1','2','menu','0','0','','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1355165158419750912','1355163372527050752','SHENYU.BUTTON.RESOURCE.MENU.ADD','','','','2','1','','1','0','system:resource:addMenu','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1355165353534578688','1355163372527050752','SHENYU.BUTTON.SYSTEM.LIST','','','','2','0','','1','0','system:resource:list','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1355165475785957376','1355163372527050752','SHENYU.BUTTON.RESOURCE.MENU.DELETE','','','','2','2','','1','0','system:resource:deleteMenu','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1355165608565039104','1355163372527050752','SHENYU.BUTTON.RESOURCE.MENU.EDIT','','','','2','3','','1','0','system:resource:editMenu','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1357977745889132544','1355163372527050752','SHENYU.BUTTON.RESOURCE.BUTTON.ADD','','','','2','4','','1','0','system:resource:addButton','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1357977912126177280','1355163372527050752','SHENYU.SYSTEM.EDITOR','','','','2','5','','1','0','system:resource:editButton','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1357977971827900416','1355163372527050752','SHENYU.SYSTEM.DELETEDATA','','','','2','6','','1','0','system:resource:deleteButton','1');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1534577121923309568', '', 'Document', '', '/document', '', 0, 3, 'file-text', 0, 0, '', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1534585430311051264', '1534577121923309568', 'API document', '', '/document/apidoc', '', 1, 0, 'api', 0, 0, '', 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("resource" (id)) */ INTO "resource"  (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES('1534585531108564992', '1534585430311051264', 'send', '', '', '', 2, 0, '', 1, 0, 'document:apirun:send', 1);






/* default permission */
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708572688384', '1346358560427216896', '1346775491550474240');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1357956838021890049', '1346358560427216896', '1357956838021890048');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708597854208', '1346358560427216896', '1346777449787125760');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708702711808', '1346358560427216896', '1347034027070337024');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708706906112', '1346358560427216896', '1347039054925148160');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708711100416', '1346358560427216896', '1347041326749691904');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708715294720', '1346358560427216896', '1347046566244003840');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708719489024', '1346358560427216896', '1347047143350874112');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708723683328', '1346358560427216896', '1347047203220369408');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708606242816', '1346358560427216896', '1346777623011880960');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708727877632', '1346358560427216896', '1347047555588042752');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708732071936', '1346358560427216896', '1347047640145211392');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708732071937', '1346358560427216896', '1347047695002513408');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708736266240', '1346358560427216896', '1347047747305484288');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708610437120', '1346358560427216896', '1346777766301888512');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708740460544', '1346358560427216896', '1347048004105940992');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708744654848', '1346358560427216896', '1347048101875167232');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708744654849', '1346358560427216896', '1347048145877610496');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708748849152', '1346358560427216896', '1347048240677269504');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708753043456', '1346358560427216896', '1347048316216684544');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708757237760', '1346358560427216896', '1347048776029843456');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007709088587777', '1346358560427216896', '1350804501819195392');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708614631424', '1346358560427216896', '1346777907096285184');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708757237761', '1346358560427216896', '1347048968414179328');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708761432064', '1346358560427216896', '1347049029323862016');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708765626368', '1346358560427216896', '1347049092552994816');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708769820672', '1346358560427216896', '1347049251395481600');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708774014976', '1346358560427216896', '1347049317178945536');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708774014977', '1346358560427216896', '1347049370014593024');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708623020032', '1346358560427216896', '1346778036402483200');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708778209280', '1346358560427216896', '1347049542417264640');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708782403584', '1346358560427216896', '1347049598155370496');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708786597888', '1346358560427216896', '1347049659023110144');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708790792192', '1346358560427216896', '1347049731047698432');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708794986496', '1346358560427216896', '1347049794008395776');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708585271296', '1346358560427216896', '1346776175553376256');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708593659904', '1346358560427216896', '1346777157943259136');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708685934593', '1346358560427216896', '1347032308726902784');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708690128896', '1346358560427216896', '1347032395901317120');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708694323200', '1346358560427216896', '1347032453707214848');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007708698517504', '1346358560427216896', '1347032509051056128');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1386680049203195905', '1346358560427216896', '1386680049203195904');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007709080199168', '1346358560427216896', '1350106119681622016');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007709080199169', '1346358560427216896', '1350107709494804480');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007709084393472', '1346358560427216896', '1350107842236137472');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007709084393473', '1346358560427216896', '1350112406754766848');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1351007709088587776', '1346358560427216896', '1350112481253994496');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1355167519859040256', '1346358560427216896', '1355163372527050752');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1355167519859040257', '1346358560427216896', '1355165158419750912');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1355167519859040258', '1346358560427216896', '1355165353534578688');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1355167519859040259', '1346358560427216896', '1355165475785957376');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1355167519859040260', '1346358560427216896', '1355165608565039104');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1357977745893326848', '1346358560427216896', '1357977745889132544');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1357977912126177281', '1346358560427216896', '1357977912126177280');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1357977971827900417', '1346358560427216896', '1357977971827900416');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1534577122279825408', '1346358560427216896', '1534577121923309568');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1534585430587875328', '1346358560427216896', '1534585430311051264');
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */ INTO permission (id, object_id, resource_id) VALUES ('1534585531389583360', '1346358560427216896', '1534585531108564992');

commit;