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

declare
num   number;

begin

select count(1) into num from user_tables where table_name = upper('dashboard_user') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('dashboard_user already exists');
else
     execute immediate 'create table dashboard_user
(
    id           VARCHAR2(128) not null,
    user_name    VARCHAR2(64) not null,
    password     VARCHAR2(128),
    role         NUMBER(10) not null,
    enabled      NUMBER(3) not null,
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id),
    constraint unique_user_name unique (user_name)
)';
execute immediate 'comment on column dashboard_user.id
  is ''primary key id''';
execute immediate 'comment on column dashboard_user.user_name
  is ''user name''';
execute immediate 'comment on column dashboard_user.password
  is ''user password''';
execute immediate 'comment on column dashboard_user.role
  is ''role''';
execute immediate 'comment on column dashboard_user.enabled
  is ''delete or not''';
execute immediate 'comment on column dashboard_user.date_created
  is ''create time''';
execute immediate 'comment on column dashboard_user.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('plugin') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('plugin already exists');
else
     execute immediate 'create table plugin
(
    id           VARCHAR2(128) not null,
    name         VARCHAR2(62) not null,
    config       CLOB,
    role         VARCHAR2(64) not null,
    sort         NUMBER(10),
    enabled      NUMBER(3) default ''0'' not null,
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id)
)';
execute immediate 'comment on column plugin.id
  is ''primary key id''';
execute immediate 'comment on column plugin.name
  is ''plugin name''';
execute immediate 'comment on column plugin.config
  is ''plugin configuration''';
execute immediate 'comment on column plugin.role
  is ''plug-in role''';
execute immediate 'comment on column plugin.sort
  is ''sort''';
execute immediate 'comment on column plugin.enabled
  is ''whether to open (0, not open, 1 open)''';
execute immediate 'comment on column plugin.date_created
  is ''create time''';
execute immediate 'comment on column plugin.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('plugin_handle') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('plugin_handle already exists');
else
     execute immediate 'create table plugin_handle
(
    id           VARCHAR2(128) not null,
    plugin_id    VARCHAR2(128) not null,
    field        VARCHAR2(100) not null,
    label        VARCHAR2(100),
    data_type    NUMBER(5) default ''1'' not null,
    type         NUMBER(5),
    sort         NUMBER(10),
    ext_obj      clob,
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id),
    constraint plugin_id_field_type unique (plugin_id,field,type)
)';
execute immediate 'comment on column plugin_handle.plugin_id
  is ''plugin id''';
execute immediate 'comment on column plugin_handle.field
  is ''field''';
execute immediate 'comment on column plugin_handle.label
  is ''label''';
execute immediate 'comment on column plugin_handle.data_type
  is ''data type 1 number 2 string''';
execute immediate 'comment on column plugin_handle.type
  is ''type, 1 means selector, 2 means rule, 3 means plugin''';
execute immediate 'comment on column plugin_handle.sort
  is ''sort''';
execute immediate 'comment on column plugin_handle.ext_obj
  is ''extra configuration (json format data)''';
execute immediate 'comment on column plugin_handle.date_created
  is ''create time''';
execute immediate 'comment on column plugin_handle.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('selector') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('selector already exists');
else
     execute immediate 'create table selector
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
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null
)';
execute immediate 'comment on column selector.id
  is ''primary key id varchar''';
execute immediate 'comment on column selector.plugin_id
  is ''plugin id''';
execute immediate 'comment on column selector.name
  is ''selector name''';
execute immediate 'comment on column selector.match_mode
  is ''matching mode (0 and 1 or)''';
execute immediate 'comment on column selector.type
  is ''type (0, full flow, 1 custom flow)''';
execute immediate 'comment on column selector.sort
  is ''sort''';
execute immediate 'comment on column selector.handle
  is ''processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)''';
execute immediate 'comment on column selector.enabled
  is ''whether to open''';
execute immediate 'comment on column selector.loged
  is ''whether to print the log''';
execute immediate 'comment on column selector.continued
  is ''whether to continue execution''';
execute immediate 'comment on column selector.date_created
  is ''create time''';
execute immediate 'comment on column selector.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('selector_condition') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('selector_condition already exists');
else
     execute immediate 'create table selector_condition
(
    id           VARCHAR2(128) not null,
    selector_id  VARCHAR2(128) not null,
    param_type   VARCHAR2(64) not null,
    operator     VARCHAR2(64) not null,
    param_name   VARCHAR2(64) not null,
    param_value  VARCHAR2(64) not null,
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id)
)';
execute immediate 'comment on column selector_condition.id
  is ''primary key id''';
execute immediate 'comment on column selector_condition.selector_id
  is ''selector id''';
execute immediate 'comment on column selector_condition.param_type
  is ''parameter type (to query uri, etc.)''';
execute immediate 'comment on column selector_condition.operator
  is ''matching character (=> <like matching)''';
execute immediate 'comment on column selector_condition.param_name
  is ''parameter name''';
execute immediate 'comment on column selector_condition.param_value
  is ''parameter value''';
execute immediate 'comment on column selector_condition.date_created
  is ''create time''';
execute immediate 'comment on column selector_condition.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('rule') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('rule already exists');
else
     execute immediate 'create table rule
(
    id           VARCHAR2(128) not null PRIMARY KEY,
    selector_id  VARCHAR2(128) not null,
    match_mode   NUMBER(10) not null,
    name         VARCHAR2(128) not null,
    enabled      NUMBER(3) not null,
    loged        NUMBER(3) not null,
    sort         NUMBER(10) not null,
    handle       VARCHAR2(1024),
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null
)';
execute immediate 'comment on column rule.id
  is ''primary key id''';
execute immediate 'comment on column rule.selector_id
  is ''selector id''';
execute immediate 'comment on column rule.match_mode
  is ''matching mode (0 and 1 or)''';
execute immediate 'comment on column rule.name
  is ''rule name''';
execute immediate 'comment on column rule.enabled
  is ''whether to open''';
execute immediate 'comment on column rule.loged
  is ''whether to log or not''';
execute immediate 'comment on column rule.sort
  is ''sort''';
execute immediate 'comment on column rule.handle
  is ''processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)''';
execute immediate 'comment on column rule.date_created
  is ''create time''';
execute immediate 'comment on column rule.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('rule_condition') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('rule_condition already exists');
else
     execute immediate 'create table rule_condition
(
    id           VARCHAR2(128) not null PRIMARY KEY,
    rule_id      VARCHAR2(128) not null,
    param_type   VARCHAR2(64) not null,
    operator     VARCHAR2(64) not null,
    param_name   VARCHAR2(64) not null,
    param_value  VARCHAR2(64) not null,
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null
)';
execute immediate 'comment on column rule_condition.id
  is ''primary key id''';
execute immediate 'comment on column rule_condition.rule_id
  is ''rule id''';
execute immediate 'comment on column rule_condition.param_type
  is ''parameter type (post query uri, etc.)''';
execute immediate 'comment on column rule_condition.operator
  is ''matching character (=> <like match)''';
execute immediate 'comment on column rule_condition.param_name
  is ''parameter name''';
execute immediate 'comment on column rule_condition.param_value
  is ''parameter value''';
execute immediate 'comment on column rule_condition.date_created
  is ''create time''';
execute immediate 'comment on column rule_condition.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('meta_data') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('meta_data already exists');
else
     execute immediate 'create table meta_data
(
    id              VARCHAR2(128) not null,
    app_name        VARCHAR2(255) not null,
    path            VARCHAR2(255) not null,
    path_desc       VARCHAR2(255) not null,
    rpc_type        VARCHAR2(64) not null,
    service_name    VARCHAR2(255),
    method_name     VARCHAR2(255),
    parameter_types VARCHAR2(255),
    rpc_ext         VARCHAR2(512),
    date_created    DATE default SYSDATE not null,
    date_updated    DATE default SYSDATE not null,
    enabled         NUMBER(3) default ''0'' not null,
    PRIMARY KEY (id)
)';
execute immediate 'comment on column meta_data.id
  is ''id''';
execute immediate 'comment on column meta_data.app_name
  is ''application name''';
execute immediate 'comment on column meta_data.path
  is ''path, cannot be repeated''';
execute immediate 'comment on column meta_data.path_desc
  is ''path description''';
execute immediate 'comment on column meta_data.rpc_type
  is ''rpc type''';
execute immediate 'comment on column meta_data.service_name
  is ''service name''';
execute immediate 'comment on column meta_data.method_name
  is ''method name''';
execute immediate 'comment on column meta_data.parameter_types
  is ''parameter types are provided with multiple parameter types separated by commas''';
execute immediate 'comment on column meta_data.rpc_ext
  is ''rpc extended information, json format''';
execute immediate 'comment on column meta_data.date_created
  is ''create time''';
execute immediate 'comment on column meta_data.date_updated
  is ''update time''';
execute immediate 'comment on column meta_data.enabled
  is ''enabled state''';
end if;

select count(1) into num from user_tables where table_name = upper('app_auth') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('app_auth already exists');
else
     execute immediate 'create table app_auth
(
    id           VARCHAR2(128) not null,
    app_key      VARCHAR2(32) not null,
    app_secret   VARCHAR2(128) not null,
    user_id      VARCHAR2(128),
    phone        VARCHAR2(255),
    ext_info     VARCHAR2(1024),
    open         NUMBER(3) not null,
    enabled      NUMBER(3) not null,
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id)
)';
execute immediate 'comment on column app_auth.id
  is ''primary key id''' ;
execute immediate 'comment on column app_auth.app_key
  is ''application identification key''';
execute immediate 'comment on column app_auth.app_secret
  is ''encryption algorithm secret''';
execute immediate 'comment on column app_auth.user_id
  is ''user id''';
execute immediate 'comment on column app_auth.phone
  is ''phone number when the user applies''';
execute immediate 'comment on column app_auth.ext_info
  is ''extended parameter json''';
execute immediate 'comment on column app_auth.open
  is ''open auth path or not''';
execute immediate 'comment on column app_auth.enabled
  is ''delete or not''';
execute immediate 'comment on column app_auth.date_created
  is ''create time''';
execute immediate 'comment on column app_auth.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('auth_param') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('auth_param already exists');
else
     execute immediate 'create table auth_param
(
    id           VARCHAR2(128) not null,
    auth_id      VARCHAR2(128),
    app_name     VARCHAR2(255) not null,
    app_param    VARCHAR2(255),
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id)
)';
execute immediate 'comment on column auth_param.id
  is ''primary key id''';
execute immediate 'comment on column auth_param.auth_id
  is ''authentication table id''';
execute immediate 'comment on column auth_param.app_name
  is ''business Module''';
execute immediate 'comment on column auth_param.app_param
  is ''service module parameters (parameters that need to be passed by the gateway) json type''';
execute immediate 'comment on column auth_param.date_created
  is ''create time''';
execute immediate 'comment on column auth_param.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('auth_path') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('auth_path already exists');
else
     execute immediate 'create table auth_path
(
    id           VARCHAR2(128) not null,
    auth_id      VARCHAR2(128) not null,
    app_name     VARCHAR2(255) not null,
    path         VARCHAR2(255) not null,
    enabled      NUMBER(3) not null,
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id)
)';
execute immediate 'comment on column auth_path.id
is ''primary key id''';
execute immediate 'comment on column auth_path.auth_id
  is ''auth table id''';
execute immediate 'comment on column auth_path.app_name
  is ''module''';
execute immediate 'comment on column auth_path.path
  is ''path''';
execute immediate 'comment on column auth_path.enabled
  is ''whether pass 1 is''';
execute immediate 'comment on column auth_path.date_created
  is ''create time''';
execute immediate 'comment on column auth_path.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('shenyu_dict') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('shenyu_dict already exists');
else
     execute immediate 'create table shenyu_dict
(
    id           VARCHAR2(128) not null,
    type         VARCHAR2(100) not null,
    dict_code    VARCHAR2(100) not null,
    dict_name    VARCHAR2(100) not null,
    dict_value   VARCHAR2(100),
    "' || 'desc' || '"       VARCHAR2(255),
    sort         NUMBER(10) not null,
    enabled      NUMBER(3),
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id),
    constraint dict_type_dict_code_dict_name unique (type,dict_code,dict_name)
)';
execute immediate 'comment on column shenyu_dict.id
  is ''primary key id''';
execute immediate 'comment on column shenyu_dict.type
  is ''type''';
execute immediate 'comment on column shenyu_dict.dict_code
  is ''dictionary encoding''';
execute immediate 'comment on column shenyu_dict.dict_name
  is ''dictionary name''';
execute immediate 'comment on column shenyu_dict.dict_value
  is ''dictionary value''';
execute immediate 'comment on column shenyu_dict."' || 'desc' || '"
  is ''dictionary description or remarks''';
execute immediate 'comment on column shenyu_dict.sort
  is ''sort''';
execute immediate 'comment on column shenyu_dict.enabled
  is ''whether it is enabled''';
execute immediate 'comment on column shenyu_dict.date_created
  is ''create time''';
execute immediate 'comment on column shenyu_dict.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('role') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('role already exists');
else
     execute immediate 'create table role
(
    id           VARCHAR2(128) not null,
    role_name    VARCHAR2(32) not null,
    description  VARCHAR2(255),
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id,role_name)
)';
execute immediate 'comment on table role
 is ''role table''';
execute immediate 'comment on column role.id
  is ''primary key id''';
execute immediate 'comment on column role.role_name
  is ''role name''';
execute immediate 'comment on column role.description
  is ''role describe''';
execute immediate 'comment on column role.date_created
  is ''create time''';
execute immediate 'comment on column role.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('user_role') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('user_role already exists');
else
     execute immediate 'create table user_role
(
    id           VARCHAR2(128) not null,
    user_id      VARCHAR2(128) not null,
    role_id      VARCHAR2(128) not null,
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id)
)';
execute immediate 'comment on table user_role
 is ''user and role bind table''';
execute immediate 'comment on column user_role.id
  is ''primary key id''';
execute immediate 'comment on column user_role.user_id
  is ''user primary key''';
execute immediate 'comment on column user_role.role_id
  is ''role primary key''';
execute immediate 'comment on column user_role.date_created
  is ''create time''';
execute immediate 'comment on column user_role.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('permission') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('permission already exists');
else
     execute immediate 'create table permission
(
    id           VARCHAR2(128) not null,
    object_id    VARCHAR2(128) not null,
    resource_id  VARCHAR2(128) not null,
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id)
)';
execute immediate 'comment on table permission
 is ''permission table''';
execute immediate 'comment on column permission.id
  is ''primary key id''';
execute immediate 'comment on column permission.object_id
  is ''user primary key id or role primary key id''';
execute immediate 'comment on column permission.resource_id
  is ''resource primary key id''';
execute immediate 'comment on column permission.date_created
  is ''create time''';
execute immediate 'comment on column permission.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = 'resource' ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('resource already exists');
else
     execute immediate 'create table "' || 'resource' || '"' || '
(
    id            VARCHAR2(128) not null,
    parent_id     VARCHAR2(128),
    title         VARCHAR2(128),
    name          VARCHAR2(32),
    url           VARCHAR2(32),
    component     VARCHAR2(32),
    resource_type NUMBER(10) not null,
    sort          NUMBER(10) not null,
    icon          VARCHAR2(32),
    is_leaf       NUMBER(3) not null,
    is_route      NUMBER(10) not null,
    perms         VARCHAR2(64),
    status        NUMBER(10) not null,
    date_created  DATE default SYSDATE not null,
    date_updated  DATE default SYSDATE not null,
    PRIMARY KEY (id)
)';
execute immediate 'comment on table "' || 'resource' || '"' || '
 is ''resource table''';
execute immediate 'comment on column "' || 'resource' || '"' || '.id
  is ''primary key id''';
execute immediate 'comment on column "' || 'resource' || '"' || '.parent_id
  is ''resource parent primary key id''';
execute immediate 'comment on column "' || 'resource' || '"' || '.title
  is ''title''';
execute immediate 'comment on column "' || 'resource' || '"' || '.name
  is ''route name''';
execute immediate 'comment on column "' || 'resource' || '"' || '.url
  is ''route url''';
execute immediate 'comment on column "' || 'resource' || '"' || '.component
  is ''component''';
execute immediate 'comment on column "' || 'resource' || '"' || '.resource_type
  is ''resource type eg 0:main menu 1:child menu 2:function button''';
execute immediate 'comment on column "' || 'resource' || '"' || '.sort
  is ''sort''';
execute immediate 'comment on column "' || 'resource' || '"' || '.icon
  is ''icon''';
execute immediate 'comment on column "' || 'resource' || '"' || '.is_leaf
  is ''leaf node 0:no 1:yes''';
execute immediate 'comment on column "' || 'resource' || '"' || '.is_route
  is ''route 1:yes 0:no''';
execute immediate 'comment on column "' || 'resource' || '"' || '.perms
  is ''button permission description sys:user:add(add);sys:user:edit(edit)''';
execute immediate 'comment on column "' || 'resource' || '"' || '.status
  is ''status 1:enable 0:disable''';
execute immediate 'comment on column "' || 'resource' || '"' || '.date_created
  is ''create time''';
execute immediate 'comment on column "' || 'resource' || '"' || '.date_updated
  is ''update time''';
end if;

select count(1) into num from user_tables where table_name = upper('data_permission') ;
if num > 0 then
  DBMS_OUTPUT.PUT_LINE('data_permission already exists');
else
     execute immediate 'create table data_permission
(
    id           VARCHAR2(128) not null,
    user_id      VARCHAR2(128) not null,
    data_id      VARCHAR2(128) not null,
    data_type    NUMBER(10) not null,
    date_created DATE default SYSDATE not null,
    date_updated DATE default SYSDATE not null,
    PRIMARY KEY (id)
)';
execute immediate 'comment on table data_permission
 is ''data permission table''';
execute immediate 'comment on column data_permission.id
  is ''primary key id''';
execute immediate 'comment on column data_permission.user_id
  is ''user primary key id''';
execute immediate 'comment on column data_permission.data_id
  is ''data(selector,rule) primary key id''';
execute immediate 'comment on column data_permission.data_type
  is ''0 selector type , 1 rule type''';
execute immediate 'comment on column data_permission.date_created
  is ''create time''';
execute immediate 'comment on column data_permission.date_updated
  is ''update time''';
end if;

/**default admin user**/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(dashboard_user(id)) */ INTO dashboard_user (id, user_name, password, role, enabled) VALUES (''1'',''admin'',''ba3253876aed6bc22d4a6ff53d8406c6ad864195ed144ab5c87621b6c233b548baeae6956df346ec8c17f5ea10f35ee3cbc514797ed7ddd3145464e2a0bab413'', ''1'', ''1'')';

/** insert admin role */
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(user_role(id)) */  INTO user_role (id, user_id, role_id) VALUES (''1351007709096976384'', ''1'', ''1346358560427216896'')';

/** insert permission role for role */
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(role(id)) */  INTO role (id,role_name,description) VALUES (''1346358560427216896'', ''super'', ''Administrator'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(role(id)) */  INTO role (id,role_name,description) VALUES (''1385482862971723776'', ''default'', ''Standard'')';

/*shenyu dict*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''degradeRuleGrade'',''DEGRADE_GRADE_RT'',''slow call ratio'',''0'',''degrade type-slow call ratio'',1,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''degradeRuleGrade'',''DEGRADE_GRADE_EXCEPTION_RATIO'',''exception ratio'',''1'',''degrade type-abnormal ratio'',0,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''degradeRuleGrade'',''DEGRADE_GRADE_EXCEPTION_COUNT'',''exception number strategy'',''2'',''degrade type-abnormal number strategy'',2,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''flowRuleGrade'',''FLOW_GRADE_QPS'',''QPS'',''1'',''grade type-QPS'',0,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''flowRuleGrade'',''FLOW_GRADE_THREAD'',''number of concurrent threads'',''0'',''degrade type-number of concurrent threads'',1,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''flowRuleControlBehavior'',''CONTROL_BEHAVIOR_DEFAULT'',''direct rejection by default'',''0'',''control behavior-direct rejection by default'',0,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''flowRuleControlBehavior'',''CONTROL_BEHAVIOR_WARM_UP'',''warm up'',''1'',''control behavior-warm up'',1,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''flowRuleControlBehavior'',''CONTROL_BEHAVIOR_RATE_LIMITER'',''constant speed queuing'',''2'',''control behavior-uniform speed queuing'',2,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''flowRuleControlBehavior'',''CONTROL_BEHAVIOR_WARM_UP_RATE_LIMITER'',''preheating uniformly queued'',''3'',''control behavior-preheating uniformly queued'',3,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''permission'',''REJECT'',''reject'',''reject'',''reject'',0,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''permission'',''ALLOW'',''allow'',''allow'',''allow'',1,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''algorithmName'',''ALGORITHM_SLIDINGWINDOW'',''slidingWindow'',''slidingWindow'',''Sliding window algorithm'',0,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''algorithmName'',''ALGORITHM_LEAKYBUCKET'',''leakyBucket'',''leakyBucket'',''Leaky bucket algorithm'',1,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''algorithmName'',''ALGORITHM_CONCURRENT'',''concurrent'',''concurrent'',''Concurrent algorithm'',2,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''algorithmName'',''ALGORITHM_TOKENBUCKET'',''tokenBucket'',''tokenBucket'',''Token bucket algorithm'',3,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''loadBalance'', ''LOAD_BALANCE'', ''roundRobin'', ''roundRobin'', ''roundRobin'', 2, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''loadBalance'', ''LOAD_BALANCE'', ''random'', ''random'', ''random'', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''loadBalance'', ''LOAD_BALANCE'', ''hash'', ''hash'', ''hash'', 0, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''status'', ''DIVIDE_STATUS'', ''close'', ''false'', ''close'', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''status'', ''DIVIDE_STATUS'', ''open'', ''true'', ''open'', 0, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''multiRuleHandle'', ''MULTI_RULE_HANDLE'', ''multiple rule'', ''1'', ''multiple rule'', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''multiRuleHandle'', ''MULTI_RULE_HANDLE'', ''single rule'', ''0'', ''single rule'', 0, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''multiSelectorHandle'', ''MULTI_SELECTOR_HANDLE'', ''multiple handle'', ''1'', ''multiple handle'', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''multiSelectorHandle'', ''MULTI_SELECTOR_HANDLE'', ''single handle'', ''0'', ''single handle'', 0, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''matchMode'', ''MATCH_MODE'', ''and'', ''0'', ''and'', 0, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''matchMode'', ''MATCH_MODE'', ''or'', ''1'', ''or'', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''operator'', ''OPERATOR'', ''match'', ''match'', ''match'', 0, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''operator'', ''OPERATOR'', ''='', ''='', ''='', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''operator'', ''OPERATOR'', ''regex'', ''regex'', ''regex'', 2, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''operator'', ''OPERATOR'', ''contains'', ''contains'', ''contains'', 3, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''operator'', ''OPERATOR'', ''TimeBefore'', ''TimeBefore'', ''TimeBefore'', 4, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''operator'', ''OPERATOR'', ''TimeAfter'', ''TimeAfter'', ''TimeAfter'', 5, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''operator'', ''OPERATOR'', ''exclude'', ''exclude'', ''exclude'', 6, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''paramType'', ''PARAM_TYPE'', ''post'', ''post'', ''post'', 0, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''paramType'', ''PARAM_TYPE'', ''uri'', ''uri'', ''uri'', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''paramType'', ''PARAM_TYPE'', ''query'', ''query'', ''query'', 2, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''paramType'', ''PARAM_TYPE'', ''host'', ''host'', ''host'', 3, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''paramType'', ''PARAM_TYPE'', ''ip'', ''ip'', ''ip'', 4, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''paramType'', ''PARAM_TYPE'', ''header'', ''header'', ''header'', 5, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''paramType'', ''PARAM_TYPE'', ''cookie'', ''cookie'', ''cookie'', 6, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''paramType'', ''PARAM_TYPE'', ''req_method'', ''req_method'', ''req_method'', 7, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''keyResolverName'',''WHOLE_KEY_RESOLVER'',''whole'',''WHOLE_KEY_RESOLVER'',''Rate limit by all request'',0,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''keyResolverName'',''REMOTE_ADDRESS_KEY_RESOLVER'',''remoteAddress'',''REMOTE_ADDRESS_KEY_RESOLVER'',''Rate limit by remote address'',1,1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''automaticTransitionFromOpenToHalfOpenEnabled'', ''AUTOMATIC_HALF_OPEN'', ''open'', ''true'', '''', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''automaticTransitionFromOpenToHalfOpenEnabled'', ''AUTOMATIC_HALF_OPEN'', ''close'', ''false'', '''', 2, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''paramType'', ''PARAM_TYPE'', ''domain'', ''domain'', ''domain'', 8, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''strategyName'', ''STRATEGY_NAME'', ''rsa'', ''rsa'', ''rsa strategy'', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''way'', ''WAY'', ''encrypt'', ''encrypt'', ''encrypt'', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''way'', ''WAY'', ''decrypt'', ''decrypt'', ''decrypt'', 1, 1)';

/*insert mode data for rateLimiter plugin*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''mode'', ''MODE'', ''cluster'', ''cluster'', ''cluster'', 0, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''mode'', ''MODE'', ''sentinel'', ''sentinel'', ''sentinel'', 1, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''mode'', ''MODE'', ''standalone'', ''standalone'', ''standalone'', 2, 1)';

/*insert dict for dubbo plugin*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''gray'', ''GRAY_STATUS'', ''close'', ''false'', ''close'', ''1'', ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''gray'', ''GRAY_STATUS'', ''open'', ''true'', ''open'', ''0'', ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''threadpool'', ''THREADPOOL'', ''shared'', ''shared'', '''', ''4'', ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''threadpool'', ''THREADPOOL'', ''fixed'', ''fixed'', '''', ''3'', ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''threadpool'', ''THREADPOOL'', ''eager'', ''eager'', '''', ''2'', ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''threadpool'', ''THREADPOOL'', ''cached'', ''cached'', '''', ''0'', ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''threadpool'', ''THREADPOOL'', ''limited'', ''limited'', '''', ''1'', ''1'')';

/* insert dict for divide plugin */
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''retryStrategy'', ''RETRY_STRATEGY'', ''current'', ''current'', ''current'', ''0'', ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''retryStrategy'', ''RETRY_STRATEGY'', ''failover'', ''failover'', ''failover'', ''1'', ''1'')';

/* insert dict for init resource,permission table */
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''table'', ''INIT_FLAG'', ''status'', ''false'', ''table (resource,permission) init status'', ''0'', ''1'')';

/* insert dict for compress algorithm  */
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''compressAlg'', ''COMPRESS_ALG'', ''none'', ''none'', '''', 0, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type,dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''compressAlg'', ''COMPRESS_ALG'', ''LZ4'', ''LZ4'', '''', 1, 1)';

/* insert dict for cacheType  */
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''cacheType'', ''CACHE_TYPE_MEMORY'', ''memory'', ''memory'', ''use memory to cache data'', 0, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */  INTO shenyu_dict (type, dict_code, dict_name, dict_value, "' || 'desc' || '", sort, enabled) VALUES (''cacheType'', ''CACHE_TYPE_REDIS'', ''redis'', ''redis'', ''use redis to cache data'', 1, 1)';

/*plugin*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''1'',''sign'',''Authentication'',  20, ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort,config,enabled) VALUES (''2'',''waf'', ''Authentication'', 50,''{"' || 'model' || '":"' || 'black' || '"}'',''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''3'',''rewrite'', ''HttpProcess'', 90,''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config,enabled) VALUES (''4'',''rateLimiter'',''FaultTolerance'', 60,''{"' || 'master' || '":"' || 'mymaster' || '","' || 'mode' || '":"' || 'standalone' || '","' || 'url' || '":"' || '192.168.1.1:6379' || '","' || 'password' || '":"' || 'abc' || '"}'', ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config,enabled) VALUES (''5'',''divide'', ''Proxy'', 200,''{"' || 'multiSelectorHandle' || '":"' || '1' || '","' || 'multiRuleHandle' || '":"' || '0' || '"}'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config,enabled) VALUES (''6'',''dubbo'',''Proxy'', 310,''{"' || 'register' || '":"' || 'zookeeper://localhost:2181' || '","' || 'multiSelectorHandle' || '":"' || '1' || '","' || 'threadpool' || '":"' || 'cached' || '","' || 'corethreads' || '":0,"' || 'threads' || '":2147483647,"' || 'queues' || '":0}'', ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''8'',''springCloud'',''Proxy'', 200, ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''9'',''hystrix'', ''FaultTolerance'', 130,''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''10'',''sentinel'', ''FaultTolerance'', 140,''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config, enabled) VALUES (''11'',''sofa'', ''Proxy'', 310, ''{"' || 'protocol' || '":"' || 'zookeeper' || '","' || 'register' || '":"' || '127.0.0.1:2181' || '"}'', ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''12'',''resilience4j'', ''FaultTolerance'', 310,''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config, enabled) VALUES (''13'', ''tars'', ''Proxy'', 310,''{"' || 'multiSelectorHandle' || '":"' || '1' || '","' || 'multiRuleHandle' || '":"' || '0' || '"}'',''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''14'', ''contextPath'', ''HttpProcess'', 80,''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config, enabled) VALUES (''15'', ''grpc'', ''Proxy'', 310,''{"' || 'multiSelectorHandle' || '":"' || '1' || '","' || 'multiRuleHandle' || '":"' || '0' || '","' || 'threadpool' || '":"' || 'cached' || '"}'',''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''16'', ''redirect'', ''HttpProcess'', 110,''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config, enabled) VALUES (''17'', ''motan'', ''Proxy'', 310,''{"' || 'register' || '":"' || '127.0.0.1:2181' || '"}'',''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''18'', ''logging'', ''Logging'', 160, ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config, enabled) VALUES (''19'', ''jwt'', ''Authentication'', 30, ''{"' || 'secretKey' || '":"' || 'key' || '"}'', ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''20'', ''request'', ''HttpProcess'', 120, ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''21'', ''oauth2'', ''Authentication'', 40, ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config, enabled) VALUES (''22'', ''paramMapping'',''HttpProcess'', 70,''{"' || 'ruleHandlePageType' || '":"' || 'custom' || '"}'', ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config, enabled) VALUES (''23'', ''modifyResponse'', ''HttpProcess'', 220, ''{"' || 'ruleHandlePageType' || '":"' || 'custom' || '"}'', ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''24'', ''cryptorRequest'', ''Cryptor'', 100, ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''25'', ''cryptorResponse'', ''Cryptor'', 410, ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config, enabled) VALUES (''26'', ''websocket'', ''Proxy'', 200, ''{"' || 'multiSelectorHandle' || '":"' || '1' || '"}'', ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, enabled) VALUES (''27'', ''generalContext'', ''Common'', 125, ''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config, enabled) VALUES (''28'', ''mqtt'', ''Proxy'', 125, ''{"' || 'port' || '": 9500,"' || 'bossGroupThreadCount' || '": 1,"' || 'maxPayloadSize' || '": 65536,"' || 'workerGroupThreadCount' || '": 12,"' || 'userName' || '": "' || 'shenyu' || '","' || 'password' || '": "' || 'shenyu' || '","' || 'isEncryptPassword' || '": false,"' || 'encryptMode' || '": "' || '","' || 'leakDetectorLevel' || '": "' || 'DISABLED' || '"}'', ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, role, sort, config, enabled) VALUES (''29'', ''loggingRocketMQ'', ''Logging'', 170,''{"' || 'topic' || '":"' || 'shenyu-access-logging' || '", "' || 'namesrvAddr' || '": "' || 'localhost:9876' || '","' || 'producerGroup' || '":"' || 'shenyu-plugin-logging-rocketmq' || '"}'', ''0'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin(id)) */  INTO plugin (id, name, config, role, sort, enabled) VALUES (''30'', ''cache'', ''{"' || 'cacheType' || '":"' || 'memory' || '"}'', ''Cache'', 10, 0)';

/*insert plugin_handle data for sentinel*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''flowRuleGrade'',''flowRuleGrade'',''3'', 2, 8, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '1' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''flowRuleControlBehavior'',''flowRuleControlBehavior'',''3'', 2, 5, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''flowRuleEnable'',''flowRuleEnable (1 or 0)'', ''1'', 2, 7, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '1' || '","' || 'rule' || '":"' || '/^[01]$/' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''flowRuleCount'',''flowRuleCount'',''1'', 2, 6, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''degradeRuleEnable'',''degradeRuleEnable (1 or 0)'', ''1'', 2, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '1' || '","' || 'rule' || '":"' || '/^[01]$/' || '"}'') ';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''degradeRuleGrade'',''degradeRuleGrade'',''3'', 2, 3, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''degradeRuleCount'',''degradeRuleCount'',''1'', 2, 1, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''degradeRuleTimeWindow'',''degradeRuleTimeWindow'',''1'', 2, 4, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''degradeRuleMinRequestAmount'',''degradeRuleMinRequestAmount'',''1'', 2, 3, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '5' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''degradeRuleStatIntervals'',''degradeRuleStatIntervals'',''1'', 2, 3, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '1' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'' ,''degradeRuleSlowRatioThreshold'',''degradeRuleSlowRatioThreshold'',''1'', 2, 3, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '0.5' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''10'', ''fallbackUri'', ''fallbackUri'', 2, 2, 9, ''{"' || 'required' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';

/*insert plugin_handle data for waf*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''2'' ,''permission'',''permission'',''3'', 2, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''2'' ,''statusCode'',''statusCode'',''2'', 2, 2)';

/*insert plugin_handle data for rateLimiter*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''4'' ,''replenishRate'',''replenishRate'', 2, 2, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '10' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''4'' ,''burstCapacity'',''burstCapacity'', 2, 2, 3, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '100' || '","' || 'rule' || '":"' || '' || '"}'')';

/*insert plugin_handle data for rewrite*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''3'', ''regex'', ''regex'', 2, 2, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''3'', ''replace'', ''replace'', 2, 2, 2)';

/*insert plugin_handle data for redirect*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''16'' ,''redirectURI'',''redirectURI'', 2, 2, 1)';

/*insert plugin_handle data for springCloud*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''8'' ,''path'',''path'', 2, 2, 1)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''8'' ,''timeout'',''timeout (ms)'', 1, 2, 2)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''8'' ,''serviceId'',''serviceId'', 2, 1, 1)';

/*insert plugin_handle data for resilience4j*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''timeoutDurationRate'',''timeoutDurationRate (ms)'', 1, 2, 1, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '5000' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''limitRefreshPeriod'',''limitRefreshPeriod (ms)'', 1, 2, 0, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '500' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''limitForPeriod'',''limitForPeriod'', 1, 2, 0, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '50' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''circuitEnable'',''circuitEnable'', 1, 2, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'rule' || '":"' || '/^[01]$/' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''timeoutDuration'',''timeoutDuration (ms)'', 1, 2, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '30000' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''12'' ,''fallbackUri'',''fallbackUri'', 2, 2, 2)';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''slidingWindowSize'',''slidingWindowSize'', 1, 2, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '100' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''slidingWindowType'',''slidingWindowType'', 1, 2, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'rule' || '":"' || '/^[01]$/' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''minimumNumberOfCalls'',''minimumNumberOfCalls'', 1, 2, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '100' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''waitIntervalFunctionInOpenState'',''waitIntervalInOpen'', 1, 2, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '60000' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''permittedNumberOfCallsInHalfOpenState'',''bufferSizeInHalfOpen'', 1, 2, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '10' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'' ,''failureRateThreshold'',''failureRateThreshold'', 1, 2, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || '50' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''12'', ''automaticTransitionFromOpenToHalfOpenEnabled'', ''automaticHalfOpen'', 3, 2, 1, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || 'true' || '","' || 'rule' || '":"' || '' || '"}'')';

/*insert plugin_handle data for plugin*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''4'', ''mode'', ''mode'', 3, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''4'', ''master'', ''master'', 2, 3, 2, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''4'', ''url'', ''url'', 2, 3, 3, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''4'', ''password'', ''password'', 2, 3, 4, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''11'', ''protocol'', ''protocol'', 2, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''11'', ''register'', ''register'', 2, 3, 2, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''2'', ''model'', ''model'', 2, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''register'', ''register'', 2, 3, 1, NULL)';

/*insert plugin_handle data for plugin rateLimiter*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''4'' ,''algorithmName'',''algorithmName'',''3'', 2, 1, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || 'slidingWindow' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''4'' ,''keyResolverName'',''keyResolverName'',''3'', 2, 4, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || 'WHOLE_KEY_RESOLVER' || '","' || 'rule' || '":"' || '' || '"}'')';

/*insert plugin_handle data for divide*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''upstreamHost'', ''host'', 2, 1, 0, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''protocol'', ''protocol'', 2, 1, 2, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '' || '","' || 'placeholder' || '":"' || 'http://' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''upstreamUrl'', ''ip:port'', 2, 1, 1, ''{"' || 'required' || '":"' || '1' || '","' || 'placeholder' || '":"' || '' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''weight'', ''weight'', 1, 1, 3, ''{"' || 'defaultValue' || '":"' || '50' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''timestamp'', ''startupTime'', 1, 1, 3, ''{"' || 'defaultValue' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'startup timestamp' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''warmup'', ''warmupTime'', 1, 1, 5, ''{"' || 'defaultValue' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'warmup time (ms)' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''status'', ''status'', 3, 1, 6, ''{"' || 'defaultValue' || '":"' || 'true' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''loadBalance'', ''loadStrategy'', 3, 2, 0, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''retry'', ''retryCount'', 1, 2, 1, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''timeout'', ''timeout'', 1, 2, 2, ''{"' || 'defaultValue' || '":"' || '3000' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''multiSelectorHandle'', ''multiSelectorHandle'', 3, 3, 0, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''multiRuleHandle'', ''multiRuleHandle'', 3, 3, 1, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''headerMaxSize'', ''headerMaxSize'', 1, 2, 3, ''{"' || 'defaultValue' || '":"' || '10240' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''requestMaxSize'', ''requestMaxSize'', 1, 2, 4, ''{"' || 'defaultValue' || '":"' || '102400' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''5'', ''retryStrategy'', ''retryStrategy'', 3, 2, 0, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || 'current' || '","' || 'placeholder' || '":"' || 'retryStrategy' || '","' || 'rule' || '":"' || '' || '"}'')';

/*insert plugin_handle data for tars*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''upstreamHost'', ''host'', 2, 1, 0, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''protocol'', ''protocol'', 2, 1, 2, ''{"' || 'defaultValue' || '":"' || '' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''upstreamUrl'', ''ip:port'', 2, 1, 1, ''{"' || 'required' || '":"' || '1' || '","' || 'placeholder' || '":"' || '' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''weight'', ''weight'', 1, 1, 3, ''{"' || 'defaultValue' || '":"' || '50' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''timestamp'', ''startupTime'', 1, 1, 3, ''{"' || 'defaultValue' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'startup timestamp' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''warmup'', ''warmupTime'', 1, 1, 5, ''{"' || 'defaultValue' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'warmup time (ms)' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''status'', ''status'', 3, 1, 6, ''{"' || 'defaultValue' || '":"' || 'true' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''loadBalance'', ''loadStrategy'', 3, 2, 0, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''retry'', ''retryCount'', 1, 2, 1, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''timeout'', ''timeout'', 1, 2, 2, ''{"' || 'defaultValue' || '":"' || '3000' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''multiSelectorHandle'', ''multiSelectorHandle'', 3, 3, 0, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''13'', ''multiRuleHandle'', ''multiRuleHandle'', 3, 3, 1, null)';

/*insert plugin_handle data for grpc*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''15'', ''upstreamUrl'', ''ip:port'', 2, 1, 1, ''{"' || 'required' || '":"' || '1' || '","' || 'placeholder' || '":"' || '' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''15'', ''weight'', ''weight'', 1, 1, 3, ''{"' || 'defaultValue' || '":"' || '50' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''15'', ''status'', ''status'', 3, 1, 6, ''{"' || 'defaultValue' || '":"' || 'true' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''15'', ''multiSelectorHandle'', ''multiSelectorHandle'', 3, 3, 0, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''15'', ''multiRuleHandle'', ''multiRuleHandle'', 3, 3, 1, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''15'', ''threadpool'', ''threadpool'', 3, 3, 0, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || 'cached' || '","' || 'placeholder' || '":"' || 'threadpool' || '","' || 'rule' || '":"' || '' || '"}'')';

/*insert plugin_handle data for context path*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''14'', ''contextPath'', ''contextPath'', 2, 2, 0)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort) VALUES (''14'', ''addPrefix'', ''addPrefix'', 2, 2, 0)';

/*insert plugin_handle data for request*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''20'', ''ruleHandlePageType'', ''ruleHandlePageType'', 3, 3, 0, ''{"' || 'required' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';

/*insert plugin_handle data for plugin jwt*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''19'' ,''secretKey'',''secretKey'',2, 3, 0, null)';

/*insert plugin_handle data for plugin Cryptor*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''24'', ''strategyName'', ''strategyName'', 3, 2, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''24'', ''fieldNames'', ''fieldNames'', 2, 2, 3, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''24'', ''decryptKey'', ''decryptKey'', 2, 2, 3, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''24'', ''encryptKey'', ''encryptKey'', 2, 2, 3, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''24'', ''way'', ''way'', 3, 2, 3, NULL)';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''25'', ''strategyName'', ''strategyName'', 3, 2, 2, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''25'', ''decryptKey'', ''decryptKey'', 2, 2, 3, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''25'', ''encryptKey'', ''encryptKey'', 2, 2, 3, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''25'', ''fieldNames'', ''fieldNames'', 2, 2, 4, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''25'', ''way'', ''way'', 3, 2, 3, NULL)';

/*insert plugin_handle data for dubbo*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''gray'', ''gray'', ''3'', ''1'', ''9'', ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || 'false' || '","' || 'placeholder' || '":"' || 'gray' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''group'', ''group'', ''2'', ''1'', ''3'', ''{"' || 'required' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'group' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''loadbalance'', ''loadbalance'', ''3'', ''2'', ''0'', ''{"' || 'required' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'loadbalance' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''multiSelectorHandle'', ''multiSelectorHandle'', ''3'', ''3'', ''0'', NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''protocol'', ''protocol'', ''2'', ''1'', ''2'', ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '' || '","' || 'placeholder' || '":"' || 'http://' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''status'', ''status'', ''3'', ''1'', ''8'', ''{"' || 'defaultValue' || '":"' || 'true' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''timestamp'', ''startupTime'', ''1'', ''1'', ''7'', ''{"' || 'defaultValue' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'startup timestamp' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''upstreamHost'', ''host'', ''2'', ''1'', ''0'', NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''upstreamUrl'', ''ip:port'', ''2'', ''1'', ''1'', ''{"' || 'required' || '":"' || '1' || '","' || 'placeholder' || '":"' || '' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''version'', ''version'', ''2'', ''1'', ''4'', ''{"' || 'required' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'version' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''warmup'', ''warmupTime'', ''1'', ''1'', ''6'', ''{"' || 'defaultValue' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'warmup time (ms)' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''weight'', ''weight'', ''1'', ''1'', ''5'', ''{"' || 'defaultValue' || '":"' || '50' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''threadpool'', ''threadpool'', ''3'', ''3'', ''0'', ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || 'cached' || '","' || 'placeholder' || '":"' || 'threadpool' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''corethreads'', ''corethreads'', ''1'', ''3'', ''0'', ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'corethreads' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''threads'', ''threads'', ''1'', ''3'', ''0'', ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '2147483647' || '","' || 'placeholder' || '":"' || 'threads' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''6'', ''queues'', ''queues'', ''1'', ''3'', ''0'', ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'queues' || '","' || 'rule' || '":"' || '' || '"}'')';

/*insert plugin_handle data for websocket*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''host'', ''host'', 2, 1, 0, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''protocol'', ''protocol'', 2, 1, 2, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '' || '","' || 'placeholder' || '":"' || 'ws://' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''url'', ''ip:port'', 2, 1, 1, ''{"' || 'required' || '":"' || '1' || '","' || 'placeholder' || '":"' || '' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''weight'', ''weight'', 1, 1, 3, ''{"' || 'defaultValue' || '":"' || '50' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''timestamp'', ''startupTime'', 1, 1, 3, ''{"' || 'defaultValue' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'startup timestamp' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''warmup'', ''warmupTime'', 1, 1, 5, ''{"' || 'defaultValue' || '":"' || '0' || '","' || 'placeholder' || '":"' || 'warmup time (ms)' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''status'', ''status'', 3, 1, 6, ''{"' || 'defaultValue' || '":"' || 'true' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''loadBalance'', ''loadStrategy'', 3, 2, 0, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''retry'', ''retryCount'', 1, 2, 1, null)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''timeout'', ''timeout'', 1, 2, 2, ''{"' || 'defaultValue' || '":"' || '3000' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''26'', ''multiSelectorHandle'', ''multiSelectorHandle'', 3, 3, 0, null)';

/*insert plugin_handle data for plugin motan*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''17'', ''register'', ''register'', 2, 3, 0, null)';

/*insert plugin_handle data for plugin mqtt*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''28'', ''port'', ''port'', 1, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''28'', ''bossGroupThreadCount'', ''bossGroupThreadCount'', 1, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''28'', ''maxPayloadSize'', ''maxPayloadSize'', 1, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''28'', ''workerGroupThreadCount'', ''workerGroupThreadCount'', 1, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''28'', ''userName'', ''userName'', 2, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''28'', ''password'', ''password'', 2, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''28'', ''isEncryptPassword'', ''isEncryptPassword'', 2, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''28'', ''encryptMode'', ''encryptMode'', 2, 3, 1, NULL)';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''28'', ''leakDetectorLevel'', ''leakDetectorLevel'', 2, 3, 1, NULL)';

/*insert plugin_handle data for plugin loggingRocketMQ*/
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''29'', ''topic'', ''topic'', 2, 3, 1, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || 'shenyu-access-logging' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''29'', ''namesrvAddr'', ''namesrvAddr'', 2, 3, 2, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || 'localhost:9876' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''29'', ''producerGroup'', ''producerGroup'', 2, 3, 3, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || 'shenyu-plugin-logging-rocketmq' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''29'', ''sampleRate'', ''sampleRate'', 2, 3, 4, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '1' || '","' || 'placeholder' || '":"' || 'optional,0,0.01~1' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''29'', ''maxResponseBody'', ''maxResponseBody'', 1, 3, 5, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":524288}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''29'', ''maxRequestBody'', ''maxRequestBody'', 1, 3, 6, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":524288}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''29'', ''compressAlg'', ''compressAlg'', 3, 3, 7, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || 'none' || '"}'')';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''29'', ''topic'', ''topic'', 2, 1, 1, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '' || '","' || 'placeholder' || '":"' || 'optional' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id,field,label,data_type,type,sort,ext_obj) VALUES (''29'', ''sampleRate'', ''sampleRate'', 2, 1, 2, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '' || '","' || 'placeholder' || '":"' || 'optional,0,0.01~1' || '"}'')';

/* insert plugin_handle data for cache */
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''cacheType'', ''cacheType'', 3, 3, 1, ''{"' || 'required' || '":"' || '1' || '","' || 'defaultValue' || '":"' || 'memory' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''database'', ''database'', 1, 3, 2, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''master'', ''master'', 2, 3, 3, ''{"' || 'required' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''mode'', ''mode'', 2, 3, 4, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || 'standalone' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''url'', ''url'', 2, 3, 5, ''{"' || 'required' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''password'', ''password'', 2, 3, 6, ''{"' || 'required' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''maxIdle'', ''maxIdle'', 1, 3, 7, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '8' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''minIdle'', ''minIdle'', 1, 3, 8, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '0' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''maxActive'', ''maxActive'', 1, 3, 9, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '8' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''maxWait'', ''maxWait'', 3, 3, 10, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '-1' || '","' || 'rule' || '":"' || '' || '"}'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */  INTO plugin_handle (plugin_id, field, label, data_type, type, sort, ext_obj) VALUES (''30'', ''timeoutSeconds'', ''timeoutSeconds'', 1, 2, 0, ''{"' || 'required' || '":"' || '0' || '","' || 'defaultValue' || '":"' || '60' || '","' || 'rule' || '":"' || '' || '"}'')';

/** insert resource for resource */
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */ INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1346775491550474240'','''',''SHENYU.MENU.PLUGIN.LIST'',''plug'',''/plug'',''PluginList'',''0'',''0'',''dashboard'',''0'',''0'','''',''1'')';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1357956838021890048'','''',''SHENYU.MENU.CONFIG.MANAGMENT'',''config'',''/config'',''config'',''0'',''1'',''api'',''0'',''0'','''',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1346777449787125760'',''1357956838021890048'',''SHENYU.MENU.SYSTEM.MANAGMENT.PLUGIN'',''plugin'',''/config/plugin'',''plugin'',''1'',''2'',''book'',''0'',''0'','''',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347034027070337024'',''1346777449787125760'',''SHENYU.BUTTON.SYSTEM.LIST'','''','''','''',''2'',''0'','''',''1'',''0'',''system:plugin:list'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347039054925148160'',''1346777449787125760'',''SHENYU.BUTTON.SYSTEM.DELETE'','''','''','''',''2'',''1'','''',''1'',''0'',''system:plugin:delete'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347041326749691904'',''1346777449787125760'',''SHENYU.BUTTON.SYSTEM.ADD'','''','''','''',''2'',''2'','''',''1'',''0'',''system:plugin:add'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347046566244003840'',''1346777449787125760'',''SHENYU.BUTTON.SYSTEM.SYNCHRONIZE'','''','''','''',''2'',''3'','''',''1'',''0'',''system:plugin:modify'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347047143350874112'',''1346777449787125760'',''SHENYU.BUTTON.SYSTEM.ENABLE'','''','''','''',''2'',''4'','''',''1'',''0'',''system:plugin:disable'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347047203220369408'',''1346777449787125760'',''SHENYU.BUTTON.SYSTEM.EDIT'','''','''','''',''2'',''5'','''',''1'',''0'',''system:plugin:edit'',''1'')';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1346777623011880960'',''1357956838021890048'',''SHENYU.PLUGIN.PLUGINHANDLE'',''pluginhandle'',''/config/pluginhandle'',''pluginhandle'',''1'',''3'',''down-square'',''0'',''0'','''',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347047555588042752'',''1346777623011880960'',''SHENYU.BUTTON.SYSTEM.LIST'','''','''','''',''2'',''0'','''',''1'',''0'',''system:pluginHandler:list'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347047640145211392'',''1346777623011880960'',''SHENYU.BUTTON.SYSTEM.DELETE'','''','''','''',''2'',''1'','''',''1'',''0'',''system:pluginHandler:delete'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347047695002513408'',''1346777623011880960'',''SHENYU.BUTTON.SYSTEM.ADD'','''','''','''',''2'',''2'','''',''1'',''0'',''system:pluginHandler:add'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347047747305484288'',''1346777623011880960'',''SHENYU.BUTTON.SYSTEM.EDIT'','''','''','''',''2'',''3'','''',''1'',''0'',''system:pluginHandler:edit'',''1'')';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1346777766301888512'',''1357956838021890048'',''SHENYU.MENU.SYSTEM.MANAGMENT.AUTHEN'',''auth'',''/config/auth'',''auth'',''1'',''4'',''audit'',''0'',''0'','''',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347048004105940992'',''1346777766301888512'',''SHENYU.BUTTON.SYSTEM.LIST'','''','''','''',''2'',''0'','''',''1'',''0'',''system:authen:list'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347048101875167232'',''1346777766301888512'',''SHENYU.BUTTON.SYSTEM.DELETE'','''','''','''',''2'',''1'','''',''1'',''0'',''system:authen:delete'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347048145877610496'',''1346777766301888512'',''SHENYU.BUTTON.SYSTEM.ADD'','''','''','''',''2'',''2'','''',''1'',''0'',''system:authen:add'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347048240677269504'',''1346777766301888512'',''SHENYU.BUTTON.SYSTEM.ENABLE'','''','''','''',''2'',''3'','''',''1'',''0'',''system:authen:disable'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347048316216684544'',''1346777766301888512'',''SHENYU.BUTTON.SYSTEM.SYNCHRONIZE'','''','''','''',''2'',''4'','''',''1'',''0'',''system:authen:modify'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347048776029843456'',''1346777766301888512'',''SHENYU.BUTTON.SYSTEM.EDIT'','''','''','''',''2'',''5'','''',''1'',''0'',''system:authen:edit'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1350804501819195392'',''1346777766301888512'',''SHENYU.BUTTON.SYSTEM.EDITRESOURCEDETAILS'','''','''','''',''2'',''6'','''',''1'',''0'',''system:authen:editResourceDetails'',''1'')';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1346777907096285184'',''1357956838021890048'',''SHENYU.MENU.SYSTEM.MANAGMENT.METADATA'',''metadata'',''/config/metadata'',''metadata'',''1'',''5'',''snippets'',''0'',''0'','''',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347048968414179328'',''1346777907096285184'',''SHENYU.BUTTON.SYSTEM.LIST'','''','''','''',''2'',''0'','''',''1'',''0'',''system:meta:list'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347049029323862016'',''1346777907096285184'',''SHENYU.BUTTON.SYSTEM.DELETE'','''','''','''',''2'',''1'','''',''1'',''0'',''system:meta:delete'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347049092552994816'',''1346777907096285184'',''SHENYU.BUTTON.SYSTEM.ADD'','''','''','''',''2'',''2'','''',''1'',''0'',''system:meta:add'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347049251395481600'',''1346777907096285184'',''SHENYU.BUTTON.SYSTEM.ENABLE'','''','''','''',''2'',''3'','''',''1'',''0'',''system:meta:disable'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347049317178945536'',''1346777907096285184'',''SHENYU.BUTTON.SYSTEM.SYNCHRONIZE'','''','''','''',''2'',''4'','''',''1'',''0'',''system:meta:modify'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347049370014593024'',''1346777907096285184'',''SHENYU.BUTTON.SYSTEM.EDIT'','''','''','''',''2'',''5'','''',''1'',''0'',''system:meta:edit'',''1'')';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1346778036402483200'',''1357956838021890048'',''SHENYU.MENU.SYSTEM.MANAGMENT.DICTIONARY'',''dict'',''/config/dict'',''dict'',''1'',''6'',''ordered-list'',''0'',''0'','''',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347049542417264640'',''1346778036402483200'',''SHENYU.BUTTON.SYSTEM.LIST'','''','''','''',''2'',''0'','''',''1'',''0'',''system:dict:list'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347049598155370496'',''1346778036402483200'',''SHENYU.BUTTON.SYSTEM.DELETE'','''','''','''',''2'',''1'','''',''1'',''0'',''system:dict:delete'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347049659023110144'',''1346778036402483200'',''SHENYU.BUTTON.SYSTEM.ADD'','''','''','''',''2'',''2'','''',''1'',''0'',''system:dict:add'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347049731047698432'',''1346778036402483200'',''SHENYU.BUTTON.SYSTEM.ENABLE'','''','''','''',''2'',''3'','''',''1'',''0'',''system:dict:disable'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347049794008395776'',''1346778036402483200'',''SHENYU.BUTTON.SYSTEM.EDIT'','''','''','''',''2'',''4'','''',''1'',''0'',''system:dict:edit'',''1'')';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1346776175553376256'','''',''SHENYU.MENU.SYSTEM.MANAGMENT'',''system'',''/system'',''system'',''0'',''2'',''setting'',''0'',''0'','''',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1346777157943259136'',''1346776175553376256'',''SHENYU.MENU.SYSTEM.MANAGMENT.USER'',''manage'',''/system/manage'',''manage'',''1'',''1'',''user'',''0'',''0'','''',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347032308726902784'',''1346777157943259136'',''SHENYU.BUTTON.SYSTEM.ADD'','''','''','''',''2'',''0'','''',''1'',''0'',''system:manager:add'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347032395901317120'',''1346777157943259136'',''SHENYU.BUTTON.SYSTEM.LIST'','''','''','''',''2'',''1'','''',''1'',''0'',''system:manager:list'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347032453707214848'',''1346777157943259136'',''SHENYU.BUTTON.SYSTEM.DELETE'','''','''','''',''2'',''2'','''',''1'',''0'',''system:manager:delete'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1347032509051056128'',''1346777157943259136'',''SHENYU.BUTTON.SYSTEM.EDIT'','''','''','''',''2'',''3'','''',''1'',''0'',''system:manager:edit'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1386680049203195904'',''1346777157943259136'',''SHENYU.BUTTON.DATA.PERMISSION.CONFIG'', '''', '''', '''', 2, 0, '''', 1, 0, ''system:manager:configureDataPermission'', 1)';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1350106119681622016'',''1346776175553376256'',''SHENYU.MENU.SYSTEM.MANAGMENT.ROLE'',''role'',''/system/role'',''role'',''1'',''0'',''usergroup-add'',''0'',''0'','''',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1350107709494804480'',''1350106119681622016'',''SHENYU.BUTTON.SYSTEM.ADD'','''','''','''',''2'',''0'','''',''1'',''0'',''system:role:add'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1350107842236137472'',''1350106119681622016'',''SHENYU.BUTTON.SYSTEM.LIST'','''','''','''',''2'',''1'','''',''1'',''0'',''system:role:list'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1350112406754766848'',''1350106119681622016'',''SHENYU.BUTTON.SYSTEM.DELETE'','''','''','''',''2'',''2'','''',''1'',''0'',''system:role:delete'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1350112481253994496'',''1350106119681622016'',''SHENYU.BUTTON.SYSTEM.EDIT'','''','''','''',''2'',''3'','''',''1'',''0'',''system:role:edit'',''1'')';

execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1355163372527050752'',''1346776175553376256'',''SHENYU.MENU.SYSTEM.MANAGMENT.RESOURCE'',''resource'',''/system/resource'',''resource'',''1'',''2'',''menu'',''0'',''0'','''',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1355165158419750912'',''1355163372527050752'',''SHENYU.BUTTON.RESOURCE.MENU.ADD'','''','''','''',''2'',''1'','''',''1'',''0'',''system:resource:addMenu'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1355165353534578688'',''1355163372527050752'',''SHENYU.BUTTON.SYSTEM.LIST'','''','''','''',''2'',''0'','''',''1'',''0'',''system:resource:list'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1355165475785957376'',''1355163372527050752'',''SHENYU.BUTTON.RESOURCE.MENU.DELETE'','''','''','''',''2'',''2'','''',''1'',''0'',''system:resource:deleteMenu'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1355165608565039104'',''1355163372527050752'',''SHENYU.BUTTON.RESOURCE.MENU.EDIT'','''','''','''',''2'',''3'','''',''1'',''0'',''system:resource:editMenu'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1357977745889132544'',''1355163372527050752'',''SHENYU.BUTTON.RESOURCE.BUTTON.ADD'','''','''','''',''2'',''4'','''',''1'',''0'',''system:resource:addButton'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1357977912126177280'',''1355163372527050752'',''SHENYU.SYSTEM.EDITOR'','''','''','''',''2'',''5'','''',''1'',''0'',''system:resource:editButton'',''1'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX("' || 'resource' || '"(id)) */  INTO "' || 'resource' || '" (id, parent_id, title, name, url, component, resource_type, sort, icon, is_leaf, is_route, perms, status) VALUES(''1357977971827900416'',''1355163372527050752'',''SHENYU.SYSTEM.DELETEDATA'','''','''','''',''2'',''6'','''',''1'',''0'',''system:resource:deleteButton'',''1'')';

/* default permission */
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708572688384'', ''1346358560427216896'', ''1346775491550474240'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1357956838021890049'', ''1346358560427216896'', ''1357956838021890048'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708597854208'', ''1346358560427216896'', ''1346777449787125760'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708702711808'', ''1346358560427216896'', ''1347034027070337024'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708706906112'', ''1346358560427216896'', ''1347039054925148160'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708711100416'', ''1346358560427216896'', ''1347041326749691904'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708715294720'', ''1346358560427216896'', ''1347046566244003840'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708719489024'', ''1346358560427216896'', ''1347047143350874112'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708723683328'', ''1346358560427216896'', ''1347047203220369408'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708606242816'', ''1346358560427216896'', ''1346777623011880960'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708727877632'', ''1346358560427216896'', ''1347047555588042752'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708732071936'', ''1346358560427216896'', ''1347047640145211392'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708732071937'', ''1346358560427216896'', ''1347047695002513408'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708736266240'', ''1346358560427216896'', ''1347047747305484288'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708610437120'', ''1346358560427216896'', ''1346777766301888512'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708740460544'', ''1346358560427216896'', ''1347048004105940992'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708744654848'', ''1346358560427216896'', ''1347048101875167232'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708744654849'', ''1346358560427216896'', ''1347048145877610496'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708748849152'', ''1346358560427216896'', ''1347048240677269504'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708753043456'', ''1346358560427216896'', ''1347048316216684544'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708757237760'', ''1346358560427216896'', ''1347048776029843456'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007709088587777'', ''1346358560427216896'', ''1350804501819195392'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708614631424'', ''1346358560427216896'', ''1346777907096285184'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708757237761'', ''1346358560427216896'', ''1347048968414179328'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708761432064'', ''1346358560427216896'', ''1347049029323862016'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708765626368'', ''1346358560427216896'', ''1347049092552994816'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708769820672'', ''1346358560427216896'', ''1347049251395481600'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708774014976'', ''1346358560427216896'', ''1347049317178945536'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708774014977'', ''1346358560427216896'', ''1347049370014593024'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708623020032'', ''1346358560427216896'', ''1346778036402483200'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708778209280'', ''1346358560427216896'', ''1347049542417264640'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708782403584'', ''1346358560427216896'', ''1347049598155370496'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708786597888'', ''1346358560427216896'', ''1347049659023110144'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708790792192'', ''1346358560427216896'', ''1347049731047698432'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708794986496'', ''1346358560427216896'', ''1347049794008395776'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708585271296'', ''1346358560427216896'', ''1346776175553376256'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708593659904'', ''1346358560427216896'', ''1346777157943259136'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708685934593'', ''1346358560427216896'', ''1347032308726902784'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708690128896'', ''1346358560427216896'', ''1347032395901317120'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708694323200'', ''1346358560427216896'', ''1347032453707214848'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007708698517504'', ''1346358560427216896'', ''1347032509051056128'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1386680049203195905'', ''1346358560427216896'', ''1386680049203195904'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007709080199168'', ''1346358560427216896'', ''1350106119681622016'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007709080199169'', ''1346358560427216896'', ''1350107709494804480'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007709084393472'', ''1346358560427216896'', ''1350107842236137472'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007709084393473'', ''1346358560427216896'', ''1350112406754766848'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1351007709088587776'', ''1346358560427216896'', ''1350112481253994496'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1355167519859040256'', ''1346358560427216896'', ''1355163372527050752'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1355167519859040257'', ''1346358560427216896'', ''1355165158419750912'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1355167519859040258'', ''1346358560427216896'', ''1355165353534578688'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1355167519859040259'', ''1346358560427216896'', ''1355165475785957376'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1355167519859040260'', ''1346358560427216896'', ''1355165608565039104'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1357977745893326848'', ''1346358560427216896'', ''1357977745889132544'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1357977912126177281'', ''1346358560427216896'', ''1357977912126177280'')';
execute immediate 'INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX (permission(id)) */  INTO permission (id, object_id, resource_id) VALUES (''1357977971827900417'', ''1346358560427216896'', ''1357977971827900416'')';


end;