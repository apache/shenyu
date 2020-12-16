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

CREATE Schema IF NOT EXISTS  `soul`;

/*Table structure for table `dashboard_user` */
CREATE TABLE IF NOT EXISTS `dashboard_user` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `user_name` varchar(64) NOT NULL COMMENT '用户名',
  `password` varchar(128) DEFAULT NULL COMMENT '用户密码',
  `role` int(4) NOT NULL COMMENT '角色',
  `enabled` tinyint(4) NOT NULL COMMENT '是否删除',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB;

/*Table structure for table `plugin` */
CREATE TABLE IF NOT EXISTS `plugin` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `name` varchar(62) NOT NULL COMMENT '插件名称',
  `config` text COMMENT '插件配置',
  `role` int(4) NOT NULL COMMENT '插件角色',
  `enabled` tinyint(4) NOT NULL DEFAULT '0' COMMENT '是否开启（0，未开启，1开启）',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS `plugin_handle` (
  `id` varchar(128) NOT NULL,
  `plugin_id` varchar(128) NOT NULL COMMENT '插件id',
  `field` varchar(100) NOT NULL COMMENT '字段',
  `label` varchar(100) DEFAULT NULL COMMENT '标签',
  `data_type` smallint(6) NOT NULL DEFAULT '1' COMMENT '数据类型 1 数字 2 字符串',
  `type` smallint(6) NULL COMMENT '类型,1 表示选择器，2 表示规则',
  `sort` int(4)  NULL COMMENT '排序',
  `ext_obj` varchar(1024) DEFAULT NULL COMMENT '额外配置（json格式数据）',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `plugin_id_field_type` (`plugin_id`,`field`,`type`)
) ENGINE=InnoDB;


/*Table structure for table `selector` */
CREATE TABLE IF NOT EXISTS `selector` (
  `id` varchar(128) NOT NULL COMMENT '主键id varchar' primary key,
  `plugin_id` varchar(128) NOT NULL COMMENT '插件id',
  `name` varchar(64) NOT NULL COMMENT '选择器名称',
  `match_mode` int(2) NOT NULL COMMENT '匹配方式（0 and  1 or)',
  `type` int(4) NOT NULL COMMENT '类型（0，全流量，1自定义流量）',
  `sort` int(4) NOT NULL COMMENT '排序',
  `handle` varchar(1024) DEFAULT NULL COMMENT '处理逻辑（此处针对不同的插件，会有不同的字段来标识不同的处理，所有存储json格式数据）',
  `enabled` tinyint(4) NOT NULL COMMENT '是否开启',
  `loged` tinyint(4) NOT NULL COMMENT '是否打印日志',
  `continued` tinyint(4) NOT NULL COMMENT '是否继续执行',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  constraint unique_selector_name unique (`name`)
) ENGINE=InnoDB;

/*Table structure for table `selector_condition` */
CREATE TABLE IF NOT EXISTS `selector_condition` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `selector_id` varchar(128) NOT NULL COMMENT '选择器id',
  `param_type` varchar(64) NOT NULL COMMENT '参数类型（post  query  uri等）',
  `operator` varchar(64) NOT NULL COMMENT '匹配符（=  > <  like match）',
  `param_name` varchar(64) NOT NULL COMMENT '参数名称',
  `param_value` varchar(64) NOT NULL COMMENT '参数值',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB;

/*Table structure for table `rule` */
CREATE TABLE IF NOT EXISTS `rule` (
  `id` varchar(128) NOT NULL COMMENT '主键id' PRIMARY KEY,
  `selector_id` varchar(128) NOT NULL COMMENT '选择器id',
  `match_mode` int(2) NOT NULL COMMENT '匹配方式（0 and  1 or)',
  `name` varchar(128) NOT NULL COMMENT '规则名称',
  `enabled` tinyint(4) NOT NULL COMMENT '是否开启',
  `loged` tinyint(4) NOT NULL COMMENT '是否记录日志',
  `sort` int(4) NOT NULL COMMENT '排序',
  `handle` varchar(1024) DEFAULT NULL COMMENT '处理逻辑（此处针对不同的插件，会有不同的字段来标识不同的处理，所有存储json格式数据）',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
   constraint unique_rule_name unique (`name`)
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS `rule_condition` (
  `id` varchar(128) NOT NULL COMMENT '主键id' PRIMARY KEY,
  `rule_id` varchar(128) NOT NULL COMMENT '规则id',
  `param_type` varchar(64) NOT NULL COMMENT '参数类型（post  query  uri等）',
  `operator` varchar(64) NOT NULL COMMENT '匹配符（=  > <  like match）',
  `param_name` varchar(64) NOT NULL COMMENT '参数名称',
  `param_value` varchar(64) NOT NULL COMMENT '参数值',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间'
) ENGINE=InnoDB;

CREATE TABLE  IF NOT EXISTS `meta_data` (
  `id` varchar(128) NOT NULL COMMENT 'id',
  `app_name` varchar(255) NOT NULL COMMENT '应用名称',
  `path` varchar(255) NOT NULL COMMENT '路径,不能重复',
  `path_desc` varchar(255) NOT NULL COMMENT '路径描述',
  `rpc_type` varchar(64) NOT NULL COMMENT 'rpc类型',
  `service_name` varchar(255) NULL DEFAULT NULL COMMENT '服务名称',
  `method_name` varchar(255) NULL DEFAULT NULL COMMENT '方法名称',
  `parameter_types` varchar(255) NULL DEFAULT NULL COMMENT '参数类型 多给参数类型 逗号隔开',
  `rpc_ext` varchar(512) NULL DEFAULT NULL COMMENT 'rpc的扩展信息，json格式',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  `enabled` tinyint(4) NOT NULL DEFAULT 0 COMMENT '启用状态',
  PRIMARY KEY (`id`)
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS `app_auth`  (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `app_key` varchar(32) NOT NULL COMMENT '应用标识key',
  `app_secret` varchar(128) NOT NULL COMMENT '加密算法secret',
  `user_id` varchar(128) NULL DEFAULT NULL COMMENT '用户id',
  `phone` varchar(255) NULL DEFAULT NULL COMMENT '用户申请时候的电话号码',
  `ext_info` varchar(1024) NULL DEFAULT NULL COMMENT '扩展参数 json',
  `enabled` tinyint(4) NOT NULL COMMENT '是否删除',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS `auth_param`  (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `auth_id` varchar(128) NULL DEFAULT NULL COMMENT '认证表id',
  `app_name` varchar(255) NOT NULL COMMENT '业务模块',
  `app_param` varchar(255) NULL DEFAULT NULL COMMENT '业务模块参数（网关需要传递的参数）json类型',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE = InnoDB;

-- ----------------------------
-- Table structure for auth_path
-- ----------------------------
CREATE TABLE IF NOT EXISTS `auth_path`  (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `auth_id` varchar(128) NOT NULL COMMENT 'auth表id',
  `app_name` varchar(255) NOT NULL COMMENT '模块',
  `path` varchar(255) NOT NULL COMMENT '路径',
  `enabled` tinyint(4) NOT NULL COMMENT '是否通过 1 是 ',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS `soul_dict` (
   `id` varchar(128) NOT NULL COMMENT '主键id',
   `type` varchar(100) NOT NULL COMMENT '类型',
   `dict_code` varchar(100) NOT NULL COMMENT '字典编码',
   `dict_name` varchar(100) NOT NULL COMMENT '字典名称',
   `dict_value` varchar(100) DEFAULT NULL COMMENT '字典值',
   `desc` varchar(255) DEFAULT NULL COMMENT '字典描述或备注',
   `sort` int(4) NOT NULL COMMENT '排序',
   `enabled` tinyint(4) DEFAULT NULL COMMENT '是否开启',
   `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
   `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
   PRIMARY KEY (`id`)
 ) ENGINE=InnoDB;
