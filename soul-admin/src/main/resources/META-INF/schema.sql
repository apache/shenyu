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

CREATE DATABASE  IF NOT EXISTS  `soul`  DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci ;

USE `soul`;

/*Table structure for table `dashboard_user` */
CREATE TABLE IF NOT EXISTS `dashboard_user` (
  `id` varchar(128) NOT NULL COMMENT 'primary key id',
  `user_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'user name',
  `password` varchar(128) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'user password',
  `role` int(4) NOT NULL COMMENT 'role',
  `enabled` tinyint(4) NOT NULL COMMENT 'delete or not',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `plugin` */
CREATE TABLE IF NOT EXISTS `plugin` (
  `id` varchar(128) NOT NULL COMMENT 'primary key id',
  `name` varchar(62) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'plugin name',
  `config` text COLLATE utf8mb4_unicode_ci COMMENT 'plugin configuration',
  `role` int(4) NOT NULL COMMENT 'plug-in role',
  `enabled` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'whether to open (0, not open, 1 open)',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `plugin_handle` (
  `id` varchar(128) NOT NULL,
  `plugin_id` varchar(128) NOT NULL COMMENT 'plugin id',
  `field` varchar(100) NOT NULL COMMENT 'field',
  `label` varchar(100) DEFAULT NULL COMMENT 'label',
  `data_type` smallint(6) NOT NULL DEFAULT '1' COMMENT 'data type 1 number 2 string',
  `type` smallint(6) NULL COMMENT 'type, 1 means selector, 2 means rule, 3 means plugin',
  `sort` int(4)  NULL COMMENT 'sort',
  `ext_obj` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'extra configuration (json format data)',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`),
  UNIQUE KEY `plugin_id_field_type` (`plugin_id`,`field`,`type`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;


/*Table structure for table `selector` */
CREATE TABLE IF NOT EXISTS `selector` (
  `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id varchar' primary key,
  `plugin_id` varchar(128) NOT NULL COMMENT 'plugin id',
  `name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'selector name',
  `match_mode` int(2) NOT NULL COMMENT 'matching mode (0 and 1 or)',
  `type` int(4) NOT NULL COMMENT 'type (0, full flow, 1 custom flow)',
  `sort` int(4) NOT NULL COMMENT 'sort',
  `handle` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)',
  `enabled` tinyint(4) NOT NULL COMMENT 'whether to open',
  `loged` tinyint(4) NOT NULL COMMENT 'whether to print the log',
  `continued` tinyint(4) NOT NULL COMMENT 'whether to continue execution',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  constraint unique_name unique (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `selector_condition` */
CREATE TABLE IF NOT EXISTS `selector_condition` (
  `id` varchar(128) NOT NULL COMMENT 'primary key id',
  `selector_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'selector id',
  `param_type` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'parameter type (to query uri, etc.)',
  `operator` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'matching character (=> <like matching)',
  `param_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'parameter name',
  `param_value` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'parameter value',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `rule` */
CREATE TABLE IF NOT EXISTS `rule` (
  `id` varchar(128) NOT NULL COMMENT 'primary key id' PRIMARY KEY,
  `selector_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'selector id',
  `match_mode` int(2) NOT NULL COMMENT 'matching mode (0 and 1 or)',
  `name` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'rule name',
  `enabled` tinyint(4) NOT NULL COMMENT 'whether to open',
  `loged` tinyint(4) NOT NULL COMMENT 'whether to log or not',
  `sort` int(4) NOT NULL COMMENT 'sort',
  `handle` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
   constraint unique_name unique (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `rule_condition` (
  `id` varchar(128) NOT NULL COMMENT 'primary key id' PRIMARY KEY,
  `rule_id` varchar(128) NOT NULL COMMENT 'rule id',
  `param_type` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'parameter type (post query uri, etc.)',
  `operator` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'matching character (=> <like match)',
  `param_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'parameter name',
  `param_value` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'parameter value',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE  IF NOT EXISTS `meta_data` (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'application name',
  `path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'path, cannot be repeated',
  `path_desc` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'path description',
  `rpc_type` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'rpc type',
  `service_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'service name',
  `method_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'method name',
  `parameter_types` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'parameter types are provided with multiple parameter types separated by commas',
  `rpc_ext` varchar(512) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'rpc extended information, json format',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  `enabled` tinyint(4) NOT NULL DEFAULT 0 COMMENT 'enabled state',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

CREATE TABLE IF NOT EXISTS `app_auth`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
  `app_key` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'application identification key',
  `app_secret` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'encryption algorithm secret',
  `user_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'user id',
  `phone` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'phone number when the user applies',
  `ext_info` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'extended parameter json',
  `enabled` tinyint(4) NOT NULL COMMENT 'delete or not',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

CREATE TABLE IF NOT EXISTS `auth_param`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
  `auth_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'authentication table id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'business Module',
  `app_param` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'service module parameters (parameters that need to be passed by the gateway) json type',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for auth_path
-- ----------------------------
CREATE TABLE IF NOT EXISTS `auth_path`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
  `auth_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'auth table id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'module',
  `path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'path',
  `enabled` tinyint(4) NOT NULL COMMENT 'whether pass 1 is',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

CREATE TABLE IF NOT EXISTS `soul_dict` (
   `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
   `type` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'type',
   `dict_code` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'dictionary encoding',
   `dict_name` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'dictionary name',
   `dict_value` varchar(100) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'dictionary value',
   `desc` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'dictionary description or remarks',
   `sort` int(4) NOT NULL COMMENT 'sort',
   `enabled` tinyint(4) DEFAULT NULL COMMENT 'whether it is enabled',
   `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
   `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
   PRIMARY KEY (`id`)
 ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*soul dict*/
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('1','degradeRuleGrade','DEGRADE_GRADE_RT','slow call ratio','0','degrade type-slow call ratio',1,1,'2020-11-18 14:39:56','2020-11-20 15:43:43');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('2','degradeRuleGrade','DEGRADE_GRADE_EXCEPTION_RATIO','exception ratio','1','degrade type-abnormal ratio',0,1,'2020-11-18 16:42:34','2020-11-20 15:42:58');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('3','degradeRuleGrade','DEGRADE_GRADE_EXCEPTION_COUNT','exception number strategy','2','degrade type-abnormal number strategy',2,1,'2020-11-19 16:23:45','2020-11-20 16:01:00');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('4','flowRuleGrade','FLOW_GRADE_QPS','QPS','1','grade type-QPS',0,1,'2020-11-20 15:42:03','2020-11-20 15:42:03');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('5','flowRuleGrade','FLOW_GRADE_THREAD','number of concurrent threads','0','degrade type-number of concurrent threads',1,1,'2020-11-20 15:44:44','2020-11-20 15:44:44');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('6','flowRuleControlBehavior','CONTROL_BEHAVIOR_DEFAULT','direct rejection by default','0','control behavior-direct rejection by default',0,1,'2020-11-20 15:46:22','2020-11-20 15:48:36');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('7','flowRuleControlBehavior','CONTROL_BEHAVIOR_WARM_UP','warm up','1','control behavior-warm up',1,1,'2020-11-20 15:47:05','2020-11-20 15:47:05');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('8','flowRuleControlBehavior','CONTROL_BEHAVIOR_RATE_LIMITER','constant speed queuing','2','control behavior-uniform speed queuing',2,1,'2020-11-20 15:49:45','2020-11-20 15:49:45');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('9','flowRuleControlBehavior','CONTROL_BEHAVIOR_WARM_UP_RATE_LIMITER','preheating uniformly queued','3','control behavior-preheating uniformly queued',3,1,'2020-11-20 15:51:25', '2020-11-20 15:51:37');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('10','permission','REJECT','reject','reject','reject',0,1,'2020-11-22 12:04:10','2020-11-22 12:04:10');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('11','permission','ALLOW','allow','allow','allow',1,1,'2020-11-22 12:04:10','2020-11-22 12:04:10');

/*plugin*/
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('1','sign','1', '0', '2018-06-14 10:17:35', '2018-06-14 10:17:35');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('2','waf', '1','{"model":"black"}','0', '2018-06-23 10:26:30', '2018-06-13 15:43:10');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('3','rewrite', '1','0', '2018-06-23 10:26:34', '2018-06-25 13:59:31');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('4','rate_limiter','1','{"master":"mymaster","mode":"standalone","url":"192.168.1.1:6379","password":"abc"}', '0', '2018-06-23 10:26:37', '2018-06-13 15:34:48');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('5','divide', '0','1', '2018-06-25 10:19:10', '2018-06-13 13:56:04');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('6','dubbo','1','{"register":"zookeeper://localhost:2181"}', '0', '2018-06-23 10:26:41', '2018-06-11 10:11:47');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('7','monitor', '1','{"metricsName":"prometheus","host":"localhost","port":"9190","async":"true"}','0', '2018-06-25 13:47:57', '2018-06-25 13:47:57');
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('8','springCloud','1', '0', '2018-06-25 13:47:57', '2018-06-25 13:47:57');
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('9','hystrix', '0','0', '2020-01-15 10:19:10', '2020-01-15 10:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('10','sentinel', '1','0', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `config`, `enabled`, `date_created`, `date_updated`) VALUES ('11','sofa', '0', '{"protocol":"zookeeper","register":"127.0.0.1:2181"}', '0', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('12','resilience4j', '1','0', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('13', 'tars', '1','0', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('14', 'context_path', '1','0', '2020-11-09 01:19:10', '2020-11-09 01:19:10');

/**default admin user**/
INSERT IGNORE INTO `dashboard_user` (`id`, `user_name`, `password`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('1','admin','jHcpKkiDbbQh7W7hh8yQSA==', '1', '1', '2018-06-23 15:12:22', '2018-06-23 15:12:23');

/*insert plugin_handle data for sentinel*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('1','10' ,'flowRuleGrade','grade type','3', 2, 8, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('2','10' ,'flowRuleControlBehavior','control behavior','3', 2, 5, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('3','10' ,'flowRuleEnable','whether control behavior is enabled (1 or 0)', '1', 2, 7, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('4','10' ,'flowRuleCount','grade count','1', 2, 6, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('5','10' ,'degradeRuleEnable','whether to open the degrade (1 or 0)', '1', 2, 2, '2020-11-09 01:19:10', '2020-11-09 01:19:10') ;
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('6','10' ,'degradeRuleGrade','degrade type','3', 2, 3, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('7','10' ,'degradeRuleCount','degrade count','1', 2, 1, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('8','10' ,'degradeRuleTimeWindow','degrade window size','1', 2, 4, '2020-11-09 01:19:10', '2020-11-09 01:19:10');

/*insert plugin_handle data for waf*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('9','2' ,'permission','permission','3', 2, 1, '2020-11-22 12:04:10', '2020-11-22 12:04:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('10','2' ,'statusCode','status code','2', 2, 2, '2020-11-22 12:04:10', '2020-11-22 12:04:10');

/*insert plugin_handle data for rate_limiter*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('11', '4' ,'replenishRate','rate', 2, 2, 2, '2020-11-24 00:17:10', '2020-11-24 00:17:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('12', '4' ,'burstCapacity','capacity', 2, 2, 1, '2020-11-24 00:17:10', '2020-11-24 00:17:10');

/*insert plugin_handle data for rewrite*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('13', '3' ,'rewriteURI','rewriteURI', 2, 2, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');

/*insert plugin_handle data for springCloud*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('14', '8' ,'path','path', 2, 2, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('15', '8' ,'timeout','timeout (ms)', 1, 2, 2, '2020-11-29 16:07:10', '2020-11-29 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('16', '8' ,'serviceId','application name', 2, 1, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');

/*insert plugin_handle data for resilience4j*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('17', '12' ,'timeoutDurationRate','control behavior timeout (ms)', 1, 2, 1, '2020-11-28 11:08:14', '2020-11-28 11:19:12');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('18', '12' ,'limitRefreshPeriod','token filling period (ms)', 1, 2, 0, '2020-11-28 11:18:54', '2020-11-28 11:22:40');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('19', '12' ,'limitForPeriod','token filling number', 1, 2, 0, '2020-11-28 11:20:11', '2020-11-28 11:20:11');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('20', '12' ,'circuitEnable','circuit enable', 1, 2, 2, '2020-11-28 11:23:09', '2020-11-28 11:24:12');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('21', '12' ,'timeoutDuration','circuit timeout (ms)', 1, 2, 2, '2020-11-28 11:25:56', '2020-11-28 11:25:56');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('22', '12' ,'fallbackUri','fallback uri', 2, 2, 2, '2020-11-28 11:26:44', '2020-11-28 11:26:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('23', '12' ,'slidingWindowSize','sliding window size', 1, 2, 2, '2020-11-28 11:27:34', '2020-11-28 11:27:34');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('24', '12' ,'slidingWindowType','sliding window type', 1, 2, 2, '2020-11-28 11:28:05', '2020-11-28 11:28:05');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('25', '12' ,'minimumNumberOfCalls','enabled error minimum calculation threshold', 1, 2, 2, '2020-11-28 11:28:34', '2020-11-28 11:28:34');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('26', '12' ,'waitIntervalFunctionInOpenState','degrade opening duration', 1, 2, 2, '2020-11-28 11:29:01', '2020-11-28 11:29:01');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('27', '12' ,'permittedNumberOfCallsInHalfOpenState','half open threshold', 1, 2, 2, '2020-11-28 11:29:55', '2020-11-28 11:29:55');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('28', '12' ,'failureRateThreshold','degrade failure rate', 1, 2, 2, '2020-11-28 11:30:40', '2020-11-28 11:30:40');

/*insert plugin_handle data for plugin*/
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('30', '4', 'mode', 'mode', 3, 3, 1, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('31', '4', 'master', 'master', 2, 3, 2, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('32', '4', 'url', 'url', 2, 3, 3, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('33', '4', 'password', 'password', 2, 3, 4, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('34', '11', 'protocol', 'protocol', 2, 3, 1, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('35', '11', 'register', 'register', 2, 3, 2, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('36', '2', 'model', 'model', 2, 3, 1, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('37', '6', 'register', 'register', 2, 3, 1, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('38', '7', 'metricsName', 'metricsName', 2, 3, 1, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('39', '7', 'host', 'host', 2, 3, 2, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('40', '7', 'port', 'port', 2, 3, 3, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO plugin_handle (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('41', '7', 'async', 'async', 2, 3, 4, NULL, '2020-12-25 00:00:00', '2020-12-25 00:00:00');

/*insert mode data for rate_limiter plugin*/
INSERT IGNORE INTO soul_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('12', 'mode', 'MODE', 'cluster', 'cluster', 'cluster', 0, 1, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO soul_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('13', 'mode', 'MODE', 'sentinel', 'sentinel', 'sentinel', 1, 1, '2020-12-25 00:00:00', '2020-12-25 00:00:00');
INSERT IGNORE INTO soul_dict (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('14', 'mode', 'MODE', 'standalone', 'standalone', 'standalone', 2, 1, '2020-12-25 00:00:00', '2020-12-25 00:00:00');

/*insert plugin_handle data for context path*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('29', '14', 'contextPath', 'contextPath', 2, 2, 0, '2020-12-25 16:13:09', '2020-12-25 16:13:09');
