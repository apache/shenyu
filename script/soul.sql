
CREATE DATABASE  IF NOT EXISTS  `soul`  DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci ;

USE `soul`;

/*Table structure for table `dashboard_user` */
CREATE TABLE IF NOT EXISTS `dashboard_user` (
  `id` varchar(128) NOT NULL COMMENT 'primary key id',
  `user_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'user name',
  `password` varchar(128) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'user password',
  `role` int(4) NOT NULL COMMENT 'role',
  `enabled` tinyint(4) NOT NULL COMMENT 'whether this user is enabled',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `plugin` */
CREATE TABLE IF NOT EXISTS `plugin` (
  `id` varchar(128) NOT NULL COMMENT 'primary key id',
  `name` varchar(62) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'plugin name',
  `config` text COLLATE utf8mb4_unicode_ci COMMENT 'plugin config',
  `role` int(4) NOT NULL COMMENT 'plugin role',
  `enabled` tinyint(4) NOT NULL DEFAULT '0' COMMENT 'whether this plugin is enabled(0 disabled, 1 enabled)',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `plugin_handle` (
  `id` varchar(128) NOT NULL,
  `plugin_id` varchar(128) NOT NULL COMMENT 'plugin id',
  `field` varchar(100) NOT NULL COMMENT 'plugin field',
  `label` varchar(100) DEFAULT NULL COMMENT 'plugin label',
  `data_type` smallint(6) NOT NULL DEFAULT '1' COMMENT 'data type: 1 numeric, 2 string',
  `type` smallint(6) NULL COMMENT 'plugin type:1 selector, 2 rule',
  `sort` int(4)  NULL COMMENT 'sort',
  `ext_obj` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'extra config(json format)',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`),
  UNIQUE KEY `plugin_id_field_type` (`plugin_id`,`field`,`type`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;


/*Table structure for table `selector` */
CREATE TABLE IF NOT EXISTS `selector` (
  `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id(varchar type)' primary key,
  `plugin_id` varchar(128) NOT NULL COMMENT 'plugin id',
  `name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'selector name',
  `match_mode` int(2) NOT NULL COMMENT 'match mode(0 and mode, 1 or mode)',
  `type` int(4) NOT NULL COMMENT 'selector type(0 all flow, 1 customized)',
  `sort` int(4) NOT NULL COMMENT 'sort',
  `handle` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'business logic(for different plugins, this field will hold different json data to represent different business logic.)',
  `enabled` tinyint(4) NOT NULL COMMENT 'whether this selector is enabled',
  `loged` tinyint(4) NOT NULL COMMENT 'print log or not',
  `continued` tinyint(4) NOT NULL COMMENT 'whether continue the execution',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  constraint unique_name unique (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `selector_condition` */
CREATE TABLE IF NOT EXISTS `selector_condition` (
  `id` varchar(128) NOT NULL COMMENT 'primary key id',
  `selector_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'selector id',
  `param_type` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'parameter type(post, query, uri, ...)',
  `operator` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'operator(=  > <  like match)',
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
  `match_mode` int(2) NOT NULL COMMENT 'match mode(0 and, 1 or)',
  `name` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'rule name',
  `enabled` tinyint(4) NOT NULL COMMENT 'whether this rule is enabled',
  `loged` tinyint(4) NOT NULL COMMENT 'print log or not',
  `sort` int(4) NOT NULL COMMENT 'sort',
  `handle` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'business logic(for different plugins, this field will hold different json data to represent different business logic.)',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
   constraint unique_name unique (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `rule_condition` (
  `id` varchar(128) NOT NULL COMMENT 'primary key id' PRIMARY KEY,
  `rule_id` varchar(128) NOT NULL COMMENT 'rule id',
  `param_type` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'parameter type(post, query, uri, ...)',
  `operator` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'operator(=  > <  like match)',
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
  `parameter_types` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'parameter types, separated by commas when multiple parameters',
  `rpc_ext` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'rpc extra information, json format',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  `enabled` tinyint(4) NOT NULL DEFAULT 0 COMMENT 'whether this meta data is enabled',
  PRIMARY KEY (`id`) USING BTREE,
  constraint unique_name unique (`path`)
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

CREATE TABLE IF NOT EXISTS `app_auth`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
  `app_key` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'application key',
  `app_secret` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'application encryption secret',
  `user_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'user id',
  `phone` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'phone number filled when user apply',
  `ext_info` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'extend information(json format)',
  `enabled` tinyint(4) NOT NULL COMMENT 'whether this auth information is enabled',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

CREATE TABLE IF NOT EXISTS `auth_param`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
  `auth_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'auth id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'application name',
  `app_param` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'application parameters which the gateway need to forward, json format',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for auth_path
-- ----------------------------
CREATE TABLE IF NOT EXISTS `auth_path`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
  `auth_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'auth id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'application name',
  `path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'path',
  `enabled` tinyint(4) NOT NULL COMMENT 'whether this auth path is enabled, 1 enabled',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

CREATE TABLE IF NOT EXISTS `soul_dict` (
   `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
   `type` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'dictionary type',
   `dict_code` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'dictionary code',
   `dict_name` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'dictionary name',
   `dict_value` varchar(100) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'dictionary value',
   `desc` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'dictionary description, remark',
   `sort` int(4) NOT NULL COMMENT 'sort',
   `enabled` tinyint(4) DEFAULT NULL COMMENT 'whether is enabled',
   `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'create time',
   `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'update time',
   PRIMARY KEY (`id`)
 ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

 /*soul dict*/
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('1','degradeRuleGrade','DEGRADE_GRADE_RT','Slow Call Proportion','0','Circuit Type-Slow Call Proportion',1,1,'2020-11-18 14:39:56','2020-11-20 15:43:43');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('2','degradeRuleGrade','DEGRADE_GRADE_EXCEPTION_RATIO','Exception Proportion','1','Circuit Type-Exception Proportion',0,1,'2020-11-18 16:42:34','2020-11-20 15:42:58');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('3','degradeRuleGrade','DEGRADE_GRADE_EXCEPTION_COUNT','Exception Number Strategy','2','Circuit Type-Exception Number Strategy',2,1,'2020-11-19 16:23:45','2020-11-20 16:01:00');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('4','flowRuleGrade','FLOW_GRADE_QPS','QPS','1','Current Limiting Threshold Type-QPS',0,1,'2020-11-20 15:42:03','2020-11-20 15:42:03');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('5','flowRuleGrade','FLOW_GRADE_THREAD','Concurrent Thread Number','0','Current Limiting Threshold type-Concurrent Thread Number',1,1,'2020-11-20 15:44:44','2020-11-20 15:44:44');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('6','flowRuleControlBehavior','CONTROL_BEHAVIOR_DEFAULT','Default Directly Reject','0','Flow Control Effect-Default Directly Reject',0,1,'2020-11-20 15:46:22','2020-11-20 15:48:36');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('7','flowRuleControlBehavior','CONTROL_BEHAVIOR_WARM_UP','Warm Up','1','Flow Control Effect-Warm Up',1,1,'2020-11-20 15:47:05','2020-11-20 15:47:05');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('8','flowRuleControlBehavior','CONTROL_BEHAVIOR_RATE_LIMITER','Queue At A Constant Speed','2','Flow Control Effect-Queue At A Constant Speed',2,1,'2020-11-20 15:49:45','2020-11-20 15:49:45');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('9','flowRuleControlBehavior','CONTROL_BEHAVIOR_WARM_UP_RATE_LIMITER','Warm Up And Queue At A Constant Speed','3','Flow Control Effect-Warm Up And Queue At A Constant Speed',3,1,'2020-11-20 15:51:25','2020-11-20 15:51:37');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('10','permission','REJECT','reject','reject','Reject',0,1,'2020-11-22 12:04:10','2020-11-22 12:04:10');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('11','permission','ALLOW','allow','allow','Allow',1,1,'2020-11-22 12:04:10','2020-11-22 12:04:10');

/*plugin*/
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('1', 'sign','1', '0', '2018-06-14 10:17:35', '2018-06-14 10:17:35');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('2', 'waf', '1','{"model":"black"}','0', '2018-06-23 10:26:30', '2018-06-13 15:43:10');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('3', 'rewrite', '1','0', '2018-06-23 10:26:34', '2018-06-25 13:59:31');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('4', 'rate_limiter','1','{"master":"mymaster","mode":"Standalone","url":"192.168.1.1:6379","password":"abc"}', '0', '2018-06-23 10:26:37', '2018-06-13 15:34:48');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('5', 'divide', '0','1', '2018-06-25 10:19:10', '2018-06-13 13:56:04');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('6', 'dubbo','1','{"register":"zookeeper://localhost:2181"}', '0', '2018-06-23 10:26:41', '2018-06-11 10:11:47');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('7', 'monitor', '1','{"metricsName":"prometheus","host":"localhost","port":"9190","async":"true"}','0', '2018-06-25 13:47:57', '2018-06-25 13:47:57');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('8', 'springCloud','1', '0', '2018-06-25 13:47:57', '2018-06-25 13:47:57');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('9', 'hystrix', '0','0', '2020-01-15 10:19:10', '2020-01-15 10:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('10', 'sentinel', '1','0', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `config`, `enabled`, `date_created`, `date_updated`) VALUES ('11', 'sofa', '0', '{"protocol":"zookeeper","register":"127.0.0.1:2181"}', '0', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('12', 'resilience4j', '1','0', '2020-11-09 01:19:10', '2020-11-09 01:19:10');

/**default admin user**/
INSERT IGNORE INTO `dashboard_user` (`id`, `user_name`, `password`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('1', 'admin', 'jHcpKkiDbbQh7W7hh8yQSA==', '1', '1', '2018-06-23 15:12:22', '2018-06-23 15:12:23');

/*insert plugin_handle data for sentinel*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('1','10', 'flowRuleGrade', 'current limiting threshold type','3', 2, 8, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('2','10', 'flowRuleControlBehavior', 'flow control effect','3', 2, 5, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('3','10', 'flowRuleEnable', 'enable flow control or not(1 or 0)','1', 2, 7, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('4','10', 'flowRuleCount', 'current limiting threshold','1', 2, 6, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('5','10', 'degradeRuleEnable', 'enable circuit or not(1 or 0)','1', 2, 2, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('6','10', 'degradeRuleGrade', 'circuit type','3', 2, 3, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('7','10', 'degradeRuleCount', 'circuit threshold','1', 2, 1, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('8','10', 'degradeRuleTimeWindow', 'the size of circuit window','1', 2, 4, '2020-11-09 01:19:10', '2020-11-09 01:19:10');

/*insert plugin_handle data for waf*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('9','2', 'permission', 'permission','3', 2, 1, '2020-11-22 12:04:10', '2020-11-22 12:04:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('10','2', 'statusCode', 'status code','2', 2, 2, '2020-11-22 12:04:10', '2020-11-22 12:04:10');

/*insert plugin_handle data for rate_limiter*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('11', '4', 'replenishRate', 'replenish rate', 2, 2, 2, '2020-11-24 00:17:10', '2020-11-24 00:17:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('12', '4', 'burstCapacity', 'burst capacity', 2, 2, 1, '2020-11-24 00:17:10', '2020-11-24 00:17:10');

/*insert plugin_handle data for rewrite*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('13', '3', 'rewriteURI', 'rewriteURI', 2, 2, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');

/*insert plugin_handle data for springCloud*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('14', '8', 'path', 'path', 2, 2, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('15', '8', 'timeout', 'timeout(ms)', 1, 2, 2, '2020-11-29 16:07:10', '2020-11-29 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('16', '8', 'serviceId', 'service id', 2, 1, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');

/*insert plugin_handle data for resilience4j*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('17', '12', 'timeoutDurationRate', 'flow control timeout(ms)', 1, 2, 1, '2020-11-28 11:08:14', '2020-11-28 11:19:12');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('18', '12', 'limitRefreshPeriod', 'token refresh period(ms)', 1, 2, 0, '2020-11-28 11:18:54', '2020-11-28 11:22:40');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('19', '12', 'limitForPeriod', 'token number for every period', 1, 2, 0, '2020-11-28 11:20:11', '2020-11-28 11:20:11');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('20', '12', 'circuitEnable', 'enable circuit', 1, 2, 2, '2020-11-28 11:23:09', '2020-11-28 11:24:12');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('21', '12', 'timeoutDuration', 'circuit timeout(ms)', 1, 2, 2, '2020-11-28 11:25:56', '2020-11-28 11:25:56');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('22', '12', 'fallbackUri', 'degraded uri', 2, 2, 2, '2020-11-28 11:26:44', '2020-11-28 11:26:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('23', '12', 'slidingWindowSize', 'sliding window size', 1, 2, 2, '2020-11-28 11:27:34', '2020-11-28 11:27:34');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('24', '12', 'slidingWindowType', 'sliding window type', 1, 2, 2, '2020-11-28 11:28:05', '2020-11-28 11:28:05');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('25', '12', 'minimumNumberOfCalls', 'minimum number of wrong call', 1, 2, 2, '2020-11-28 11:28:34', '2020-11-28 11:28:34');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('26', '12', 'waitIntervalFunctionInOpenState', 'how long it will last that the circuit is in open state', 1, 2, 2, '2020-11-28 11:29:01', '2020-11-28 11:29:01');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('27', '12', 'permittedNumberOfCallsInHalfOpenState', 'threshold which the circuit is in half open state', 1, 2, 2, '2020-11-28 11:29:55', '2020-11-28 11:29:55');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('28', '12', 'failureRateThreshold', 'circuit failure rate threshold', 1, 2, 2, '2020-11-28 11:30:40', '2020-11-28 11:30:40');
