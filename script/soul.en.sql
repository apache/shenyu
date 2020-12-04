
CREATE DATABASE  IF NOT EXISTS  `soul`  DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci ;

USE `soul`;

/*Table structure of table dashboard_user*/
CREATE TABLE IF NOT EXISTS `dashboard_user` (
  `ID` VARCHAR (128) NOT NULL COMMENT'primary key ID',
  `USER_NAME` VARCHAR(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'user name',
  `Password` VARCHAR (128) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT'User password',
  `role` INT (4) NOT NULL COMMENT'role',
  `Enable` TINYINT (4) NOT NULL COMMENT'Delete or not',
  `DATE_CREATED` Timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'Creation time',
  `Date date_updated` Timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'Update time',
  PRIMARY KEY (`number`)
) Engine = InnoDB default character set = utf8mb4 COLLATE = utf8mb4_unicode_ci;

/*Table structure of table `plugin`*/
CREATE TABLE IF NOT EXISTS `plugin` (
  `ID` VARCHAR (128) NOT NULL COMMENT'primary key ID',
  `Name` VARCHAR (62) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'plugin name',
  `Configuration` text COLLATE utf8mb4_unicode_ci COMMENT'plugin configuration',
  `role` INT (4) NOT NULL COMMENT'plug-in role',
  `Enable` TINYINT (4) NOT NULL DEFAULT '0' COMMENT'Whether to open (0, not open, 1 open)',
  `DATE_CREATED` Timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'Creation time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'update time',
  PRIMARY KEY (`number`)
) Engine = InnoDB default character set = utf8mb4 COLLATE = utf8mb4_unicode_ci;

/*Table structure of table `plugin_handle`*/
CREATE TABLE IF NOT EXISTS `plugin_handle `(
  `ID` VARCHAR(128) NOT NULL,
  `plugin_id` VARCHAR(128) NOT NULL COMMENT'plugin ID',
  `Field` VARCHAR(100) NOT NULL COMMENT'Field',
  `label` VARCHAR(100) DEFAULT NULL COMMENT'label',
  `DATA_TYPE` SMALLINT (6) NOT NULL DEFAULT '1' COMMENT'Data type 1 number 2 string',
  `Type `SMALLINT(6) NULL COMMENT'type, 1 means selector, 2 means rule',
  `sort` INT (4) NULL COMMENT'sort',
  `ext_obj` VARCHAR (1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT'Extra configuration (JSON format data)',
  `DATE_CREATED` Timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'Creation time',
  `Date date_updated` Timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'Update time',
  PRIMARY KEY (`number`)
  UNIQUE KEY `plugin_id_field_type`(`plugin_id`, `field`, `type`)
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = dynamic;


/*Table structure of table `selector`*/
CREATE TABLE IF NOT EXISTS `Select` (
  `ID` VARCHAR (128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'primary key ID VARCHAR' primary key,
  `plugin_id` VARCHAR(128) NOT NULL COMMENT'plugin ID',
  `Name` VARCHAR(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'selector name',
  `match_mode` INT(2) NOT NULL COMMENT'matching mode (0 and 1 or)',
  `Type` INT (4) NOT NULL COMMENT'Type (0, full flow, 1 custom flow)',
  `sort` INT (4) NOT NULL COMMENT'sort',
  `Handle` VARCHAR (1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT'Processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)',
  `Enable` TINYINT (4) NOT NULL COMMENT'Whether to open',
  `loged` TINYINT(4) NOT NULL COMMENT'Whether to print the log',
  `Continue` TINYINT (4) NOT NULL COMMENT'whether to continue execution',
  `DATE_CREATED` Timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'Creation time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'update time',
  Constrain UNIQUE_NAME to be unique (`name`)
) Engine = InnoDB default character set = utf8mb4 COLLATE = utf8mb4_unicode_ci;

/* Table structure of table `selector_condition`*/
CREATE TABLE IF NOT EXISTS `selector_condition `(
  `ID` VARCHAR (128) NOT NULL COMMENT'primary key ID',
  `selector_id` VARCHAR(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'selector ID',
  `param_type` VARCHAR( 64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'parameter type (to query URI, etc.)',
  `Operator` VARCHAR(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'matching character (=> <like matching)',
  `PARAM_NAME` VARCHAR(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'parameter name',
  `param_value` VARCHAR(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'parameter value',
  `DATE_CREATED` Timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'Creation time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'update time',
  PRIMARY KEY (`number`)
) Engine = InnoDB default character set = utf8mb4 COLLATE = utf8mb4_unicode_ci;

/*Table structure of table `rule`*/
CREATE TABLE IF NOT EXISTS `rules` (
  `ID` VARCHAR( 128) NOT NULL COMMENT'Primary key ID' PRIMARY KEY,
  `selector_id` VARCHAR(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'selector ID',
  `match_mode` INT(2) NOT NULL COMMENT'matching mode (0 and 1 or)',
  `Name` VARCHAR(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'Rule name',
  `Enable` TINYINT (4) NOT NULL COMMENT'Whether to open',
  `loged` TINYINT(4) NOT NULL COMMENT'Whether to log or not',
  `sort` INT (4) NOT NULL COMMENT'sort',
  `Handle` VARCHAR (1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT'Processing logic (here for different plug-ins, there will be different fields to identify different processes, all data in JSON format is stored)',
  `DATE_CREATED` Timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'Creation time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'update time',
   Constrain UNIQUE_NAME to be unique (`name`)
) Engine = InnoDB default character set = utf8mb4 COLLATE = utf8mb4_unicode_ci;

/*Table structure of table `rule_condition`*/
CREATE TABLE IF NOT EXISTS `rule_condition` (
  `id` varchar(128) NOT NULL COMMENT'primary key id' PRIMARY KEY,
  `rule_id` varchar(128) NOT NULL COMMENT'rule id',
  `param_type` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'parameter type (post query uri, etc.)',
  `operator` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'matching character (=> <like match)',
  `param_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'parameter name',
  `param_value` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'parameter value',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'created time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'Update time'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure of table `meta_data`*/
CREATE TABLE IF NOT EXISTS `meta_data` (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'application name',
  `path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'Path, cannot be repeated',
  `path_desc` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'path description',
  `rpc_type` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'rpc type',
  `service_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT'service name',
  `method_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT'Method name',
  `parameter_types` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT'Parameter types are provided with multiple parameter types separated by commas',
  `rpc_ext` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT'rpc extended information, json format',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'created time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'update time',
  `enabled` tinyint(4) NOT NULL DEFAULT 0 COMMENT'enabled state',
  PRIMARY KEY (`id`) USING BTREE,
  constraint unique_name unique (`path`)
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

/*Table structure of table `app_auth`*/
CREATE TABLE IF NOT EXISTS `app_auth` (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'primary key id',
  `app_key` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'application identification key',
  `app_secret` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'encryption algorithm secret',
  `user_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT'user id',
  `phone` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT'Phone number when the user applies',
  `ext_info` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT'Extended parameter json',
  `enabled` tinyint(4) NOT NULL COMMENT'Delete or not',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'created time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'update time',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

/*Table structure of table `auth_param`*/
CREATE TABLE IF NOT EXISTS `auth_param` (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'primary key id',
  `auth_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT'Authentication table id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'Business Module',
  `app_param` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT'Service module parameters (parameters that need to be passed by the gateway) json type',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'created time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'update time',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

/*Table structure of table `auth_path`*/
CREATE TABLE IF NOT EXISTS `auth_path` (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'primary key id',
  `auth_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'auth table id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'module',
  `path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'path',
  `enabled` tinyint(4) NOT NULL COMMENT'Whether pass 1 is',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'created time',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'update time',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

/*Table structure of table `soul_dict`*/
CREATE TABLE IF NOT EXISTS `soul_dict` (
    `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'primary key id',
    `type` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'type',
    `dict_code` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'Dictionary encoding',
    `dict_name` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT'Dictionary name',
    `dict_value` varchar(100) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT'Dictionary value',
    `desc` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT'Dictionary description or remarks',
    `sort` int(4) NOT NULL COMMENT'sort',
    `enabled` tinyint(4) DEFAULT NULL COMMENT'whether it is enabled',
    `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT'created time',
    `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT'update time',
    PRIMARY KEY (`id`)
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

 /*soul dict*/
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 1','degradeRuleGrade','DEGRADE_GRADE_RT','Slow call ratio','0','Fuse type-slow call ratio',1,1,'2020-11-18 14:39:56','2020- 11-20 15:43:43');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 2','degradeRuleGrade','DEGRADE_GRADE_EXCEPTION_RATIO','abnormal ratio','1','fuse type-abnormal ratio',0,1,'2020-11-18 16:42:34','2020-11- 20 15:42:58');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 3','degradeRuleGrade','DEGRADE_GRADE_EXCEPTION_COUNT','Abnormal Number Strategy','2','Fuse Type-Abnormal Number Strategy',2,1,'2020-11-19 16:23:45','2020- 11-20 16:01:00');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 4','flowRuleGrade','FLOW_GRADE_QPS','QPS','1','Current limit threshold type-QPS',0,1,'2020-11-20 15:42:03','2020-11- 20 15:42:03');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 5','flowRuleGrade','FLOW_GRADE_THREAD','Number of concurrent threads','0','Current limit threshold type-number of concurrent threads',1,1,'2020-11-20 15:44:44',' 2020-11-20 15:44:44');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 6','flowRuleControlBehavior','CONTROL_BEHAVIOR_DEFAULT','Direct rejection by default','0','Flow control effect-Direct rejection by default',0,1,'2020-11-20 15:46:22','2020 -11-20 15:48:36');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 7','flowRuleControlBehavior','CONTROL_BEHAVIOR_WARM_UP','Warm Up','1','Flow Control Effect-Warm Up',1,1,'2020-11-20 15:47:05','2020-11 -20 15:47:05');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 8','flowRuleControlBehavior','CONTROL_BEHAVIOR_RATE_LIMITER','constant speed queuing','2','flow control effect-uniform speed queuing',2,1,'2020-11-20 15:49:45','2020-11 -20 15:49:45');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 9','flowRuleControlBehavior','CONTROL_BEHAVIOR_WARM_UP_RATE_LIMITER','Preheating uniformly queued','3','Flow control effect-preheating uniformly queued',3,1,'2020-11-20 15:51:25', '2020-11-20 15:51:37');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 10','permission','REJECT','reject','reject','reject',0,1,'2020-11-22 12:04:10','2020-11-22 12 :04:10');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES (' 11','permission','ALLOW','allow','allow','allow',1,1,'2020-11-22 12:04:10','2020-11-22 12 :04:10');

/*plugin*/
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('1','sign','1', '0', ' 2018-06-14 10:17:35', '2018-06-14 10:17:35');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('2','waf', '1',' {"model":"black"}','0', '2018-06-23 10:26:30', '2018-06-13 15:43:10');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('3','rewrite', '1','0', ' 2018-06-23 10:26:34', '2018-06-25 13:59:31');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('4','rate_limiter','1',' {"master":"mymaster","mode":"Standalone","url":"192.168.1.1:6379","password":"abc"}', '0', '2018-06-23 10 :26:37', '2018-06-13 15:34:48');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('5','divide', '0','1', ' 2018-06-25 10:19:10', '2018-06-13 13:56:04');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('6','dubbo','1',' {"register":"zookeeper://localhost:2181"}', '0', '2018-06-23 10:26:41', '2018-06-11 10:11:47');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('7','monitor', '1',' {"metricsName":"prometheus","host":"localhost","port":"9190","async":"true"}','0', '2018-06-25 13:47:57 ', '2018-06-25 13:47:57');
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('8','springCloud','1', '0', ' 2018-06-25 13:47:57', '2018-06-25 13:47:57');
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('9','hystrix', '0','0', ' 2020-01-15 10:19:10', '2020-01-15 10:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('10','sentinel', '1','0', ' 2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `config`, `enabled`, `date_created`, `date_updated`) VALUES ('11','sofa', '0', ' {"protocol":"zookeeper","register":"127.0.0.1:2181"}', '0', '2020-11-09 01:19:10', '2020-11-09 01:19: 10');
INSERT IGNORE INTO `plugin` (`id`, `name`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('12','resilience4j', '1','0', ' 2020-11-09 01:19:10', '2020-11-09 01:19:10');

/**default admin user**/
INSERT IGNORE INTO `dashboard_user` (`id`, `user_name`, `password`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('1','admin','jHcpKkiDbbQh7W7hh8yQSA==' , '1', '1', '2018-06-23 15:12:22', '2018-06-23 15:12:23');

/*insert plugin_handle data for sentinel*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('1','10' ,'flowRuleGrade','Current limit threshold type','3', 2, 8, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('2','10' ,'flowRuleControlBehavior','flow control effect','3', 2, 5, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('3','10' ,'flowRuleEnable','Whether flow control is enabled (1 or 0)', '1', 2, 7, '2020-11-09 01:19:10', '2020-11-09 01:19:10' );
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('4','10' ,'flowRuleCount','Flow limit threshold','1', 2, 6, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('5','10' ,'degradeRuleEnable','Whether to open the fuse (1 or 0)', '1', 2, 2, '2020-11-09 01:19:10', '2020-11-09 01:19:10') ;
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('6','10' ,'degradeRuleGrade','Fuse Type','3', 2, 3, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('7','10' ,'degradeRuleCount','fuse threshold','1', 2, 1, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('8','10' ,'degradeRuleTimeWindow','Fuse window size','1', 2, 4, '2020-11-09 01:19:10', '2020-11-09 01:19:10');

/*insert plugin_handle data for waf*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('9','2' ,'permission','permission','3', 2, 1, '2020-11-22 12:04:10', '2020-11-22 12:04:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('10','2' ,'statusCode','Status Code','2', 2, 2, '2020-11-22 12:04:10', '2020-11-22 12:04:10');

/*insert plugin_handle data for rate_limiter*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('11', '4' ,'replenishRate','Rate', 2, 2, 2, '2020-11-24 00:17:10', '2020-11-24 00:17:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('12', '4' ,'burstCapacity','Capacity', 2, 2, 1, '2020-11-24 00:17:10', '2020-11-24 00:17:10');

/*insert plugin_handle data for rewrite*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('13', '3' ,'rewriteURI','rewriteURI', 2, 2, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');

/*insert plugin_handle data for springCloud*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('14', '8' ,'path','path', 2, 2, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('15', '8' ,'timeout','Timeout (ms)', 1, 2, 2, '2020-11-29 16:07:10', '2020-11-29 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('16', '8' ,'serviceId','application name', 2, 1, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');

/*insert plugin_handle data for resilience4j*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('17', '12' ,'timeoutDurationRate','Flow control timeout (ms)', 1, 2, 1, '2020-11-28 11:08:14', '2020-11-28 11:19:12');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('18', '12' ,'limitRefreshPeriod','token filling period (ms)', 1, 2, 0, '2020-11-28 11:18:54', '2020-11-28 11:22:40');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('19', '12' ,'limitForPeriod','token filling number', 1, 2, 0, '2020-11-28 11:20:11', '2020-11-28 11:20:11');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('20', '12' ,'circuitEnable','Open fuse', 1, 2, 2, '2020-11-28 11:23:09', '2020-11-28 11:24:12');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('21', '12' ,'timeoutDuration','Fuse timeout (ms)', 1, 2, 2, '2020-11-28 11:25:56', '2020-11-28 11:25:56');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('22', '12' ,'fallbackUri','Downgrade uri', 2, 2, 2, '2020-11-28 11:26:44', '2020-11-28 11:26:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('23', '12' ,'slidingWindowSize','sliding window size', 1, 2, 2, '2020-11-28 11:27:34', '2020-11-28 11:27:34');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('24', '12' ,'slidingWindowType','sliding window type', 1, 2, 2, '2020-11-28 11:28:05', '2020-11-28 11:28:05');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('25', '12' ,'minimumNumberOfCalls','Error minimum calculation threshold', 1, 2, 2, '2020-11-28 11:28:34', '2020-11-28 11:28:34');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('26', '12' ,'waitIntervalFunctionInOpenState','Fuse opening duration', 1, 2, 2, '2020-11-28 11:29:01', '2020-11-28 11:29:01');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('27', '12' ,'permittedNumberOfCallsInHalfOpenState','Half open threshold', 1, 2, 2, '2020-11-28 11:29:55', '2020-11-28 11:29:55');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('28', '12' ,'failureRateThreshold','fuse error rate', 1, 2, 2, '2020-11-28 11:30:40', '2020-11-28 11:30:40');
