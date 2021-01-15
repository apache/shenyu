
CREATE DATABASE  IF NOT EXISTS  `soul`  DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci ;

USE `soul`;

/*Table structure for table `dashboard_user` */
CREATE TABLE IF NOT EXISTS `dashboard_user` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `user_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '用户名',
  `password` varchar(128) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '用户密码',
  `role` int(4) NOT NULL COMMENT '角色',
  `enabled` tinyint(4) NOT NULL COMMENT '是否删除',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `plugin` */
CREATE TABLE IF NOT EXISTS `plugin` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `name` varchar(62) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '插件名称',
  `config` text COLLATE utf8mb4_unicode_ci COMMENT '插件配置',
  `role` int(4) NOT NULL COMMENT '插件角色',
  `enabled` tinyint(4) NOT NULL DEFAULT '0' COMMENT '是否开启（0，未开启，1开启）',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `plugin_handle` (
  `id` varchar(128) NOT NULL,
  `plugin_id` varchar(128) NOT NULL COMMENT '插件id',
  `field` varchar(100) NOT NULL COMMENT '字段',
  `label` varchar(100) DEFAULT NULL COMMENT '标签',
  `data_type` smallint(6) NOT NULL DEFAULT '1' COMMENT '数据类型 1 数字 2 字符串',
  `type` smallint(6) NULL COMMENT '类型,1 表示选择器，2 表示规则',
  `sort` int(4)  NULL COMMENT '排序',
  `ext_obj` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '额外配置（json格式数据）',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `plugin_id_field_type` (`plugin_id`,`field`,`type`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;


/*Table structure for table `selector` */
CREATE TABLE IF NOT EXISTS `selector` (
  `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id varchar' primary key,
  `plugin_id` varchar(128) NOT NULL COMMENT '插件id',
  `name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '选择器名称',
  `match_mode` int(2) NOT NULL COMMENT '匹配方式（0 and  1 or)',
  `type` int(4) NOT NULL COMMENT '类型（0，全流量，1自定义流量）',
  `sort` int(4) NOT NULL COMMENT '排序',
  `handle` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '处理逻辑（此处针对不同的插件，会有不同的字段来标识不同的处理，所有存储json格式数据）',
  `enabled` tinyint(4) NOT NULL COMMENT '是否开启',
  `loged` tinyint(4) NOT NULL COMMENT '是否打印日志',
  `continued` tinyint(4) NOT NULL COMMENT '是否继续执行',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  constraint unique_name unique (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `selector_condition` */
CREATE TABLE IF NOT EXISTS `selector_condition` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `selector_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '选择器id',
  `param_type` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数类型（post  query  uri等）',
  `operator` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '匹配符（=  > <  like match）',
  `param_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数名称',
  `param_value` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数值',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `rule` */
CREATE TABLE IF NOT EXISTS `rule` (
  `id` varchar(128) NOT NULL COMMENT '主键id' PRIMARY KEY,
  `selector_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '选择器id',
  `match_mode` int(2) NOT NULL COMMENT '匹配方式（0 and  1 or)',
  `name` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '规则名称',
  `enabled` tinyint(4) NOT NULL COMMENT '是否开启',
  `loged` tinyint(4) NOT NULL COMMENT '是否记录日志',
  `sort` int(4) NOT NULL COMMENT '排序',
  `handle` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '处理逻辑（此处针对不同的插件，会有不同的字段来标识不同的处理，所有存储json格式数据）',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
   constraint unique_name unique (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `rule_condition` (
  `id` varchar(128) NOT NULL COMMENT '主键id' PRIMARY KEY,
  `rule_id` varchar(128) NOT NULL COMMENT '规则id',
  `param_type` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数类型（post  query  uri等）',
  `operator` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '匹配符（=  > <  like match）',
  `param_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数名称',
  `param_value` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数值',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE  IF NOT EXISTS `meta_data` (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '应用名称',
  `path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '路径,不能重复',
  `path_desc` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '路径描述',
  `rpc_type` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'rpc类型',
  `service_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '服务名称',
  `method_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '方法名称',
  `parameter_types` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '参数类型 多给参数类型 逗号隔开',
  `rpc_ext` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'rpc的扩展信息，json格式',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  `enabled` tinyint(4) NOT NULL DEFAULT 0 COMMENT '启用状态',
  PRIMARY KEY (`id`) USING BTREE,
  constraint unique_name unique (`path`)
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

CREATE TABLE IF NOT EXISTS `app_auth`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id',
  `app_key` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '应用标识key',
  `app_secret` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '加密算法secret',
  `user_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '用户id',
  `phone` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '用户申请时候的电话号码',
  `ext_info` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '扩展参数 json',
  `enabled` tinyint(4) NOT NULL COMMENT '是否删除',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

CREATE TABLE IF NOT EXISTS `auth_param`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id',
  `auth_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '认证表id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '业务模块',
  `app_param` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '业务模块参数（网关需要传递的参数）json类型',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for auth_path
-- ----------------------------
CREATE TABLE IF NOT EXISTS `auth_path`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id',
  `auth_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'auth表id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '模块',
  `path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '路径',
  `enabled` tinyint(4) NOT NULL COMMENT '是否通过 1 是 ',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

CREATE TABLE IF NOT EXISTS `soul_dict` (
   `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id',
   `type` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '类型',
   `dict_code` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '字典编码',
   `dict_name` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '字典名称',
   `dict_value` varchar(100) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '字典值',
   `desc` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '字典描述或备注',
   `sort` int(4) NOT NULL COMMENT '排序',
   `enabled` tinyint(4) DEFAULT NULL COMMENT '是否开启',
   `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
   `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
   PRIMARY KEY (`id`)
 ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- ----------------------------
-- Table structure for permission role
-- ----------------------------
CREATE TABLE IF NOT EXISTS `role` (
   `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id',
   `role_name` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '角色名称',
   `description` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '角色描述',
   `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
   `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
   PRIMARY KEY (`id`,`role_name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='角色表';
-- ----------------------------
-- Table structure for user_role
-- ----------------------------
CREATE TABLE IF NOT EXISTS `user_role` (
    `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id',
    `user_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '用户主键',
    `role_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '角色主键',
    `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='用户与角色绑定表';
-- ----------------------------
-- Table structure for permission
-- ----------------------------
CREATE TABLE IF NOT EXISTS `permission` (
   `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id',
   `object_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '用户key 或者 角色 key',
   `resource_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '资源id',
   `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
   `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
   PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='菜单和按钮权限表';
-- ----------------------------
-- Table structure for resource
-- ----------------------------
CREATE TABLE IF NOT EXISTS `resource` (
   `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id',
   `parent_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '资源父id',
   `title` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '资源标题',
   `name` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '页面使用路由名称',
   `url` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '路由使用的url',
   `component` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '页面使用组件名称',
   `resource_type` int(4) NOT NULL COMMENT '资源类型 0:主菜单 1:子菜单 2:功能按钮',
   `sort` int(4) NOT NULL COMMENT '菜单显示排序',
   `icon` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '菜单显示图标',
   `is_leaf` tinyint(1) NOT NULL COMMENT '是否是叶子节点 0 不是 1 是',
   `is_route` int(4) NOT NULL COMMENT '是否是路由(拓展使用) 1:是 0:不是',
   `perms` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '页面子控件权限设置 sys:user:add(新增功能)/sys:user:edit(编辑功能)',
   `status` int(4) NOT NULL COMMENT '资源状态 1 有效 0 无效',
   `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
   `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
   PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='资源表';

 /*soul dict*/
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('1','degradeRuleGrade','DEGRADE_GRADE_RT','慢调用比例','0','熔断类型-慢调用比例',1,1,'2020-11-18 14:39:56','2020-11-20 15:43:43');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('2','degradeRuleGrade','DEGRADE_GRADE_EXCEPTION_RATIO','异常比例','1','熔断类型-异常比例',0,1,'2020-11-18 16:42:34','2020-11-20 15:42:58');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('3','degradeRuleGrade','DEGRADE_GRADE_EXCEPTION_COUNT','异常数策略','2','熔断类型-异常数策略',2,1,'2020-11-19 16:23:45','2020-11-20 16:01:00');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('4','flowRuleGrade','FLOW_GRADE_QPS','QPS','1','限流阈值类型-QPS',0,1,'2020-11-20 15:42:03','2020-11-20 15:42:03');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('5','flowRuleGrade','FLOW_GRADE_THREAD','并发线程数','0','限流阈值类型-并发线程数',1,1,'2020-11-20 15:44:44','2020-11-20 15:44:44');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('6','flowRuleControlBehavior','CONTROL_BEHAVIOR_DEFAULT','默认直接拒绝','0','流控效果-默认直接拒绝',0,1,'2020-11-20 15:46:22','2020-11-20 15:48:36');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('7','flowRuleControlBehavior','CONTROL_BEHAVIOR_WARM_UP','Warm Up','1','流控效果-Warm Up',1,1,'2020-11-20 15:47:05','2020-11-20 15:47:05');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('8','flowRuleControlBehavior','CONTROL_BEHAVIOR_RATE_LIMITER','匀速排队','2','流控效果-匀速排队',2,1,'2020-11-20 15:49:45','2020-11-20 15:49:45');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('9','flowRuleControlBehavior','CONTROL_BEHAVIOR_WARM_UP_RATE_LIMITER','预热均匀排队','3','流控效果-预热均匀排队',3,1,'2020-11-20 15:51:25','2020-11-20 15:51:37');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('10','permission','REJECT','reject(拒绝)','reject','拒绝',0,1,'2020-11-22 12:04:10','2020-11-22 12:04:10');
INSERT IGNORE INTO `soul_dict` (`id`, `type`,`dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('11','permission','ALLOW','allow(允许)','allow','允许',1,1,'2020-11-22 12:04:10','2020-11-22 12:04:10');

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
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('13', 'tars', '1','0', '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO `plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('14', 'context_path', '1','0', '2020-11-09 01:19:10', '2020-11-09 01:19:10');

/**default admin user**/
INSERT IGNORE INTO `dashboard_user` (`id`, `user_name`, `password`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('1', 'admin', 'jHcpKkiDbbQh7W7hh8yQSA==', '1', '1', '2018-06-23 15:12:22', '2018-06-23 15:12:23');

/*insert plugin_handle data for sentinel*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('1','10', 'flowRuleGrade', '限流阈值类型','3', 2, 8, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('2','10', 'flowRuleControlBehavior', '流控效果','3', 2, 5, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('3','10', 'flowRuleEnable', '是否开启流控(1或0)','1', 2, 7, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('4','10', 'flowRuleCount', '限流阈值','1', 2, 6, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('5','10', 'degradeRuleEnable', '是否开启熔断(1或0)','1', 2, 2, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('6','10', 'degradeRuleGrade', '熔断类型','3', 2, 3, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('7','10', 'degradeRuleCount', '熔断阈值','1', 2, 1, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('8','10', 'degradeRuleTimeWindow', '熔断窗口大小','1', 2, 4, '2020-11-09 01:19:10', '2020-11-09 01:19:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('9','10', 'fallbackUri', '熔断URI', 2, 2, 9, '2020-12-04 17:32:27', '2020-12-04 17:33:18');

/*insert plugin_handle data for waf*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('9','2', 'permission', '许可','3', 2, 1, '2020-11-22 12:04:10', '2020-11-22 12:04:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('10','2', 'statusCode', '状态码','2', 2, 2, '2020-11-22 12:04:10', '2020-11-22 12:04:10');

/*insert plugin_handle data for rate_limiter*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('11', '4', 'replenishRate', '速率', 2, 2, 2, '2020-11-24 00:17:10', '2020-11-24 00:17:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('12', '4', 'burstCapacity', '容量', 2, 2, 1, '2020-11-24 00:17:10', '2020-11-24 00:17:10');

/*insert plugin_handle data for rewrite*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('13', '3', 'rewriteURI', 'rewriteURI', 2, 2, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');

/*insert plugin_handle data for springCloud*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('14', '8', 'path', '路径', 2, 2, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('15', '8', 'timeout', '超时时间(ms)', 1, 2, 2, '2020-11-29 16:07:10', '2020-11-29 16:07:10');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('16', '8', 'serviceId', '应用名称', 2, 1, 1, '2020-11-29 16:07:10', '2020-11-29 16:07:10');

/*insert plugin_handle data for resilience4j*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('17', '12', 'timeoutDurationRate', '流控超时(ms)', 1, 2, 1, '2020-11-28 11:08:14', '2020-11-28 11:19:12');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('18', '12', 'limitRefreshPeriod', 'token填充周期(ms)', 1, 2, 0, '2020-11-28 11:18:54', '2020-11-28 11:22:40');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('19', '12', 'limitForPeriod', 'token填充个数', 1, 2, 0, '2020-11-28 11:20:11', '2020-11-28 11:20:11');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('20', '12', 'circuitEnable', '开启熔断', 1, 2, 2, '2020-11-28 11:23:09', '2020-11-28 11:24:12');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('21', '12', 'timeoutDuration', '熔断超时(ms)', 1, 2, 2, '2020-11-28 11:25:56', '2020-11-28 11:25:56');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('22', '12', 'fallbackUri', '降级uri', 2, 2, 2, '2020-11-28 11:26:44', '2020-11-28 11:26:51');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('23', '12', 'slidingWindowSize', '滑动窗口大小', 1, 2, 2, '2020-11-28 11:27:34', '2020-11-28 11:27:34');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('24', '12', 'slidingWindowType', '滑动窗口类型', 1, 2, 2, '2020-11-28 11:28:05', '2020-11-28 11:28:05');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('25', '12', 'minimumNumberOfCalls', '错误最小计算阈值', 1, 2, 2, '2020-11-28 11:28:34', '2020-11-28 11:28:34');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('26', '12', 'waitIntervalFunctionInOpenState', '熔断器开启持续时间', 1, 2, 2, '2020-11-28 11:29:01', '2020-11-28 11:29:01');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('27', '12', 'permittedNumberOfCallsInHalfOpenState', '半开阈值', 1, 2, 2, '2020-11-28 11:29:55', '2020-11-28 11:29:55');
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('28', '12', 'failureRateThreshold', '熔断错误率', 1, 2, 2, '2020-11-28 11:30:40', '2020-11-28 11:30:40');

/*insert plugin_handle data for context path*/
INSERT IGNORE INTO plugin_handle (`id`,`plugin_id`,`field`,`label`,`data_type`,`type`,`sort`,`date_created`,`date_updated`) VALUES ('29', '14', 'contextPath', 'contextPath', 2, 2, 0, '2020-12-25 16:13:09', '2020-12-25 16:13:09');

/** insert permission role for role */
INSERT IGNORE INTO `role` (`id`,`role_name`,`description`,`date_created`,`date_updated`) VALUES ('1346358560427216896', 'super', '超级管理员', '2021-01-05 01:31:10', '2021-01-08 17:00:07');

/** insert resource ror resource */
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1346775491550474240', '', 'SOUL.MENU.PLUGIN.LIST', 'plug', '/plug', 'PluginList', 0, 0, 'dashboard', 0, 0, '', 1, '2021-01-06 05:07:54', '2021-01-07 18:34:11');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1346776175553376256', '', 'SOUL.MENU.SYSTEM.MANAGMENT', 'system', '/system', 'system', 0, 1, 'setting', 0, 0, '', 1, '2021-01-06 05:10:37', '2021-01-07 11:41:02');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1346777157943259136', '1346776175553376256', 'SOUL.MENU.SYSTEM.MANAGMENT.USER', 'manage', '/system/manage', 'manage', 1, 1, '', 0, 0, '', 1, '2021-01-06 05:14:31', '2021-01-15 23:46:34');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1346777449787125760', '1346776175553376256', 'SOUL.MENU.SYSTEM.MANAGMENT.PLUGIN', 'plugin', '/system/plugin', 'plugin', 1, 2, '', 0, 0, '', 1, '2021-01-06 05:15:41', '2021-01-15 23:46:35');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1346777623011880960', '1346776175553376256', 'SOUL.PLUGIN.PLUGINHANDLE', 'pluginhandle', '/system/pluginhandle', 'pluginhandle', 1, 3, '', 0, 0, '', 1, '2021-01-06 05:16:22', '2021-01-15 23:46:36');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1346777766301888512', '1346776175553376256', 'SOUL.MENU.SYSTEM.MANAGMENT.AUTHEN', 'auth', '/system/auth', 'auth', 1, 4, '', 0, 0, '', 1, '2021-01-06 05:16:56', '2021-01-15 23:46:37');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1346777907096285184', '1346776175553376256', 'SOUL.MENU.SYSTEM.MANAGMENT.METADATA', 'metadata', '/system/metadata', 'metadata', 1, 5, '', 0, 0, '', 1, '2021-01-06 05:17:30', '2021-01-15 23:46:39');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1346778036402483200', '1346776175553376256', 'SOUL.MENU.SYSTEM.MANAGMENT.DICTIONARY', 'dict', '/system/dict', 'dict', 1, 6, '', 0, 0, '', 1, '2021-01-06 05:18:00', '2021-01-15 23:46:41');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347026381504262144', '1346775491550474240', 'divide', 'divide', '/plug/divide', 'divide', 1, 0, '', 0, 0, '', 1, '2021-01-06 21:44:51', '2021-01-07 11:44:50');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347026805170909184', '1346775491550474240', 'hystrix', 'hystrix', '/plug/hystrix', 'hystrix', 1, 1, '', 0, 0, '', 1, '2021-01-06 21:46:32', '2021-01-07 11:46:31');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347027413357572096', '1346775491550474240', 'rewrite', 'rewrite', '/plug/rewrite', 'rewrite', 1, 2, '', 0, 0, '', 1, '2021-01-06 21:48:57', '2021-01-07 11:48:56');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347027482244820992', '1346775491550474240', 'springCloud', 'springCloud', '/plug/springCloud', 'springCloud', 1, 3, '', 0, 0, '', 1, '2021-01-06 21:49:13', '2021-01-07 11:49:12');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347027526339538944', '1346775491550474240', 'sign', 'sign', '/plug/sign', 'sign', 1, 5, '', 0, 0, '', 1, '2021-01-06 21:49:23', '2021-01-07 14:12:07');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347027566034432000', '1346775491550474240', 'waf', 'waf', '/plug/waf', 'waf', 1, 6, '', 0, 0, '', 1, '2021-01-06 21:49:33', '2021-01-07 14:12:09');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347027647999520768', '1346775491550474240', 'rate_limter', 'rate_limter', '/plug/rate_limter', 'rate_limter', 1, 7, '', 0, 0, '', 1, '2021-01-06 21:49:53', '2021-01-07 14:12:11');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347027717792739328', '1346775491550474240', 'dubbo', 'dubbo', '/plug/dubbo', 'dubbo', 1, 8, '', 0, 0, '', 1, '2021-01-06 21:50:09', '2021-01-07 14:12:12');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347027769747582976', '1346775491550474240', 'monitor', 'monitor', '/plug/monitor', 'monitor', 1, 9, '', 0, 0, '', 1, '2021-01-06 21:50:22', '2021-01-07 14:12:14');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347027830602739712', '1346775491550474240', 'sentinel', 'sentinel', '/plug/sentinel', 'sentinel', 1, 10, '', 0, 0, '', 1, '2021-01-06 21:50:36', '2021-01-07 14:12:16');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347027918121086976', '1346775491550474240', 'resilience4j', 'resilience4j', '/plug/resilience4j', 'resilience4j', 1, 11, '', 0, 0, '', 1, '2021-01-06 21:50:57', '2021-01-07 14:12:20');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347027995199811584', '1346775491550474240', 'tars', 'tars', '/plug/tars', 'tars', 1, 12, '', 0, 0, '', 1, '2021-01-06 21:51:15', '2021-01-07 14:12:21');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347028169120821248', '1346775491550474240', 'context_path', 'context_path', '/plug/context_path', 'context_path', 1, 13, '', 0, 0, '', 1, '2021-01-06 21:51:57', '2021-01-07 14:12:24');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347032308726902784', '1346777157943259136', '添加用户按钮', '', '', '', 2, 0, '', 1, 0, 'system:manager:add', 1, '2021-01-06 22:08:24', '2021-01-07 18:36:55');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347032395901317120', '1346777157943259136', '查询用户按钮', '', '', '', 2, 1, '', 1, 0, 'system:manager:list', 1, '2021-01-06 22:08:44', '2021-01-07 18:36:56');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347032453707214848', '1346777157943259136', '删除用户按钮', '', '', '', 2, 2, '', 1, 0, 'system:manager:delete', 1, '2021-01-06 22:08:58', '2021-01-07 18:36:57');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347032509051056128', '1346777157943259136', '编辑用户按钮', '', '', '', 2, 3, '', 1, 0, 'system:manager:edit', 1, '2021-01-06 22:09:11', '2021-01-07 18:36:58');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347034027070337024', '1346777449787125760', '查询插件按钮', '', '', '', 2, 0, '', 1, 0, 'system:plugin:list', 1, '2021-01-06 22:15:00', '2021-01-07 18:36:58');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347039054925148160', '1346777449787125760', '删除插件按钮', '', '', '', 2, 1, '', 1, 0, 'system:plugin:delete', 1, '2021-01-06 22:34:38', '2021-01-07 18:36:59');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347041326749691904', '1346777449787125760', '添加插件按钮', '', '', '', 2, 2, '', 1, 0, 'system:plugin:add', 1, '2021-01-06 22:44:14', '2021-01-07 18:37:00');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347046566244003840', '1346777449787125760', '同步插件按钮', '', '', '', 2, 3, '', 1, 0, 'system:plugin:modify', 1, '2021-01-07 13:05:03', '2021-01-07 18:37:01');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347047143350874112', '1346777449787125760', '启动或禁用插件按钮', '', '', '', 2, 4, '', 1, 0, 'system:plugin:disable', 1, '2021-01-07 13:07:21', '2021-01-07 18:37:01');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347047203220369408', '1346777449787125760', '编辑插件按钮', '', '', '', 2, 5, '', 1, 0, 'system:plugin:edit', 1, '2021-01-07 13:07:35', '2021-01-07 18:37:02');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347047555588042752', '1346777623011880960', '查询插件处理按钮', '', '', '', 2, 0, '', 1, 0, 'system:pluginHandler:list', 1, '2021-01-07 13:08:59', '2021-01-07 18:37:03');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347047640145211392', '1346777623011880960', '删除插件处理按钮', '', '', '', 2, 1, '', 1, 0, 'system:pluginHandler:delete', 1, '2021-01-07 13:09:19', '2021-01-07 18:37:03');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347047695002513408', '1346777623011880960', '添加插件处理按钮', '', '', '', 2, 2, '', 1, 0, 'system:pluginHandler:add', 1, '2021-01-07 13:09:32', '2021-01-07 18:37:04');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347047747305484288', '1346777623011880960', '编辑插件处理按钮', '', '', '', 2, 3, '', 1, 0, 'system:pluginHandler:edit', 1, '2021-01-07 13:09:45', '2021-01-07 18:37:05');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347048004105940992', '1346777766301888512', '查询认证按钮', '', '', '', 2, 0, '', 1, 0, 'system:authen:list', 1, '2021-01-07 13:10:46', '2021-01-07 18:37:06');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347048101875167232', '1346777766301888512', '删除认证按钮', '', '', '', 2, 1, '', 1, 0, 'system:authen:delete', 1, '2021-01-07 13:11:09', '2021-01-07 18:37:07');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347048145877610496', '1346777766301888512', '添加认证按钮', '', '', '', 2, 2, '', 1, 0, 'system:authen:add', 1, '2021-01-07 13:11:20', '2021-01-07 18:37:07');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347048240677269504', '1346777766301888512', '启用或禁用认证按钮', '', '', '', 2, 3, '', 1, 0, 'system:authen:disable', 1, '2021-01-07 13:11:42', '2021-01-07 18:37:08');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347048316216684544', '1346777766301888512', '同步认证按钮', '', '', '', 2, 4, '', 1, 0, 'system:authen:modify', 1, '2021-01-07 13:12:00', '2021-01-07 18:37:09');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347048776029843456', '1346777766301888512', '编辑认证按钮', '', '', '', 2, 5, '', 1, 0, 'system:authen:edit', 1, '2021-01-07 13:13:50', '2021-01-07 18:37:13');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347048968414179328', '1346777907096285184', '查询元数据按钮', '', '', '', 2, 0, '', 1, 0, 'system:meta:list', 1, '2021-01-07 13:14:36', '2021-01-07 18:37:15');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347049029323862016', '1346777907096285184', '删除元数据按钮', '', '', '', 2, 1, '', 1, 0, 'system:meta:delete', 1, '2021-01-07 13:14:50', '2021-01-07 18:37:15');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347049092552994816', '1346777907096285184', '添加元数据按钮', '', '', '', 2, 2, '', 1, 0, 'system:meta:add', 1, '2021-01-07 13:15:05', '2021-01-07 18:37:17');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347049251395481600', '1346777907096285184', '启用或禁用元数据按钮', '', '', '', 2, 3, '', 1, 0, 'system:meta:disable', 1, '2021-01-07 13:15:43', '2021-01-07 18:37:18');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347049317178945536', '1346777907096285184', '同步元数据按钮', '', '', '', 2, 4, '', 1, 0, 'system:meta:modify', 1, '2021-01-07 13:15:59', '2021-01-07 18:37:19');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347049370014593024', '1346777907096285184', '编辑元数据按钮', '', '', '', 2, 5, '', 1, 0, 'system:meta:edit', 1, '2021-01-07 13:16:11', '2021-01-07 18:37:20');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347049542417264640', '1346778036402483200', '查询字典按钮', '', '', '', 2, 0, '', 1, 0, 'system:dict:list', 1, '2021-01-07 13:16:53', '2021-01-07 18:37:20');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347049598155370496', '1346778036402483200', '删除字典按钮', '', '', '', 2, 1, '', 1, 0, 'system:dict:delete', 1, '2021-01-07 13:17:06', '2021-01-07 18:37:21');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347049659023110144', '1346778036402483200', '添加字典按钮', '', '', '', 2, 2, '', 1, 0, 'system:dict:add', 1, '2021-01-07 13:17:20', '2021-01-07 18:37:22');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347049731047698432', '1346778036402483200', '启用或禁用字典按钮', '', '', '', 2, 3, '', 1, 0, 'system:dict:disable', 1, '2021-01-07 13:17:38', '2021-01-07 18:37:23');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347049794008395776', '1346778036402483200', '编辑字典按钮', '', '', '', 2, 4, '', 1, 0, 'system:dict:edit', 1, '2021-01-07 13:17:53', '2021-01-07 18:37:23');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347050493052071936', '1347026381504262144', '添加divide选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:divideSelector:add', 1, '2021-01-07 13:20:39', '2021-01-15 22:58:50');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347050998931271680', '1347026381504262144', '删除divide选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:divideSelector:delete', 1, '2021-01-07 13:22:40', '2021-01-07 18:37:25');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347051241320099840', '1347026381504262144', '添加divide规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:divideRule:add', 1, '2021-01-07 13:23:38', '2021-01-15 22:59:23');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347051306788990976', '1347026381504262144', '删除divide规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:divideRule:delete', 1, '2021-01-07 13:23:53', '2021-01-07 18:37:26');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347051641725136896', '1347026381504262144', '同步divide按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:divide:modify', 1, '2021-01-07 13:25:13', '2021-01-07 18:37:27');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347051850521784320', '1347026805170909184', '添加hystrix选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:hyxtrixSelector:add', 1, '2021-01-07 13:26:03', '2021-01-15 23:09:55');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347051853025783808', '1347026805170909184', '删除hystrix选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:hyxtrixSelector:delete', 1, '2021-01-07 13:26:03', '2021-01-15 23:10:00');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347051855538171904', '1347026805170909184', '添加hystrix规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:hyxtrixRule:add', 1, '2021-01-07 13:26:04', '2021-01-15 23:10:37');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347051857962479616', '1347026805170909184', '删除hystrix规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:hyxtrixRule:delete', 1, '2021-01-07 13:26:05', '2021-01-15 23:10:07');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347051860495839232', '1347026805170909184', '同步hystrix按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:hyxtrix:modify', 1, '2021-01-07 13:26:05', '2021-01-15 23:10:16');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347052833968631808', '1347027413357572096', '添加rewrite选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:rewriteSelector:add', 1, '2021-01-07 13:29:57', '2021-01-15 23:21:46');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347052836300664832', '1347027413357572096', '删除rewrite选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:rewriteSelector:delete', 1, '2021-01-07 13:29:58', '2021-01-07 18:37:32');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347052839198928896', '1347027413357572096', '添加rewrite规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:rewriteRule:add', 1, '2021-01-07 13:29:59', '2021-01-15 23:21:53');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347052841824563200', '1347027413357572096', '删除rewrite规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:rewriteRule:delete', 1, '2021-01-07 13:29:59', '2021-01-07 18:37:35');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347052843993018368', '1347027413357572096', '同步rewrite按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:rewrite:modify', 1, '2021-01-07 13:30:00', '2021-01-07 18:37:37');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053324018528256', '1347027482244820992', '添加springCloud选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:springCloudSelector:add', 1, '2021-01-07 13:31:54', '2021-01-15 23:22:44');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053326988095488', '1347027482244820992', '删除springCloud选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:springCloudSelector:delete', 1, '2021-01-07 13:31:55', '2021-01-07 18:37:39');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053329378848768', '1347027482244820992', '添加springCloud规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:springCloudRule:add', 1, '2021-01-07 13:31:55', '2021-01-15 23:22:35');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053331744436224', '1347027482244820992', '删除springCloud规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:springCloudRule:delete', 1, '2021-01-07 13:31:56', '2021-01-07 18:37:40');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053334470733824', '1347027482244820992', '同步springCloud按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:springCloud:modify', 1, '2021-01-07 13:31:57', '2021-01-07 18:37:40');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053363814084608', '1347027526339538944', '添加sign选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:signSelector:add', 1, '2021-01-07 13:32:04', '2021-01-15 23:22:59');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053366552965120', '1347027526339538944', '删除sign选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:signCloudRule:add', 1, '2021-01-07 13:32:04', '2021-01-07 18:37:42');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053369413480448', '1347027526339538944', '添加sign规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:signRule:add', 1, '2021-01-07 13:32:05', '2021-01-15 23:23:02');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053372164943872', '1347027526339538944', '删除sign规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:signRule:delete', 1, '2021-01-07 13:32:06', '2021-01-07 18:37:43');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053375029653504', '1347027526339538944', '同步sign按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:sign:modify', 1, '2021-01-07 13:32:06', '2021-01-07 18:37:44');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053404050042880', '1347027566034432000', '添加waf选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:wafSelector:add', 1, '2021-01-07 13:32:13', '2021-01-15 23:24:38');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053406939918336', '1347027566034432000', '删除waf选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:wafSelector:delete', 1, '2021-01-07 13:32:14', '2021-01-07 18:37:46');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053409842376704', '1347027566034432000', '添加waf规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:wafRule:add', 1, '2021-01-07 13:32:15', '2021-01-15 23:24:42');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053413067796480', '1347027566034432000', '删除waf规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:wafRule:delete', 1, '2021-01-07 13:32:15', '2021-01-07 18:37:47');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053415945089024', '1347027566034432000', '同步waf按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:waf:modify', 1, '2021-01-07 13:32:16', '2021-01-07 18:37:50');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053442419535872', '1347027647999520768', '添加rate_limiter选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:rate_limiterSelector:add', 1, '2021-01-07 13:32:22', '2021-01-15 23:27:19');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053445191970816', '1347027647999520768', '删除rate_limiter选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:rate_limiterSelector:delete', 1, '2021-01-07 13:32:23', '2021-01-07 18:38:23');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053447695970304', '1347027647999520768', '添加rate_limiter规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:rate_limiterRule:add', 1, '2021-01-07 13:32:24', '2021-01-15 23:27:25');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053450304827392', '1347027647999520768', '删除rate_limiter规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:rate_limiterRule:delete', 1, '2021-01-07 13:32:24', '2021-01-07 18:38:22');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053452737523712', '1347027647999520768', '同步rate_limiter按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:rate_limiter:modify', 1, '2021-01-07 13:32:25', '2021-01-07 18:38:22');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053477844627456', '1347027717792739328', '添加dubbo选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:dubboSelector:add', 1, '2021-01-07 13:32:31', '2021-01-15 23:27:45');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053480977772544', '1347027717792739328', '删除dubbo选择器按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:dubboSelector:delete', 1, '2021-01-07 13:32:32', '2021-01-07 18:38:20');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053483712458752', '1347027717792739328', '添加dubbo规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:dubboRule:add', 1, '2021-01-07 13:32:32', '2021-01-15 23:27:49');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053486426173440', '1347027717792739328', '删除dubbo规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:dubboRule:delete', 1, '2021-01-07 13:32:33', '2021-01-07 18:38:18');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053489571901440', '1347027717792739328', '同步dubbo按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:dubbo:modify', 1, '2021-01-07 13:32:34', '2021-01-07 18:38:18');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053516423835648', '1347027769747582976', '添加monitor选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:monitorSelector:add', 1, '2021-01-07 13:32:40', '2021-01-15 23:27:58');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053519401791488', '1347027769747582976', '删除monitor选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:monitorSelector:delete', 1, '2021-01-07 13:32:41', '2021-01-07 18:38:16');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053522182615040', '1347027769747582976', '添加monitor规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:monitorRule:add', 1, '2021-01-07 13:32:41', '2021-01-15 23:28:00');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053525034741760', '1347027769747582976', '删除monitor规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:monitorRule:delete', 1, '2021-01-07 13:32:42', '2021-01-07 18:38:14');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053527819759616', '1347027769747582976', '同步monitor按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:monitor:modify', 1, '2021-01-07 13:32:43', '2021-01-07 18:38:13');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053554310983680', '1347027830602739712', '添加sentinel选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:sentinelSelector:add', 1, '2021-01-07 13:32:49', '2021-01-15 23:28:15');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053556512993280', '1347027830602739712', '删除sentinel选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:sentinelSelector:delete', 1, '2021-01-07 13:32:50', '2021-01-07 18:38:10');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053559050547200', '1347027830602739712', '添加sentinel规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:sentinelRule:add', 1, '2021-01-07 13:32:50', '2021-01-15 23:28:20');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053561579712512', '1347027830602739712', '删除sentinel规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:sentinelRule:delete', 1, '2021-01-07 13:32:51', '2021-01-07 18:38:08');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053564016603136', '1347027830602739712', '同步sentinel按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:sentinel:modify', 1, '2021-01-07 13:32:51', '2021-01-07 18:38:07');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053595729735680', '1347027918121086976', '添加resilience4j选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:resilience4jSelector:add', 1, '2021-01-07 13:32:59', '2021-01-15 23:28:33');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053598829326336', '1347027918121086976', '删除resilience4j选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:resilience4jSelector:delete', 1, '2021-01-07 13:33:00', '2021-01-07 18:38:06');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053601572401152', '1347027918121086976', '添加resilience4j规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:resilience4jRule:add', 1, '2021-01-07 13:33:00', '2021-01-15 23:28:36');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053604093177856', '1347027918121086976', '删除resilience4j规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:resilience4jRule:delete', 1, '2021-01-07 13:33:01', '2021-01-07 18:38:04');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053606622343168', '1347027918121086976', '同步resilience4j按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:resilience4j:modify', 1, '2021-01-07 13:33:02', '2021-01-07 18:38:04');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053631159021568', '1347027995199811584', '添加tars选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:tarsSelector:add', 1, '2021-01-07 13:33:07', '2021-01-15 23:28:48');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053633809821696', '1347027995199811584', '删除tars选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:tarsSelector:delete', 1, '2021-01-07 13:33:08', '2021-01-07 18:38:02');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053636439650304', '1347027995199811584', '添加tars规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:tarsRule:add', 1, '2021-01-07 13:33:09', '2021-01-15 23:28:51');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053638968815616', '1347027995199811584', '删除tars规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:tarsRule:delete', 1, '2021-01-07 13:33:09', '2021-01-07 18:38:01');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053641346985984', '1347027995199811584', '同步tars按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:tars:modify', 1, '2021-01-07 13:33:10', '2021-01-07 18:38:00');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053666227597312', '1347028169120821248', '添加context_path选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:context_pathSelector:add', 1, '2021-01-07 13:33:16', '2021-01-15 23:29:04');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053668538658816', '1347028169120821248', '删除context_path选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:context_pathSelector:delete', 1, '2021-01-07 13:33:16', '2021-01-07 18:37:59');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053670791000064', '1347028169120821248', '添加context_path规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:context_pathRule:add', 1, '2021-01-07 13:33:17', '2021-01-15 23:29:07');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053673043341312', '1347028169120821248', '删除context_path规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:context_pathRule:delete', 1, '2021-01-07 13:33:17', '2021-01-07 18:37:56');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347053675174047744', '1347028169120821248', '同步context_path按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:context_path:modify', 1, '2021-01-07 13:33:18', '2021-01-07 18:37:57');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347063567603609600', '1346775491550474240', 'sofa', 'sofa', '/plug/sofa', 'sofa', 1, 4, '', 0, 0, '', 1, '2021-01-07 14:12:36', '2021-01-15 23:24:04');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347064011369361408', '1347063567603609600', '添加修改sofa选择器按钮', '', '', '', 2, 0, '', 1, 0, 'plugin:sofaSelector:add', 1, '2021-01-07 14:14:22', '2021-01-07 18:37:54');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347064013848195072', '1347063567603609600', '删除sofa选择器按钮', '', '', '', 2, 1, '', 1, 0, 'plugin:sofaSelector:delete', 1, '2021-01-07 14:14:23', '2021-01-07 18:37:53');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347064016373166080', '1347063567603609600', '添加修改sofa规则按钮', '', '', '', 2, 2, '', 1, 0, 'plugin:sofaRule:add', 1, '2021-01-07 14:14:23', '2021-01-07 18:37:53');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347064019007188992', '1347063567603609600', '删除sofa规则按钮', '', '', '', 2, 3, '', 1, 0, 'plugin:sofaRule:delete', 1, '2021-01-07 14:14:24', '2021-01-07 18:37:52');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1347064021486022656', '1347063567603609600', '同步sofa按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:sofa:modify', 1, '2021-01-07 14:14:25', '2021-01-07 18:37:51');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350096617689751552', '1347026381504262144', '编辑divide选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:divideSelector:edit', 1, '2021-01-15 23:04:52', '2021-01-15 23:05:53');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350096630197166080', '1347026381504262144', '编辑divide规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:divideRule:edit', 1, '2021-01-15 23:04:55', '2021-01-15 23:06:05');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350098233939632128', '1347026805170909184', '编辑hystrix选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:hystrixSelector:edit', 1, '2021-01-15 23:11:17', '2021-01-15 23:11:56');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350098236741427200', '1347026805170909184', '编辑hystrix规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:hystrixRule:edit', 1, '2021-01-15 23:11:18', '2021-01-15 23:12:13');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350099831950163968', '1347027413357572096', '编辑rewrite选择器按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:rewriteSelector:edit', 1, '2021-01-15 23:17:38', '2021-01-15 23:22:18');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350099836492595200', '1347027413357572096', '编辑rewrite规则按钮', '', '', '', 2, 4, '', 1, 0, 'plugin:rewriteRule:edit', 1, '2021-01-15 23:17:39', '2021-01-15 23:31:55');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350099893203779584', '1347027482244820992', '编辑springCloud选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:springCloudSelector:edit', 1, '2021-01-15 23:17:53', '2021-01-15 23:30:48');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350099896441782272', '1347027482244820992', '编辑springCloud规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:springCloudRule:edit', 1, '2021-01-15 23:17:54', '2021-01-15 23:32:05');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350099936379944960', '1347027526339538944', '编辑sign选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:signSelector:edit', 1, '2021-01-15 23:18:03', '2021-01-15 23:34:29');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350099939177545728', '1347027526339538944', '编辑sign规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:signRule:edit', 1, '2021-01-15 23:18:04', '2021-01-15 23:34:23');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350099976435548160', '1347027566034432000', '编辑waf选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:wafSelector:edit', 1, '2021-01-15 23:18:13', '2021-01-15 23:34:19');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350099979434475520', '1347027566034432000', '编辑waf规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:wafRule:edit', 1, '2021-01-15 23:18:13', '2021-01-15 23:34:14');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100013341229056', '1347027647999520768', '编辑rate_limiter选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:rate_limiterSelector:edit', 1, '2021-01-15 23:18:21', '2021-01-15 23:34:10');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100016319184896', '1347027647999520768', '编辑rate_limiter规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:rate_limiterRule:edit', 1, '2021-01-15 23:18:22', '2021-01-15 23:34:03');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100053757542400', '1347027717792739328', '编辑dubbo选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:dubboSelector:edit', 1, '2021-01-15 23:18:31', '2021-01-15 23:33:53');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100056525783040', '1347027717792739328', '编辑dubbo规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:dubboRule:edit', 1, '2021-01-15 23:18:32', '2021-01-15 23:33:50');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100110510669824', '1347027769747582976', '编辑monitor选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:monitorSelector:edit', 1, '2021-01-15 23:18:45', '2021-01-15 23:33:44');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100113283104768', '1347027769747582976', '编辑monitor规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:monitorRule:edit', 1, '2021-01-15 23:18:45', '2021-01-15 23:33:39');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100147437322240', '1347027830602739712', '编辑sentinel选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:sentinelSelector:edit', 1, '2021-01-15 23:18:53', '2021-01-15 23:33:29');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100150096510976', '1347027830602739712', '编辑sentinel规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:sentinelRule:edit', 1, '2021-01-15 23:18:54', '2021-01-15 23:33:18');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100190894505984', '1347027918121086976', '编辑resilience4j选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:resilience4jSelector:edit', 1, '2021-01-15 23:19:04', '2021-01-15 23:33:13');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100193801158656', '1347027918121086976', '编辑resilience4j规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:resilience4jRule:edit', 1, '2021-01-15 23:19:05', '2021-01-15 23:33:05');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100229360467968', '1347027995199811584', '编辑tars选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:tarsSelector:edit', 1, '2021-01-15 23:19:13', '2021-01-15 23:31:25');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100232451670016', '1347027995199811584', '编辑tars规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:tarsRule:edit', 1, '2021-01-15 23:19:14', '2021-01-15 23:32:48');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100269307019264', '1347028169120821248', '编辑context_path选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:context_pathSelector:edit', 1, '2021-01-15 23:19:23', '2021-01-15 23:30:13');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100272083648512', '1347028169120821248', '编辑context_path规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:context_pathRule:edit', 1, '2021-01-15 23:19:23', '2021-01-15 23:32:34');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100334205485056', '1347063567603609600', '编辑sofa选择器按钮', '', '', '', 2, 5, '', 1, 0, 'plugin:sofaSelector:edit', 1, '2021-01-15 23:19:38', '2021-01-15 23:30:15');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350100337363795968', '1347063567603609600', '编辑sofa规则按钮', '', '', '', 2, 6, '', 1, 0, 'plugin:sofaRule:edit', 1, '2021-01-15 23:19:39', '2021-01-15 23:32:42');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350106119681622016', '1346776175553376256', 'SOUL.MENU.SYSTEM.MANAGMENT.ROLE', 'role', '/system/role', 'role', 1, 0, '', 1, 0, '', 1, '2021-01-15 23:42:37', '2021-01-15 23:46:31');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350107709494804480', '1350106119681622016', '添加角色按钮', '', '', '', 2, 0, '', 1, 0, 'system:role:add', 1, '2021-01-15 23:48:56', '2021-01-15 23:48:56');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350107842236137472', '1350106119681622016', '查询角色按钮', '', '', '', 2, 1, '', 1, 0, 'system:role:list', 1, '2021-01-15 23:49:28', '2021-01-15 23:49:28');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350112406754766848', '1350106119681622016', '删除角色按钮', '', '', '', 2, 2, '', 1, 0, 'system:role:delete', 1, '2021-01-16 00:07:36', '2021-01-16 00:08:19');
INSERT IGNORE INTO `resource` (`id`,`parent_id`,`title`,`name`,`url`,`component`,`resource_type`,`sort`,`icon`,`is_leaf`,`is_route`,`perms`,`status`,`date_created`,`date_updated`) VALUES ('1350112481253994496', '1350106119681622016', '编辑角色按钮', '', '', '', 2, 3, '', 1, 0, 'system:role:edit', 1, '2021-01-16 00:07:54', '2021-01-16 00:08:20');
