/*
SQLyog Ultimate v12.09 (64 bit)
MySQL - 5.5.56-MariaDB : Database - skyway
*********************************************************************
*/

/*!40101 SET NAMES utf8 */;

/*!40101 SET SQL_MODE=''*/;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;
CREATE DATABASE /*!32312 IF NOT EXISTS*/`soul` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci */;

USE `soul`;

CREATE TABLE IF NOT EXISTS `app_auth`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id',
  `app_key` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '应用标识key',
  `app_secret` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '加密算法secret',
  `user_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '用户id',
  `phone` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '用户申请时候的电话号码',
  `ext_info` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '扩展参数 json',
  `enabled` tinyint(4) NOT NULL COMMENT '是否删除',
  `date_created` timestamp(0) NOT NULL DEFAULT CURRENT_TIMESTAMP(0) ON UPDATE CURRENT_TIMESTAMP(0) COMMENT '创建时间',
  `date_updated` timestamp(0) NOT NULL DEFAULT CURRENT_TIMESTAMP(0) COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

CREATE TABLE IF NOT EXISTS `auth_param`  (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id',
  `auth_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '认证表id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '业务模块',
  `app_param` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '业务模块参数（网关需要传递的参数）json类型',
  `date_created` timestamp(0) NOT NULL DEFAULT CURRENT_TIMESTAMP(0) ON UPDATE CURRENT_TIMESTAMP(0) COMMENT '创建时间',
  `date_updated` timestamp(0) NOT NULL DEFAULT CURRENT_TIMESTAMP(0) COMMENT '更新时间',
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
  `date_created` timestamp(0) NOT NULL DEFAULT CURRENT_TIMESTAMP(0) ON UPDATE CURRENT_TIMESTAMP(0) COMMENT '创建时间',
  `date_updated` timestamp(0) NOT NULL DEFAULT CURRENT_TIMESTAMP(0) COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

/*Table structure for table `dashboard_user` */
CREATE TABLE IF NOT EXISTS `dashboard_user` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `user_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '用户名',
  `password` varchar(128) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '用户密码',
  `role` int(4) NOT NULL COMMENT '角色',
  `enabled` tinyint(4) NOT NULL COMMENT '是否删除',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `plugin` */
CREATE TABLE IF NOT EXISTS `plugin` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `name` varchar(62) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '插件名称',
  `config` text COLLATE utf8mb4_unicode_ci COMMENT '插件配置',
  `role` int(4) NOT NULL COMMENT '插件角色',
  `enabled` tinyint(4) NOT NULL DEFAULT '0' COMMENT '是否开启（0，未开启，1开启）',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `rule` */
CREATE TABLE IF NOT EXISTS `rule` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `selector_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '选择器id',
  `match_mode` int(2) NOT NULL COMMENT '匹配方式（0 and  1 or)',
  `name` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '规则名称',
  `enabled` tinyint(4) NOT NULL COMMENT '是否开启',
  `loged` tinyint(4) NOT NULL COMMENT '是否记录日志',
  `sort` int(4) NOT NULL COMMENT '排序',
  `handle` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '处理逻辑（此处针对不同的插件，会有不同的字段来标识不同的处理，所有存储json格式数据）',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `rule_condition` */
CREATE TABLE IF NOT EXISTS `rule_condition` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `rule_id` varchar(128) NOT NULL COMMENT '规则id',
  `param_type` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数类型（post  query  uri等）',
  `operator` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '匹配符（=  > <  like match）',
  `param_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数名称',
  `param_value` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数值',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `selector` */
CREATE TABLE IF NOT EXISTS `selector` (
  `id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键id varchar',
  `plugin_id` varchar(128) NOT NULL COMMENT '插件id',
  `name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '选择器名称',
  `match_mode` int(2) NOT NULL COMMENT '匹配方式（0 and  1 or)',
  `type` int(4) NOT NULL COMMENT '类型（0，全流量，1自定义流量）',
  `sort` int(4) NOT NULL COMMENT '排序',
  `handle` varchar(1024) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '处理逻辑（此处针对不同的插件，会有不同的字段来标识不同的处理，所有存储json格式数据）',
  `enabled` tinyint(4) NOT NULL COMMENT '是否开启',
  `loged` tinyint(4) NOT NULL COMMENT '是否打印日志',
  `continued` tinyint(4) NOT NULL COMMENT '是否继续执行',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

/*Table structure for table `selector_condition` */
CREATE TABLE IF NOT EXISTS `selector_condition` (
  `id` varchar(128) NOT NULL COMMENT '主键id',
  `selector_id` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '选择器id',
  `param_type` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数类型（post  query  uri等）',
  `operator` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '匹配符（=  > <  like match）',
  `param_name` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数名称',
  `param_value` varchar(64) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '参数值',
  `date_created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `date_updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
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
  `date_created` datetime(0) NOT NULL COMMENT '创建时间',
  `date_updated` datetime(0) NOT NULL ON UPDATE CURRENT_TIMESTAMP(0) COMMENT '更新时间',
  `enabled` tinyint(4) NOT NULL DEFAULT 0 COMMENT '启用状态',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

SET FOREIGN_KEY_CHECKS = 1;

/*plugin*/
INSERT IGNORE INTO `soul`.`plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('1', 'sign','0', '0', '2018-06-14 10:17:35', '2018-06-14 10:17:35');
INSERT IGNORE INTO `soul`.`plugin` (`id`, `name`,`role`,`enabled`, `date_created`, `date_updated`) VALUES ('2', 'waf', '0','0', '2018-06-23 10:26:30', '2018-06-13 15:43:10');
INSERT IGNORE INTO `soul`.`plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('3', 'rewrite', '0','0', '2018-06-23 10:26:34', '2018-06-25 13:59:31');
INSERT IGNORE INTO `soul`.`plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('4', 'rate_limiter','0','{"master":"mymaster","mode":"Standalone","url":"192.168.1.1:6379","password":"abc"}', '0', '2018-06-23 10:26:37', '2018-06-13 15:34:48');
INSERT IGNORE INTO `soul`.`plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('5', 'divide', '0','1', '2018-06-25 10:19:10', '2018-06-13 13:56:04');
INSERT IGNORE INTO `soul`.`plugin` (`id`, `name`,`role`,`enabled`, `date_created`, `date_updated`) VALUES ('6', 'dubbo','0', '1', '2018-06-23 10:26:41', '2018-06-11 10:11:47');
INSERT IGNORE INTO `soul`.`plugin` (`id`, `name`,`role`,`config`,`enabled`, `date_created`, `date_updated`) VALUES ('7', 'monitor', '0','{"userName":"xiaoyu","database":"databases","url":"http://localhost:8086","password":"test222"}','0', '2018-06-25 13:47:57', '2018-06-25 13:47:57');
INSERT IGNORE INTO `soul`.`plugin` (`id`, `name`,`role`, `enabled`, `date_created`, `date_updated`) VALUES ('8', 'springCloud','0', '1', '2018-06-25 13:47:57', '2018-06-25 13:47:57');

/**user**/
INSERT IGNORE INTO `soul`.`dashboard_user` (`id`, `user_name`, `password`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('1', 'admin', '123456', '1', '1', '2018-06-23 15:12:22', '2018-06-23 15:12:23');