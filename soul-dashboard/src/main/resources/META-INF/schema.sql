CREATE DATABASE  IF NOT EXISTS `soul` ;

USE `soul`;

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


/**user**/
INSERT IGNORE INTO `soul`.`dashboard_user` (`id`, `user_name`, `password`, `role`, `enabled`, `date_created`, `date_updated`) VALUES ('1', 'admin', '123456', '1', '1', '2018-06-23 15:12:22', '2018-06-23 15:12:23');