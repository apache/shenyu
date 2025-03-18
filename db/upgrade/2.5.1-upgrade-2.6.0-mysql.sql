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

-- this file works for MySQL.

ALTER TABLE selector ADD COLUMN match_restful TINYINT(0) NOT NULL default 0
COMMENT 'whether to match restful(0 cache, 1 not cache)';
COMMIT;

ALTER TABLE rule ADD COLUMN match_restful TINYINT(0) NOT NULL default 0
COMMENT 'whether to match restful(0 cache, 1 not cache)';
COMMIT;

/* insert plugin_handle data for plugin CryptorRequest */
INSERT INTO `plugin_handle` VALUES ('1630760188111376384', '24', 'mapType', 'mapType', 3, 2, 3, '{\"required\":\"0\",\"defaultValue\":\"all\",\"rule\":\"\"}', '2023-03-01 10:41:41', '2023-03-01 10:42:21');
/* insert plugin_handle data for plugin cryptorResponse */
INSERT INTO `plugin_handle` VALUES ('1630768384280514560', '25', 'mapType', 'mapType', 3, 2, 4, '{\"required\":\"0\",\"defaultValue\":\"all\",\"rule\":\"\"}', '2023-03-01 11:14:15', '2023-03-01 11:15:14');

/* insert plugin_handle data for plugin_handle mapType */
INSERT INTO `shenyu_dict` VALUES ('1630761573833920512', 'mapType', 'mapType', 'all', 'all', '', 0, 1, '2023-03-01 10:47:11', '2023-03-01 10:47:11');
INSERT INTO `shenyu_dict` VALUES ('1630761984393367552', 'mapType', 'mapType', 'field', 'field', '', 1, 1, '2023-03-01 10:48:49', '2023-03-01 10:48:49');
INSERT INTO `shenyu_dict` VALUES ('1572621976689762308', 'loadBalance', 'LOAD_BALANCE', 'p2c', 'p2c', 'p2c', 4, 1, '2023-03-07 22:12:12', '2023-03-07 22:12:12');
INSERT INTO `shenyu_dict` VALUES ('1572621976689762309', 'loadBalance', 'LOAD_BALANCE', 'shortestResponse', 'shortestResponse', 'shortestResponse', 5, 1, '2023-03-17 10:12:12', '2023-03-17 10:12:12');

/* add column into plugin table */
ALTER TABLE `plugin` ADD COLUMN `plugin_jar` mediumblob NULL COMMENT 'plugin jar';

/*add discovery table for discovery*/
CREATE TABLE `discovery`
(
    `id`           varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `name`         varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the discovery name',
    `level`        varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '0 selector,1 plugin  2 global',
    `plugin_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  COMMENT 'the plugin name',
    `type`         varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'local,zookeeper,etcd,consul,nacos',
    `server_list`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  COMMENT 'register server url (,)',
    `props`     text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci COMMENT 'the discovery pops (json) ',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

/*add discovery table for discovery*/
CREATE TABLE `discovery_handler`
(
    `id`           varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `discovery_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the discovery id',
    `handler`         varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the handler',
    `listener_node` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  COMMENT 'register server listener to node',
    `props`     text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci COMMENT 'the discovery pops (json) ',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

/*add discovery_upstream table for discovery*/
CREATE TABLE `discovery_upstream`
(
    `id`           varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `discovery_handler_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the discovery handler id',
    `protocol`     varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  COMMENT 'for http, https, tcp, ws',
    `url`          varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'ip:port',
    `status`      int(0) NOT NULL COMMENT 'type (0, healthy, 1 unhealthy)',
    `weight`      int(0) NOT NULL COMMENT 'the weight for lists',
    `props`      text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci COMMENT 'the other field (json)',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

/*add proxy_selector table for discovery*/
CREATE TABLE `proxy_selector`
(
    `id`           varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `name`         varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the proxy name',
    `plugin_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the plugin name',
    `type`         varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'proxy type for tcp, upd, ws',
    `forward_port` int(0) NOT NULL COMMENT 'the proxy forward port',
    `props`      text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci COMMENT 'the other field (json)',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

/*add discovery_rel table for discovery*/
CREATE TABLE `discovery_rel`
(
    `id`           varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'primary key id',
    `plugin_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the plugin name',
    `discovery_handler_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the discovery handler id',
    `selector_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  COMMENT 'the selector id ',
    `proxy_selector_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci COMMENT 'the proxy selector id',
    `date_created` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT 'create time',
    `date_updated` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3) COMMENT 'update time',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

INSERT INTO `plugin` VALUES ('42', 'tcp', NULL, 'Proxy', 320, 1, '2023-05-30 18:02:53', '2022-05-30 18:02:53',null);
/* insert plugin data for  loggingHuaweiLtsPlugin */
INSERT INTO `plugin` VALUES ('43', 'loggingHuaweiLts', '{\"totalSizeInBytes\":\"104857600\",\"maxBlockMs\":\"0\",\"ioThreadCount\":\"1\",\"batchSizeThresholdInBytes\":\"524288\",\"batchCountThreshold\":\"4096\",\"lingerMs\":\"2000\",\"retries\":\"100\",\"baseRetryBackoffMs\":\"100\",\"maxRetryBackoffMs\":\"100\",\"enableLocalTest\":\"true\",\"setGiveUpExtraLongSingleLog\":\"false\"}', 'Logging', 177, 0, '2023-07-05 14:03:53.686', '2023-07-06 12:42:07.234', NULL);
/* insert permission data for  loggingHuaweiLtsPlugin */
INSERT INTO `permission` VALUES ('1572525965658820609', '1346358560427216896', '1676471945048780800', '2023-07-07 23:20:04.962', '2023-07-07 23:20:14.170');
INSERT INTO `permission` VALUES ('1572525965658820610', '1346358560427216896', '1676471945124278272', '2023-07-07 23:21:23.648', '2023-07-07 23:21:23.648');
INSERT INTO `permission` VALUES ('1572525965658820611', '1346358560427216896', '1676471945124278273', '2023-07-07 23:23:40.409', '2023-07-07 23:23:40.409');
INSERT INTO `permission` VALUES ('1572525965658820612', '1346358560427216896', '1676471945124278274', '2023-07-07 23:24:03.398', '2023-07-07 23:24:03.398');
INSERT INTO `permission` VALUES ('1572525965658820613', '1346358560427216896', '1676471945124278275', '2023-07-07 23:24:19.165', '2023-07-07 23:24:19.165');
INSERT INTO `permission` VALUES ('1572525965658820614', '1346358560427216896', '1676471945124278276', '2023-07-07 23:24:52.339', '2023-07-07 23:24:52.339');
INSERT INTO `permission` VALUES ('1572525965658820615', '1346358560427216896', '1676471945124278277', '2023-07-07 23:25:30.528', '2023-07-07 23:25:30.528');
INSERT INTO `permission` VALUES ('1572525965658820616', '1346358560427216896', '1676471945124278278', '2023-07-07 23:25:50.772', '2023-07-07 23:25:50.772');
INSERT INTO `permission` VALUES ('1572525965658820617', '1346358560427216896', '1676471945124278279', '2023-07-07 23:26:11.518', '2023-07-07 23:26:11.518');
INSERT INTO `permission` VALUES ('1572525965658820618', '1346358560427216896', '1676471945124278280', '2023-07-07 23:26:37.388', '2023-07-07 23:26:37.388');
/* insert plugin_handle data for  loggingHuaweiLtsPlugin */
INSERT INTO `plugin_handle` VALUES ('1676472478492946432', '43', 'projectId', 'projectId', 2, 3, 0, '{\"required\":\"1\",\"rule\":\"\"}', '2023-07-05 14:06:00.893', '2023-07-07 22:50:00.597');
INSERT INTO `plugin_handle` VALUES ('1676473313352380416', '43', 'logGroupId', 'logGroupId', 2, 3, 1, '{\"required\":\"1\",\"rule\":\"\"}', '2023-07-05 14:09:19.928', '2023-07-07 22:50:00.606');
INSERT INTO `plugin_handle` VALUES ('1676473453001732096', '43', 'logStreamId', 'logStreamId', 2, 3, 2, '{\"required\":\"1\",\"rule\":\"\"}', '2023-07-05 14:09:53.224', '2023-07-07 22:50:00.607');
INSERT INTO `plugin_handle` VALUES ('1676473657121730560', '43', 'accessKeyId', 'AccessKey', 2, 3, 4, '{\"required\":\"1\",\"rule\":\"\"}', '2023-07-05 14:10:41.897', '2023-07-07 22:50:00.608');
INSERT INTO `plugin_handle` VALUES ('1676474055324758016', '43', 'accessKeySecret', 'accessKey', 2, 3, 5, '{\"required\":\"1\",\"rule\":\"\"}', '2023-07-05 14:12:16.828', '2023-07-07 22:50:00.609');
INSERT INTO `plugin_handle` VALUES ('1676474340008947712', '43', 'regionName', 'regionName', 2, 3, 6, '{\"required\":\"1\",\"rule\":\"\"}', '2023-07-05 14:13:24.703', '2023-07-07 22:50:00.610');
INSERT INTO `plugin_handle` VALUES ('1676474810655993856', '43', 'totalSizeInBytes', 'totalSizeInBytes', 1, 3, 8, '{\"required\":\"0\",\"defaultValue\":\"104857600\",\"rule\":\"\"}', '2023-07-05 14:15:16.913', '2023-07-07 22:50:00.611');
INSERT INTO `plugin_handle` VALUES ('1676475051081887744', '43', 'maxBlockMs', 'maxBlockMs', 1, 3, 9, '{\"required\":\"0\",\"defaultValue\":\"0\",\"rule\":\"\"}', '2023-07-05 14:16:14.236', '2023-07-07 22:50:00.612');
INSERT INTO `plugin_handle` VALUES ('1676475293634293760', '43', 'ioThreadCount', 'ioThreadCount', 1, 3, 10, '{\"required\":\"0\",\"defaultValue\":\"1\",\"rule\":\"\"}', '2023-07-05 14:17:12.065', '2023-07-07 22:50:00.612');
INSERT INTO `plugin_handle` VALUES ('1676475611772252160', '43', 'batchSizeThresholdInBytes', 'batchSizeThresholdInBytes', 1, 3, 11, '{\"required\":\"0\",\"defaultValue\":\"524288\",\"rule\":\"\"}', '2023-07-05 14:18:27.915', '2023-07-07 22:50:00.614');
INSERT INTO `plugin_handle` VALUES ('1676475862545494016', '43', 'batchCountThreshold', 'batchCountThreshold', 1, 3, 12, '{\"required\":\"0\",\"defaultValue\":\"4096\",\"rule\":\"\"}', '2023-07-05 14:19:27.704', '2023-07-07 22:50:00.615');
INSERT INTO `plugin_handle` VALUES ('1676476047950508032', '43', 'lingerMs', 'lingerMs', 1, 3, 12, '{\"required\":\"0\",\"defaultValue\":\"2000\",\"rule\":\"\"}', '2023-07-05 14:20:11.908', '2023-07-07 22:50:00.616');
INSERT INTO `plugin_handle` VALUES ('1676476207938039808', '43', 'retries', 'retries', 1, 3, 13, '{\"required\":\"0\",\"defaultValue\":\"100\",\"rule\":\"\"}', '2023-07-05 14:20:50.052', '2023-07-07 22:50:00.617');
INSERT INTO `plugin_handle` VALUES ('1676476515359551488', '43', 'baseRetryBackoffMs', 'baseRetryBackoffMs', 1, 3, 14, '{\"required\":\"0\",\"defaultValue\":\"100\",\"rule\":\"\"}', '2023-07-05 14:22:03.347', '2023-07-07 22:50:00.618');
INSERT INTO `plugin_handle` VALUES ('1676476639779385344', '43', 'maxRetryBackoffMs', 'maxRetryBackoffMs', 1, 3, 15, '{\"required\":\"0\",\"defaultValue\":\"100\",\"rule\":\"\"}', '2023-07-05 14:22:33.010', '2023-07-07 22:50:00.619');
INSERT INTO `plugin_handle` VALUES ('1676477312923234304', '43', 'enableLocalTest', 'enableLocalTest', 2, 3, 15, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2023-07-05 14:25:13.500', '2023-07-07 22:50:00.619');
INSERT INTO `plugin_handle` VALUES ('1676477594361032704', '43', 'setGiveUpExtraLongSingleLog', 'setGiveUpExtraLongSingleLog', 2, 3, 16, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}', '2023-07-05 14:26:20.600', '2023-07-07 22:50:00.620');
INSERT INTO `plugin_handle` VALUES ('1676477594361032705', '43', 'keyword', 'keyword', 2, 2, 0, '{\"required\":\"0\",\"placeholder\":\"please use ‘;’ to split keyword\",\"rule\":\"\"}', '2023-07-05 14:26:20.600', '2023-07-07 22:50:00.620');
INSERT INTO `plugin_handle` VALUES ('1676477594361032706', '43', 'maskType', 'maskType', 3, 2, 1, '{\"required\":\"0\",\"defaultValue\":\"dataMaskByMD5\",\"rule\":\"\"}', '2023-07-05 14:26:20.600', '2023-07-07 22:50:00.620');
INSERT INTO `plugin_handle` VALUES ('1676477594361032707', '43', 'maskStatus', 'maskStatus', 3, 2, 2, '{\"required\":\"0\",\"defaultValue\":\"false\",\"rule\":\"\"}','2023-07-05 14:26:20.600', '2023-07-07 22:50:00.620');
/* insert resource data for loggingHuaweiLtsPlugin */
INSERT INTO `resource` VALUES ('1676471945048780800', '1346775491550474240', 'loggingHuaweiLts', 'loggingHuaweiLts', '/plug/loggingHuaweiLts', 'loggingHuaweiLts', 1, 0, 'block', 0, 0, '', 1, '2023-07-05 14:03:53.699', '2023-07-05 14:03:53.709');
INSERT INTO `resource` VALUES ('1676471945124278272', '1676471945048780800', 'SHENYU.BUTTON.PLUGIN.SELECTOR.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingHuaweiLtsSelector:add', 1, '2023-07-05 14:03:53.721', '2023-07-05 14:03:53.721');
INSERT INTO `resource` VALUES ('1676471945124278273', '1676471945048780800', 'SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingHuaweiLtsSelector:delete', 1, '2023-07-05 14:03:53.721', '2023-07-05 14:03:53.721');
INSERT INTO `resource` VALUES ('1676471945124278274', '1676471945048780800', 'SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingHuaweiLtsSelector:edit', 1, '2023-07-05 14:03:53.721', '2023-07-05 14:03:53.721');
INSERT INTO `resource` VALUES ('1676471945124278275', '1676471945048780800', 'SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingHuaweiLtsSelector:query', 1, '2023-07-05 14:03:53.721', '2023-07-05 14:03:53.721');
INSERT INTO `resource` VALUES ('1676471945124278276', '1676471945048780800', 'SHENYU.BUTTON.PLUGIN.RULE.ADD', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingHuaweiLtsRule:add', 1, '2023-07-05 14:03:53.721', '2023-07-05 14:03:53.721');
INSERT INTO `resource` VALUES ('1676471945124278277', '1676471945048780800', 'SHENYU.BUTTON.PLUGIN.RULE.DELETE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingHuaweiLtsRule:delete', 1, '2023-07-05 14:03:53.721', '2023-07-05 14:03:53.721');
INSERT INTO `resource` VALUES ('1676471945124278278', '1676471945048780800', 'SHENYU.BUTTON.PLUGIN.RULE.EDIT', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingHuaweiLtsRule:edit', 1, '2023-07-05 14:03:53.721', '2023-07-05 14:03:53.721');
INSERT INTO `resource` VALUES ('1676471945124278279', '1676471945048780800', 'SHENYU.BUTTON.PLUGIN.RULE.QUERY', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingHuaweiLtsRule:query', 1, '2023-07-05 14:03:53.721', '2023-07-05 14:03:53.721');
INSERT INTO `resource` VALUES ('1676471945124278280', '1676471945048780800', 'SHENYU.BUTTON.PLUGIN.SYNCHRONIZE', '', '', '', 2, 0, '', 1, 0, 'plugin:loggingHuaweiLts:modify', 1, '2023-07-05 14:03:53.721', '2023-07-05 14:03:53.721');


ALTER TABLE shenyu_dict MODIFY COLUMN dict_value varchar(2048) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'dictionary value';

INSERT INTO `shenyu_dict` VALUES ('1679002911061737472', 'discoveryMode', 'DISCOVERY_MODE', 'zookeeper', '{"baseSleepTimeMilliseconds":"1000","maxRetries":"3","maxSleepTimeMilliseconds":"1000","connectionTimeoutMilliseconds":"1000","sessionTimeoutMilliseconds":"1000","namespace":"","digest":null}', 'discoery mode to link zookeeper', 0, 1,'2023-03-01 10:48:49', '2023-03-01 10:48:49');

INSERT INTO `plugin_handle` VALUES ('1678997037438107647', '42', 'discoveryHandler', 'discoveryHandler', 2, 1, 0, '{"required":"0","defaultValue":"url,protocol,status,weight","rule":""}', '2023-07-10 14:41:27', '2023-08-17 16:58:25.259000000');
INSERT INTO `plugin_handle` VALUES ('1678997037438107648', '42', 'bossGroupThreadCount', 'bossGroupThreadCount', 2, 1, 1, '{\"required\":\"0\",\"defaultValue\":\"1\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1678997142656417792', '42', 'workerGroupThreadCount', 'workerGroupThreadCount', 2, 1, 2, '{\"required\":\"0\",\"defaultValue\":\"12\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1678997399104552960', '42', 'clientMaxIdleTimeMs', 'clientMaxIdleTimeMs', 2, 1, 7, '{\"required\":\"0\",\"defaultValue\":\"30000\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1678997479614218240', '42', 'clientPendingAcquireMaxCount', 'clientPendingAcquireMaxCount', 2, 1, 4, '{\"required\":\"0\",\"defaultValue\":\"5\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1678996921914392576', '42', 'loadBalance', 'loadBalance', 3, 1, 3, '{\"required\":\"0\",\"defaultValue\":\"random\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1678997769998467072', '42', 'clientMaxLifeTimeMs', 'clientMaxLifeTimeMs', 2, 1, 8, '{\"required\":\"0\",\"defaultValue\":\"60000\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1678997277012557824', '42', 'clientMaxConnections', 'clientMaxConnections', 2, 1, 6, '{\"required\":\"0\",\"defaultValue\":\"20\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
INSERT INTO `plugin_handle` VALUES ('1678997557628272640', '42', 'clientPendingAcquireTimeout', 'clientPendingAcquireTimeout', 2, 1, 5, '{\"required\":\"0\",\"defaultValue\":\"5\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');

/* motan rpc */
UPDATE `plugin` set config = '{\"registerProtocol\":\"zk\",\"registerAddress\":\"127.0.0.1:8002\",\"corethreads\":0,\"threads\":2147483647,\"queues\":0,\"threadpool\":\"shared\"}' WHERE id = '17';
UPDATE `plugin_handle` set field = 'registerProtocol', label = 'registerProtocol', sort = 0, ext_obj = '{\"required\":\"0\",\"defaultValue\":\"direct\",\"placeholder\":\"registerProtocol\",\"rule\":\"\"}' WHERE id = '1529402613204172834';
UPDATE `plugin_handle` set sort = 2 where id = '1529402613204172835';
UPDATE `plugin_handle` set sort = 3 where id = '1529402613204172836';
UPDATE `plugin_handle` set sort = 4 where id = '1529402613204172837';
UPDATE `plugin_handle` set sort = 5 where id = '1529402613204172838';

INSERT INTO `plugin_handle` VALUES ('1678997557628272641', '17', 'registerAddress', 'registerAddress', 2, 3, 1, '{\"required\":\"0\",\"defaultValue\":\"127.0.0.1:8002\",\"placeholder\":\"registerAddress\",\"rule\":\"\"}', '2023-01-10 10:08:01.158', '2023-01-10 10:08:01.158');
