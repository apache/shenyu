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

ALTER TABLE selector ADD COLUMN match_restful TINYINT(0) NOT NULL
COMMENT 'whether to match restful(0 cache, 1 not cache)';
COMMIT;

ALTER TABLE rule ADD COLUMN match_restful TINYINT(0) NOT NULL
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