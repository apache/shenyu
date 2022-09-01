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

/* insert plugin for loggingPulsar */
INSERT INTO `plugin` VALUES ('35', 'loggingPulsar', '{\"topic":\"shenyu-access-logging\", \"serviceUrl\": \"pulsar://localhost:6650\"}', 'Logging', 185, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin for loggingClickHouse */
INSERT INTO `plugin` VALUES ('38', 'loggingClickHouse', '{\"host\":\"127.0.0.1\",\"port\":\"8123\",\"databse\":\"shenyu-gateway\",\"username\":\"foo\",\"password\":\"bar\"}', 'Logging', 195, 0, '2022-06-30 21:00:00', '2022-06-30 21:00:00');


/* insert plugin_handle data for plugin loggingPulsar */
INSERT INTO `plugin_handle` VALUES ('1529402613204172906', '35', 'topic', 'topic', 2, 3, 1, '{\"required\":\"1\",\"defaultValue\":\"shenyu-access-logging\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172907', '35', 'serviceUrl', 'serviceUrl', 2, 3, 2, '{\"required":"1",\"defaultValue\":\"pulsar://localhost:6650\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172908', '35', 'sampleRate', 'sampleRate', 2, 3, 4, '{\"required":"0",\"defaultValue\":\"1\",\"placeholder\":\"optional,0,0.01~1\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172909', '35', 'maxResponseBody', 'maxResponseBody', 1, 3, 5, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172910', '35', 'maxRequestBody', 'maxRequestBody', 1, 3, 6, '{\"required\":\"0\",\"defaultValue\":524288}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172911', '35', 'compressAlg', 'compressAlg', 3, 3, 7, '{\"required\":\"0\",\"defaultValue\":\"none\"}', '2022-06-30 21:00:00', '2022-06-30 21:00:00');
/* insert plugin_handle data for plugin loggingClickHouse */
INSERT INTO `plugin_handle` VALUES ('1529402613204172798', '38', 'host', 'host', 2, 3, 1, '{"required":"0","defaultValue":"127.0.0.1"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1529402613204172799', '38', 'port', 'port', 2, 3, 2, '{"required":"0","defaultValue":"8123"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1556899301440503808', '38', 'database', 'database', 2, 2, 0, '{"required":"0","defaultValue":"shenyu-gateway","placeholder":"database"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1556899400849702912', '38', 'username', 'username', 2, 2, 0, '{"required":"0","defaultValue":"foo","placeholder":"username"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
INSERT INTO `plugin_handle` VALUES ('1556899492809818112', '38', 'password', 'password', 2, 2, 0, '{"required":"0","defaultValue":"bar","placeholder":"password"}', '2022-06-19 22:00:00', '2022-06-19 22:00:00');
