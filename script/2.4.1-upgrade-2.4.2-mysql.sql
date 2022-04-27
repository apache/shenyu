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

-- upgrade plguin
-- name & role
UPDATE plugin SET role = 'Authentication' WHERE name = 'sign';
UPDATE plugin SET role = 'FaultTolerance' WHERE name = 'sentinel';
UPDATE plugin SET role = 'Proxy' WHERE name = 'sofa';
UPDATE plugin SET role = 'FaultTolerance' WHERE name = 'resilience4j';
UPDATE plugin SET role = 'Proxy' WHERE name = 'tars';
UPDATE plugin SET role = 'HttpProcess', name = 'contextPath' WHERE name = 'context_path';
UPDATE plugin SET role = 'Proxy' WHERE name = 'grpc';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'redirect';
UPDATE plugin SET role = 'Proxy' WHERE name = 'motan';
UPDATE plugin SET role = 'Logging' WHERE name = 'logging';
UPDATE plugin SET role = 'Authentication' WHERE name = 'jwt';
UPDATE plugin SET role = 'Authentication' WHERE name = 'waf';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'request';
UPDATE plugin SET role = 'Authentication' WHERE name = 'oauth2';
UPDATE plugin SET role = 'HttpProcess', name = 'paramMapping' WHERE name = 'param_mapping';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'modifyResponse';
UPDATE plugin SET role = 'Cryptor', name = 'cryptorRequest' WHERE name = 'cryptor_request';
UPDATE plugin SET role = 'Cryptor', name = 'cryptorResponse' WHERE name = 'cryptor_response';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'websocket';
UPDATE plugin SET role = 'HttpProcess' WHERE name = 'rewrite';
UPDATE plugin SET role = 'FaultTolerance', name = 'rateLimiter' WHERE name = 'rate_limiter';
UPDATE plugin SET role = 'Proxy' WHERE name = 'divide';
UPDATE plugin SET role = 'Proxy' WHERE name = 'dubbo';
UPDATE plugin SET role = 'Monitor' WHERE name = 'monitor';
UPDATE plugin SET role = 'Proxy' WHERE name = 'springCloud';
UPDATE plugin SET role = 'FaultTolerance' WHERE name = 'hystrix';

-- upgrade resource
-- title & name & component &url
UPDATE resource SET title = 'rateLimiter', name = 'rateLimiter', component = 'rateLimiter', url = REPLACE(url, 'rate_limiter', 'rateLimiter') WHERE title = 'rate_limiter';
UPDATE resource SET title = 'contextPath', name = 'contextPath', component = 'contextPath', url = REPLACE(url, 'context_path', 'contextPath') WHERE title = 'context_path';
UPDATE resource SET title = 'cryptorRequest', name = 'cryptorRequest', component = 'cryptorRequest', url = REPLACE(url, 'cryptor_request', 'cryptorRequest') WHERE title = 'cryptor_request';
UPDATE resource SET title = 'cryptorResponse', name = 'cryptorResponse', component = 'cryptorResponse', url = REPLACE(url, 'cryptor_response', 'cryptorResponse') WHERE title = 'cryptor_response';
UPDATE resource SET title = 'modifyResponse', name = 'modifyResponse', component = 'modifyResponse', url = REPLACE(url, 'modifyResponse', 'modifyResponse') WHERE title = 'modify_response';
UPDATE resource SET title = 'paramMapping', name = 'paramMapping', component = 'paramMapping', url = REPLACE(url, 'param_mapping', 'paramMapping') WHERE title = 'param_mapping';

-- perms
UPDATE resource SET perms = REPLACE(perms, 'rate_limiter', 'rateLimiter') WHERE perms LIKE 'plugin:rate_limiter%';
UPDATE resource SET perms = REPLACE(perms, 'context_path', 'contextPath') WHERE perms LIKE 'plugin:context_path%';
UPDATE resource SET perms = REPLACE(perms, 'cryptor_r', 'cryptorR') WHERE perms LIKE 'plugin:cryptor_r%';
UPDATE resource SET perms = REPLACE(perms, 'modifyResponse', 'modifyResponse') WHERE perms LIKE 'plugin:modify_response%';
UPDATE resource SET perms = REPLACE(perms, 'param_mapping', 'paramMapping') WHERE perms LIKE 'plugin:param_mapping%';

-- add dubbo multiSelectorHandle
INSERT IGNORE INTO plugin_handle (id, plugin_id, field, label, data_type, type, sort, ext_obj, date_created, date_updated) VALUES ('1509837592619499556', '6', 'multiSelectorHandle', 'multiSelectorHandle', '3', '3', '0', NULL, '2021-03-08 13:18:44', '2021-03-09 10:32:51');
-- update the dubbo loadbalance data_type to 3
UPDATE plugin_handle SET data_type = '3' WHERE plugin_id = '6' AND field = 'loadbalance';

-- change shenyu-dict unique key
ALTER TABLE shenyu_dict ADD UNIQUE KEY `dict_type_dict_code_dict_name` (`type`, `dict_code`,`dict_name`);