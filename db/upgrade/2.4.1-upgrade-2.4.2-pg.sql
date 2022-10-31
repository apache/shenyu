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

-- this file works for PostgreSQL, can not use "`" syntax.

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
INSERT INTO plugin_handle (id, plugin_id, field, label, data_type, type, sort, ext_obj, date_created, date_updated) VALUES ('1509837592619499556', '6', 'multiSelectorHandle', 'multiSelectorHandle', '3', '3', '0', NULL, '2021-03-08 13:18:44', '2021-03-09 10:32:51');
-- update the dubbo loadbalance data_type to 3
UPDATE plugin_handle SET data_type = '3' WHERE plugin_id = '6' AND field = 'loadbalance';

-- change shenyu-dict unique key
ALTER TABLE shenyu_dict ADD CONSTRAINT un_dict_type_dict_code_dict_name UNIQUE (type,dict_code,dict_name);

-- add app_auth date_created,date_updated default
ALTER TABLE app_auth ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE app_auth ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add auth_param date_created,date_updated default
ALTER TABLE auth_param ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE auth_param ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add auth_path date_created,date_updated default
ALTER TABLE auth_path ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE auth_path ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add meta_data date_created,date_updated default
ALTER TABLE meta_data ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE meta_data ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add dashboard_user date_created,date_updated default
ALTER TABLE dashboard_user ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE dashboard_user ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add data_permission date_created,date_updated default
ALTER TABLE data_permission ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE data_permission ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add permission date_created,date_updated default
ALTER TABLE permission ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE permission ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add plugin date_created,date_updated default
ALTER TABLE plugin ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE plugin ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add plugin_handle date_created,date_updated default
ALTER TABLE plugin_handle ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE plugin_handle ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add resource date_created,date_updated default
ALTER TABLE resource ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE resource ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add role date_created,date_updated default
ALTER TABLE "role" ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE "role" ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add rule date_created,date_updated default
ALTER TABLE "rule" ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE "rule" ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add rule_condition date_created,date_updated default
ALTER TABLE rule_condition ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE rule_condition ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add selector date_created,date_updated default
ALTER TABLE selector ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE selector ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add selector_condition date_created,date_updated default
ALTER TABLE selector_condition ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE selector_condition ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add shenyu_dict date_created,date_updated default
ALTER TABLE shenyu_dict ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE shenyu_dict ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);

-- add user_role date_created,date_updated default
ALTER TABLE user_role ALTER COLUMN date_created SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
ALTER TABLE user_role ALTER COLUMN date_updated SET DEFAULT TIMEZONE('UTC-8'::TEXT, NOW()::TIMESTAMP(0) WITHOUT TIME ZONE);
