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

-- Prior schema referenced by graph publications and permission seeds.
CREATE TABLE namespace (id VARCHAR(128) PRIMARY KEY, namespace_id VARCHAR(50) NOT NULL);
CREATE TABLE namespace_user_rel (id VARCHAR(128) PRIMARY KEY, namespace_id VARCHAR(50) NOT NULL, user_id VARCHAR(128) NOT NULL);
CREATE TABLE resource (
    id VARCHAR(128) PRIMARY KEY, parent_id VARCHAR(128), title VARCHAR(255),
    resource_type INT, sort INT, is_leaf INT, is_route INT, perms VARCHAR(255), status INT
);
CREATE TABLE permission (
    id VARCHAR(128) PRIMARY KEY, object_id VARCHAR(128), resource_id VARCHAR(128),
    date_created TIMESTAMP, date_updated TIMESTAMP
);
