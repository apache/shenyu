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

INSERT INTO "public"."resource" VALUES ('1697141926247763968','1346776175553376256','SHENYU.MENU.SYSTEM.MANAGMENT.ALERT','','/system/alert','',1,3,'alert',0,0,'',1,'2023-08-31 14:59:01','2023-08-31 06:59:01');
INSERT INTO "public"."resource" VALUES ('1697146375729025024', '1697141926247763968', 'SHENYU.BUTTON.SYSTEM.LIST', '', '', '', 2, 0, 'unordered-list', 1, 0, 'system:alert:list', 1, '2023-08-31 15:16:42', '2023-08-31 07:22:07');
INSERT INTO "public"."resource" VALUES ('1697145808210333696','1697141926247763968','SHENYU.BUTTON.SYSTEM.ADD','','','',2,0,'plus',1,0,'system:alert:add',1,'2023-08-31 15:14:26','2023-08-31 07:14:26');
INSERT INTO "public"."resource" VALUES ('1697146617513873408','1697141926247763968','SHENYU.BUTTON.SYSTEM.DELETE','','','',2,0,'delete',1,0,'system:alert:delete',1,'2023-08-31 15:17:39','2023-08-31 07:22:07');
INSERT INTO "public"."resource" VALUES ('1697146860540235776','1697141926247763968','SHENYU.BUTTON.SYSTEM.EDIT','','','',2,0,'edit',1,0,'system:alert:edit',1,'2023-08-31 15:18:37','2023-08-31 07:18:37');

INSERT INTO "public"."permission" VALUES ('1697141926281318400', '1346358560427216896', '1697141926247763968', '2023-08-31 14:59:01', '2023-08-31 06:59:01');
INSERT INTO "public"."permission" VALUES ('1697145808239693824', '1346358560427216896', '1697145808210333696', '2023-08-31 15:14:26', '2023-08-31 07:14:26');
INSERT INTO "public"."permission" VALUES ('1697146375754190848', '1346358560427216896', '1697146375729025024', '2023-08-31 15:16:42', '2023-08-31 07:16:42');
INSERT INTO "public"."permission" VALUES ('1697146617543233536', '1346358560427216896', '1697146617513873408', '2023-08-31 15:17:39', '2023-08-31 07:17:39');
INSERT INTO "public"."permission" VALUES ('1697146860569595904', '1346358560427216896', '1697146860540235776', '2023-08-31 15:18:37', '2023-08-31 07:18:37');