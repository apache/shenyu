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

-- this file works for Oracle, can not use "`" syntax.

ALTER TABLE selector ADD match_restful NUMBER(3) not null;
COMMENT ON COLUMN SELECTOR.match_restful IS 'whether to match restful(0 cache, 1 not cache)';
COMMIT;

ALTER TABLE rule ADD match_restful NUMBER(3) not null;
COMMENT ON COLUMN RULE.match_restful IS 'whether to match restful(0 cache, 1 not cache)';
COMMIT;

/* insert plugin_handle data for plugin CryptorRequest */
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1630760188111376384', '24', 'mapType', 'mapType', 3, 2, 3, '{\"required\":\"0\",\"defaultValue\":\"all\",\"rule\":\"\"}');
/* insert plugin_handle data for plugin cryptorResponse */
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1630768384280514560', '25', 'mapType', 'mapType', 3, 2, 4, '{\"required\":\"0\",\"defaultValue\":\"all\",\"rule\":\"\"}');

/* insert plugin_handle data for plugin_handle mapType */
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1630761573833920512', 'mapType', 'mapType', 'all', 'all', '', 0, 1);
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1630761984393367552', 'mapType', 'mapType', 'field', 'field', '', 1, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1545812228228259844', 'loadBalance', 'LOAD_BALANCE', 'p2c', 'p2c', 'p2c', 4, 1);
INSERT /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ INTO SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
VALUES ('1545812228228259845', 'loadBalance', 'LOAD_BALANCE', 'shortestResponse', 'shortestResponse', 'shortestResponse', 5, 1);

/* add column into plugin table */
ALTER TABLE plugin ADD plugin_jar BLOB NULL;
COMMENT ON COLUMN plugin.plugin_jar IS 'plugin jar';
