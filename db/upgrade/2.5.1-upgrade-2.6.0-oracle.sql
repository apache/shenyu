/* insert plugin_handle data for plugin CryptorRequest */
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1630760188111376384', '24', 'mapType', 'mapType', 3, 2, 3, '{"required":"0","defaultValue":"all","rule":""}');
/* insert plugin_handle data for plugin cryptorResponse */
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(plugin_handle(plugin_id, field, type)) */ into plugin_handle (ID, PLUGIN_ID, FIELD, LABEL, DATA_TYPE, TYPE, SORT, EXT_OBJ)
values ('1630768384280514560', '25', 'mapType', 'mapType', 3, 2, 3, '{"required":"0","defaultValue":"all","rule":""}');

/* insert plugin_handle data for plugin_handle mapType */
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1630761573833920512', 'mapType', 'mapType', 'all', 'all', '', 1, 1);
insert /*+ IGNORE_ROW_ON_DUPKEY_INDEX(shenyu_dict(type, dict_code, dict_name)) */ into SHENYU_DICT (ID, TYPE, DICT_CODE, DICT_NAME, DICT_VALUE, "desc", SORT, ENABLED)
values ('1630761984393367552', 'mapType', 'mapType', 'field', 'field', '', 1, 1);