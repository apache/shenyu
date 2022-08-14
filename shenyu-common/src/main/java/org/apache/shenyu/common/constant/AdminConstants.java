/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.common.constant;

/**
 * The type Admin constants.
 */
public final class AdminConstants {

    /**
     * The constant SUCCESS, equals to ShenyuResultMessage.SUCCESS.
     */
    public static final String SUCCESS = "SUCCESS";

    /**
     * The constant PLUGIN_NAME_IS_EXIST.
     */
    public static final String PLUGIN_NAME_IS_EXIST = "The plugin name already exists and can't be added repeatedly!";

    /**
     * The constant ID_NOT_EXIST.
     */
    public static final String ID_NOT_EXIST = "ID NOT EXIST!";

    /**
     * The constant PLUGIN_NAME_NOT_EXIST.
     */
    public static final String PLUGIN_NAME_NOT_EXIST = "The plugin name not exist!";

    /**
     * The constant SYS_PLUGIN_NOT_DELETE.
     */
    public static final String SYS_PLUGIN_NOT_DELETE = "System plugins can't be deleted!";

    /**
     * The constant SYS_PLUGIN_ID_NOT_EXIST.
     */
    public static final String SYS_PLUGIN_ID_NOT_EXIST = "The plugin(s) does not exist！";

    /**
     * The constant DATA_PATH_IS_EXIST.
     */
    public static final String DATA_PATH_IS_EXIST = "The path already exists and can't be added repeatedly!";

    /**
     * The constant PARAMS_ERROR.
     */
    public static final String PARAMS_ERROR = "Error parameter！";

    /**
     * The constant Super Role ID.
     */
    public static final String ROLE_SUPER_ID = "1346358560427216896";

    /**
     * The constant Plugin Menu ID.
     */
    public static final String RESOURCE_PLUGIN_ID = "1346775491550474240";

    /**
     * The constant Plugin Url prefix.
     */
    public static final String RESOURCE_PLUGIN_URL_PREFIX = "/plug/";

    /**
     * The constant Plugin Resource Icon.
     */
    public static final String RESOURCE_PLUGIN_DEFAULT_ICON = "block";

    /**
     * The constant Selector Name.
     */
    public static final String DATA_PERMISSION_SELECTOR = "selector";

    /**
     * The constant Rule Name.
     */
    public static final String DATA_PERMISSION_RULE = "rule";

    /**
     * The constant Admin Name.
     */
    public static final String ADMIN_NAME = "admin";

    /**
     * The constant selector data type.
     */
    public static final Integer SELECTOR_DATA_TYPE = 0;

    /**
     * the constant rule data type.
     */
    public static final Integer RULE_DATA_TYPE = 1;

    /**
     * the constant selector add.
     */
    public static final String PLUGIN_SELECTOR_ADD = "SHENYU.BUTTON.PLUGIN.SELECTOR.ADD";

    /**
     * the constant selector type add.
     */
    public static final String PLUGIN_TYPE_SELECTOR_ADD = "Selector:add";

    /**
     * the constant selector query.
     */
    public static final String PLUGIN_SELECTOR_QUERY = "SHENYU.BUTTON.PLUGIN.SELECTOR.QUERY";

    /**
     * the constant selector type query.
     */
    public static final String PLUGIN_TYPE_SELECTOR_QUERY = "Selector:query";

    /**
     * the constant selector edit.
     */
    public static final String PLUGIN_SELECTOR_EDIT = "SHENYU.BUTTON.PLUGIN.SELECTOR.EDIT";

    /**
     * the constant selector type edit.
     */
    public static final String PLUGIN_TYPE_SELECTOR_EDIT = "Selector:edit";

    /**
     * the constant selector delete.
     */
    public static final String PLUGIN_SELECTOR_DELETE = "SHENYU.BUTTON.PLUGIN.SELECTOR.DELETE";

    /**
     * the constant selector type delete.
     */
    public static final String PLUGIN_TYPE_SELECTOR_DELETE = "Selector:delete";

    /**
     * the constant rule add.
     */
    public static final String PLUGIN_RULE_ADD = "SHENYU.BUTTON.PLUGIN.RULE.ADD";

    /**
     * the constant rule type add.
     */
    public static final String PLUGIN_TYPE_RULE_ADD = "Rule:add";

    /**
     * the constant rule query.
     */
    public static final String PLUGIN_RULE_QUERY = "SHENYU.BUTTON.PLUGIN.RULE.QUERY";

    /**
     * the constant rule type query.
     */
    public static final String PLUGIN_TYPE_RULE_QUERY = "Rule:query";

    /**
     * the constant rule edit.
     */
    public static final String PLUGIN_RULE_EDIT = "SHENYU.BUTTON.PLUGIN.RULE.EDIT";

    /**
     * the constant rule type edit.
     */
    public static final String PLUGIN_TYPE_RULE_EDIT = "Rule:edit";

    /**
     * the constant rule delete.
     */
    public static final String PLUGIN_RULE_DELETE = "SHENYU.BUTTON.PLUGIN.RULE.DELETE";

    /**
     * the constant rule type delete.
     */
    public static final String PLUGIN_TYPE_RULE_DELETE = "Rule:delete";

    /**
     * the constant plugin synchronize.
     */
    public static final String PLUGIN_SYNCHRONIZE = "SHENYU.BUTTON.PLUGIN.SYNCHRONIZE";

    /**
     * the constant plugin type modify.
     */
    public static final String PLUGIN_TYPE_SYNCHRONIZE = ":modify";

    /**
     * the constant table init dict type.
     */
    public static final String DICT_TABLE_FLAG_TYPE = "table";

    /**
     * the constant table init dict code.
     */
    public static final String DICT_TABLE_FLAG_DICTCODE = "INIT_FLAG";

    /**
     * the constant table init dict name.
     */
    public static final String DICT_TABLE_FLAG_DICTNAME = "status";

    /**
     * the constant table init dict desc.
     */
    public static final String DICT_TABLE_FLAG_DESC = "table(resource,permission) init status";

    /**
     * the constant table init dict sort.
     */
    public static final Integer DICT_TABLE_FLAG_SORT = 0;

    /**
     * The constant URI_SUFFIX.
     */
    public static final String URI_SUFFIX = "/**";

    /**
     * The constant URI_SLASH_SUFFIX.
     */
    public static final String URI_SLASH_SUFFIX = "/";

    /**
     * The constant URI_VARIABLE_SUFFIX.
     */
    public static final String URI_VARIABLE_SUFFIX = "{";

    /**
     * the constant api document global flag dict code.
     */
    public static final String DICT_API_DOC_FLAG_DICTCODE = "API_DOC_GLOBAL_FLAG";

    /**
     * the constant api document global flag dict name.
     */
    public static final String DICT_API_DOC_FLAG_DICTNAME = "status";

    /**
     * the constant api document environment dict name.
     */
    public static final String DICT_TYPE_API_DOC_ENV = "apidocEnv";

}
