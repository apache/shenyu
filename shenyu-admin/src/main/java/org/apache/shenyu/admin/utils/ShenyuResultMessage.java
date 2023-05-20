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

package org.apache.shenyu.admin.utils;

/**
 * result message.
 */
public final class ShenyuResultMessage {
    
    public static final String SUCCESS = "SUCCESS";
    
    public static final String CREATE_SUCCESS = "CREATE_SUCCESS";
    
    public static final String DELETE_SUCCESS = "DELETE_SUCCESS";
    
    public static final String UPDATE_SUCCESS = "UPDATE_SUCCESS";
    
    public static final String QUERY_SUCCESS = "QUERY_SUCCESS";
    
    public static final String QUERY_FAILED = "QUERY_FAILED";
    
    public static final String DETAIL_SUCCESS = "DETAIL_SUCCESS";
    
    public static final String DETAIL_FAILED = "DETAIL_FAILED";
    
    public static final String ENABLE_SUCCESS = "ENABLE_SUCCESS";
    
    public static final String SYNC_SUCCESS = "SYNC_SUCCESS";
    
    public static final String SYNC_FAIL = "SYNC_FAIL";
    
    public static final String ROLE_CREATE_ERROR = "ROLE_CREATE_ERROR";
    
    public static final String DASHBOARD_USER_LOGIN_ERROR = "DASHBOARD_USER_LOGIN_ERROR";
    
    public static final String DASHBOARD_QUERY_ERROR = "DASHBOARD_QUERY_ERROR";
    
    public static final String DASHBOARD_MODIFY_PASSWORD_ERROR = "DASHBOARD_MODIFY_PASSWORD_ERROR";
    
    public static final String DASHBOARD_CREATE_USER_ERROR = "DASHBOARD_CREATE_USER_ERROR";
    
    public static final String PLATFORM_LOGIN_SUCCESS = "PLATFORM_LOGIN_SUCCESS";
    
    public static final String PLATFORM_LOGIN_ERROR = "PLATFORM_LOGIN_ERROR";
    
    public static final String LOGIN_USER_DISABLE_ERROR = "LOGIN_USER_DISABLE_ERROR";
    
    public static final String PARAMETER_ERROR = "PARAMETER_ERROR";
    
    public static final String UNIQUE_INDEX_CONFLICT_ERROR = "UNIQUE_INDEX_CONFLICT_ERROR";
    
    public static final String APPKEY_NOT_EXIST_ERROR = "APPKEY_NOT_EXIST_ERROR";
    
    public static final String TOKEN_IS_ERROR = "TOKEN_IS_ERROR";
    
    public static final String TOKEN_HAS_NO_PERMISSION = "TOKEN_HAS_NO_PERMISSION";
    
    public static final String MENU_SUCCESS = "MENU_SUCCESS";
    
    public static final String MENU_FAILED = "MENU_FAILED";
    
    public static final String SAVE_SUCCESS = "SAVE_SUCCESS";
    
    public static final String NOT_FOUND_EXCEPTION = "NOT_FOUND_EXCEPTION";
    
    public static final String PASSWORD_MUST = "PASSWORD_MUST";
    
    public static final String PASSWORD_IS_DEFAULT = "PASSWORD_IS_DEFAULT";
    
    public static final String PASSWORD_USED_FOR_LONG_TIME = "PASSWORD_USED_FOR_LONG_TIME";

    public static final String APP_AUTH_ID_NOT_NULL = "app auth id not null";

    public static final String APP_AUTH_ID_NOT_EXISTED = "app auth is not existed";

    public static final String APP_AUTH_APPKEY_NOT_NULL = "app auth appKey not null";

    public static final String APP_AUTH_APPSECRET_NOT_NULL = "app auth appSecret not null";

    public static final String NUMBER_IS_ILLEGAL = "number is illegal, length 7 to 11! e.g. +1234567 or 1234567";

    public static final String APPKEY_NOT_EXISTED = "app key not existed";

    public static final String CURRENTPAGE_NOT_NULL = "currentPage not null";

    public static final String PAGESIZE_NOT_NULL = "pageSize not null";

    public static final String AUTH_PATH_NOT_EXISTED = "auth path not existed";

    public static final String SYSTEM_IS_BUSY = "The system is busy, please try again later";

    public static final String USER_NOT_FOUND = "user is not found";

    public static final String DISCOVERY_UPSTRAM_IS_NOT_EXISTED = "discovery upstream is not existed";

    public static final String ONLY_BE_USED_BY_ADMIN = "This function can only be used by the admin(root) user";

    public static final String SIZE_MAX_1000 = "size max support is 1000";

    public static final String SIZE_MIN_1 = "size min support is 1";

    public static final String PLUGIN_NOT_EXISTED = "plugin is not existed";

    public static final String RULE_NOT_EXISTED = "rule not existed";

    public static final String PROXY_SELECTOR_NOT_EXISTED = "proxy selector not existed";

    public static final String RESOURCE_NOT_EXISTED = "resource not existed";

    public static final String ROLE_NOT_EXISTED = "role is not existed";

    public static final String SELECTOR_NOT_EXISTED = "selector is not existed";

    public static final String DICT_NOT_EXISTED = "dict is not existed";

    public static final String BATCH_ENABLE_SUCCESS = "batch enable success";

    public static final String TAG_NOT_EXISTED = "tag is not existed";

    public static final String API_ID_NOT_EXISTED = "the apiId is not exited";
    
    private ShenyuResultMessage() {
    
    }
}
