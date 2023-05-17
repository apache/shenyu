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

import org.apache.commons.lang3.StringUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.Objects;

/**
 * result message.
 */
public enum ShenyuResultMessage {
    SUCCESS,
    CREATE_SUCCESS,
    DELETE_SUCCESS,
    UPDATE_SUCCESS,
    QUERY_SUCCESS,
    QUERY_FAILED,
    DETAIL_SUCCESS,
    DETAIL_FAILED,
    ENABLE_SUCCESS,
    SYNC_SUCCESS,
    SYNC_FAIL,
    ROLE_CREATE_ERROR,
    DASHBOARD_USER_LOGIN_ERROR,
    DASHBOARD_QUERY_ERROR,
    DASHBOARD_MODIFY_PASSWORD_ERROR,
    DASHBOARD_CREATE_USER_ERROR,
    PLATFORM_LOGIN_SUCCESS,
    PLATFORM_LOGIN_ERROR,
    LOGIN_USER_DISABLE_ERROR,
    PARAMETER_ERROR,
    UNIQUE_INDEX_CONFLICT_ERROR,
    APPKEY_NOT_EXIST_ERROR,
    TOKEN_IS_ERROR,
    TOKEN_HAS_NO_PERMISSION,
    MENU_SUCCESS,
    MENU_FAILED,
    SAVE_SUCCESS,
    NOT_FOUND_EXCEPTION,
    PASSWORD_MUST,
    PASSWORD_IS_DEFAULT,
    PASSWORD_USED_FOR_LONG_TIME,
    PLUGIN_NAME_IS_EXIST,
    RESOURCE_NAME_IS_EXIST,
    ID_NOT_EXIST,
    PLUGIN_NAME_NOT_EXIST,
    SYS_PLUGIN_NOT_DELETE,
    SYS_PLUGIN_ID_NOT_EXIST,
    SYS_API_ID_NOT_EXIST,
    DATA_PATH_IS_EXIST,
    PARAMS_ERROR,
    PLUGIN_JAR_IS_NOT_RIGHT,
    PROXY_SELECTOR_NAME_IS_EXIST ,
    PROXY_SELECTOR_ID_IS_NOT_EXIST ,
    SYSTEM_IS_BUSY;


    /**
     * getMessageContext,
     * <p>
     * If the configuration content does not exist, it will return enum name
     */

    public static String getI18n(ShenyuResultMessage key) {
        String i18n = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes()))
                .getRequest().getHeader(I18nUtil.HEADER_LOCATION);
        String result = I18nUtil.getString(i18n, key.name());
        if (StringUtils.isBlank(result)) {
            return key.name();
        }
        return result;
    }
}
