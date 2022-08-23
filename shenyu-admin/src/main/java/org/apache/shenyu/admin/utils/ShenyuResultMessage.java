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
 *  result message.
 */
public final class ShenyuResultMessage {

    public static final String SUCCESS = "success";

    public static final String CREATE_SUCCESS = "create success";

    public static final String DELETE_SUCCESS = "delete success";

    public static final String UPDATE_SUCCESS = "update success";

    public static final String QUERY_SUCCESS = "query success";

    public static final String QUERY_FAILED = "query failed";

    public static final String DETAIL_SUCCESS = "detail success";

    public static final String DETAIL_FAILED = "detail failed";

    public static final String ENABLE_SUCCESS = "enable success";

    public static final String SYNC_SUCCESS = "sync success";

    public static final String SYNC_FAIL = "sync fail";

    public static final String ROLE_CREATE_ERROR = "can not create super role";

    public static final String DASHBOARD_USER_LOGIN_ERROR = "user not login please login first";

    public static final String DASHBOARD_QUERY_ERROR = "user info is empty";

    public static final String DASHBOARD_MODIFY_PASSWORD_ERROR = "can not modify other user password";

    public static final String DASHBOARD_CREATE_USER_ERROR = "empty user info, please confirm";

    public static final String PLATFORM_LOGIN_SUCCESS = "login dashboard user success";

    public static final String PLATFORM_LOGIN_ERROR = "username or password error";

    public static final String LOGIN_USER_DISABLE_ERROR = "the user is disabled";

    public static final String PARAMETER_ERROR = "parameter error";

    public static final String UNIQUE_INDEX_CONFLICT_ERROR = "unique index conflict, please enter again";

    public static final String APPKEY_NOT_EXIST_ERROR = "the appKey passed in does not exist";

    public static final String TOKEN_IS_ERROR = "token is error";

    public static final String TOKEN_HAS_NO_PERMISSION = "token has no permission";

    public static final String MENU_SUCCESS = "get menu and permission success";

    public static final String MENU_FAILED = "get menu and permission failed";

    public static final String SAVE_SUCCESS = "save success";

    public static final String NOT_FOUND_EXCEPTION = "not found exception";
}
