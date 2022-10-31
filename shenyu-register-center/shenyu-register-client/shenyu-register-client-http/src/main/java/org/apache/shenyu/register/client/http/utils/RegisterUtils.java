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

package org.apache.shenyu.register.client.http.utils;

import okhttp3.Headers;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import static org.apache.shenyu.common.constant.Constants.SUCCESS;

/**
 * RegisterUtils.
 */
public final class RegisterUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(RegisterUtils.class);

    private RegisterUtils() {
    }

    /**
     * Do register.
     *
     * @param json        the json
     * @param url         the url
     * @param type        the type
     * @param accessToken the token
     * @throws IOException the io exception
     */
    public static void doRegister(final String json, final String url, final String type, final String accessToken) throws IOException {
        if (StringUtils.isBlank(accessToken)) {
            LOGGER.error("{} client register error accessToken is null, please check the config : {} ", type, json);
            return;
        }
        Headers headers = new Headers.Builder().add(Constants.X_ACCESS_TOKEN, accessToken).build();
        String result = OkHttpTools.getInstance().post(url, json, headers);
        if (Objects.equals(SUCCESS, result)) {
            LOGGER.info("{} client register success: {} ", type, json);
        } else {
            LOGGER.error("{} client register error: {} ", type, json);
        }
    }

    /**
     * Do register.
     *
     * @param json the json
     * @param url  the url
     * @param type the type
     * @throws IOException the io exception
     */
    public static void doRegister(final String json, final String url, final String type) throws IOException {
        String result = OkHttpTools.getInstance().post(url, json);
        if (Objects.equals(SUCCESS, result)) {
            LOGGER.info("{} client register success: {} ", type, json);
        } else {
            LOGGER.error("{} client register error: {} ", type, json);
        }
    }

    /**
     * Do login.
     *
     * @param username the username
     * @param password the password
     * @param url      the ulr
     * @return Optional token
     * @throws IOException the io exception
     */
    public static Optional<Object> doLogin(final String username, final String password, final String url) throws IOException {
        Map<String, Object> loginMap = new HashMap<>(2);
        loginMap.put(Constants.LOGIN_NAME, username);
        loginMap.put(Constants.PASS_WORD, password);
        String result = OkHttpTools.getInstance().get(url, loginMap);
        Map<String, Object> resultMap = GsonUtils.getInstance().convertToMap(result);
        if (!String.valueOf(CommonErrorCode.SUCCESSFUL).equals(String.valueOf(resultMap.get(Constants.ADMIN_RESULT_CODE)))) {
            return Optional.empty();
        }
        String tokenJson = GsonUtils.getInstance().toJson(resultMap.get(Constants.ADMIN_RESULT_DATA));
        LOGGER.info("login success: {} ", tokenJson);
        Map<String, Object> tokenMap = GsonUtils.getInstance().convertToMap(tokenJson);
        return Optional.ofNullable(tokenMap.get(Constants.ADMIN_RESULT_TOKEN));
    }
}
