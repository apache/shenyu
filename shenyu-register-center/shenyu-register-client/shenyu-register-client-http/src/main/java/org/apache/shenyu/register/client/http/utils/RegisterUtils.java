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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

/**
 * RegisterUtils.
 */
public final class RegisterUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(RegisterUtils.class);

    private RegisterUtils() {
    }

    /**
     * call register api.
     *
     * @param json        request body
     * @param url         url
     * @param rpcType rcp type
     * @throws IOException exception
     */
    public static void doRegister(final String json, final String url, final String rpcType) throws IOException {
        String result = OkHttpTools.getInstance().post(url, json);
        if ("success".equals(result)) {
            LOGGER.info("{} client register success: {} ", rpcType, json);
        } else {
            LOGGER.error("{} client register error: {} ", rpcType, json);
        }
    }
}
