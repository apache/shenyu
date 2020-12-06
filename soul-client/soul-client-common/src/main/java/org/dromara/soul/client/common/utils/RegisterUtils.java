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

package org.dromara.soul.client.common.utils;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.constant.AdminConstants;
import org.dromara.soul.common.enums.RpcTypeEnum;

import java.io.IOException;

/**
 * RegisterUtils.
 *
 * @author severez
 */
@Slf4j
public final class RegisterUtils {

    private RegisterUtils() {
    }

    /**
     * call register api.
     *
     * @param json        request body
     * @param url         url
     * @param rpcTypeEnum rcp type
     */
    public static void doRegister(final String json, final String url, final RpcTypeEnum rpcTypeEnum) {
        try {
            String result = OkHttpTools.getInstance().post(url, json);
            if (AdminConstants.SUCCESS.equals(result)) {
                log.info("{} client register success: {} ", rpcTypeEnum.getName(), json);
            } else {
                log.error("{} client register error: {} ", rpcTypeEnum.getName(), json);
            }
        } catch (IOException e) {
            log.error("cannot register soul admin param, url: {}, request body: {}", url, json, e);
        }
    }

}
