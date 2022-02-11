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

package org.apache.shenyu.protocol.mqtt.utils;

import org.springframework.util.DigestUtils;
import java.nio.charset.StandardCharsets;

/**
 * encrypt util.
 */
public class EncryptUtil {

    /**
     * choose encrypt mode.
     * @param encryptMode encryptMode
     * @param password password
     * @return encrypt password.
     */
    public static String choose(final String encryptMode, final String password) {

        switch (encryptMode) {
            case "MD5":
                return md5(password);
            default:
                return password;
        }
    }

    /**
     * md5 encrypt.
     * @param password password
     * @return encrypt password
     */
    private static String md5(final String password) {
        return DigestUtils.md5DigestAsHex(password.getBytes(StandardCharsets.UTF_8));
    }
}
