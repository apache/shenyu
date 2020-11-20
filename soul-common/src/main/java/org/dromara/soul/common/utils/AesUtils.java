/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.common.utils;

import cn.hutool.core.util.CharsetUtil;
import cn.hutool.crypto.SecureUtil;
import cn.hutool.crypto.symmetric.AES;

import java.util.Optional;

/**
 * The type Aes utils.
 *
 * @author nuo-promise
 */
public class AesUtils {

    private static final String AES_KEY = "2095132720951327";

    /**
     * Aes Encryption string.
     *
     * @param src    the src
     * @return  the string
     */
    public static String aesEncryption(final String src) {
        if (Optional.ofNullable(src).isPresent() && !src.isEmpty()) {
            AES aes = SecureUtil.aes(AES_KEY.getBytes());
            return aes.encryptHex(src).toUpperCase();
        }
        return "";
    }

    /**
     * Aes Decryption string.
     *
     * @param src    the src
     * @return the string
     */
    public static String aesDecryption(final String src) {
        if (Optional.ofNullable(src).isPresent() && !src.isEmpty()) {
            AES aes = SecureUtil.aes(AES_KEY.getBytes());
            return aes.decryptStr(src, CharsetUtil.CHARSET_UTF_8);
        }
        return "";
    }
}
