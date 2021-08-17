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

import java.util.Optional;

/**
 * The type Aes utils.
 */
public class AesUtils {

    /**
     * Aes Encryption string.
     *
     * @param src    the src
     * @param aesKey key
     * @param iv     iv
     * @return the string
     */
    public static String aesEncryption(final String src, final String aesKey, final String iv) {
        if (Optional.ofNullable(src).isPresent() && !src.isEmpty()) {
            return CipherUtils.encryptHex(src, aesKey, iv);
        }
        return null;
    }

    /**
     * Aes Decryption string.
     *
     * @param src    the src
     * @param aesKey key
     * @param iv     iv
     * @return the string
     */
    public static String aesDecryption(final String src, final String aesKey, final String iv) {
        if (Optional.ofNullable(src).isPresent() && !src.isEmpty()) {
            return CipherUtils.decryptStr(src, aesKey, iv);
        }
        return null;
    }
}
