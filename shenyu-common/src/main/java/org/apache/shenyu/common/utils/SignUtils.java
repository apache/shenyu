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

package org.apache.shenyu.common.utils;

import com.google.common.collect.ImmutableMap;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

/**
 * SignUtils.
 */
public final class SignUtils {

    public static final String SIGN_MD5 = "MD5";

    public static final String SIGN_HMD5 = "HMD5";

    public static final String SIGN_HS256 = "HS256";

    public static final String SIGN_HS512 = "HS512";

    private static final Map<String, SignFunction> SIGN_FUNCTION_MAP = ImmutableMap.of(
            SIGN_MD5, (key, data) -> DigestUtils.md5Hex(data + key),
            SIGN_HMD5, HmacUtils::hmacMd5Hex,
            SIGN_HS256, HmacUtils::hmacSha256Hex,
            SIGN_HS512, HmacUtils::hmacSha512Hex
    );

    /**
     * Returns signature of data as hex string (lowercase).
     *
     * @param algorithmName the name of sign algorithm
     * @param key           key
     * @param data          data to sign
     * @return signature
     * @throws NullPointerException          if key or data is null
     * @throws UnsupportedOperationException if algorithmName isn't supported
     */
    public static String sign(final String algorithmName, final String key, final String data) {
        if (Objects.isNull(key) || Objects.isNull(data)) {
            throw new NullPointerException("Key or data is null.");
        }

        return Optional.ofNullable(SIGN_FUNCTION_MAP.get(algorithmName))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported sign algorithm:" + algorithmName))
                .sign(key, data);
    }

    /**
     * Generate key string.
     *
     * @return the string
     */
    public static String generateKey() {
        return UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();
    }

    @FunctionalInterface
    private interface SignFunction {
        String sign(String key, String data);
    }

}
