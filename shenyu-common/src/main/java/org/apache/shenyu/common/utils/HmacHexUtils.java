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

import org.apache.commons.codec.digest.HmacAlgorithms;
import org.apache.commons.codec.digest.HmacUtils;

/**
 * HmacUtils.
 */
public class HmacHexUtils {

    /**
     * Returns a HmacMd5 Message Authentication Code (MAC) as hex string (lowercase).
     *
     * @param key           The key
     * @param valueToDigest The value to use to digest
     * @return Message Authentication Code
     */
    public static String hmacMd5Hex(final String key, final String valueToDigest) {
        return getHmacHex(HmacAlgorithms.HMAC_MD5, key, valueToDigest);
    }

    /**
     * Returns a HmacSha256 Message Authentication Code (MAC) as hex string (lowercase).
     *
     * @param key           The key
     * @param valueToDigest The value to use to digest
     * @return Message Authentication Code
     */
    public static String hmacSha256Hex(final String key, final String valueToDigest) {
        return getHmacHex(HmacAlgorithms.HMAC_SHA_256, key, valueToDigest);

    }

    /**
     * Returns a HmacSha512 Message Authentication Code (MAC) as hex string (lowercase).
     *
     * @param key           The key
     * @param valueToDigest The value to use to digest
     * @return Message Authentication Code
     */
    public static String hmacSha512Hex(final String key, final String valueToDigest) {
        return getHmacHex(HmacAlgorithms.HMAC_SHA_512, key, valueToDigest);
    }

    private static String getHmacHex(final HmacAlgorithms algorithm, final String key, final String valueToDigest) {
        return new HmacUtils(algorithm, key).hmacHex(valueToDigest);
    }

}
