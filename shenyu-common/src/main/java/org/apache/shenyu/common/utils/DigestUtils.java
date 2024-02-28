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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.exception.ShenyuException;

import java.util.Optional;

/**
 * DigestUtils.
 */
public class DigestUtils {

    /**
     * Calculates the SHA-512 digest and returns the value as a hex string.
     * if data is null or "" ,it will return null.
     * @param data – Data to digest
     * @return SHA-512 digest as a hex string.
     */
    public static String sha512Hex(final String data) {
        return Optional.ofNullable(data).filter(StringUtils::isNoneEmpty).map(item -> {
            try {
                return org.apache.commons.codec.digest.DigestUtils.sha512Hex(data);
            } catch (Exception e) {
                throw new ShenyuException(e);
            }
        }).orElse(null);
    }

    /**
     * Calculates the MD5 digest and returns the value as a 32 character hex string.
     * @param data  Data to digest
     * @return MD5 digest as a hex string
     */
    public static String md5Hex(final String data) {
        return org.apache.commons.codec.digest.DigestUtils.md5Hex(data);
    }

    /**
     * Calculates the MD5 digest and returns the value as a 32 character hex string.
     * @param data Data to digest
     * @return MD5 digest as a hex string
     */
    public static String md5Hex(final byte[] data) {
        return org.apache.commons.codec.digest.DigestUtils.md5Hex(data);
    }
}
