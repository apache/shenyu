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

import java.security.MessageDigest;

import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.exception.ShenyuException;

/**
 * The type SHA utils.
 */
public class ShaUtils {

    /**
     * sh512 Encryption string.
     *
     * @param src the src
     * @return the string
     */
    public static String shaEncryption(final String src) {
        return Optional.ofNullable(src).map(item -> {
            if (StringUtils.isEmpty(src)) {
                return null;
            }
            try {
                MessageDigest messageDigest = MessageDigest.getInstance("SHA-512");
                messageDigest.update(item.getBytes());
                byte[] byteBuffer = messageDigest.digest();
                StringBuffer strHexString = new StringBuffer();
                for (byte b:byteBuffer) {
                    String hex = Integer.toHexString(0xff & b);
                    if (hex.length() == 1) {
                        strHexString.append('0');
                    }
                    strHexString.append(hex);
                }
                return strHexString.toString();
            } catch (Exception e) {
                throw new ShenyuException(e);
            }
        }).orElse(null);
    }
}
