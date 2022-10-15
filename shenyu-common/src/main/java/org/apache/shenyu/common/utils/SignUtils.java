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

import com.google.common.collect.Maps;
import org.springframework.util.DigestUtils;

import java.util.Comparator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * SignUtils.
 */
public final class SignUtils {

    private static final Map<String, String> EMPTY_HASH_MAP = Maps.newHashMap();

    private static final SignUtils SIGN_UTILS = new SignUtils();

    private SignUtils() {
    }

    /**
     * getInstance.
     *
     * @return {@linkplain SignUtils}
     */
    public static SignUtils getInstance() {
        return SIGN_UTILS;
    }

    /**
     * acquired sign.
     *
     * @param signKey sign key
     * @param jsonParams json params
     * @param queryParams  url query params
     * @return sign
     */
    public static String generateSign(final String signKey, final Map<String, String> jsonParams, final Map<String, String> queryParams) {
        final String jsonSign = Optional.ofNullable(jsonParams).orElse(EMPTY_HASH_MAP).keySet().stream()
                .sorted(Comparator.naturalOrder())
                .map(key -> String.join("", key, jsonParams.get(key)))
                .collect(Collectors.joining()).trim();
        final String querySign = Optional.ofNullable(queryParams).orElse(EMPTY_HASH_MAP).keySet().stream()
                .sorted(Comparator.naturalOrder())
                .map(key -> String.join("", key, queryParams.get(key)))
                .collect(Collectors.joining()).trim();
        final String sign = String.join("", jsonSign, querySign, signKey);
        // TODO this is a risk for error charset coding with getBytes
        return DigestUtils.md5DigestAsHex(sign.getBytes()).toUpperCase();
    }

    /**
     * isValid.
     *
     * @param sign    sign
     * @param jsonParams json params
     * @param queryParams  url query params
     * @param signKey sign key
     * @return boolean
     */
    public boolean isValid(final String sign, final Map<String, String> jsonParams, final Map<String, String> queryParams, final String signKey) {
        return Objects.equals(sign, generateSign(signKey, jsonParams, queryParams));
    }

    /**
     * Generate key string.
     *
     * @return the string
     */
    public String generateKey() {
        return UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();
    }

}
