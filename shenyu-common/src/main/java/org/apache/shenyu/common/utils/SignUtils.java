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

    /**
     * acquired sign.
     *
     * @param signKey sign key
     * @param jsonParams json params
     * @param queryParams  url query params
     * @return sign
     */
    public static String generateSign(final String signKey, final Map<String, String> jsonParams, final Map<String, String> queryParams) {
        final String jsonSign = Optional.ofNullable(jsonParams).map(e -> e.keySet().stream()
                .sorted(Comparator.naturalOrder())
                .map(key -> String.join("", key, jsonParams.get(key)))
                .collect(Collectors.joining()).trim())
                .orElse("");
        final String querySign = Optional.ofNullable(queryParams).map(e -> e.keySet().stream()
                .sorted(Comparator.naturalOrder())
                .map(key -> String.join("", key, queryParams.get(key)))
                .collect(Collectors.joining()).trim())
                .orElse("");
        final String sign = String.join("", jsonSign, querySign, signKey);
        // TODO this is a risk for error charset coding with getBytes
        return DigestUtils.md5Hex(sign.getBytes()).toUpperCase();
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
    public static boolean isValid(final String sign, final Map<String, String> jsonParams, final Map<String, String> queryParams, final String signKey) {
        return Objects.equals(sign, generateSign(signKey, jsonParams, queryParams));
    }

    /**
     * Generate key string.
     *
     * @return the string
     */
    public static String generateKey() {
        return UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();
    }

    /**
     * Transform to string map.
     *
     * @param map source map
     * @return string map
     */
    public static Map<String, String> transStringMap(final Map<String, Object> map) {
        return Optional.ofNullable(map)
                .map(m -> m.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> Objects.toString(e.getValue(), null))))
                .orElse(null);
    }

}
