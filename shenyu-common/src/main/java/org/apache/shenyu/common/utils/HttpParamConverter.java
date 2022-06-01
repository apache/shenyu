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

import org.apache.shenyu.common.exception.ShenyuException;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StringUtils;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The type Http param converter.
 */
public final class HttpParamConverter {

    private static final Pattern PATTERN = Pattern.compile("([^&=]+)(=?)([^&]+)?");

    /**
     * of.
     *
     * @param supplier supplier
     * @return String string
     */
    public static String ofString(final Supplier<String> supplier) {
        return GsonUtils.getInstance().toJson(initQueryParams(supplier.get()));
    }

    /**
     * map.
     *
     * @param <K>      the type parameter
     * @param <V>      the type parameter
     * @param supplier supplier
     * @return String string
     */
    public static <K, V> String toMap(final Supplier<MultiValueMap<K, V>> supplier) {
        return GsonUtils.getInstance().toJson(supplier.get().toSingleValueMap());
    }

    /**
     * Init query params map.
     *
     * @param query the query
     * @return the map
     */
    public static Map<String, String> initQueryParams(final String query) {
        final Map<String, String> queryParams = new LinkedHashMap<>();
        if (StringUtils.hasLength(query)) {
            final Matcher matcher = PATTERN.matcher(query);
            while (matcher.find()) {
                String name = decodeQueryParam(matcher.group(1));
                String eq = matcher.group(2);
                String value = matcher.group(3);
                value = StringUtils.hasLength(value) ? decodeQueryParam(value) : (StringUtils.hasLength(eq) ? "" : null);
                queryParams.put(name, value);
            }
        }
        return queryParams;
    }

    /**
     * Decode query param string.
     *
     * @param value the value
     * @return the string
     */
    public static String decodeQueryParam(final String value) {
        try {
            return URLDecoder.decode(value, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new ShenyuException(e);
        }
    }
}
