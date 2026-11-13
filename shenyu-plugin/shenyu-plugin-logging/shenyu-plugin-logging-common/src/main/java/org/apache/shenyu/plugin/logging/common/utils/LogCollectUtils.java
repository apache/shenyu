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

package org.apache.shenyu.plugin.logging.common.utils;

import com.google.common.collect.Sets;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.springframework.http.HttpHeaders;

import java.util.Collections;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * log collect utils.
 */
public class LogCollectUtils {

    private static final Set<String> BINARY_TYPE_LIST = Sets.newHashSet("image", "multipart", "cbor",
            "octet-stream", "pdf", "javascript", "css", "html", "x-protobuf");

    private static final Set<String> DEFAULT_MASKED_HEADERS = Set.of(
            "authorization", "proxy-authorization", "cookie", "set-cookie", "shenyu-authorization", "token");

    /**
     * Capture the case-insensitive header masking policy for one request.
     *
     * @param config plugin configuration
     * @return immutable lowercase header names
     */
    public static Set<String> getMaskedHeaders(final GenericGlobalConfig config) {
        if (!config.isMaskCredentialHeaders()) {
            return Collections.emptySet();
        }
        Set<String> headers = new HashSet<>(DEFAULT_MASKED_HEADERS);
        if (Objects.nonNull(config.getMaskedHeaders())) {
            config.getMaskedHeaders().stream().filter(name -> Objects.nonNull(name) && !name.trim().isEmpty())
                    .map(name -> name.trim().toLowerCase(Locale.ROOT)).forEach(headers::add);
        }
        return Collections.unmodifiableSet(headers);
    }

    /**
     * Test a header against a captured masking policy.
     *
     * @param name header name
     * @param maskedHeaders lowercase header names
     * @return whether to replace the complete value
     */
    public static boolean isMaskedHeader(final String name, final Set<String> maskedHeaders) {
        return maskedHeaders.contains(name.toLowerCase(Locale.ROOT));
    }

    /**
     * judge whether is binary type.
     *
     * @param headers request or response header
     * @return whether binary type
     */
    public static boolean isNotBinaryType(final HttpHeaders headers) {
        return Optional.ofNullable(headers).map(HttpHeaders::getContentType)
                .map(contentType -> !BINARY_TYPE_LIST.contains(contentType.getType())
                        && !BINARY_TYPE_LIST.contains(contentType.getSubtype()))
                .orElse(true);
    }

    /**
     * get request header string.
     *
     * @param headers request headers
     * @return header string
     */
    public static String getHeaders(final HttpHeaders headers) {
        return getHeaders(headers, DEFAULT_MASKED_HEADERS);
    }

    /**
     * Serialize headers with the request's masking policy without modifying live headers.
     *
     * @param headers request or response headers
     * @param maskedHeaders lowercase header names
     * @return header JSON
     */
    public static String getHeaders(final HttpHeaders headers, final Set<String> maskedHeaders) {
        Map<String, String> map = headers.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, entry -> isMaskedHeader(entry.getKey(), maskedHeaders) ? "***" : String.join(",", entry.getValue())));
        return JsonUtils.toJson(map);
    }
}
