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

import org.apache.shenyu.common.utils.JsonUtils;
import org.springframework.http.HttpHeaders;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * log collect utils.
 */
public class LogCollectUtils {

    private static final List<String> BINARY_TYPE_LIST = Arrays.asList("image", "multipart", "cbor",
            "octet-stream", "pdf", "javascript", "css", "html");

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
        Map<String, String> map = headers.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, entry -> String.join(",", entry.getValue())));
        return JsonUtils.toJson(map);
    }
}
