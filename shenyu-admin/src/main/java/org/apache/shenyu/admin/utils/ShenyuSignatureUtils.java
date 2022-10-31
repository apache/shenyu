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

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.springframework.util.DigestUtils;

/**
 * Shenyu Signature tool.
 */
public class ShenyuSignatureUtils {

    /**
     * At present, it is positioned as 1.0.0 write dead, string type.
     */
    public static final String VERSION = "1.0.0";

    /**
     * generate Sign.
     * @param sign sign
     * @return String
     */
    public static String generateSign(final String sign) {
        return DigestUtils.md5DigestAsHex(sign.getBytes()).toUpperCase();
    }

    /**
     * getSignContent.
     * @param secureKey secureKey
     * @param timestamp timestamp
     * @param path path
     * @return String
     */
    public static String getSignContent(final String secureKey, final String timestamp, final String path) {
        Map<String, String> map = new HashMap<>(3);
        map.put("timestamp", timestamp);
        map.put("path", path);
        map.put("version", VERSION);

        List<String> storedKeys = Arrays.stream(map.keySet()
                .toArray(new String[] {}))
            .sorted(Comparator.naturalOrder())
            .collect(Collectors.toList());
        final String signContent = storedKeys.stream()
            .map(key -> String.join("", key, map.get(key)))
            .collect(Collectors.joining()).trim()
            .concat(secureKey);
        return signContent;
    }

}
