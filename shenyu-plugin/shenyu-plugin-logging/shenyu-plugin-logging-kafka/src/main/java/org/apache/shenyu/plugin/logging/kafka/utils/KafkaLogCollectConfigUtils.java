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

package org.apache.shenyu.plugin.logging.kafka.utils;

import org.springframework.util.AntPathMatcher;

import java.util.HashMap;
import java.util.Map;

/**
 * kafka log collect config utils.
 */
public class KafkaLogCollectConfigUtils {

    private static final AntPathMatcher MATCHER = new AntPathMatcher();

    private static Map<String, String> apiTopicMap = new HashMap<>();

    public KafkaLogCollectConfigUtils() {
    }

    /**
     * set api topic map.
     * @param uriTopicMap api topic map
     */
    public static void setTopic(final Map<String, String> uriTopicMap) {
        apiTopicMap = uriTopicMap;
    }

    /**
     * get message queue topic.
     *
     * @param path request path
     * @return topic
     */
    public static String getTopic(final String path) {
        for (Map.Entry<String, String> entry : apiTopicMap.entrySet()) {
            String pattern = entry.getKey();
            if (MATCHER.match(pattern, path)) {
                return entry.getValue();
            }
        }
        return "";
    }
}
