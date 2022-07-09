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

package org.apache.shenyu.plugin.logging.rocketmq.utils;

import org.apache.shenyu.plugin.logging.rocketmq.config.LogCollectConfig;
import org.springframework.util.AntPathMatcher;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * RocketMQ log collect ConfigUtils.
 */
public final class RocketLogCollectConfigUtils {

    private static final AntPathMatcher MATCHER = new AntPathMatcher();

    private static Map<String, String> apiTopicMap = new HashMap<>();

    private static LogCollectConfig.GlobalLogConfig globalLogConfig;

    private static final LogCollectConfig.GlobalLogConfig DEFAULT_GLOBAL_LOG_CONFIG =
            new LogCollectConfig.GlobalLogConfig();

    public RocketLogCollectConfigUtils() {
    }

    /**
     * set global config.
     *
     * @param config global config
     */
    public static void setGlobalConfig(final LogCollectConfig.GlobalLogConfig config) {
        globalLogConfig = config;
    }

    /**
     * get global config.
     *
     * @return {@linkplain LogCollectConfig.GlobalLogConfig}
     */
    public static LogCollectConfig.GlobalLogConfig getGlobalLogConfig() {
        return Optional.ofNullable(globalLogConfig).orElse(DEFAULT_GLOBAL_LOG_CONFIG);
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
