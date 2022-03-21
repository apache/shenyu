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

package org.apache.shenyu.plugin.logging.utils;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.logging.config.LogCollectConfig;
import org.apache.shenyu.plugin.logging.sampler.CountSampler;
import org.apache.shenyu.plugin.logging.sampler.Sampler;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.util.AntPathMatcher;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * log collect config Utils.
 */
public final class LogCollectConfigUtils {

    private static final AntPathMatcher MATCHER = new AntPathMatcher();

    private static LogCollectConfig logCollectConfig;

    private static final LogCollectConfig.LogFieldSwitchConfig DEFAULT_LOG_FIELD_SWITCH_CONFIG =
            new LogCollectConfig.LogFieldSwitchConfig();

    private static final LogCollectConfig.GlobalLogConfig DEFAULT_GLOBAL_LOG_CONFIG =
            new LogCollectConfig.GlobalLogConfig();

    private static final Map<String, Sampler> API_SAMPLER_MAP = new HashMap<>();

    private static Sampler globalSampler = Sampler.ALWAYS_SAMPLE;

    private LogCollectConfigUtils() {
    }

    /**
     * set log collect config.
     * @param logCollectConfig log collect config.
     */
    public static void setLogCollectConfig(final LogCollectConfig logCollectConfig) {
        LogCollectConfigUtils.logCollectConfig = logCollectConfig;
        init();
    }

    /**
     * init logging config.
     */
    public static void init() {
        if (MapUtils.isNotEmpty(logCollectConfig.getLogApiSwitchConfigMap())) {
            logCollectConfig.getLogApiSwitchConfigMap().forEach((path, config) -> {
                if (StringUtils.isBlank(config.getSampleRate())) {
                    API_SAMPLER_MAP.put(path, globalSampler);
                } else {
                    API_SAMPLER_MAP.put(path, CountSampler.create(config.getSampleRate()));
                }
            });
        }
        if (Objects.nonNull(logCollectConfig.getGlobalLogConfig())) {
            globalSampler = CountSampler.create(logCollectConfig.getGlobalLogConfig().getSampleRate());
        }
    }

    /**
     * judge whether sample.
     *
     * @param request request
     * @return whether sample
     */
    public static boolean isSampled(final ServerHttpRequest request) {
        if (Objects.isNull(logCollectConfig) || MapUtils.isEmpty(logCollectConfig.getLogApiSwitchConfigMap())) {
            return true;
        }
        String path = request.getURI().getPath();
        Map<String, LogCollectConfig.LogApiConfig> apiConfigMap = logCollectConfig.getLogApiSwitchConfigMap();
        for (Map.Entry<String, LogCollectConfig.LogApiConfig> entry : apiConfigMap.entrySet()) {
            String pattern = entry.getKey();
            if (MATCHER.match(pattern, path)) {
                return Optional.ofNullable(API_SAMPLER_MAP.get(pattern))
                        .map(sampler -> sampler.isSampled(request))
                        .orElse(globalSampler.isSampled(request));
            }
        }
        return true;
    }

    /**
     * judge whether request body too large.
     *
     * @param bodySize body size
     * @return whether request body too large
     */
    public static boolean isRequestBodyTooLarge(final int bodySize) {
        if (Objects.isNull(logCollectConfig) || Objects.isNull(logCollectConfig.getGlobalLogConfig())) {
            return false;
        }
        return bodySize > logCollectConfig.getGlobalLogConfig().getMaxRequestBody();
    }

    /**
     * judge whether response body too large.
     *
     * @param bodySize body size.
     * @return whether response body too large
     */
    public static boolean isResponseBodyTooLarge(final int bodySize) {
        if (Objects.isNull(logCollectConfig) || Objects.isNull(logCollectConfig.getGlobalLogConfig())) {
            return false;
        }
        return bodySize > logCollectConfig.getGlobalLogConfig().getMaxResponseBody();
    }

    /**
     * get log field switch config.
     *
     * @return log field switch config
     */
    public static LogCollectConfig.LogFieldSwitchConfig getLogFieldSwitchConfig() {
        return Optional.ofNullable(logCollectConfig).map(LogCollectConfig::getLogFieldSwitchConfig)
                .orElse(DEFAULT_LOG_FIELD_SWITCH_CONFIG);
    }

    /**
     * get global log config.
     *
     * @return global log config
     */
    public static LogCollectConfig.GlobalLogConfig getGlobalLogConfig() {
        return Optional.ofNullable(logCollectConfig).map(LogCollectConfig::getGlobalLogConfig)
                .orElse(DEFAULT_GLOBAL_LOG_CONFIG);
    }

    /**
     * get message queue topic.
     *
     * @param path request path
     * @return topic
     */
    public static String getTopic(final String path) {
        if (StringUtils.isBlank(path)) {
            return "";
        }
        Map<String, LogCollectConfig.LogApiConfig> apiConfigMap = logCollectConfig.getLogApiSwitchConfigMap();
        if (MapUtils.isEmpty(apiConfigMap)) {
            return "";
        }
        for (Map.Entry<String, LogCollectConfig.LogApiConfig> entry : apiConfigMap.entrySet()) {
            String pattern = entry.getKey().replace("[", "").replace("]", "");
            if (MATCHER.match(pattern, path)) {
                return entry.getValue().getTopic();
            }
        }
        return "";
    }
}
