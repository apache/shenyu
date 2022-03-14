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

package org.apache.shenyu.agent.plugin.logging.common.utils;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.agent.api.config.LogCollectConfig;
import org.apache.shenyu.agent.core.locator.ShenyuAgentLocator;
import org.apache.shenyu.agent.core.yaml.ShenyuYamlEngine;
import org.apache.shenyu.agent.plugin.logging.common.sampler.Sampler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

    private static final Logger LOG = LoggerFactory.getLogger(LogCollectConfigUtils.class);

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
     * init logging config.
     */
    public static void init() {
        try {
            logCollectConfig = ShenyuYamlEngine.unmarshal(ShenyuAgentLocator.locatorConf("logging-meta.yaml"),
                    LogCollectConfig.class);
            if (MapUtils.isNotEmpty(logCollectConfig.getLogApiSwitchConfigMap())) {
                logCollectConfig.getLogApiSwitchConfigMap().forEach((path, config) -> {
                    if (StringUtils.isBlank(config.getSampleRate())) {
                        API_SAMPLER_MAP.put(path, globalSampler);
                    } else {
                        API_SAMPLER_MAP.put(path, Sampler.create(config.getSampleRate()));
                    }
                });
            }
            if (Objects.nonNull(logCollectConfig.getGlobalLogConfig())) {
                globalSampler = Sampler.create(logCollectConfig.getGlobalLogConfig().getSampleRate());
            }

        } catch (Exception e) {
            LOG.error("Exception loader logging meta data config error", e);
        }
    }

    /**
     * judge whether sample.
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
     * @return  log field switch config
     */
    public static LogCollectConfig.LogFieldSwitchConfig getLogFieldSwitchConfig() {
        return Optional.ofNullable(logCollectConfig).map(LogCollectConfig::getLogFieldSwitchConfig)
                .orElse(DEFAULT_LOG_FIELD_SWITCH_CONFIG);
    }

    /**
     * get global log config.
     * @return global log config
     */
    public static LogCollectConfig.GlobalLogConfig getGlobalLogConfig() {
        return Optional.ofNullable(logCollectConfig).map(LogCollectConfig::getGlobalLogConfig)
                .orElse(DEFAULT_GLOBAL_LOG_CONFIG);
    }

    /**
     * get message queue topic.
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
