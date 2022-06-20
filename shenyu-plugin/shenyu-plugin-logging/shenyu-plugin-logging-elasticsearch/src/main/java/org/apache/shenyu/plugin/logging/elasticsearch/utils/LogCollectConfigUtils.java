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

package org.apache.shenyu.plugin.logging.elasticsearch.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.logging.elasticsearch.config.LogCollectConfig.GlobalLogConfig;
import org.apache.shenyu.plugin.logging.elasticsearch.sampler.CountSampler;
import org.apache.shenyu.plugin.logging.elasticsearch.sampler.Sampler;
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

    private static GlobalLogConfig globalLogConfig;

    private static final GlobalLogConfig DEFAULT_GLOBAL_LOG_CONFIG =
            new GlobalLogConfig();

    private static Map<String, Sampler> apiSamplerMap = new HashMap<>();

    private static Sampler globalSampler = Sampler.ALWAYS_SAMPLE;

    private LogCollectConfigUtils() {
    }

    /**
     * set global config.
     * @param config global config
     */
    public static void setGlobalConfig(final GlobalLogConfig config) {
        globalLogConfig = config;
    }

    /**
     * set api sample.
     * @param uriSampleMap api sample map
     */
    public static void setSampler(final Map<String, String> uriSampleMap) {
        Map<String, Sampler> samplerMap = new HashMap<>();
        uriSampleMap.forEach((path, sampler) -> {
            if (StringUtils.isBlank(sampler)) {
                samplerMap.put(path, globalSampler);
            } else {
                samplerMap.put(path, CountSampler.create(sampler));
            }
        });
        apiSamplerMap = samplerMap;
    }

    /**
     * set global Sampler.
     * @param sampler global sampler
     */
    public static void setGlobalSampler(final String sampler) {
        if (StringUtils.isNotBlank(sampler)) {
            try {
                globalSampler = CountSampler.create(sampler);
            } catch (Exception e) {
                globalSampler = Sampler.ALWAYS_SAMPLE;
            }
        }
    }

    /**
     * judge whether sample.
     *
     * @param request request
     * @return whether sample
     */
    public static boolean isSampled(final ServerHttpRequest request) {
        String path = request.getURI().getPath();
        for (Map.Entry<String, Sampler> entry : apiSamplerMap.entrySet()) {
            String pattern = entry.getKey();
            if (MATCHER.match(pattern, path)) {
                return entry.getValue().isSampled(request);
            }
        }
        if (globalSampler != null) {
            return globalSampler.isSampled(request);
        }
        return true;
    }

    /**
     * judge whether request org.apache.shenyu.plugin.logging.body too large.
     *
     * @param bodySize org.apache.shenyu.plugin.logging.body size
     * @return whether request org.apache.shenyu.plugin.logging.body too large
     */
    public static boolean isRequestBodyTooLarge(final int bodySize) {
        if (Objects.isNull(globalLogConfig)) {
            return false;
        }
        return bodySize > globalLogConfig.getMaxRequestBody();
    }

    /**
     * judge whether response org.apache.shenyu.plugin.logging.body too large.
     *
     * @param bodySize org.apache.shenyu.plugin.logging.body size.
     * @return whether response org.apache.shenyu.plugin.logging.body too large
     */
    public static boolean isResponseBodyTooLarge(final int bodySize) {
        if (Objects.isNull(globalLogConfig)) {
            return false;
        }
        return bodySize > globalLogConfig.getMaxResponseBody();
    }

    /**
     * get global log config.
     *
     * @return global log config
     */
    public static GlobalLogConfig getGlobalLogConfig() {
        return Optional.ofNullable(globalLogConfig).orElse(DEFAULT_GLOBAL_LOG_CONFIG);
    }
}
