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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.common.sampler.CountSampler;
import org.apache.shenyu.plugin.logging.common.sampler.Sampler;
import org.springframework.web.server.ServerWebExchange;

import java.util.Objects;
import java.util.Optional;

/**
 * log collect config Utils.
 */
public final class LogCollectConfigUtils {

    private static GenericGlobalConfig genericGlobalConfig = new GenericGlobalConfig();

    private static Sampler globalSampler = Sampler.ALWAYS_SAMPLE;

    private LogCollectConfigUtils() {
    }

    /**
     * set api sample.
     *
     * @param sampler sample
     * @return Sampler
     */
    public static Sampler setSampler(final String sampler) {
        if (StringUtils.isBlank(sampler)) {
            return globalSampler;
        } else {
            return CountSampler.create(sampler);
        }
    }

    /**
     * set global Sampler.
     *
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
     * judge whether request body too large.
     *
     * @param bodySize body size
     * @return whether request body too large
     */
    public static boolean isRequestBodyTooLarge(final int bodySize) {
        if (Objects.isNull(genericGlobalConfig)) {
            return false;
        }
        return bodySize > genericGlobalConfig.getMaxRequestBody();
    }

    /**
     * judge whether response body too large.
     *
     * @param bodySize body size.
     * @return whether response body too large
     */
    public static boolean isResponseBodyTooLarge(final int bodySize) {
        if (Objects.isNull(genericGlobalConfig)) {
            return false;
        }
        return bodySize > genericGlobalConfig.getMaxResponseBody();
    }

    /**
     * judge whether sample.
     *
     * @param exchange     exchange
     * @param selectorData selectorData
     * @return whether sample
     */
    public static boolean isSampled(final ServerWebExchange exchange, final SelectorData selectorData) {
        return Optional.ofNullable(AbstractLogPluginDataHandler.getSelectApiConfigMap().get(selectorData.getId()))
                .map(GenericApiConfig::getSampler)
                .map(sampler -> sampler.isSampled(exchange, selectorData))
                .orElseGet(() -> Optional.ofNullable(AbstractLogPluginDataHandler.getPluginGlobalConfigMap().get(selectorData.getPluginId()))
                        .map(GenericGlobalConfig::getSampler)
                        .map(sampler -> sampler.isSampled(exchange, selectorData))
                        .orElse(true));
    }
}
