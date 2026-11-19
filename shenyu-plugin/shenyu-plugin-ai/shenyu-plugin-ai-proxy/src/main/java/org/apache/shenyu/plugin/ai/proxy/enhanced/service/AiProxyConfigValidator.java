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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;

import java.net.URI;
import java.util.Objects;

/**
 * Field constraints shared by partial defaults and resolved request configuration.
 */
public final class AiProxyConfigValidator {

    private AiProxyConfigValidator() {
    }

    /**
     * Validate optional fields without changing their values or requiring a complete configuration.
     *
     * @param config configuration to validate
     * @throws IllegalArgumentException with a fixed, input-free reason when a field is invalid
     */
    public static void validate(final AiCommonConfig config) {
        if (Objects.nonNull(config.getMaxTokens()) && config.getMaxTokens() <= 0) {
            throw new IllegalArgumentException("maxTokens.nonPositive");
        }
        if (Objects.nonNull(config.getTemperature())) {
            if (!Double.isFinite(config.getTemperature())) {
                throw new IllegalArgumentException("temperature.notFinite");
            }
            if (config.getTemperature() < 0) {
                throw new IllegalArgumentException("temperature.negative");
            }
        }
        if (StringUtils.isNotBlank(config.getBaseUrl())) {
            parseBaseUrl(config.getBaseUrl());
        }
    }

    /**
     * Parse an HTTP(S) base endpoint without credentials, query or fragment.
     *
     * @param baseUrl required base endpoint
     * @return parsed endpoint without changing its value
     * @throws IllegalArgumentException with a fixed, input-free reason if the endpoint is invalid
     */
    public static URI parseBaseUrl(final String baseUrl) {
        if (StringUtils.isBlank(baseUrl)) {
            throw new IllegalArgumentException("baseUrl.invalidEndpoint");
        }
        final URI uri;
        try {
            uri = URI.create(baseUrl);
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("baseUrl.invalidSyntax");
        }
        if (!("http".equalsIgnoreCase(uri.getScheme()) || "https".equalsIgnoreCase(uri.getScheme()))
                || Objects.isNull(uri.getHost()) || Objects.nonNull(uri.getRawUserInfo())
                || Objects.nonNull(uri.getRawQuery()) || Objects.nonNull(uri.getRawFragment()) || uri.getPort() == 0 || uri.getPort() > 65535) {
            throw new IllegalArgumentException("baseUrl.invalidEndpoint");
        }
        return uri;
    }
}
