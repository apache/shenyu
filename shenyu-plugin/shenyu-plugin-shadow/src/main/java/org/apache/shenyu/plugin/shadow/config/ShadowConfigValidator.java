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

package org.apache.shenyu.plugin.shadow.config;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Validates a shadow rule handle before it is cached.
 *
 * <p>An invalid rule is never partially applied: the rule is removed from the
 * runtime registry and the plugin passes traffic through untouched.</p>
 */
public final class ShadowConfigValidator {

    private static final String HTTP_SCHEME = "http";

    private static final String HTTPS_SCHEME = "https";

    private static final int MIN_CONCURRENT = 1;

    private static final int MIN_TIMEOUT_MILLIS = 1;

    private static final double MIN_RATE = 0D;

    private static final double MAX_RATE = 1D;

    private ShadowConfigValidator() {
    }

    /**
     * Validate a parsed handle.
     *
     * @param handle the parsed rule handle
     * @return the first problem, or empty when the handle is usable
     */
    public static Optional<String> validate(final ShadowHandle handle) {
        if (Objects.isNull(handle)) {
            return Optional.of("shadow handle is missing");
        }
        if (!Boolean.TRUE.equals(handle.getEnabled())) {
            return Optional.empty();
        }
        return validateTarget(handle.getTarget())
                .or(() -> validateMethods(handle.getMethods()))
                .or(() -> validateRate(handle.getSampleRate()))
                .or(() -> validateBounds(handle))
                .or(() -> validateHeaders(handle.getExtraHeaders()));
    }

    private static Optional<String> validateTarget(final String target) {
        if (StringUtils.isBlank(target)) {
            return Optional.of("shadow target is required");
        }
        final URI uri;
        try {
            uri = new URI(target);
        } catch (URISyntaxException e) {
            return Optional.of("shadow target is not a valid URI");
        }
        if (!uri.isAbsolute() || Objects.isNull(uri.getHost())) {
            return Optional.of("shadow target must be an absolute URI with a host");
        }
        if (!HTTP_SCHEME.equalsIgnoreCase(uri.getScheme()) && !HTTPS_SCHEME.equalsIgnoreCase(uri.getScheme())) {
            return Optional.of("shadow target must use http or https");
        }
        if (Objects.nonNull(uri.getUserInfo())) {
            return Optional.of("shadow target must not carry user info");
        }
        if (Objects.nonNull(uri.getFragment())) {
            return Optional.of("shadow target must not carry a fragment");
        }
        if (Objects.nonNull(uri.getRawQuery())) {
            return Optional.of("shadow target must not carry a query");
        }
        return Optional.empty();
    }

    private static Optional<String> validateMethods(final List<String> methods) {
        if (Objects.isNull(methods) || methods.isEmpty()) {
            return Optional.of("shadow methods must not be empty");
        }
        return methods.stream().anyMatch(StringUtils::isBlank)
                ? Optional.of("shadow methods must not contain blank names") : Optional.empty();
    }

    private static Optional<String> validateRate(final Double rate) {
        if (Objects.isNull(rate) || rate < MIN_RATE || rate > MAX_RATE) {
            return Optional.of("shadow sampleRate must be within [0, 1]");
        }
        return Optional.empty();
    }

    private static Optional<String> validateBounds(final ShadowHandle handle) {
        if (Objects.isNull(handle.getMaxConcurrent()) || handle.getMaxConcurrent() < MIN_CONCURRENT) {
            return Optional.of("shadow maxConcurrent must be at least " + MIN_CONCURRENT);
        }
        if (Objects.isNull(handle.getQueueCapacity()) || handle.getQueueCapacity() < 0) {
            return Optional.of("shadow queueCapacity must not be negative");
        }
        if (Objects.isNull(handle.getTimeoutMillis()) || handle.getTimeoutMillis() < MIN_TIMEOUT_MILLIS) {
            return Optional.of("shadow timeoutMillis must be at least " + MIN_TIMEOUT_MILLIS);
        }
        if (Objects.isNull(handle.getMaxRequestBodyBytes()) || handle.getMaxRequestBodyBytes() < 0) {
            return Optional.of("shadow maxRequestBodyBytes must not be negative");
        }
        if (Objects.isNull(handle.getMaxResponseBodyBytes()) || handle.getMaxResponseBodyBytes() < 0) {
            return Optional.of("shadow maxResponseBodyBytes must not be negative");
        }
        if (Objects.isNull(handle.getForwardHeaders())) {
            return Optional.of("shadow forwardHeaders must not be null");
        }
        if (Objects.isNull(handle.getCompareHeaders())) {
            return Optional.of("shadow compareHeaders must not be null");
        }
        if (Objects.isNull(handle.getIgnoreJsonPaths())) {
            return Optional.of("shadow ignoreJsonPaths must not be null");
        }
        return Optional.empty();
    }

    private static Optional<String> validateHeaders(final Map<String, String> headers) {
        if (Objects.isNull(headers)) {
            return Optional.empty();
        }
        for (Map.Entry<String, String> entry : headers.entrySet()) {
            if (hasLineBreak(entry.getKey()) || hasLineBreak(entry.getValue())) {
                return Optional.of("shadow header contains a line break");
            }
        }
        return Optional.empty();
    }

    private static boolean hasLineBreak(final String value) {
        return Objects.nonNull(value) && (value.indexOf('\r') >= 0 || value.indexOf('\n') >= 0);
    }
}
