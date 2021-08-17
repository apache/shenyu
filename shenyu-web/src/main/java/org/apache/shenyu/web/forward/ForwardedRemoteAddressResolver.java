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

package org.apache.shenyu.web.forward;

import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Parses the client address from the X-Forwarded-For header. If header is not present.
 * falls back to {@link RemoteAddressResolver} and
 * {@link ServerHttpRequest#getRemoteAddress()}. Use the static constructor methods which
 * meets your security requirements.
 */
public class ForwardedRemoteAddressResolver implements RemoteAddressResolver {
    /**
     * Forwarded-For header name.
     */
    public static final String X_FORWARDED_FOR = "X-Forwarded-For";

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ForwardedRemoteAddressResolver.class);

    private final RemoteAddressResolver defaultRemoteIpResolver = new RemoteAddressResolver() {
    };

    private final int maxTrustedIndex;

    /**
     * Instantiates a new Forwarded remote address resolver.
     *
     * @param maxTrustedIndex the max trusted index
     */
    public ForwardedRemoteAddressResolver(final int maxTrustedIndex) {
        this.maxTrustedIndex = maxTrustedIndex;
    }

    /**
     * Trust all forwarded remote address resolver.
     *
     * @return the forwarded remote address resolver
     */
    public static ForwardedRemoteAddressResolver trustAll() {
        return new ForwardedRemoteAddressResolver(Integer.MAX_VALUE);
    }

    /**
     * Max trusted index forwarded remote address resolver.
     *
     * @param maxTrustedIndex the max trusted index
     * @return the forwarded remote address resolver
     */
    public static ForwardedRemoteAddressResolver maxTrustedIndex(final int maxTrustedIndex) {
        Assert.isTrue(maxTrustedIndex > 0, "An index greater than 0 is required");
        return new ForwardedRemoteAddressResolver(maxTrustedIndex);
    }

    @Override
    public InetSocketAddress resolve(final ServerWebExchange exchange) {
        List<String> xForwardedValues = extractXForwardedValues(exchange);
        Collections.reverse(xForwardedValues);
        if (!xForwardedValues.isEmpty()) {
            int index = Math.min(xForwardedValues.size(), maxTrustedIndex) - 1;
            return new InetSocketAddress(xForwardedValues.get(index), 0);
        }
        return defaultRemoteIpResolver.resolve(exchange);
    }

    private List<String> extractXForwardedValues(final ServerWebExchange exchange) {
        List<String> xForwardedValues = exchange.getRequest().getHeaders()
                .get(X_FORWARDED_FOR);
        if (xForwardedValues == null || xForwardedValues.isEmpty()) {
            return Collections.emptyList();
        }
        if (xForwardedValues.size() > 1) {
            LOG.warn("Multiple X-Forwarded-For headers found, discarding all");
            return Collections.emptyList();
        }
        List<String> values = Arrays.asList(xForwardedValues.get(0).split(", "));
        if (values.size() == 1 && !StringUtils.hasText(values.get(0))) {
            return Collections.emptyList();
        }
        return values;
    }
}
