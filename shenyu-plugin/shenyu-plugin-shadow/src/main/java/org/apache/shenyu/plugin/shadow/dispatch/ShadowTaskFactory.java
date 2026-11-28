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

package org.apache.shenyu.plugin.shadow.dispatch;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Builds the immutable shadow call from the observed primary exchange.
 */
public final class ShadowTaskFactory {

    /**
     * Marks a request as shadow traffic at the destination.
     */
    public static final String SHADOW_HEADER = "X-Shenyu-Shadow";

    private ShadowTaskFactory() {
    }

    /**
     * Build one shadow call.
     *
     * <p>Only the configured headers are forwarded. Cookies, authorization and
     * hop-by-hop headers are never copied implicitly, so a shadow target cannot
     * receive production credentials by default.</p>
     *
     * @param exchange the observed exchange
     * @param handle the validated rule handle
     * @param body the bounded body copy
     * @return the immutable shadow call, or empty when the target cannot be combined with the request path
     */
    public static Optional<ShadowTask> create(final ServerWebExchange exchange, final ShadowHandle handle,
                                              final byte[] body) {
        ServerHttpRequest request = exchange.getRequest();
        return targetUri(exchange, handle.getTarget())
                .map(uri -> new ShadowTask(request.getMethod().name(), uri,
                        headers(request.getHeaders(), handle), body, handle.getTimeoutMillis()));
    }

    /**
     * Combine the shadow target with the effective request path and query.
     *
     * <p>The primary URI resolved by the {@code uri} plugin at order 205 wins:
     * it already prefers {@code REWRITE_URI} and carries the final query, so a
     * rewrite-only rule mirrors the path the primary actually forwards to. The
     * rewritten path helper is only a fallback for chains where that plugin did
     * not resolve a URI. The raw components are concatenated and parsed with
     * {@link URI#create(String)}, which never re-encodes them, so an already
     * encoded path such as {@code /a%2Fb} is preserved exactly.</p>
     */
    private static Optional<URI> targetUri(final ServerWebExchange exchange, final String target) {
        try {
            URI base = URI.create(target);
            URI primary = exchange.getAttribute(Constants.HTTP_URI);
            String primaryPath = Objects.isNull(primary)
                    ? RequestUrlUtils.getRewrittenRawPath(exchange) : primary.getRawPath();
            String primaryQuery = Objects.isNull(primary)
                    ? exchange.getRequest().getURI().getRawQuery() : primary.getRawQuery();
            StringBuilder composed = new StringBuilder(base.getScheme()).append("://")
                    .append(base.getRawAuthority())
                    .append(joinPath(base.getRawPath(), primaryPath));
            if (StringUtils.isNotEmpty(primaryQuery)) {
                composed.append('?').append(primaryQuery);
            }
            return Optional.of(URI.create(composed.toString()));
        } catch (IllegalArgumentException e) {
            return Optional.empty();
        }
    }

    private static String joinPath(final String basePath, final String requestPath) {
        String left = Objects.requireNonNullElse(basePath, "");
        String right = Objects.requireNonNullElse(requestPath, "");
        if (left.endsWith("/") && right.startsWith("/")) {
            return left + right.substring(1);
        }
        return left + right;
    }

    private static HttpHeaders headers(final HttpHeaders source, final ShadowHandle handle) {
        HttpHeaders result = new HttpHeaders();
        for (String name : handle.getForwardHeaders()) {
            List<String> values = source.get(name);
            if (Objects.nonNull(values) && !values.isEmpty()) {
                result.put(name, new ArrayList<>(values));
            }
        }
        Map<String, String> extra = handle.getExtraHeaders();
        if (Objects.nonNull(extra)) {
            extra.forEach(result::set);
        }
        result.set(SHADOW_HEADER, Boolean.TRUE.toString());
        return result;
    }
}
