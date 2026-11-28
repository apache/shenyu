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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.web.server.ServerWebExchange;

import java.util.Objects;
import java.util.Optional;

/**
 * Decides whether one request may be mirrored at all.
 *
 * <p>Eligibility is deliberately conservative: only supported HTTP requests
 * whose method an operator named are candidates. Body bounds are checked later,
 * while the body is observed.</p>
 */
public final class ShadowEligibility {

    private static final String UPGRADE = "Upgrade";

    private static final String WEBSOCKET = "websocket";

    private ShadowEligibility() {
    }

    /**
     * Check whether the request may be mirrored.
     *
     * @param exchange the primary exchange
     * @param handle the validated rule handle
     * @return the first skip reason, or empty when the request is eligible
     */
    public static Optional<ShadowSkipReason> check(final ServerWebExchange exchange, final ShadowHandle handle) {
        ShenyuContext context = exchange.getAttribute(Constants.CONTEXT);
        if (Objects.isNull(context) || !RpcTypeEnum.HTTP.getName().equals(context.getRpcType())) {
            return Optional.of(ShadowSkipReason.NOT_HTTP);
        }
        HttpMethod method = exchange.getRequest().getMethod();
        if (Objects.isNull(method) || !isAllowedMethod(handle, method)) {
            return Optional.of(ShadowSkipReason.METHOD_NOT_ALLOWED);
        }
        if (isStreaming(exchange.getRequest().getHeaders())) {
            return Optional.of(ShadowSkipReason.STREAMING_REQUEST);
        }
        return Optional.empty();
    }

    private static boolean isAllowedMethod(final ShadowHandle handle, final HttpMethod method) {
        return handle.getMethods().stream().anyMatch(name -> name.equalsIgnoreCase(method.name()));
    }

    private static boolean isStreaming(final HttpHeaders headers) {
        if (WEBSOCKET.equalsIgnoreCase(headers.getUpgrade())) {
            return true;
        }
        return headers.getAccept().stream().anyMatch(MediaType.TEXT_EVENT_STREAM::isCompatibleWith);
    }
}
