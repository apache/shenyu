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

package org.apache.shenyu.plugin.api.utils;

import org.apache.shenyu.common.constant.Constants;
import org.springframework.web.server.ServerWebExchange;

/**
 * The trace id utils.
 */
public final class TraceIdUtils {

    private TraceIdUtils() {
    }

    /**
     * Acquires the trace id of the current request.
     *
     * <p>The id is filled by the tracing plugin. While no span is active, for example because
     * tracing is disabled, the WebFlux log prefix is returned instead, so that a request scoped
     * prefix is always available to the log.</p>
     *
     * @param exchange the current exchange
     * @return the trace id of the request
     */
    public static String traceId(final ServerWebExchange exchange) {
        final Object traceId = exchange.getAttributes().get(Constants.SHENYU_AGENT_TRACE_ID);
        if (traceId instanceof String) {
            return (String) traceId;
        }
        return exchange.getLogPrefix();
    }
}
