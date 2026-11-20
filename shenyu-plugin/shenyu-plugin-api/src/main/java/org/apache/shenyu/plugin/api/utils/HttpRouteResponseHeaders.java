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

import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;

import java.util.Objects;

/** Keeps the gateway's response headers across the handoff from a timed route to its upstream. */
public final class HttpRouteResponseHeaders {

    public static final String ATTRIBUTE = HttpRouteResponseHeaders.class.getName();

    private HttpHeaders original;

    private boolean timedOut;

    /**
     * Apply upstream response metadata, saving the gateway headers before the first update.
     * @param exchange current exchange
     * @param update header and status changes from the upstream
     */
    public static void apply(final ServerWebExchange exchange, final Runnable update) {
        HttpRouteResponseHeaders state = exchange.getAttribute(ATTRIBUTE);
        if (Objects.isNull(state)) {
            update.run();
        } else {
            state.apply(exchange.getResponse().getHeaders(), update);
        }
    }

    private synchronized void apply(final HttpHeaders headers, final Runnable update) {
        if (!timedOut) {
            if (Objects.isNull(original)) {
                original = new HttpHeaders();
                original.addAll(headers);
            }
            update.run();
        }
    }

    /**
     * Restore the uncommitted gateway headers and prevent late upstream callbacks from reapplying them.
     * @param exchange current exchange
     */
    public synchronized void restore(final ServerWebExchange exchange) {
        timedOut = true;
        if (Objects.nonNull(original) && !exchange.getResponse().isCommitted()) {
            HttpHeaders headers = exchange.getResponse().getHeaders();
            headers.clear();
            headers.putAll(original);
        }
    }
}
