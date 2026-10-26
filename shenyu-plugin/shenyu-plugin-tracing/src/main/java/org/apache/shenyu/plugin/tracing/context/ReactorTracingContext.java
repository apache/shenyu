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

package org.apache.shenyu.plugin.tracing.context;

import io.opentelemetry.context.Context;
import org.apache.shenyu.plugin.tracing.TracingConstants;
import org.springframework.web.server.ServerWebExchange;
import reactor.util.context.ContextView;

import java.util.Objects;

/**
 * Carries the active tracing context of the current request.
 *
 * <p>The context is stored in the exchange attributes, which every plugin of the request shares and
 * which are not bound to a thread, and it is written into the Reactor context as well, so that
 * operators which switch threads, for example while the gateway scheduler is enabled, can restore
 * it without relying on a thread local.</p>
 *
 * <p>The attributes are intentionally kept until the exchange itself is collected: readers which run
 * after the plugin chain has terminated, for example the error log of a failed request, still need
 * the trace of that request. A request which is processed again overwrites them, so the attributes
 * always describe the latest attempt.</p>
 */
public final class ReactorTracingContext {

    private ReactorTracingContext() {
    }

    /**
     * Stores the active tracing context of the given exchange.
     *
     * @param exchange the current exchange
     * @param tracingContext the tracing context to store
     */
    public static void store(final ServerWebExchange exchange, final Context tracingContext) {
        exchange.getAttributes().put(TracingConstants.TRACING_CONTEXT, tracingContext);
    }

    /**
     * Acquires the active tracing context of the given exchange.
     *
     * @param exchange the current exchange
     * @return the tracing context, or the root context when tracing is not active
     */
    public static Context current(final ServerWebExchange exchange) {
        Object tracingContext = exchange.getAttributes().get(TracingConstants.TRACING_CONTEXT);
        if (tracingContext instanceof Context) {
            return (Context) tracingContext;
        }
        return Context.root();
    }

    /**
     * Acquires the active tracing context from the Reactor context.
     *
     * @param view the Reactor context view
     * @return the tracing context, or the root context when tracing is not active
     */
    public static Context current(final ContextView view) {
        if (Objects.isNull(view)) {
            return Context.root();
        }
        return view.getOrDefault(TracingConstants.REACTOR_CONTEXT_KEY, Context.root());
    }
}
