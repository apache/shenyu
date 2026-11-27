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

package org.apache.shenyu.plugin.api.observation;

import org.apache.shenyu.common.metrics.PluginObservationEvent;
import org.springframework.web.server.ServerWebExchange;

import java.util.concurrent.atomic.AtomicInteger;

/** Internal correlation bridge shared by the legacy chain and actual matching adapter. */
public final class PluginObservationContext {

    public static final String KEY = PluginObservationContext.class.getName();

    private static final AtomicInteger ACTIVE = new AtomicInteger();

    private PluginObservationContext() {
    }

    /** Register one explicitly enabled owner at startup. */
    public static void activate() {
        ACTIVE.incrementAndGet();
    }

    /** Release one owner at shutdown. */
    public static void deactivate() {
        ACTIVE.decrementAndGet();
    }

    /**
     * Read the actual construction invocation only when observation is enabled and sampled.
     * @param exchange framework-owned exchange, never passed to exporters
     * @return invocation or null; no event/matching allocation on the disabled path
     */
    public static Invocation current(final ServerWebExchange exchange) {
        if (ACTIVE.get() == 0) {
            return null;
        }
        Object value = exchange.getAttributes().get(KEY);
        return value instanceof Invocation ? (Invocation) value : null;
    }

    /** Exporters see immutable summaries, not this internal execution correlation object. */
    public interface Invocation {

        /**
         * Whether this invocation still has a matching record allowance.
         * @return whether to allocate a summary
         */
        boolean acceptsMatching();

        /**
         * Offer the matching decision actually made.
         * @param matching immutable bounded summary
         */
        void matching(PluginObservationEvent.Matching matching);
    }
}
