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

package org.apache.shenyu.plugin.httpclient;

import org.apache.shenyu.plugin.api.tracing.TraceContextCarrier;
import org.apache.shenyu.plugin.api.tracing.TraceContextPropagator;
import org.apache.shenyu.spi.Join;
import org.springframework.web.server.ServerWebExchange;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * A propagator which stands in for the tracing implementation, so that the http client plugin can be
 * verified without depending on the tracing module.
 */
@Join
public class TestTraceContextPropagator implements TraceContextPropagator {

    /**
     * The traceparent which the propagator writes into the carrier.
     */
    static final String INJECTED_TRACEPARENT = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01";

    private static final AtomicInteger INJECTIONS = new AtomicInteger();

    @Override
    public void inject(final ServerWebExchange exchange, final TraceContextCarrier carrier) {
        INJECTIONS.incrementAndGet();
        // the same contract as the tracing propagator: the context of the client is replaced
        carrier.remove("traceparent");
        carrier.set("traceparent", INJECTED_TRACEPARENT);
    }

    /**
     * Forgets the injections of the previous test case.
     */
    static void reset() {
        INJECTIONS.set(0);
    }

    /**
     * Acquires the number of injections.
     *
     * @return the number of injections
     */
    static int injections() {
        return INJECTIONS.get();
    }
}
