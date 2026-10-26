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

package org.apache.shenyu.plugin.api.tracing;

import org.apache.shenyu.spi.SPI;
import org.springframework.web.server.ServerWebExchange;

/**
 * Injects the trace context of a request into the outgoing call to the upstream service, so that the
 * upstream continues the same trace instead of starting an unrelated one.
 *
 * <p>A protocol plugin injects the context of its own protocol: the http client plugin hands an http
 * carrier to the propagator which is registered under {@link #HTTP}, a grpc or dubbo plugin would
 * hand its own carrier to the propagator of that protocol. The plugin of a protocol which does not
 * have a propagator simply injects nothing, and tracing stays optional for the deployment.</p>
 */
@SPI
public interface TraceContextPropagator {

    /**
     * The extension name of the propagator which injects into the http headers.
     */
    String HTTP = "http";

    /**
     * Injects the trace context of the given exchange into the carrier of the outgoing call.
     *
     * <p>The implementation decides whether the exchange has a trace context at all, and it is
     * responsible for not leaving a stale trace context of the client in the carrier. The trace
     * context itself, the traceparent and the trace state, belongs to the trace and is therefore
     * rebuilt from the context of the gateway, while the entries which are not trace scoped, such as
     * the W3C baggage, are forwarded as the client sent them.</p>
     *
     * @param exchange the current exchange
     * @param carrier the carrier of the outgoing call
     */
    void inject(ServerWebExchange exchange, TraceContextCarrier carrier);
}
