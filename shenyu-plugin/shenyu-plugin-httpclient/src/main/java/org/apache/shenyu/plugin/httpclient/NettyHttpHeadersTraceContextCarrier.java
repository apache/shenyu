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

import io.netty.handler.codec.http.HttpHeaders;
import org.apache.shenyu.plugin.api.tracing.TraceContextCarrier;

/**
 * The {@link TraceContextCarrier} of the outgoing request which the netty http client builds, whose
 * headers are the headers of netty rather than the ones of spring.
 */
final class NettyHttpHeadersTraceContextCarrier implements TraceContextCarrier {

    private final HttpHeaders headers;

    /**
     * Instantiates a new netty http headers trace context carrier.
     *
     * @param headers the headers of the outgoing request
     */
    NettyHttpHeadersTraceContextCarrier(final HttpHeaders headers) {
        this.headers = headers;
    }

    @Override
    public void set(final String name, final String value) {
        headers.set(name, value);
    }

    @Override
    public void remove(final String name) {
        headers.remove(name);
    }
}
