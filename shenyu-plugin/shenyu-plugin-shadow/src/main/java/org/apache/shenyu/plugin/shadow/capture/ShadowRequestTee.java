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

package org.apache.shenyu.plugin.shadow.capture;

import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;

/**
 * Decorates the primary request so the shadow copy observes its body.
 *
 * <p>The decorator only observes: it does not buffer, delay or release the
 * primary buffers, and it is built once per exchange so every subscription
 * sees the same teed stream.</p>
 */
public final class ShadowRequestTee extends ServerHttpRequestDecorator {

    private final Flux<DataBuffer> body;

    public ShadowRequestTee(final ServerHttpRequest delegate, final ShadowBodyCapture capture) {
        super(delegate);
        this.body = super.getBody().doOnNext(capture::append);
    }

    @Override
    public Flux<DataBuffer> getBody() {
        return body;
    }

    /**
     * Wrap the exchange with a teed request body.
     *
     * @param exchange the primary exchange
     * @param capture the bounded body capture
     * @return an exchange whose request body is observed by the capture
     */
    public static ServerWebExchange tee(final ServerWebExchange exchange, final ShadowBodyCapture capture) {
        return exchange.mutate().request(new ShadowRequestTee(exchange.getRequest(), capture)).build();
    }
}
