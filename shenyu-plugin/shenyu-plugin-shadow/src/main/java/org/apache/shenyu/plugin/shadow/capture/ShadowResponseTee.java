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

import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * Decorates the primary response so the shadow copy observes its body.
 *
 * <p>The decorator only observes: it does not buffer, delay or release the
 * primary buffers, and the primary response stays authoritative.</p>
 */
public final class ShadowResponseTee extends ServerHttpResponseDecorator {

    private final ShadowBodyCapture capture;

    public ShadowResponseTee(final ServerHttpResponse delegate, final ShadowBodyCapture capture) {
        super(delegate);
        this.capture = capture;
    }

    @Override
    public Mono<Void> writeWith(final Publisher<? extends DataBuffer> body) {
        return super.writeWith(Flux.from(body).doOnNext(capture::append));
    }

    @Override
    public Mono<Void> writeAndFlushWith(final Publisher<? extends Publisher<? extends DataBuffer>> body) {
        return super.writeAndFlushWith(Flux.from(body)
                .map(chunk -> Flux.from(chunk).doOnNext(capture::append)));
    }

    /**
     * Wrap the exchange with a teed response body.
     *
     * @param exchange the primary exchange
     * @param capture the bounded body capture
     * @return an exchange whose response body is observed by the capture
     */
    public static ServerWebExchange tee(final ServerWebExchange exchange, final ShadowBodyCapture capture) {
        return exchange.mutate().response(new ShadowResponseTee(exchange.getResponse(), capture)).build();
    }
}
