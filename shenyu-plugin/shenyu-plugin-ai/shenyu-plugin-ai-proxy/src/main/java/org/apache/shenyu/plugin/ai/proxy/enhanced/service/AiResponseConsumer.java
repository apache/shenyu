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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

/**
 * Consumes one upstream response while tracking whether response bytes have crossed the client response boundary.
 */
final class AiResponseConsumer {

    /**
     * Consume an upstream response without losing the first body signal.
     *
     * <p>The emitted flag changes only after a response body element is observed. Once set, the execution
     * coordinator must not start another upstream attempt.</p>
     *
     * @param response upstream response
     * @param consumer client response consumer
     * @param emitted whether response bytes have crossed the response boundary
     * @return response consumption completion
     */
    Mono<Void> consume(final AiUpstreamResponse response,
                       final Function<AiUpstreamResponse, Mono<Void>> consumer,
                       final AtomicBoolean emitted) {
        Objects.requireNonNull(response, "response must not be null");
        Objects.requireNonNull(consumer, "consumer must not be null");
        Objects.requireNonNull(emitted, "emitted must not be null");

        return response.getBody()
                .switchOnFirst((signal, body) -> {
                    if (signal.isOnError()) {
                        return Flux.error(Objects.requireNonNull(signal.getThrowable()));
                    }
                    AiUpstreamResponse ready = new AiUpstreamResponse(
                            response.getStatusCode(),
                            response.getHeaders(),
                            body.doOnNext(bytes -> emitted.set(true)));
                    return Mono.defer(() -> consumer.apply(ready)).flux();
                })
                .then();
    }
}
