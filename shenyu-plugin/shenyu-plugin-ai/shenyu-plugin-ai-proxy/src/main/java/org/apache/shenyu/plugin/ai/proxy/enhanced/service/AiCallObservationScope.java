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

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.observation.AiObservationSupport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * Establishes and completes logical-call observation around an AI exchange.
 */
final class AiCallObservationScope {

    private final AiCallObserver observer;

    AiCallObservationScope(final AiCallObserver observer) {
        this.observer = observer;
    }

    Mono<Void> observe(
            final Function<AiUpstreamResponse, Mono<Void>> responseConsumer,
            final Function<Function<AiUpstreamResponse, Mono<Void>>, Mono<Void>> exchange) {
        return Mono.deferContextual(view -> {
            AiRequestObservation inherited =
                    view.getOrDefault(AiRequestObservation.REACTOR_KEY, null);
            AiRequestObservation observation = Objects.nonNull(inherited)
                    ? inherited
                    : new AiRequestObservation(
                    observer,
                    AiObservationSupport.context(view, null, null));
            AtomicReference<Integer> status = new AtomicReference<>();

            Function<AiUpstreamResponse, Mono<Void>> observedConsumer = response -> {
                status.set(response.getStatusCode());
                return responseConsumer.apply(response);
            };

            return Mono.defer(() -> exchange.apply(observedConsumer))
                    .doOnError(error ->
                            observation.failure(AiObservationSupport.outcome(error)))
                    .doFinally(signal -> {
                        if (Objects.isNull(inherited)) {
                            observation.finish(signal, status.get());
                        }
                    })
                    .contextWrite(context -> context
                            .put(AiRequestObservation.REACTOR_KEY, observation)
                            .put(AgentRequestContext.REACTOR_KEY, observation.getContext()));
        });
    }
}
