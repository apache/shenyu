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

import org.springframework.web.reactive.function.client.ExchangeFilterFunction;
import reactor.core.publisher.Flux;
import reactor.core.publisher.SignalType;
import reactor.core.publisher.Sinks;

/**
 * Connects downstream cancellation to the raw response body across SDK windows.
 * Each subscription owns its signal; cached clients never hold request state.
 */
public final class AiStreamCancellation {

    private static final Object CONTEXT_KEY = new Object();

    private AiStreamCancellation() {
    }

    /**
     * Creates a filter that cancels the raw HTTP body when its caller disconnects.
     *
     * @return the request-context-aware response filter
     */
    public static ExchangeFilterFunction responseFilter() {
        return (request, next) -> reactor.core.publisher.Mono.deferContextual(context -> {
            if (!context.hasKey(CONTEXT_KEY)) {
                return next.exchange(request);
            }
            final Sinks.Empty<Void> cancellation = context.get(CONTEXT_KEY);
            return next.exchange(request).map(response -> response.mutate()
                    .body(body -> body.takeUntilOther(cancellation.asMono())).build());
        });
    }

    /**
     * Gives each subscription an independent signal including retries and fallback.
     *
     * @param source the SDK response stream
     * @param <T> the response element type
     * @return the stream with request-local cancellation propagation
     */
    public static <T> Flux<T> propagate(final Flux<T> source) {
        return Flux.defer(() -> {
            final Sinks.Empty<Void> cancellation = Sinks.empty();
            return source.contextWrite(context -> context.put(CONTEXT_KEY, cancellation))
                    .doFinally(signal -> {
                        if (signal == SignalType.CANCEL) {
                            cancellation.tryEmitEmpty();
                        }
                    });
        });
    }
}
