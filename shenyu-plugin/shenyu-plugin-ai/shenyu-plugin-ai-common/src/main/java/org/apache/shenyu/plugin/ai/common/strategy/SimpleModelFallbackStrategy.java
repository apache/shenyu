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

package org.apache.shenyu.plugin.ai.common.strategy;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatResponse;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

/**
 * A fallback strategy that simply executes a call with a pre-configured client.
 */
public final class SimpleModelFallbackStrategy implements FallbackStrategy {

    /**
     * The constant INSTANCE.
     */
    public static final FallbackStrategy INSTANCE = new SimpleModelFallbackStrategy();

    private static final Logger LOG = LoggerFactory.getLogger(SimpleModelFallbackStrategy.class);

    private SimpleModelFallbackStrategy() {
    }

    @Override
    public Mono<ChatResponse> fallback(final ChatClient fallbackClient, final String requestBody, final Throwable originalError) {
        LOG.warn("Executing simple model fallback strategy due to error: {}", originalError.getMessage());

        return Mono.fromCallable(() -> fallbackClient.prompt()
                        .user(requestBody)
                        .call()
                        .chatResponse())
                .subscribeOn(Schedulers.boundedElastic())
                .doOnSuccess(response -> LOG.info("Fallback call successful."))
                .doOnError(fallbackError -> LOG.error("Fallback call also failed.", fallbackError));
    }

    @Override
    public Flux<ChatResponse> fallbackStream(final ChatClient fallbackClient, final String requestBody, final Throwable originalError) {
        LOG.warn("Executing simple model fallback stream strategy due to error: {}", originalError.getMessage());

        return Flux.defer(() -> fallbackClient.prompt()
                        .user(requestBody)
                        .stream()
                        .chatResponse())
                .subscribeOn(Schedulers.boundedElastic())
                .doOnComplete(() -> LOG.info("Fallback stream completed successfully."))
                .doOnError(fallbackError -> LOG.error("Fallback stream also failed.", fallbackError));
    }
}