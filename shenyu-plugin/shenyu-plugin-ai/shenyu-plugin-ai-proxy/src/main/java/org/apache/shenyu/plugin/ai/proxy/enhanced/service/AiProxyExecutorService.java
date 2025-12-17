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

import org.apache.shenyu.plugin.ai.common.strategy.SimpleModelFallbackStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.ai.retry.NonTransientAiException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.util.retry.Retry;

import java.time.Duration;
import java.util.Optional;

/**
 * AI proxy executor service.
 */
public class AiProxyExecutorService {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyExecutorService.class);

    /**
     * Execute the AI call with retry and fallback.
     *
     * @param mainClient      the main chat client
     * @param fallbackClientOpt the optional fallback chat client
     * @param requestBody     the request body
     * @return a Mono containing the ChatResponse
     */
    public Mono<ChatResponse> execute(final ChatClient mainClient, final Optional<ChatClient> fallbackClientOpt, final String requestBody) {
        final Mono<ChatResponse> mainCall = doChatCall(mainClient, requestBody);

        return mainCall
                .retryWhen(Retry.backoff(3, Duration.ofSeconds(1))
                        .filter(throwable -> !(throwable instanceof NonTransientAiException))
                        .onRetryExhaustedThrow((retryBackoffSpec, retrySignal) -> {
                            LOG.warn("Retries exhausted for AI call after {} attempts.",
                                    retrySignal.totalRetries(), retrySignal.failure());
                            return new NonTransientAiException("Retries exhausted. Triggering fallback.",
                                    retrySignal.failure());
                        }))
                .onErrorResume(NonTransientAiException.class,
                        throwable -> handleFallback(throwable, fallbackClientOpt, requestBody));
    }

    protected Mono<ChatResponse> doChatCall(final ChatClient client, final String requestBody) {
        return Mono.fromCallable(() -> client.prompt().user(requestBody).call().chatResponse())
                .subscribeOn(Schedulers.boundedElastic());
    }

    private Mono<ChatResponse> handleFallback(final Throwable throwable, final Optional<ChatClient> fallbackClientOpt, final String requestBody) {
        LOG.warn("AI main call failed or retries exhausted, attempting to fallback...", throwable);

        if (fallbackClientOpt.isEmpty()) {
            return Mono.error(throwable);
        }

        return SimpleModelFallbackStrategy.INSTANCE.fallback(fallbackClientOpt.get(), requestBody, throwable);
    }

    /**
     * Execute the AI call with retry and fallback.
     *
     * @param mainClient      the main chat client
     * @param fallbackClientOpt the optional fallback chat client
     * @param requestBody     the request body
     * @return a Flux containing the ChatResponse
     */
    public Flux<ChatResponse> executeStream(final ChatClient mainClient, final Optional<ChatClient> fallbackClientOpt, final String requestBody) {
        final Flux<ChatResponse> mainStream = doChatStream(mainClient, requestBody);

        return mainStream
                .retryWhen(Retry.max(1)
                        .onRetryExhaustedThrow((retryBackoffSpec, retrySignal) -> {
                            LOG.warn("Retrying stream once failed. Attempts: {}. Triggering fallback.",
                                    retrySignal.totalRetries(), retrySignal.failure());
                            return new NonTransientAiException("Stream failed after 1 retry. Triggering fallback.", retrySignal.failure());
                        }))
                .onErrorResume(NonTransientAiException.class,
                        throwable -> handleFallbackStream(throwable, fallbackClientOpt, requestBody));
    }

    protected Flux<ChatResponse> doChatStream(final ChatClient client, final String requestBody) {
        return Flux.defer(() -> client.prompt().user(requestBody).stream().chatResponse())
                .subscribeOn(Schedulers.boundedElastic());
    }

    private Flux<ChatResponse> handleFallbackStream(final Throwable throwable, final Optional<ChatClient> fallbackClientOpt, final String requestBody) {
        LOG.warn("AI main stream failed or retries exhausted, attempting to fallback...", throwable);

        if (fallbackClientOpt.isEmpty()) {
            return Flux.error(throwable);
        }

        return SimpleModelFallbackStrategy.INSTANCE.fallbackStream(fallbackClientOpt.get(), requestBody, throwable);
    }
}