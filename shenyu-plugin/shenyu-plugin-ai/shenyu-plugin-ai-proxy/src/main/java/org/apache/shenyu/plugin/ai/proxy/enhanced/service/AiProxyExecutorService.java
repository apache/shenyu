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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.openai.api.OpenAiApi;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletion;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionChunk;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionRequest;
import org.springframework.ai.retry.NonTransientAiException;
import org.springframework.http.ResponseEntity;
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
     * Execute a streaming AI call directly via {@link OpenAiApi}, bypassing Spring AI's
     * {@code createRequest()} which loses fields like {@code reasoning_content}.
     *
     * @param mainApi       the main OpenAiApi
     * @param fallbackApiOpt the optional fallback OpenAiApi
     * @param request       the ChatCompletionRequest with all fields preserved
     * @return a Flux of ChatCompletionChunk
     */
    public Flux<ChatCompletionChunk> executeDirectStream(final OpenAiApi mainApi,
            final Optional<OpenAiApi> fallbackApiOpt, final ChatCompletionRequest request) {
        return mainApi.chatCompletionStream(request)
                .doOnError(e -> UpstreamErrorLogger.logUpstreamError(LOG, e, "direct stream"))
                .retryWhen(Retry.max(1)
                        .onRetryExhaustedThrow((retryBackoffSpec, retrySignal) -> {
                            LOG.warn("Direct stream retry exhausted. Triggering fallback.",
                                    retrySignal.failure());
                            return new NonTransientAiException(
                                    "Direct stream failed after 1 retry. Triggering fallback.",
                                    retrySignal.failure());
                        }))
                .onErrorResume(NonTransientAiException.class,
                        throwable -> handleDirectFallbackStream(throwable, fallbackApiOpt, request));
    }

    /**
     * Execute a non-streaming AI call directly via {@link OpenAiApi}.
     *
     * @param mainApi       the main OpenAiApi
     * @param fallbackApiOpt the optional fallback OpenAiApi
     * @param request       the ChatCompletionRequest with all fields preserved
     * @return a Mono of ResponseEntity containing ChatCompletion
     */
    public Mono<ResponseEntity<ChatCompletion>> executeDirectCall(final OpenAiApi mainApi,
            final Optional<OpenAiApi> fallbackApiOpt, final ChatCompletionRequest request) {
        return Mono.fromCallable(() -> mainApi.chatCompletionEntity(request))
                .subscribeOn(Schedulers.boundedElastic())
                .doOnError(e -> UpstreamErrorLogger.logUpstreamError(LOG, e, "direct call"))
                .retryWhen(Retry.backoff(3, Duration.ofSeconds(1))
                        .filter(throwable -> !(throwable instanceof NonTransientAiException))
                        .onRetryExhaustedThrow((retryBackoffSpec, retrySignal) -> {
                            LOG.warn("Direct call retries exhausted after {} attempts. Triggering fallback.",
                                    retrySignal.totalRetries(), retrySignal.failure());
                            return new NonTransientAiException("Direct call retries exhausted. Triggering fallback.",
                                    retrySignal.failure());
                        }))
                .onErrorResume(NonTransientAiException.class,
                        throwable -> handleDirectFallbackCall(throwable, fallbackApiOpt, request));
    }

    private Flux<ChatCompletionChunk> handleDirectFallbackStream(final Throwable throwable,
            final Optional<OpenAiApi> fallbackApiOpt, final ChatCompletionRequest request) {
        LOG.warn("Main direct stream failed, attempting fallback...", throwable);

        if (fallbackApiOpt.isEmpty()) {
            return Flux.error(throwable);
        }

        LOG.info("Using fallback OpenAiApi for direct stream");
        return fallbackApiOpt.get().chatCompletionStream(request);
    }

    private Mono<ResponseEntity<ChatCompletion>> handleDirectFallbackCall(final Throwable throwable,
            final Optional<OpenAiApi> fallbackApiOpt, final ChatCompletionRequest request) {
        LOG.warn("Main direct call failed, attempting fallback...", throwable);

        if (fallbackApiOpt.isEmpty()) {
            return Mono.error(throwable);
        }

        LOG.info("Using fallback OpenAiApi for direct call");
        return Mono.fromCallable(() -> fallbackApiOpt.get().chatCompletionEntity(request))
                .subscribeOn(Schedulers.boundedElastic());
    }
}
