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

import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.protocol.OpenAiProtocolAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.openai.api.OpenAiApi;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletion;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionChunk;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionRequest;
import org.springframework.ai.retry.NonTransientAiException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.util.retry.Retry;

import java.time.Duration;
import java.util.Objects;
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
     * @param mainApi         the main OpenAiApi
     * @param fallbackCtxOpt the optional fallback context (api + config)
     * @param request        the ChatCompletionRequest with all fields preserved
     * @param requestBody    the original request body for rebuilding fallback request
     * @param stream         whether this is a streaming request
     * @return a Flux of ChatCompletionChunk
     */
    public Flux<ChatCompletionChunk> executeDirectStream(final OpenAiApi mainApi,
            final Optional<FallbackContext> fallbackCtxOpt, final ChatCompletionRequest request,
            final String requestBody, final boolean stream) {
        return mainApi.chatCompletionStream(request)
                .doOnError(e -> UpstreamErrorLogger.logUpstreamError(LOG, e, "direct stream"))
                .retryWhen(Retry.max(1)
                        .filter(AiProxyExecutorService::isRetryable)
                        .onRetryExhaustedThrow((retryBackoffSpec, retrySignal) -> {
                            LOG.warn("Direct stream retry exhausted. Triggering fallback.",
                                    retrySignal.failure());
                            return new NonTransientAiException(
                                    "Direct stream failed after 1 retry. Triggering fallback.",
                                    retrySignal.failure());
                        }))
                .onErrorResume(NonTransientAiException.class,
                        throwable -> handleDirectFallbackStream(throwable, fallbackCtxOpt, requestBody, stream));
    }

    /**
     * Execute a non-streaming AI call directly via {@link OpenAiApi}.
     *
     * @param mainApi         the main OpenAiApi
     * @param fallbackCtxOpt the optional fallback context (api + config)
     * @param request        the ChatCompletionRequest with all fields preserved
     * @param requestBody    the original request body for rebuilding fallback request
     * @return a Mono of ResponseEntity containing ChatCompletion
     */
    public Mono<ResponseEntity<ChatCompletion>> executeDirectCall(final OpenAiApi mainApi,
            final Optional<FallbackContext> fallbackCtxOpt, final ChatCompletionRequest request,
            final String requestBody) {
        return Mono.fromCallable(() -> mainApi.chatCompletionEntity(request))
                .subscribeOn(Schedulers.boundedElastic())
                .doOnError(e -> UpstreamErrorLogger.logUpstreamError(LOG, e, "direct call"))
                .retryWhen(Retry.backoff(3, Duration.ofSeconds(1))
                        .filter(AiProxyExecutorService::isRetryable)
                        .onRetryExhaustedThrow((retryBackoffSpec, retrySignal) -> {
                            LOG.warn("Direct call retries exhausted after {} attempts. Triggering fallback.",
                                    retrySignal.totalRetries(), retrySignal.failure());
                            return new NonTransientAiException("Direct call retries exhausted. Triggering fallback.",
                                    retrySignal.failure());
                        }))
                .onErrorResume(NonTransientAiException.class,
                        throwable -> handleDirectFallbackCall(throwable, fallbackCtxOpt, requestBody));
    }

    private Flux<ChatCompletionChunk> handleDirectFallbackStream(final Throwable throwable,
            final Optional<FallbackContext> fallbackCtxOpt, final String requestBody, final boolean stream) {
        LOG.warn("Main direct stream failed, attempting fallback...", throwable);

        if (fallbackCtxOpt.isEmpty()) {
            return Flux.error(throwable);
        }

        final FallbackContext ctx = fallbackCtxOpt.get();
        LOG.info("Using fallback OpenAiApi for direct stream");
        final ChatCompletionRequest fallbackRequest = OpenAiProtocolAdapter.toChatCompletionRequest(
                requestBody, stream, ctx.config());
        return ctx.api().chatCompletionStream(fallbackRequest);
    }

    private Mono<ResponseEntity<ChatCompletion>> handleDirectFallbackCall(final Throwable throwable,
            final Optional<FallbackContext> fallbackCtxOpt, final String requestBody) {
        LOG.warn("Main direct call failed, attempting fallback...", throwable);

        if (fallbackCtxOpt.isEmpty()) {
            return Mono.error(throwable);
        }

        final FallbackContext ctx = fallbackCtxOpt.get();
        LOG.info("Using fallback OpenAiApi for direct call");
        final ChatCompletionRequest fallbackRequest = OpenAiProtocolAdapter.toChatCompletionRequest(
                requestBody, false, ctx.config());
        return Mono.fromCallable(() -> ctx.api().chatCompletionEntity(fallbackRequest))
                .subscribeOn(Schedulers.boundedElastic());
    }

    /**
     * Determine if the error is retryable.
     * Retries transient network errors and retryable HTTP statuses (429, 5xx).
     * Non-retryable: NonTransientAiException, client errors (400/401/403/404).
     */
    private static boolean isRetryable(final Throwable throwable) {
        if (throwable instanceof NonTransientAiException) {
            return false;
        }
        final WebClientResponseException webClientEx = findWebClientResponseException(throwable);
        if (Objects.nonNull(webClientEx)) {
            final int status = webClientEx.getStatusCode().value();
            return status == 429 || status >= 500;
        }
        return true;
    }

    private static WebClientResponseException findWebClientResponseException(final Throwable e) {
        Throwable current = e;
        while (Objects.nonNull(current)) {
            if (current instanceof WebClientResponseException ex) {
                return ex;
            }
            current = current.getCause();
        }
        return null;
    }

    /**
     * Container for fallback context: the OpenAiApi instance and the fallback config
     * used to rebuild the request with fallback-specific model/temperature/maxTokens.
     */
    public record FallbackContext(OpenAiApi api, AiCommonConfig config) {
    }
}
