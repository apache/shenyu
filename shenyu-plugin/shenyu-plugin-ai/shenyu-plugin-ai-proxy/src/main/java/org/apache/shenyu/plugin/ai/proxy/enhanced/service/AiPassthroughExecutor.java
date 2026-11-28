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

import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.execution.AiResponseConsumer;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.observation.AiObservationSupport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.observation.AiUpstreamUsageTap;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.orchestration.AiProxyRetryPolicy;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.orchestration.AiRetrySettings;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;
import reactor.core.scheduler.Schedulers;

import java.io.ByteArrayOutputStream;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

/**
 * Streams vendor-native exchanges and replays eligible rate-limited uploads.
 */
final class AiPassthroughExecutor {

    private static final int MAX_RATE_LIMIT_BODY_BYTES = 16 * 1024;

    private final AiProxyTransport aiProxyTransport;

    private final AiProxyRetryProperties retryProperties;

    private final AiRetrySettings retrySettings;

    private final AiResponseConsumer responseConsumer = new AiResponseConsumer();

    AiPassthroughExecutor(final AiProxyTransport transport, final AiProxyRetryProperties retryProperties, final AiRetrySettings retrySettings) {
        this.aiProxyTransport = transport;
        this.retryProperties = retryProperties;
        this.retrySettings = retrySettings;
    }

    Mono<Void> forwardNative(final AiUpstreamRequest request, final Flux<DataBuffer> upload, final String protocol,
                             final AiRequestObservation observation, final Function<AiUpstreamResponse, Mono<Void>> writer) {
        return Mono.defer(() -> {
            int maxRetries = nativeStatusRetries(protocol);
            AiUploadReplayBuffer copy = new AiUploadReplayBuffer();
            long deadline = nativeTimeNanos() + retryProperties.getRequestDeadline().toNanos();
            NativeCall call = new NativeCall(request, copy, protocol, observation, writer, maxRetries, deadline);
            return sendNative(call, maxRetries == 0 ? upload : copy.capture(upload), 0, null);
        });
    }

    private int nativeStatusRetries(final String protocol) {
        AiProxyRetryProperties.ReplayProperties replay = retryProperties.getReplay();
        AiProxyRetryProperties.ReplayMode mode = switch (protocol) {
            case OpenAiChat.NAME -> retrySettings.replayMode(ShenyuAiRequest.Operation.CHAT_COMPLETION, false, false);
            case OpenAiResponses.NAME -> retrySettings.replayMode(ShenyuAiRequest.Operation.RESPONSE, false, false);
            default -> replay.getResponseStatus();
        };
        // Raw routes have no provider declaration of replay safety.
        return mode == AiProxyRetryProperties.ReplayMode.CONSERVATIVE ? retryProperties.getStreamingMaxRetries() : 0;
    }

    private static long nativeTimeNanos() {
        return Schedulers.parallel().now(TimeUnit.NANOSECONDS);
    }

    private Mono<Void> sendNative(final NativeCall call, final Flux<DataBuffer> upload, final int retries, final AiUpstreamResponse previousResponse) {
        long remaining = call.deadline() - nativeTimeNanos();
        if (remaining <= 0) {
            return Objects.nonNull(previousResponse) ? call.write(previousResponse) : Mono.error(nativeDeadlineExceeded());
        }
        Mono<AiUpstreamResponse> prepared = aiProxyTransport.prepareStreaming(call.request(), upload);
        AiAttemptEvent.Kind kind = retries == 0 ? AiAttemptEvent.Kind.PRIMARY : AiAttemptEvent.Kind.RETRY;
        AiRequestObservation.Attempt attempt = call.observation().startAttempt(call.observation().getContext(), kind, "passthrough", call.protocol(), false);
        return prepared.flatMap(response -> {
            AiUpstreamResponse observed = observeNative(response, call.protocol(), attempt);
            if (response.getStatusCode() == HttpStatus.TOO_MANY_REQUESTS.value() && retries < call.maxRetries() && call.uploadCopy().isComplete()) {
                Duration delay = nativeRetryDelay(response.getHeaders(), retries);
                if (Objects.nonNull(delay) && delay.toNanos() < call.deadline() - nativeTimeNanos()) {
                    return bufferRateLimitResponse(observed, delay, call);
                }
            }
            return responseConsumer.consume(observed, call::write, new AtomicBoolean()).then(Mono.<RateLimitRetry>empty());
        }).timeout(Duration.ofNanos(remaining), Mono.error(AiPassthroughExecutor::nativeDeadlineExceeded))
                .doOnSuccess(ignored -> attempt.finish(SignalType.ON_COMPLETE, null))
                .doOnError(error -> attempt.finish(SignalType.ON_ERROR, AiObservationSupport.outcome(error)))
                .doOnCancel(() -> attempt.finish(SignalType.CANCEL, null))
                .contextWrite(context -> context.put(AiRequestObservation.Attempt.REACTOR_KEY, attempt))
                .flatMap(retry -> {
                    if (retry.delay().toNanos() >= call.deadline() - nativeTimeNanos()) {
                        return call.write(retry.response());
                    }
                    return Mono.delay(retry.delay()).then(Mono.defer(() ->
                            sendNative(call, call.uploadCopy().replay(), retries + 1, retry.response())));
                });
    }

    private Mono<RateLimitRetry> bufferRateLimitResponse(final AiUpstreamResponse response, final Duration delay, final NativeCall call) {
        ByteArrayOutputStream prefix = new ByteArrayOutputStream();
        return response.getBody().skipWhile(chunk -> {
            if (chunk.length > MAX_RATE_LIMIT_BODY_BYTES - prefix.size()) {
                return false;
            }
            prefix.writeBytes(chunk);
            return true;
        }).<RateLimitRetry>switchOnFirst((signal, remainder) -> {
            Flux<byte[]> buffered = prefix.size() == 0 ? Flux.empty() : Flux.just(prefix.toByteArray());
            if (signal.isOnComplete() && delay.toNanos() < call.deadline() - nativeTimeNanos()) {
                AiUpstreamResponse saved = new AiUpstreamResponse(response.getStatusCode(), response.getHeaders(), buffered);
                return Mono.just(new RateLimitRetry(delay, saved));
            }
            AiUpstreamResponse streaming = new AiUpstreamResponse(response.getStatusCode(), response.getHeaders(), buffered.concatWith(remainder));
            return responseConsumer.consume(streaming, call::write, new AtomicBoolean()).then(Mono.empty());
        }).singleOrEmpty();
    }

    private static AiTransportException nativeDeadlineExceeded() {
        return new AiTransportException(AiTransportException.Kind.STREAM_DURATION_TIMEOUT, "AI passthrough deadline exceeded", null);
    }

    private Duration nativeRetryDelay(final Map<String, List<String>> headers, final int retries) {
        Duration retryAfter = AiProxyRetryPolicy.retryAfter(headers, retrySettings);
        if (AiProxyRetryPolicy.hasRetryAfter(headers) && !retrySettings.acceptsRetryAfter(retryAfter)) {
            return null;
        }
        Duration delay = retrySettings.retryDelay(true, retries, System.nanoTime());
        return Objects.nonNull(retryAfter) && retryAfter.compareTo(delay) > 0 ? retryAfter : delay;
    }

    private AiUpstreamResponse observeNative(final AiUpstreamResponse response, final String protocol, final AiRequestObservation.Attempt attempt) {
        boolean streaming = response.getHeaders().entrySet().stream().filter(entry -> HttpHeaders.CONTENT_TYPE.equalsIgnoreCase(entry.getKey()))
                .flatMap(entry -> entry.getValue().stream()).anyMatch(value -> MediaType.TEXT_EVENT_STREAM_VALUE.equalsIgnoreCase(value.split(";", 2)[0].trim()));
        attempt.response(response.getStatusCode(), streaming);
        if (response.getStatusCode() < 200 || response.getStatusCode() >= 300) {
            return response;
        }
        if (OpenAiChat.NAME.equals(protocol) || OpenAiResponses.NAME.equals(protocol)) {
            return new AiUpstreamUsageTap(attempt, response.getHeaders(), streaming, OpenAiChat.NAME.equals(protocol), OpenAiResponses.NAME.equals(protocol)).attach(response);
        }
        attempt.usage(new AiUsage(null, null, null, AiUsage.Status.UNSUPPORTED));
        return response;
    }

    private record NativeCall(AiUpstreamRequest request, AiUploadReplayBuffer uploadCopy, String protocol, AiRequestObservation observation,
                              Function<AiUpstreamResponse, Mono<Void>> writer, int maxRetries, long deadline) {
        private Mono<Void> write(final AiUpstreamResponse response) {
            uploadCopy.discard();
            return writer.apply(response);
        }
    }

    private record RateLimitRetry(Duration delay, AiUpstreamResponse response) {
    }
}
