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

package org.apache.shenyu.plugin.shadow.dispatch;

import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.apache.shenyu.plugin.shadow.capture.ShadowBodyCapture;
import org.apache.shenyu.plugin.shadow.compare.ShadowComparator;
import org.apache.shenyu.plugin.shadow.compare.ShadowObservation;
import org.apache.shenyu.plugin.shadow.report.ShadowReporter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.ArrayBlockingQueue;

/**
 * Bounded asynchronous shadow dispatcher for one rule.
 *
 * <p>Work is accepted only while the rule's queue has room; a full queue drops
 * the sample instead of growing memory or applying backpressure to the primary
 * request. Every attempt is bounded by the rule's timeout, and its result is
 * compared and reported without touching the primary response.</p>
 */
public final class ShadowDispatcher implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(ShadowDispatcher.class);

    private static final int MIN_CAPACITY = 1;

    private final WebClient webClient;

    private final ShadowHandle handle;

    private final String ruleKey;

    private final ShadowReporter reporter;

    private final Sinks.Many<ShadowCall> sink;

    private final Disposable subscription;

    public ShadowDispatcher(final WebClient webClient, final ShadowHandle handle, final String ruleKey,
                            final ShadowReporter reporter) {
        this.webClient = Objects.requireNonNull(webClient);
        this.handle = Objects.requireNonNull(handle);
        this.ruleKey = Objects.requireNonNull(ruleKey);
        this.reporter = Objects.requireNonNull(reporter);
        this.sink = Sinks.many().unicast()
                .onBackpressureBuffer(new ArrayBlockingQueue<>(Math.max(handle.getQueueCapacity(), MIN_CAPACITY)));
        this.subscription = sink.asFlux()
                .flatMap(this::mirror, Math.max(handle.getMaxConcurrent(), MIN_CAPACITY))
                .subscribe(ignored -> { }, error -> LOG.warn("shadow dispatcher terminated", error));
    }

    /**
     * Offer one shadow call without blocking the caller.
     *
     * @param task the shadow request
     * @param primary the primary response observation
     * @return true when the call was queued, false when the queue is full
     */
    public boolean dispatch(final ShadowTask task, final ShadowObservation primary) {
        return sink.tryEmitNext(new ShadowCall(task, primary)).isSuccess();
    }

    @Override
    public void close() {
        sink.tryEmitComplete();
        subscription.dispose();
    }

    private Mono<Void> mirror(final ShadowCall call) {
        ShadowTask task = call.getTask();
        long start = System.nanoTime();
        // Build the request lazily: a value that cannot be turned into a request
        // (for example an unsupported method) must fail this attempt only, never
        // terminate the dispatcher subscription for the whole rule.
        return Mono.defer(() -> {
            WebClient.RequestBodySpec spec = webClient.method(HttpMethod.valueOf(task.getMethod()))
                    .uri(task.getUri())
                    .headers(headers -> headers.addAll(task.getHeaders()));
            WebClient.RequestHeadersSpec<?> request = task.getBody().length == 0
                    ? spec : spec.bodyValue(task.getBody());
            return request.exchangeToMono(response -> observe(response, call, start));
        })
                .timeout(Duration.ofMillis(task.getTimeoutMillis()))
                .onErrorResume(error -> {
                    reporter.failure(ruleKey, error);
                    return Mono.empty();
                })
                .then();
    }

    private Mono<Void> observe(final ClientResponse response, final ShadowCall call, final long start) {
        HttpHeaders headers = response.headers().asHttpHeaders();
        int status = response.statusCode().value();
        ShadowBodyCapture capture = new ShadowBodyCapture(handle.getMaxResponseBodyBytes());
        return response.bodyToFlux(DataBuffer.class)
                .doOnNext(capture::append)
                .then(Mono.fromRunnable(() -> report(call, status, headers, capture, start)));
    }

    private void report(final ShadowCall call, final int status, final HttpHeaders headers,
                        final ShadowBodyCapture capture, final long start) {
        ShadowObservation shadow = new ShadowObservation(status, headers, capture.bytes(), capture.isTruncated(),
                System.nanoTime() - start);
        reporter.report(ruleKey, ShadowComparator.compare(handle, call.getPrimary(), shadow), call.getPrimary(), shadow);
    }
}
