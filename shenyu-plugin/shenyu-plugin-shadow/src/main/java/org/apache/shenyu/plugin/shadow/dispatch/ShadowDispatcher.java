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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpMethod;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import java.net.URI;
import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.ArrayBlockingQueue;

/**
 * Bounded asynchronous shadow dispatcher for one rule.
 *
 * <p>Work is accepted only while the rule's queue has room; a full queue drops
 * the sample instead of growing memory or applying backpressure to the primary
 * request. Every attempt is bounded by the rule's timeout and its failure never
 * reaches the primary response.</p>
 */
public final class ShadowDispatcher implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(ShadowDispatcher.class);

    private static final int MIN_CAPACITY = 1;

    private final WebClient webClient;

    private final Sinks.Many<ShadowTask> sink;

    private final Disposable subscription;

    public ShadowDispatcher(final WebClient webClient, final int maxConcurrent, final int queueCapacity) {
        this.webClient = webClient;
        this.sink = Sinks.many().unicast()
                .onBackpressureBuffer(new ArrayBlockingQueue<>(Math.max(queueCapacity, MIN_CAPACITY)));
        this.subscription = sink.asFlux()
                .flatMap(this::mirror, Math.max(maxConcurrent, MIN_CAPACITY))
                .subscribe(ignored -> { }, error -> LOG.warn("shadow dispatcher terminated", error));
    }

    /**
     * Offer one shadow call without blocking the caller.
     *
     * @param task the shadow call
     * @return true when the task was queued, false when the queue is full
     */
    public boolean dispatch(final ShadowTask task) {
        return sink.tryEmitNext(task).isSuccess();
    }

    @Override
    public void close() {
        sink.tryEmitComplete();
        subscription.dispose();
    }

    private Mono<Void> mirror(final ShadowTask task) {
        WebClient.RequestHeadersSpec<?> request = webClient.method(HttpMethod.valueOf(task.getMethod()))
                .uri(task.getUri())
                .headers(headers -> headers.addAll(task.getHeaders()))
                .bodyValue(task.getBody());
        return request.exchangeToMono(response -> response.releaseBody())
                .timeout(Duration.ofMillis(task.getTimeoutMillis()))
                .doOnError(error -> LOG.warn("shadow attempt to {} failed with {}",
                        redactedTarget(task.getUri()), error.getClass().getSimpleName()))
                .onErrorResume(error -> Mono.empty());
    }

    /**
     * Describe a shadow target without its query string.
     *
     * <p>Queries can carry request tokens, and the failure path must not copy
     * them into the gateway log.</p>
     */
    private static String redactedTarget(final URI uri) {
        String authority = Objects.isNull(uri.getRawAuthority()) ? "" : uri.getRawAuthority();
        String path = Objects.isNull(uri.getRawPath()) ? "" : uri.getRawPath();
        return uri.getScheme() + "://" + authority + path;
    }
}
