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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.Timer;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Observe acquisition termination without changing the delegate's scheduling, timeout or cancellation.
 * Existing Reactor Netty gauges remain registered by the underlying provider.
 */
final class HttpClientPoolMetrics {

    private final String name;

    private final Timer success;

    private final Timer timeout;

    private final Timer error;

    private final Timer cancelled;

    HttpClientPoolMetrics(final String name) {
        this.name = name;
        success = timer("success");
        timeout = timer("timeout");
        error = timer("error");
        cancelled = timer("cancelled");
    }

    Mono<? extends Connection> observe(final Mono<? extends Connection> acquisition) {
        return Mono.defer(() -> {
            long start = System.nanoTime();
            AtomicBoolean recorded = new AtomicBoolean();
            return acquisition
                    .doOnNext(connection -> record(success, start, recorded))
                    .doOnError(cause -> record(cause instanceof TimeoutException ? timeout : error, start, recorded))
                    .doOnCancel(() -> record(cancelled, start, recorded));
        });
    }

    private Timer timer(final String outcome) {
        return Timer.builder("shenyu.httpclient.pool.acquire")
                .description("Connection acquisition duration, including connection establishment when required")
                .tags("name", name, "outcome", outcome)
                .register(Metrics.globalRegistry);
    }

    private void record(final Timer timer, final long start, final AtomicBoolean recorded) {
        if (recorded.compareAndSet(false, true)) {
            timer.record(System.nanoTime() - start, TimeUnit.NANOSECONDS);
        }
    }

}
