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

package org.apache.shenyu.plugin.httpclient;

import io.micrometer.core.instrument.FunctionCounter;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Metrics;
import org.apache.shenyu.plugin.base.circuitbreaker.LocalPoolRejectionException;
import org.slf4j.Logger;

import java.util.concurrent.atomic.AtomicLong;
import java.util.function.LongSupplier;

/**
 * Counts rejected HTTP attempts and limits the summary shared by initial attempts and retries.
 */
final class LocalPoolRejectionReporter {

    static final LocalPoolRejectionReporter INSTANCE = new LocalPoolRejectionReporter(Metrics.globalRegistry, System::nanoTime);

    private static final long SUMMARY_INTERVAL_NANOS = 30_000_000_000L;

    private final AtomicLong rejectedAttempts = new AtomicLong();

    private final AtomicLong lastSummaryNanos;

    private final LongSupplier nanoTime;

    LocalPoolRejectionReporter(final MeterRegistry registry, final LongSupplier nanoTime) {
        this.nanoTime = nanoTime;
        lastSummaryNanos = new AtomicLong(nanoTime.getAsLong() - SUMMARY_INTERVAL_NANOS);
        FunctionCounter.builder("shenyu.httpclient.pool.rejections", rejectedAttempts, AtomicLong::get)
                .description("HTTP attempts rejected because the local connection pool pending queue is full")
                .register(registry);
    }

    void logAttemptError(final Logger logger, final Throwable error) {
        if (!(error instanceof LocalPoolRejectionException)) {
            logger.error(error.getMessage(), error);
            return;
        }
        rejectedAttempts.incrementAndGet();
        final long previous = lastSummaryNanos.get();
        final long now = nanoTime.getAsLong();
        if (now - previous >= SUMMARY_INTERVAL_NANOS && lastSummaryNanos.compareAndSet(previous, now)) {
            logger.warn("Local HTTP connection pool rejected requests (total rejected attempts: {})", rejectedAttempts.get());
        }
    }
}
