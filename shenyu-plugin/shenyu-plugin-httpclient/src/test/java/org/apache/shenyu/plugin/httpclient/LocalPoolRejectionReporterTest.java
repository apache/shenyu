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

import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.apache.shenyu.plugin.base.circuitbreaker.LocalPoolRejectionException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

class LocalPoolRejectionReporterTest {

    private final SimpleMeterRegistry registry = new SimpleMeterRegistry();

    @AfterEach
    void closeRegistry() {
        registry.close();
    }

    @Test
    void countsEveryRejectionAndRateLimitsTheStacklessSummary() {
        final AtomicLong now = new AtomicLong();
        final LocalPoolRejectionReporter reporter = new LocalPoolRejectionReporter(registry, now::get);
        final Logger firstAttempt = mock(Logger.class);
        final Logger retry = mock(Logger.class);
        final LocalPoolRejectionException rejection = new LocalPoolRejectionException(new IllegalStateException("pool full"));
        final String summary = "Local HTTP connection pool rejected requests (total rejected attempts: {})";

        reporter.logAttemptError(firstAttempt, rejection);
        reporter.logAttemptError(retry, rejection);
        now.set(TimeUnit.SECONDS.toNanos(30) - 1);
        reporter.logAttemptError(firstAttempt, rejection);
        assertEquals(3.0, registry.get("shenyu.httpclient.pool.rejections").functionCounter().count());
        verify(firstAttempt).warn(summary, 1L);
        verifyNoMoreInteractions(firstAttempt);
        verifyNoInteractions(retry);

        now.incrementAndGet();
        reporter.logAttemptError(retry, rejection);
        reporter.logAttemptError(firstAttempt, rejection);
        assertEquals(5.0, registry.get("shenyu.httpclient.pool.rejections").functionCounter().count());
        verify(retry).warn(summary, 4L);
        verifyNoMoreInteractions(firstAttempt, retry);
    }

    @Test
    void preservesOtherErrorsWithTheirStack() {
        final LocalPoolRejectionReporter reporter = new LocalPoolRejectionReporter(registry, () -> 0);
        final Logger logger = mock(Logger.class);
        final TimeoutException timeout = new TimeoutException("timed out");

        reporter.logAttemptError(logger, timeout);

        assertEquals(0.0, registry.get("shenyu.httpclient.pool.rejections").functionCounter().count());
        verify(logger).error("timed out", timeout);
        verifyNoMoreInteractions(logger);
    }
}
