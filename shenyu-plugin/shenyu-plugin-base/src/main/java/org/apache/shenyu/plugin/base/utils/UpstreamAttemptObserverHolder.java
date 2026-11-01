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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.spi.Join;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Holds the process-wide {@link UpstreamAttemptObserver}. Registration is opt-in:
 * an observability consumer (metrics, agent, tracing) registers its implementation at
 * startup, mirroring how {@code MetricsReporter} registers metric backends. With no
 * observer registered the HTTP transport path allocates no observation objects at all,
 * preserving the uninstrumented behavior and overhead (#8209).
 */
@Join
public final class UpstreamAttemptObserverHolder {

    private static final UpstreamAttemptObserver NOOP = new UpstreamAttemptObserver() {
        @Override
        public void onAttemptStarted(final UpstreamAttemptObservation observation) {
        }

        @Override
        public void onHeadersReady(final UpstreamAttemptObservation observation) {
        }

        @Override
        public void onAttemptFinished(final UpstreamAttemptObservation observation) {
        }
    };

    private static final AtomicReference<UpstreamAttemptObserver> OBSERVER = new AtomicReference<>(NOOP);

    private UpstreamAttemptObserverHolder() {
    }

    /**
     * Register the process-wide observer, replacing any previous one. Registering the
     * returned {@code NOOP} disables the instrumentation.
     *
     * @param observer the observer, must not be null
     */
    public static void register(final UpstreamAttemptObserver observer) {
        OBSERVER.set(Objects.requireNonNull(observer, "observer"));
    }

    /**
     * Get the current observer. Never null; the default no-op implementation is
     * returned when nothing is registered, so callers can either compare against
     * {@link #isInstrumented()} to skip allocation or invoke it directly.
     *
     * @return the current observer
     */
    public static UpstreamAttemptObserver get() {
        return OBSERVER.get();
    }

    /**
     * Remove the registered observer, restoring the no-op default.
     */
    public static void unregister() {
        OBSERVER.set(NOOP);
    }

    /**
     * Whether an observability consumer registered an observer. When false, the
     * transport path skips observation allocation entirely.
     *
     * @return true when instrumented
     */
    public static boolean isInstrumented() {
        return OBSERVER.get() != NOOP;
    }
}
