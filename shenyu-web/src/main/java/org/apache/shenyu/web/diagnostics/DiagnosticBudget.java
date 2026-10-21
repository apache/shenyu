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

package org.apache.shenyu.web.diagnostics;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * An independent time and size budget handed to a single collector.
 *
 * <p>Collectors are expected to poll {@link #shouldStop()} between bounded work units and
 * to respect {@link #maxBytes()} so a slow or greedy section cannot consume the whole
 * bundle allowance or delay other sections. Cancellation is cooperative: the orchestrator
 * sets it when the section deadline passes or the caller goes away.</p>
 */
public final class DiagnosticBudget {

    private final long deadlineNanos;

    private final int maxBytes;

    private final AtomicBoolean cancelled = new AtomicBoolean(false);

    /**
     * Instantiates a new budget.
     *
     * @param timeoutMs the wall-clock budget in milliseconds
     * @param maxBytes  the maximum content size in bytes
     */
    public DiagnosticBudget(final long timeoutMs, final int maxBytes) {
        this.deadlineNanos = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeoutMs);
        this.maxBytes = maxBytes;
    }

    /**
     * Whether the time budget is exhausted.
     *
     * @return true if expired
     */
    public boolean expired() {
        return System.nanoTime() - deadlineNanos >= 0;
    }

    /**
     * Whether the orchestrator asked this collector to stop.
     *
     * @return true if cancelled
     */
    public boolean isCancelled() {
        return cancelled.get();
    }

    /**
     * Request cooperative cancellation.
     */
    public void cancel() {
        cancelled.set(true);
    }

    /**
     * Whether the collector should stop, either because it was cancelled or ran out of time.
     *
     * @return true if the collector should stop producing content
     */
    public boolean shouldStop() {
        return isCancelled() || expired();
    }

    /**
     * Get the maximum content size in bytes.
     *
     * @return the max bytes
     */
    public int maxBytes() {
        return maxBytes;
    }
}
