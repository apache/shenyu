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

package org.apache.shenyu.springboot.starter.ai.ledger;

import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiTerminalEvent;

import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.LongAdder;

/** Explicit composite observer, preserving each distinct application sink without duplicate ledger delivery. */
public final class LedgerObserver implements AiCallObserver.Composite {

    private final AiUsageLedgerRecorder recorder;

    private final List<AiCallObserver> delegates;

    private final LongAdder failedDelegates = new LongAdder();

    public LedgerObserver(final AiUsageLedgerRecorder recorder, final List<AiCallObserver> delegates) {
        this.recorder = Objects.requireNonNull(recorder, "recorder");
        Set<AiCallObserver> distinct = Collections.newSetFromMap(new IdentityHashMap<>());
        this.delegates = delegates.stream().filter(distinct::add).toList();
        if (this.delegates.size() > 16) {
            throw new IllegalArgumentException("Too many application observers");
        }
    }

    @Override
    public void record(final AiTerminalEvent event) {
        recorder.record(event);
        delegates.forEach(delegate -> deliver(() -> delegate.record(event)));
    }

    @Override
    public void recordAttempt(final AiAttemptEvent event) {
        recorder.recordAttempt(event);
        delegates.forEach(delegate -> deliver(() -> delegate.recordAttempt(event)));
    }

    /**
     * Failed application callbacks never alter response or ledger delivery.
     * @return failed callback count
     */
    public long failedDelegateCallbacks() {
        return failedDelegates.sum();
    }

    private void deliver(final Runnable callback) {
        try {
            callback.run();
        } catch (RuntimeException error) {
            failedDelegates.increment();
        }
    }
}
