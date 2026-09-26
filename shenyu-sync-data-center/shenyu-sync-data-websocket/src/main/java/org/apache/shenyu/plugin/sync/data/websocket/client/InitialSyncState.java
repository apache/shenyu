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

package org.apache.shenyu.plugin.sync.data.websocket.client;

import org.apache.shenyu.common.dto.WebsocketSyncFrame;
import org.apache.shenyu.common.utils.InitialSyncApplication;

import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

/**
 * Tracks one connection's initial synchronization without revoking a successful startup.
 */
public final class InitialSyncState {

    private final AtomicBoolean ready;

    private String requestId;

    private int sequence;

    private boolean failed;

    private long startedAt;

    private int pending;

    private boolean ended;

    /**
     * Create connection state.
     * @param ready shared startup latch
     */
    public InitialSyncState(final AtomicBoolean ready) {
        this.ready = ready;
    }

    /**
     * Start an independent attempt.
     * @return request identifier
     */
    public synchronized String begin() {
        requestId = UUID.randomUUID().toString();
        sequence = 0;
        failed = false;
        pending = 0;
        ended = false;
        startedAt = System.nanoTime();
        return requestId;
    }

    /**
     * Whether an incomplete attempt should be retried on a fresh connection.
     * @return timeout or failed application before startup succeeded
     */
    public synchronized boolean needsReconnect() {
        return !ready.get() && (failed || System.nanoTime() - startedAt >= TimeUnit.SECONDS.toNanos(60));
    }

    /**
     * Apply a frame before acknowledging it.
     * @param frame frame
     * @param apply synchronous configuration application
     */
    public synchronized void accept(final WebsocketSyncFrame frame, final Consumer<String> apply) {
        if (failed || ended || Objects.isNull(requestId) || !Objects.equals(requestId, frame.getRequestId())) {
            return;
        }
        if (System.nanoTime() - startedAt >= TimeUnit.SECONDS.toNanos(60) || frame.getSequence() != sequence) {
            failed = true;
            return;
        }
        if (Objects.isNull(frame.getPayload())) {
            ended = true;
            completeIfApplied();
            return;
        }
        try {
            pending++;
            sequence++;
            String attempt = requestId;
            InitialSyncApplication.run(() -> apply.accept(frame.getPayload())).whenComplete((ignored, error) -> applied(attempt, error));
        } catch (RuntimeException ex) {
            failed = true;
            throw ex;
        }
    }

    /**
     * Include interleaved incremental application in the current attempt's completion boundary.
     * @param action incremental callback
     */
    public synchronized void applyIncremental(final Runnable action) {
        if (Objects.isNull(requestId) || failed) {
            action.run();
            return;
        }
        pending++;
        String attempt = requestId;
        try {
            InitialSyncApplication.run(action).whenComplete((ignored, error) -> applied(attempt, error));
        } catch (RuntimeException ex) {
            failed = true;
            throw ex;
        }
    }

    private synchronized void applied(final String attempt, final Throwable error) {
        if (!Objects.equals(requestId, attempt)) {
            return;
        }
        pending--;
        if (Objects.nonNull(error)) {
            failed = true;
        }
        completeIfApplied();
    }

    private void completeIfApplied() {
        if (ended && pending == 0 && !failed && System.nanoTime() - startedAt < TimeUnit.SECONDS.toNanos(60)) {
            ready.set(true);
            requestId = null;
        }
    }

    /**
     * Invalidate the current attempt without revoking an earlier success.
     */
    public synchronized void invalidate() {
        requestId = null;
        failed = true;
    }
}
