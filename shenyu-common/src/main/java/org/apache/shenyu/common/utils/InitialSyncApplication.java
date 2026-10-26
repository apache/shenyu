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

package org.apache.shenyu.common.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

/**
 * Tracks configuration application, including asynchronous work registered by subscribers.
 * Subscribers must register their completion stage before returning from the callback.
 * The stage must cover all deferred configuration work, including nested tasks. Ongoing
 * upstream health checks and request-time connections are outside this boundary.
 */
public final class InitialSyncApplication {

    private static final ThreadLocal<List<CompletableFuture<?>>> ACTIVE = new ThreadLocal<>();

    private InitialSyncApplication() {
    }

    /**
     * Run callbacks and collect their asynchronous completion stages without blocking the socket.
     * @param action callback
     * @return completion of the callback and all registered application work
     */
    public static CompletableFuture<Void> run(final Runnable action) {
        List<CompletableFuture<?>> previous = ACTIVE.get();
        List<CompletableFuture<?>> pending = new ArrayList<>();
        ACTIVE.set(pending);
        try {
            action.run();
            CompletableFuture<Void> completion = CompletableFuture.allOf(pending.toArray(new CompletableFuture<?>[0]));
            if (Objects.nonNull(previous)) {
                previous.add(completion);
            }
            return completion;
        } finally {
            if (Objects.nonNull(previous)) {
                ACTIVE.set(previous);
            } else {
                ACTIVE.remove();
            }
        }
    }

    /**
     * Register deferred configuration application during the subscriber callback.
     * Outside initial synchronization this leaves the existing asynchronous behavior unchanged.
     * @param completion completion stage, exceptional completion prevents readiness
     */
    public static void register(final CompletionStage<?> completion) {
        List<CompletableFuture<?>> pending = ACTIVE.get();
        if (Objects.nonNull(pending)) {
            pending.add(Objects.requireNonNull(completion).toCompletableFuture());
        }
    }

    /**
     * Whether failures must propagate to the synchronization caller.
     * @return active
     */
    public static boolean isActive() {
        return Objects.nonNull(ACTIVE.get());
    }
}
