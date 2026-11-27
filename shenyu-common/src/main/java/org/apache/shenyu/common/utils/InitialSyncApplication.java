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

    private static final ThreadLocal<Application> ACTIVE = new ThreadLocal<>();

    private InitialSyncApplication() {
    }

    /**
     * Run callbacks and collect their asynchronous completion stages without blocking the socket.
     * @param action callback
     * @return completion of the callback and all registered application work
     */
    public static CompletableFuture<Void> run(final Runnable action) {
        return collect(action, true);
    }

    /**
     * Observe a best-effort apply without stopping a batch at its first rejected row.
     * An enclosing initial synchronization keeps its fail-fast behavior.
     *
     * @param action callback
     * @return completion of the callback work, including failures reported by subscribers
     */
    public static CompletableFuture<Void> observe(final Runnable action) {
        return collect(action, isActive());
    }

    private static CompletableFuture<Void> collect(final Runnable action, final boolean failFast) {
        Application previous = ACTIVE.get();
        Application application = new Application(failFast);
        ACTIVE.set(application);
        try {
            action.run();
            CompletableFuture<Void> completion = application.completion();
            if (Objects.nonNull(previous)) {
                previous.pending.add(completion);
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
     * Both {@link #run(Runnable)} and {@link #observe(Runnable)} include the registered work in their completion.
     * @param completion completion stage for the deferred application
     */
    public static void register(final CompletionStage<?> completion) {
        Application application = ACTIVE.get();
        if (Objects.nonNull(application)) {
            application.pending.add(Objects.requireNonNull(completion).toCompletableFuture());
        }
    }

    /**
     * Report a logged failure while the subscriber continues applying the remaining rows.
     *
     * @param failure rejection from the subscriber or its handler
     */
    public static void reportFailure(final Throwable failure) {
        Application application = ACTIVE.get();
        if (Objects.nonNull(application) && Objects.isNull(application.failure)) {
            application.failure = Objects.requireNonNull(failure);
        }
    }

    /**
     * Whether failures must propagate synchronously to the initial synchronization caller.
     * @return active
     */
    public static boolean isActive() {
        Application application = ACTIVE.get();
        return Objects.nonNull(application) && application.failFast;
    }

    private static final class Application {

        private final boolean failFast;

        private final List<CompletableFuture<?>> pending = new ArrayList<>();

        private Throwable failure;

        private Application(final boolean failFast) {
            this.failFast = failFast;
        }

        private CompletableFuture<Void> completion() {
            if (Objects.nonNull(failure)) {
                pending.add(CompletableFuture.failedFuture(failure));
            }
            return CompletableFuture.allOf(pending.toArray(new CompletableFuture<?>[0]));
        }
    }
}
