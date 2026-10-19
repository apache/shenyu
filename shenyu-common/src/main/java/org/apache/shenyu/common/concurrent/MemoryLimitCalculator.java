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

package org.apache.shenyu.common.concurrent;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The memory limit is calculated from the amount of memory the JVM can still commit
 * up to {@link java.lang.Runtime#maxMemory()} (the {@code -Xmx} headroom), not just
 * from the free part of the current committed heap, and it can be used with
 * {@link org.apache.shenyu.common.concurrent.MemoryLimiter}.
 *
 * @see org.apache.shenyu.common.concurrent.MemoryLimiter
 */
public class MemoryLimitCalculator {

    private static volatile long maxAvailable;

    private static final AtomicBoolean REFRESH_STARTED = new AtomicBoolean(false);

    private static void checkAndScheduleRefresh() {
        if (!REFRESH_STARTED.get()) {
            // see https://github.com/apache/dubbo/pull/10178
            refresh();
            if (REFRESH_STARTED.compareAndSet(false, true)) {
                ScheduledExecutorService scheduledExecutorService =
                        new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("Shenyu-Memory-Calculator-", false));
                // check every 50 ms to improve performance
                scheduledExecutorService.scheduleWithFixedDelay(MemoryLimitCalculator::refresh, 50, 50, TimeUnit.MILLISECONDS);
                Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                    REFRESH_STARTED.set(false);
                    scheduledExecutorService.shutdown();
                }
                ));
            }
        }
    }
    
    private static void refresh() {
        final Runtime runtime = Runtime.getRuntime();
        final long used = runtime.totalMemory() - runtime.freeMemory();
        // The JVM can grow the committed heap up to maxMemory(), so the memory that is
        // genuinely still available is the headroom to -Xmx, not only the free part of
        // the currently committed heap. When the committed heap already equals the
        // maximum (-Xms == -Xmx), this is identical to freeMemory().
        maxAvailable = runtime.maxMemory() - used;
    }

    /**
     * Get the maximum available memory of the current JVM.
     *
     * @return maximum available memory
     */
    public static long maxAvailable() {
        checkAndScheduleRefresh();
        return maxAvailable;
    }

    /**
     * Take the current JVM's maximum available memory
     * as a percentage of the result as the limit.
     *
     * @param percentage percentage
     * @return available memory
     */
    public static long calculate(final float percentage) {
        if (percentage <= 0 || percentage > 1) {
            throw new IllegalArgumentException();
        }
        checkAndScheduleRefresh();
        return (long) (maxAvailable() * percentage);
    }

    /**
     * By default, it takes 80% of the maximum available memory of the current JVM.
     *
     * @return available memory
     */
    public static long defaultLimit() {
        checkAndScheduleRefresh();
        return (long) (maxAvailable() * 0.8);
    }
}
