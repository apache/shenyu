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

import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Shenyu System clock.<br>
 * Refer to cn.hutool.core.date.SystemClock<br>
 */
public class ShenyuClock {

    /**
     * Period
     */
    private final int period;

    /**
     * Now
     */
    private final AtomicLong now;

    /**
     * Thread name
     */
    private static final String THREAD_NAME = "shenyu-clock-schedule";

    /**
     * Instance holder.
     */
    private static class InstanceHolder {

        /**
         * System clock instance
         */
        private static final ShenyuClock INSTANCE = new ShenyuClock(1);
    }

    private ShenyuClock(int period) {
        this.period = period;
        this.now = new AtomicLong(System.currentTimeMillis());
        scheduleClockUpdating();
    }

    /**
     * Instance.
     *
     * @return System clock instance
     */
    private static ShenyuClock instance() {
        return InstanceHolder.INSTANCE;
    }

    /**
     * Schedule clock updating.
     */
    private void scheduleClockUpdating() {
        ScheduledThreadPoolExecutor scheduler = new ScheduledThreadPoolExecutor(1, runnable -> {
            Thread thread = new Thread(runnable, THREAD_NAME);
            thread.setDaemon(true);
            return thread;
        });
        scheduler.scheduleAtFixedRate(() -> now.set(System.currentTimeMillis()), period, period, TimeUnit.MILLISECONDS);
    }

    /**
     * Current time millis.
     *
     * @return current time millis
     */
    private long currentTimeMillis() {
        return now.get();
    }

    /**
     * Now.
     *
     * @return current time millis
     */
    public static long now() {
        return instance().currentTimeMillis();
    }
}
