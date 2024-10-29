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

package org.apache.shenyu.admin.scale.scaler.dynamic;

import org.apache.shenyu.admin.scale.config.ScaleProperties;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.time.Duration;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;

@Component
public class TaskSchedulerManager {

    private static final String THREAD_NAME_PREFIX = "MonitorTask-";

    private final ThreadPoolTaskScheduler scheduler;

    private final ConcurrentHashMap<String, ScheduledFuture<?>> taskMap = new ConcurrentHashMap<>();

    public TaskSchedulerManager(final ScaleProperties scaleProperties) {
        this.scheduler = new ThreadPoolTaskScheduler();
        this.scheduler.setPoolSize(scaleProperties.getPoolSize() > 0 ? scaleProperties.getPoolSize() : 1);
        this.scheduler.setThreadNamePrefix(THREAD_NAME_PREFIX);
        this.scheduler.initialize();
    }

    /**
     * registerMonitorTask.
     *
     * @param taskName taskName
     * @param task task
     * @param interval interval
     */
    public void registerMonitorTask(final String taskName, final Runnable task, final long interval) {
        cancelMonitorTask(taskName);
        ScheduledFuture<?> future = scheduler.scheduleAtFixedRate(
                task,
                Instant.now().plusMillis(interval),
                Duration.ofMillis(interval)
        );

        taskMap.put(taskName, future);
    }

    /**
     * cancelMonitorTask.
     *
     * @param taskName taskName
     */
    public void cancelMonitorTask(final String taskName) {
        ScheduledFuture<?> future = taskMap.remove(taskName);
        if (future != null && !future.isCancelled()) {
            future.cancel(true);
        }
    }


    /**
     * shutdown.
     */
    public void shutdown() {
        taskMap.values().forEach(future -> future.cancel(true));
        scheduler.shutdown();
    }
}