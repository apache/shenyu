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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests scheduler cleanup through the Spring component lifecycle.
 */
class TaskSchedulerManagerTest {

    private AnnotationConfigApplicationContext context;

    private TaskSchedulerManager manager;

    @BeforeEach
    void setUp() {
        context = new AnnotationConfigApplicationContext(ScaleProperties.class, TaskSchedulerManager.class);
        manager = context.getBean(TaskSchedulerManager.class);
    }

    @AfterEach
    void tearDown() {
        context.close();
        // Release threads even if the lifecycle regression assertion fails.
        manager.shutdown();
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testContextCloseShutsDownScheduler(final boolean cancelTaskBeforeClose) throws InterruptedException {
        final ThreadPoolTaskScheduler scheduler = (ThreadPoolTaskScheduler) ReflectionTestUtils.getField(manager, "scheduler");
        final ScheduledThreadPoolExecutor executor = scheduler.getScheduledThreadPoolExecutor();
        final CountDownLatch taskStarted = new CountDownLatch(1);
        manager.registerMonitorTask("monitor", taskStarted::countDown, 10);
        assertTrue(taskStarted.await(5, TimeUnit.SECONDS), "The monitor task should start before closing the context");

        if (cancelTaskBeforeClose) {
            manager.cancelMonitorTask("monitor");
        }
        assertFalse(executor.isShutdown());

        context.close();

        assertTrue(executor.isShutdown(), "Closing the context should shut down the monitor scheduler");
        assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS), "Monitor threads should terminate after context close");
    }
}
