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


package org.apache.shenyu.plugin.huawei.lts.client;

import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CloudLogCallbackBackpressureTest {

    @Test
    void saturatedPoolRunsCallbackOnSubmittingThread() throws Exception {
        ThreadPoolExecutor executor = ReflectionTestUtils.invokeMethod(HuaweiLtsLogCollectClient.class, "createThreadPoolExecutor", 1);
        assertEquals(60, executor.getKeepAliveTime(TimeUnit.SECONDS));
        executor.setMaximumPoolSize(1);
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        try {
            executor.execute(() -> {
                entered.countDown();
                try {
                    release.await();
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
                }
            });
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            int capacity = executor.getQueue().remainingCapacity();
            for (int i = 0; i < capacity; i++) {
                executor.execute(() -> { });
            }
            AtomicReference<Thread> callbackThread = new AtomicReference<>();
            executor.execute(() -> callbackThread.set(Thread.currentThread()));
            assertSame(Thread.currentThread(), callbackThread.get());
            assertEquals(capacity, executor.getQueue().size());
        } finally {
            executor.shutdownNow();
            release.countDown();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }
}
