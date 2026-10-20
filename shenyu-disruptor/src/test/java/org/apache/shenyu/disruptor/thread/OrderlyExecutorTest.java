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

package org.apache.shenyu.disruptor.thread;

import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OrderlyExecutorTest {

    @Test
    void testShutdownWaitsForWorkerAndRejectsNewTasks() throws Exception {
        OrderlyExecutor executor = newExecutor();
        CountDownLatch running = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        SingletonExecutor worker = executor.select("key");
        try {
            worker.execute(() -> {
                running.countDown();
                await(release);
            });
            assertTrue(running.await(2, TimeUnit.SECONDS));
            executor.shutdown();
            assertTrue(worker.isShutdown());
            assertFalse(executor.isTerminated());
            assertFalse(executor.awaitTermination(1, TimeUnit.MILLISECONDS));
            assertThrows(RejectedExecutionException.class, () -> worker.execute(() -> { }));
            release.countDown();
            assertTrue(executor.awaitTermination(2, TimeUnit.SECONDS));
            assertTrue(executor.isTerminated());
        } finally {
            release.countDown();
            executor.shutdownNow();
        }
    }

    @Test
    void testShutdownNowReturnsWorkerQueueAndInterruptsWorker() throws Exception {
        OrderlyExecutor executor = newExecutor();
        CountDownLatch running = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        Runnable pending = () -> { };
        try {
            SingletonExecutor worker = executor.select("key");
            worker.execute(() -> {
                running.countDown();
                await(release);
            });
            assertTrue(running.await(2, TimeUnit.SECONDS));
            worker.execute(pending);
            assertEquals(Collections.singletonList(pending), executor.shutdownNow());
            assertTrue(executor.awaitTermination(2, TimeUnit.SECONDS));
        } finally {
            release.countDown();
            executor.shutdownNow();
        }
    }

    private OrderlyExecutor newExecutor() {
        return new OrderlyExecutor(true, 2, 2, 0, TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy());
    }

    private void await(final CountDownLatch latch) {
        try {
            latch.await();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
