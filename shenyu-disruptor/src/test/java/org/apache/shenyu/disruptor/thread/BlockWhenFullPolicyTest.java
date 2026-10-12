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

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BlockWhenFullPolicyTest {

    @Test
    void testBackpressurePreservesOrder() throws Exception {
        SingletonExecutor executor = new SingletonExecutor(Executors.defaultThreadFactory(), 1);
        CountDownLatch started = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        List<Integer> order = new CopyOnWriteArrayList<>();
        FutureTask<Void> submit = new FutureTask<>(() -> {
            executor.execute(() -> order.add(3));
            return null;
        });
        Thread producer = new Thread(submit);
        try {
            executor.execute(() -> {
                started.countDown();
                await(release);
                order.add(1);
            });
            assertTrue(started.await(5, TimeUnit.SECONDS));
            executor.execute(() -> order.add(2));
            producer.start();
            assertThrows(TimeoutException.class, () -> submit.get(100, TimeUnit.MILLISECONDS));
            assertEquals(1, executor.getQueue().size());
            release.countDown();
            submit.get(5, TimeUnit.SECONDS);
            executor.shutdown();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
            assertEquals(Arrays.asList(1, 2, 3), order);
        } finally {
            release.countDown();
            producer.interrupt();
            producer.join(5000);
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test
    void testShutdownReleasesBlockedProducer() throws Exception {
        SingletonExecutor executor = new SingletonExecutor(Executors.defaultThreadFactory(), 1);
        CountDownLatch started = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        FutureTask<Void> submit = new FutureTask<>(() -> {
            assertThrows(RejectedExecutionException.class, () -> executor.execute(() -> { }));
            return null;
        });
        Thread producer = new Thread(submit);
        try {
            executor.execute(() -> {
                started.countDown();
                await(release);
            });
            assertTrue(started.await(5, TimeUnit.SECONDS));
            executor.execute(() -> { });
            producer.start();
            assertThrows(TimeoutException.class, () -> submit.get(100, TimeUnit.MILLISECONDS));
            executor.shutdown();
            submit.get(5, TimeUnit.SECONDS);
        } finally {
            release.countDown();
            producer.interrupt();
            producer.join(5000);
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test
    void testInterruptedProducerRestoresInterruptFlag() {
        SingletonExecutor executor = new SingletonExecutor(Executors.defaultThreadFactory(), 1);
        executor.getQueue().add(() -> { });
        try {
            Thread.currentThread().interrupt();
            assertThrows(RejectedExecutionException.class, () -> new BlockWhenFullPolicy().rejectedExecution(() -> { }, executor));
            assertTrue(Thread.currentThread().isInterrupted());
        } finally {
            Thread.interrupted();
            executor.shutdownNow();
        }
    }

    private void await(final CountDownLatch latch) {
        try {
            latch.await();
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
        }
    }
}

