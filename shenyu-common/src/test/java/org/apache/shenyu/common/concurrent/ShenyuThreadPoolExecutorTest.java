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

import net.bytebuddy.agent.ByteBuddyAgent;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.lang.instrument.Instrumentation;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for ShenyuThreadPoolExecutor.
 */
public class ShenyuThreadPoolExecutorTest {

    private static Instrumentation instrumentation;

    @BeforeAll
    public static void initialExecutor() {
        ByteBuddyAgent.install();
        instrumentation = ByteBuddyAgent.getInstrumentation();
    }

    @Test
    public void testNullCommand() {
        ShenyuThreadPoolExecutor executor = getTestExecutor(new MemoryLimitedTaskQueue<>(instrumentation));
        assertThrows(NullPointerException.class, () -> executor.execute(null));
    }

    @ParameterizedTest
    @MethodSource("taskQueues")
    public void testGrowsWhenCoreWorkerIsBusy(final TaskQueue<Runnable> queue) throws Exception {
        ShenyuThreadPoolExecutor executor = getTestExecutor(queue);
        CountDownLatch started = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        try {
            final Future<?> first = executor.submit(() -> {
                started.countDown();
                release.await();
                return null;
            });
            assertTrue(started.await(5, TimeUnit.SECONDS));
            executor.submit(() -> { }).get(5, TimeUnit.SECONDS);
            release.countDown();
            first.get(5, TimeUnit.SECONDS);
        } finally {
            release.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @ParameterizedTest
    @MethodSource("taskQueues")
    public void testOffersToIdleWorker(final TaskQueue<Runnable> queue) {
        EagerExecutorService executor = mock(EagerExecutorService.class);
        when(executor.getPoolSize()).thenReturn(2);
        when(executor.getActiveCount()).thenReturn(1);
        when(executor.getMaximumPoolSize()).thenReturn(3);
        queue.setExecutor(executor);
        Runnable task = () -> { };

        assertTrue(queue.offer(task));
        assertSame(task, queue.poll());
    }

    private static Stream<TaskQueue<Runnable>> taskQueues() {
        return Stream.of(new MemorySafeTaskQueue<>(1), new MemoryLimitedTaskQueue<>(instrumentation));
    }

    private ShenyuThreadPoolExecutor getTestExecutor(final TaskQueue<Runnable> queue) {
        return new ShenyuThreadPoolExecutor(1, 2, 60, TimeUnit.SECONDS, queue,
                ShenyuThreadFactory.create("Test", true), new ThreadPoolExecutor.AbortPolicy());
    }

}
