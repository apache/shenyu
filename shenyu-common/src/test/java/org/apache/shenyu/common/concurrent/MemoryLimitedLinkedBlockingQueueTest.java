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

import java.lang.instrument.Instrumentation;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for MemoryLimitedLinkedBlcokingQueue.
 */
public class MemoryLimitedLinkedBlockingQueueTest {

    private static Instrumentation instrumentation;

    @BeforeAll
    public static void initialInstrumentation() {
        ByteBuddyAgent.install();
        instrumentation = ByteBuddyAgent.getInstrumentation();
    }

    @Test
    public void test() throws Exception {
        MemoryLimitedLinkedBlockingQueue<Runnable> queue = new MemoryLimitedLinkedBlockingQueue<>(1, instrumentation);
        //an Runnable needs more than 1 byte of space, so it will fail here
        assertThat(queue.offer(() -> {
        }), is(false));

        //will success
        queue.setMemoryLimit(Integer.MAX_VALUE);
        assertThat(queue.offer(() -> {
        }), is(true));
    }

    @Test
    public void testGetMemoryLimit() {
        MemoryLimitedLinkedBlockingQueue<Runnable> queue = new MemoryLimitedLinkedBlockingQueue<>(instrumentation);
        assertEquals(queue.getMemoryLimit(), Integer.MAX_VALUE);
    }

    @Test
    public void testOfferWhenTimeout() throws InterruptedException {
        MemoryLimitedLinkedBlockingQueue<Runnable> queue = new MemoryLimitedLinkedBlockingQueue<>(1, instrumentation);
        assertFalse(queue.offer(() -> {
        }, 2, TimeUnit.SECONDS));
    }

    @Test
    public void testPoll() {
        MemoryLimitedLinkedBlockingQueue<Integer> queue = new MemoryLimitedLinkedBlockingQueue<>(instrumentation);
        Integer testObject = 0;
        queue.offer(testObject);
        assertEquals(testObject, queue.poll());
        assertEquals(0, queue.getCurrentMemory());
    }

    @Test
    public void testClear() {
        MemoryLimitedLinkedBlockingQueue<Runnable> queue = new MemoryLimitedLinkedBlockingQueue<>(instrumentation);
        queue.offer(() -> {
        });
        queue.clear();
        assertEquals(0, queue.getCurrentMemory());
    }

    @Test
    public void testPut() throws InterruptedException, ExecutionException {
        Integer testObject = 0;
        long testObjectSize = instrumentation.getObjectSize(testObject);
        MemoryLimitedLinkedBlockingQueue<Integer> queue = new MemoryLimitedLinkedBlockingQueue<>(2 * testObjectSize + 1, instrumentation);
        queue.put(testObject);
        queue.put(testObject);
        assertEquals(2, queue.size());
        ExecutorService executorService = Executors.newFixedThreadPool(1);
        Future<Boolean> putResult = executorService.submit(() -> {
            try {
                queue.put(testObject);
                return Boolean.TRUE;
            } catch (InterruptedException e) {
                return Boolean.FALSE;
            }
        });
        Thread.sleep(2000);
        queue.poll();
        assertTrue(putResult.get());
        assertEquals(2, queue.size());
    }

    @Test
    void testTake() throws InterruptedException, ExecutionException {
        MemoryLimitedLinkedBlockingQueue<Runnable> queue = new MemoryLimitedLinkedBlockingQueue<>(instrumentation);
        ExecutorService executorService = Executors.newFixedThreadPool(1);
        Future<Runnable> takeResult = executorService.submit(queue::take);
        Thread.sleep(2000);
        queue.put(() -> { });
        takeResult.get();
        assertEquals(0, queue.size());
    }

    @Test
    void testPollWhenTimeoutWithNull() throws InterruptedException {
        MemoryLimitedLinkedBlockingQueue<Runnable> queue = new MemoryLimitedLinkedBlockingQueue<>(instrumentation);
        Runnable runnable = queue.poll(1, TimeUnit.SECONDS);
        assertNull(runnable);
    }

    @Test
    void testRemoveSuccess() throws InterruptedException {
        MemoryLimitedLinkedBlockingQueue<Integer> queue = new MemoryLimitedLinkedBlockingQueue<>(instrumentation);
        Integer testObject = 0;
        queue.put(testObject);
        assertTrue(queue.remove(testObject));
        assertEquals(0, queue.getCurrentMemory());
    }
}
