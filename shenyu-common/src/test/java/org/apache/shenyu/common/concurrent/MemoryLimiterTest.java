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
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;


/**
 * Test cases for MemoryLimiter.
 */
public final class MemoryLimiterTest {

    private static Instrumentation instrumentation;

    private static Object testObject;

    private static long testObjectSize;

    @BeforeAll
    public static void initializeInstrumentation() {
        ByteBuddyAgent.install();
        instrumentation = ByteBuddyAgent.getInstrumentation();
        testObject = Integer.MAX_VALUE;
        testObjectSize = instrumentation.getObjectSize(testObject);
    }

    @Test
    public void testCreateMemoryLimiterWhenIllegal() {
        long lessThanZero = -1;
        assertThrows(IllegalArgumentException.class, () -> new MemoryLimiter(lessThanZero, instrumentation));
    }

    @Test
    public void testSetMemoryLimiterWhenIllegal() {
        long lessThanZero = -1;
        MemoryLimiter memoryLimiter = new MemoryLimiter(instrumentation);
        assertThrows(IllegalArgumentException.class, () -> memoryLimiter.setMemoryLimit(lessThanZero));
    }

    @Test
    public void testAcquireWhenNullObject() {
        MemoryLimiter memoryLimiter = new MemoryLimiter(instrumentation);
        assertThrows(NullPointerException.class, () -> memoryLimiter.acquire(null));
        assertThrows(NullPointerException.class, () -> memoryLimiter.acquire(null, 10, TimeUnit.SECONDS));
        assertThrows(NullPointerException.class, () -> memoryLimiter.acquireInterruptibly(null));
    }

    @Test
    public void testAcquireWhenEqualToLimit() {
        MemoryLimiter memoryLimiter = new MemoryLimiter(testObjectSize, instrumentation);
        assertFalse(memoryLimiter.acquire(testObject));
    }

    @Test
    public void testAcquireWhenExceedLimit() {
        MemoryLimiter memoryLimiter = new MemoryLimiter(testObjectSize + 1, instrumentation);
        assertTrue(memoryLimiter.acquire(testObject));
        memoryLimiter.setMemoryLimit(testObjectSize - 1);
        assertFalse(memoryLimiter.acquire(testObject));
        memoryLimiter.setMemoryLimit(testObjectSize + 1);
        assertFalse(memoryLimiter.acquire(testObject));
    }

    @Test
    public void testAcquireConcurrent() throws Exception {
        MemoryLimiter memoryLimiter = new MemoryLimiter(testObjectSize * 2 + 1, instrumentation);
        ExecutorService executorService = Executors.newFixedThreadPool(2);
        // two thread acquire concurrently.
        for (int i = 0; i < 2; i++) {
            Future<Boolean> acquireResult = executorService.submit(() -> memoryLimiter.acquire(testObject));
            assertTrue(acquireResult.get());
        }
        assertEquals(testObjectSize * 2, memoryLimiter.getCurrentMemory());
        executorService.shutdown();
    }

    @Test
    public void testAcquireWithTimeWaitNotRelease() throws InterruptedException {
        MemoryLimiter memoryLimiter = new MemoryLimiter(testObjectSize + 1, instrumentation);
        memoryLimiter.acquire(testObject);
        assertFalse(memoryLimiter.acquire(testObject, 1, TimeUnit.SECONDS));
    }

    @Test
    public void testAcquireWithTimeWaitAfterRelease() throws Exception {
        MemoryLimiter memoryLimiter = new MemoryLimiter(testObjectSize + 1, instrumentation);
        memoryLimiter.acquire(testObject);
        ExecutorService executorService = Executors.newFixedThreadPool(1);
        Future<Boolean> acquireResult = executorService.submit(() -> memoryLimiter.acquire(testObject, 4, TimeUnit.SECONDS));
        Thread.sleep(2000);
        memoryLimiter.release(testObject);
        assertTrue(acquireResult.get());
        assertEquals(testObjectSize, memoryLimiter.getCurrentMemory());
        executorService.shutdown();
    }

    @Test
    public void testAcquireWaitForNotify() throws Exception {
        MemoryLimiter memoryLimiter = new MemoryLimiter(testObjectSize + 1, instrumentation);
        memoryLimiter.acquire(testObject);
        ExecutorService executorService = Executors.newFixedThreadPool(1);
        Future<Boolean> acquireResult = executorService.submit(() -> {
            try {
                memoryLimiter.acquireInterruptibly(testObject);
                return Boolean.TRUE;
            } catch (InterruptedException e) {
                return Boolean.FALSE;
            }
        });
        Thread.sleep(3000);
        memoryLimiter.release(testObject);
        assertTrue(acquireResult.get());
        assertEquals(testObjectSize, memoryLimiter.getCurrentMemory());
        executorService.shutdown();
    }

    @Test
    public void testReleaseWhenNullObject() throws InterruptedException {
        MemoryLimiter memoryLimiter = new MemoryLimiter(testObjectSize + 1, instrumentation);
        memoryLimiter.acquire(testObject);

        memoryLimiter.release(null);
        assertEquals(testObjectSize, memoryLimiter.getCurrentMemory());

        memoryLimiter.releaseInterruptibly(null);
        assertEquals(testObjectSize, memoryLimiter.getCurrentMemory());

        memoryLimiter.releaseInterruptibly(null, 1, TimeUnit.SECONDS);
        assertEquals(testObjectSize, memoryLimiter.getCurrentMemory());
    }

    @Test
    public void testReleaseInterruptiblyWaitForNotify() throws Exception {
        MemoryLimiter memoryLimiter = new MemoryLimiter(testObjectSize + 1, instrumentation);
        ExecutorService executorService = Executors.newFixedThreadPool(1);
        Future<Boolean> acquireResult = executorService.submit(() -> {
            try {
                memoryLimiter.releaseInterruptibly(testObject);
                return Boolean.TRUE;
            } catch (InterruptedException e) {
                return Boolean.FALSE;
            }
        });
        Thread.sleep(3000);
        memoryLimiter.acquire(testObject);
        assertTrue(acquireResult.get());
        assertEquals(0, memoryLimiter.getCurrentMemory());
        executorService.shutdown();
    }

    @Test
    public void testReleaseInterruptiblyWithTimeWait() throws Exception {
        MemoryLimiter memoryLimiter = new MemoryLimiter(testObjectSize + 1, instrumentation);
        ExecutorService executorService = Executors.newFixedThreadPool(1);
        Future<Boolean> acquireResult = executorService.submit(() -> {
            try {
                memoryLimiter.releaseInterruptibly(testObject, 4, TimeUnit.SECONDS);
                return Boolean.TRUE;
            } catch (InterruptedException e) {
                return Boolean.FALSE;
            }
        });
        Thread.sleep(2000);
        memoryLimiter.acquire(testObject);
        assertTrue(acquireResult.get());
        assertEquals(0, memoryLimiter.getCurrentMemory());
        executorService.shutdown();
    }

    @Test
    public void testRemainMemory() {
        MemoryLimiter memoryLimiter = new MemoryLimiter(testObjectSize + 1, instrumentation);
        memoryLimiter.acquire(testObject);
        assertEquals(1, memoryLimiter.getCurrentRemainMemory());
    }
}
