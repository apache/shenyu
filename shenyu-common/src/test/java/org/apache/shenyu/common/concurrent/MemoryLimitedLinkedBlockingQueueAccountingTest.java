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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Accounting coverage for seeded and bulk removals on {@link MemoryLimitedLinkedBlockingQueue}.
 */
public class MemoryLimitedLinkedBlockingQueueAccountingTest {

    private static Instrumentation instrumentation;

    @BeforeAll
    public static void initialInstrumentation() {
        ByteBuddyAgent.install();
        instrumentation = ByteBuddyAgent.getInstrumentation();
    }

    @Test
    void testCollectionConstructorChargesSeedMemory() {
        Integer testObject = 7;
        long testObjectSize = instrumentation.getObjectSize(testObject);
        MemoryLimitedLinkedBlockingQueue<Integer> queue = new MemoryLimitedLinkedBlockingQueue<>(
                List.of(testObject), testObjectSize * 4, instrumentation);
        assertEquals(1, queue.size());
        assertEquals(testObjectSize, queue.getCurrentMemory());
    }

    @Test
    void testDrainToReleasesReservedMemory() {
        Integer first = 1;
        Integer second = 2;
        long itemSize = instrumentation.getObjectSize(first);
        MemoryLimitedLinkedBlockingQueue<Integer> queue = new MemoryLimitedLinkedBlockingQueue<>(itemSize * 4, instrumentation);
        assertTrue(queue.offer(first));
        assertTrue(queue.offer(second));
        List<Integer> drained = new ArrayList<>();
        assertEquals(2, queue.drainTo(drained));
        assertEquals(0, queue.size());
        assertEquals(0, queue.getCurrentMemory());
        assertTrue(queue.offer(first));
    }

    @Test
    void testIteratorRemoveReleasesReservedMemory() {
        Integer first = 1;
        Integer second = 2;
        long itemSize = instrumentation.getObjectSize(first);
        MemoryLimitedLinkedBlockingQueue<Integer> queue = new MemoryLimitedLinkedBlockingQueue<>(itemSize * 4, instrumentation);
        assertTrue(queue.offer(first));
        assertTrue(queue.offer(second));
        Iterator<Integer> iterator = queue.iterator();
        assertEquals(first, iterator.next());
        iterator.remove();
        assertEquals(1, queue.size());
        assertEquals(itemSize, queue.getCurrentMemory());
    }

    @Test
    void testRemoveIfReleasesReservedMemory() {
        Integer first = 1;
        Integer second = 2;
        long itemSize = instrumentation.getObjectSize(first);
        MemoryLimitedLinkedBlockingQueue<Integer> queue = new MemoryLimitedLinkedBlockingQueue<>(itemSize * 4, instrumentation);
        assertTrue(queue.offer(first));
        assertTrue(queue.offer(second));
        assertTrue(queue.removeIf(value -> value.equals(first)));
        assertEquals(1, queue.size());
        assertEquals(itemSize, queue.getCurrentMemory());
    }

    @Test
    void testCollectionConstructorTakeAndPollReleaseMemory() throws InterruptedException {
        Integer testObject = 7;
        long testObjectSize = instrumentation.getObjectSize(testObject);
        MemoryLimitedLinkedBlockingQueue<Integer> taken = new MemoryLimitedLinkedBlockingQueue<>(
                List.of(testObject), testObjectSize * 4, instrumentation);
        assertEquals(testObject, taken.take());
        assertEquals(0, taken.getCurrentMemory());
        assertTrue(taken.offer(testObject));

        MemoryLimitedLinkedBlockingQueue<Integer> polled = new MemoryLimitedLinkedBlockingQueue<>(
                List.of(testObject), testObjectSize * 4, instrumentation);
        assertEquals(testObject, polled.poll());
        assertEquals(0, polled.getCurrentMemory());
        assertEquals(0, polled.size());
    }

    @Test
    void testCollectionConstructorRejectsOverBudgetSeed() {
        Integer first = 1;
        Integer second = 2;
        long itemSize = instrumentation.getObjectSize(first);
        assertThrows(IllegalStateException.class, () -> new MemoryLimitedLinkedBlockingQueue<>(
                List.of(first, second), itemSize + 1, instrumentation));
    }

    @Test
    void testDrainToMaxElementsReleasesOnlyDrainedMemory() {
        Integer first = 1;
        Integer second = 2;
        long itemSize = instrumentation.getObjectSize(first);
        MemoryLimitedLinkedBlockingQueue<Integer> queue = new MemoryLimitedLinkedBlockingQueue<>(itemSize * 4, instrumentation);
        assertTrue(queue.offer(first));
        assertTrue(queue.offer(second));
        List<Integer> drained = new ArrayList<>();
        assertEquals(1, queue.drainTo(drained, 1));
        assertEquals(List.of(first), drained);
        assertEquals(1, queue.size());
        assertEquals(itemSize, queue.getCurrentMemory());
    }

    @Test
    void testDrainToRejectsSelfAndLeavesAccountingUntouched() {
        Integer testObject = 1;
        long itemSize = instrumentation.getObjectSize(testObject);
        MemoryLimitedLinkedBlockingQueue<Integer> queue = new MemoryLimitedLinkedBlockingQueue<>(itemSize * 4, instrumentation);
        assertTrue(queue.offer(testObject));
        assertThrows(IllegalArgumentException.class, () -> queue.drainTo(queue));
        assertEquals(1, queue.size());
        assertEquals(itemSize, queue.getCurrentMemory());
    }

    @Test
    void testRemoveAllAndRetainAllReleaseMemory() {
        Integer first = 1;
        Integer second = 2;
        long itemSize = instrumentation.getObjectSize(first);
        MemoryLimitedLinkedBlockingQueue<Integer> removed = new MemoryLimitedLinkedBlockingQueue<>(itemSize * 6, instrumentation);
        assertTrue(removed.offer(first));
        assertTrue(removed.offer(second));
        assertTrue(removed.removeAll(List.of(first)));
        assertEquals(1, removed.size());
        assertEquals(itemSize, removed.getCurrentMemory());

        MemoryLimitedLinkedBlockingQueue<Integer> retained = new MemoryLimitedLinkedBlockingQueue<>(itemSize * 6, instrumentation);
        assertTrue(retained.offer(first));
        assertTrue(retained.offer(second));
        Integer third = 3;
        assertTrue(retained.offer(third));
        assertTrue(retained.retainAll(List.of(second)));
        assertEquals(1, retained.size());
        assertEquals(itemSize, retained.getCurrentMemory());
        assertEquals(second, retained.poll());
        assertEquals(0, retained.getCurrentMemory());
    }
}
