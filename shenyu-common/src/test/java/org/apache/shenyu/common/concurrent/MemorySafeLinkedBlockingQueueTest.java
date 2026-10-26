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

import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

public class MemorySafeLinkedBlockingQueueTest {
    @Test
    public void test() {
        // A floor above every byte the JVM can ever commit rejects deterministically.
        // Integer.MAX_VALUE is the largest floor the constructor accepts, so skip
        // on heaps that are at least that large.
        assumeTrue(Runtime.getRuntime().maxMemory() < Integer.MAX_VALUE);
        MemorySafeLinkedBlockingQueue<Runnable> queue = new MemorySafeLinkedBlockingQueue<>((int) (Runtime.getRuntime().maxMemory() + 1));
        // all memory is reserved for JVM, so it will fail here
        assertThat(queue.offer(() -> {
        }), is(false));

        // only 1 Byte memory is reserved for the JVM, so this will succeed
        queue.setMaxFreeMemory(1);
        assertThat(queue.offer(() -> {
        }), is(true));
    }

    @Test
    public void testAdmissionCountsMemoryTheHeapCanStillCommit() {
        final Runtime runtime = Runtime.getRuntime();
        final long committed = runtime.totalMemory();
        final long free = runtime.freeMemory();
        final long max = runtime.maxMemory();
        // Only meaningful when the committed heap can still grow toward -Xmx.
        assumeTrue(max - committed > 32L * 1024 * 1024);
        final long headroom = max - (committed - free);
        // A floor that cannot be met from the free committed heap alone, but that
        // still leaves more memory available than the headroom to -Xmx.
        final long floor = (free + headroom) / 2;
        assumeTrue(floor > free && floor < headroom && floor < Integer.MAX_VALUE);
        MemorySafeLinkedBlockingQueue<Runnable> queue = new MemorySafeLinkedBlockingQueue<>((int) floor);
        assertThat(queue.offer(() -> {
        }), is(true));
    }

    @Test
    public void testCustomReject() {
        assumeTrue(Runtime.getRuntime().maxMemory() < Integer.MAX_VALUE);
        MemorySafeLinkedBlockingQueue<Runnable> queue = new MemorySafeLinkedBlockingQueue<>((int) (Runtime.getRuntime().maxMemory() + 1));
        queue.setRejector(new AbortPolicy<>());
        assertThrows(RejectException.class, () -> queue.offer(() -> {
        }));
    }

    @Test
    public void testDiscardOldestDoesNotReenterMemoryCheck() {
        Runnable oldest = () -> { };
        Runnable replacement = () -> { };
        assumeTrue(Runtime.getRuntime().maxMemory() < Integer.MAX_VALUE);
        MemorySafeLinkedBlockingQueue<Runnable> queue =
                new MemorySafeLinkedBlockingQueue<>(Collections.singletonList(oldest), (int) (Runtime.getRuntime().maxMemory() + 1));
        queue.setRejector(new DiscardOldestPolicy<>());

        assertThat(queue.offer(replacement), is(false));
        assertThat(queue.size(), is(1));
        assertThat(queue.peek(), is(replacement));
    }

    @Test
    public void testMutableConfigurationIsVolatile() throws NoSuchFieldException {
        Field maxFreeMemory = MemorySafeLinkedBlockingQueue.class.getDeclaredField("maxFreeMemory");
        Field rejector = MemorySafeLinkedBlockingQueue.class.getDeclaredField("rejector");

        assertTrue(Modifier.isVolatile(maxFreeMemory.getModifiers()));
        assertTrue(Modifier.isVolatile(rejector.getModifiers()));
    }
}
