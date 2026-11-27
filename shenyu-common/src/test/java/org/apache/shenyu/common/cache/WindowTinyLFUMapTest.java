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

package org.apache.shenyu.common.cache;

import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * WindowTinyLFUMapTest.
 */
public class WindowTinyLFUMapTest {
    
    @Test
    @SuppressWarnings("all")
    public void weakKeyCache() {
        Map<String, String> map = new WindowTinyLFUMap<>(100, 100, Boolean.TRUE);
        String key1 = new String("abc");
        String key2 = new String("abc");
        map.put(key1, "1");
        map.put(key2, "1");
        Assert.assertEquals(2, map.size());
        Assert.assertEquals(key1, key2);
        Assert.assertNull(map.get("abc"));
    }
    
    @Test
    @SuppressWarnings("all")
    public void strongKeyCache() {
        Map<String, String> map = new WindowTinyLFUMap<>(100, 100, Boolean.FALSE);
        String key1 = new String("abc");
        String key2 = new String("abc");
        map.put(key1, "1");
        map.put(key2, "1");
        Assert.assertEquals(map.get(key1), map.get(key2));
        Assert.assertEquals(1, map.size());
    }
    
    /**
     * Several threads must not build their own value for the same first-access key:
     * Map's default get-then-put computeIfAbsent lets every racer run the mapping
     * function and keeps only one winner's value, silently discarding the mutations
     * the losers already applied to their own instances (#8441 review).
     */
    @Test
    public void computeIfAbsentIsAtomicForConcurrentFirstAccessOfOneKey() throws Exception {
        final WindowTinyLFUMap<String, Object> map = new WindowTinyLFUMap<>(1000);
        final int threads = 8;
        final CountDownLatch start = new CountDownLatch(1);
        final AtomicInteger constructions = new AtomicInteger();
        final List<Object> results = new CopyOnWriteArrayList<>();
        final List<Thread> workers = new ArrayList<>();
        for (int i = 0; i < threads; i++) {
            final Thread worker = new Thread(() -> {
                try {
                    start.await();
                } catch (InterruptedException ignored) {
                    Thread.currentThread().interrupt();
                }
                results.add(map.computeIfAbsent("race-key", k -> {
                    constructions.incrementAndGet();
                    // hold the mapping open so every racer is inside at once on the
                    // non-atomic default implementation
                    try {
                        Thread.sleep(50);
                    } catch (InterruptedException ignored) {
                        Thread.currentThread().interrupt();
                    }
                    return new Object();
                }));
            });
            worker.start();
            workers.add(worker);
        }
        start.countDown();
        for (Thread worker : workers) {
            worker.join();
        }
        Assert.assertEquals(1, constructions.get());
        Assert.assertEquals(1, results.stream().distinct().count());
    }
}
