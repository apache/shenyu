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

package org.apache.shenyu.sync.data.http.refresh;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PluginDataRefreshRetryTest {

    @BeforeEach
    @AfterEach
    void resetPluginCache() {
        AbstractDataRefresh.GROUP_CACHE.remove(ConfigGroupEnum.PLUGIN);
    }

    @Test
    void testFailedSnapshotRemainsEligibleForRetryFromAnotherWorker() {
        AtomicBoolean failOnce = new AtomicBoolean(true);
        AtomicReference<String> applied = new AtomicReference<>();
        PluginDataSubscriber subscriber = new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData data) {
                if ("newer".equals(data.getName()) && failOnce.getAndSet(false)) {
                    throw new IllegalStateException("temporary subscriber failure");
                }
                applied.set(data.getName());
            }
        };
        PluginDataRefresh firstWorker = new PluginDataRefresh(subscriber);
        final PluginDataRefresh retryWorker = new PluginDataRefresh(subscriber);
        JsonObject older = snapshot("older", 1L);
        JsonObject newer = snapshot("newer", 2L);

        assertTrue(firstWorker.refresh(older));
        assertThrows(IllegalStateException.class, () -> firstWorker.refresh(newer));
        assertEquals("older", applied.get());
        assertEquals("older", retryWorker.cacheConfigData().getMd5());
        assertFalse(retryWorker.refresh(snapshot("stale", 0L)));
        assertTrue(retryWorker.refresh(newer));
        assertEquals("newer", applied.get());
        assertEquals("newer", firstWorker.cacheConfigData().getMd5());
        assertFalse(firstWorker.refresh(newer));
        assertFalse(retryWorker.refresh(older));
        assertEquals("newer", applied.get());
    }

    @Test
    void testConcurrentWorkerDoesNotAdvertiseAnUnappliedSnapshot() throws Exception {
        CountDownLatch applying = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        AtomicReference<String> applied = new AtomicReference<>();
        PluginDataSubscriber subscriber = new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData data) {
                if ("middle".equals(data.getName())) {
                    applying.countDown();
                    try {
                        assertTrue(release.await(5, TimeUnit.SECONDS));
                    } catch (InterruptedException ex) {
                        Thread.currentThread().interrupt();
                        throw new IllegalStateException(ex);
                    }
                }
                applied.set(data.getName());
            }
        };
        PluginDataRefresh firstWorker = new PluginDataRefresh(subscriber);
        PluginDataRefresh secondWorker = new PluginDataRefresh(subscriber);
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            assertTrue(firstWorker.refresh(snapshot("older", 1L)));
            final Future<Boolean> middle = executor.submit(() -> firstWorker.refresh(snapshot("middle", 2L)));
            assertTrue(applying.await(5, TimeUnit.SECONDS));
            assertEquals("older", secondWorker.cacheConfigData().getMd5());

            Future<Boolean> newest = executor.submit(() -> secondWorker.refresh(snapshot("newest", 3L)));
            release.countDown();
            assertTrue(middle.get(5, TimeUnit.SECONDS));
            assertTrue(newest.get(5, TimeUnit.SECONDS));
            assertEquals("newest", applied.get());
            assertEquals("newest", firstWorker.cacheConfigData().getMd5());
        } finally {
            release.countDown();
            executor.shutdownNow();
        }
    }

    private static JsonObject snapshot(final String name, final long version) {
        PluginData plugin = new PluginData();
        plugin.setName(name);
        JsonObject input = new JsonObject();
        input.add(ConfigGroupEnum.PLUGIN.name(), GsonUtils.getGson().toJsonTree(new ConfigData<>(name, version, List.of(plugin))));
        return input;
    }
}
