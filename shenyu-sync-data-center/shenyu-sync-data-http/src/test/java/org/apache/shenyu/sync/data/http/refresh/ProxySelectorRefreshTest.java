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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for {@link ProxySelectorRefresh}.
 */
public final class ProxySelectorRefreshTest {

    private final StubProxySelectorDataSubscriber subscriber = new StubProxySelectorDataSubscriber();

    private final ProxySelectorRefresh proxySelectorRefresh =
            new ProxySelectorRefresh(Collections.singletonList(subscriber));

    @BeforeEach
    @AfterEach
    public void clearProxySelectorGroup() {
        AbstractDataRefresh.GROUP_CACHE.remove(ConfigGroupEnum.PROXY_SELECTOR);
    }

    @Test
    public void testRefreshWithEmptyDataShouldClearSubscriberCache() {
        List<ProxySelectorData> empty = Collections.emptyList();
        proxySelectorRefresh.refresh(empty);
        assertTrue(subscriber.refreshed, "empty snapshot must invoke subscriber.refresh() to clear stale data");
        assertFalse(subscriber.subscribed, "empty snapshot must not subscribe data");
    }

    @Test
    public void testRefreshWithDataShouldSubscribe() {
        proxySelectorRefresh.refresh(Collections.singletonList(new ProxySelectorData()));
        assertTrue(subscriber.subscribed);
    }

    @Test
    public void testNonEmptySnapshotRemovesOnlyMissingSelector() {
        assertTrue(proxySelectorRefresh.refresh(snapshot(1, "alpha", "beta")));
        assertTrue(proxySelectorRefresh.refresh(snapshot(2, "beta")));

        assertEquals(Set.of("beta"), subscriber.names);
        assertEquals(List.of("alpha"), subscriber.removed);
        assertFalse(subscriber.refreshed, "the retained selector must not be torn down by a full clear");

        assertFalse(proxySelectorRefresh.refresh(snapshot(1, "alpha", "beta")));
        assertEquals(Set.of("beta"), subscriber.names);
        assertTrue(proxySelectorRefresh.refresh(snapshot(3)));
        assertTrue(subscriber.names.isEmpty());
        assertTrue(subscriber.refreshed);
    }

    @Test
    public void testNewerSnapshotWaitsForOlderCallbacks() throws Exception {
        ExecutorService executor = Executors.newFixedThreadPool(2);
        CountDownLatch newerStarted = new CountDownLatch(1);
        try {
            final Future<Boolean> older = executor.submit(() -> proxySelectorRefresh.refresh(snapshot(1, "blocked")));
            assertTrue(subscriber.callbackEntered.await(5, TimeUnit.SECONDS));
            final Future<Boolean> newer = executor.submit(() -> {
                newerStarted.countDown();
                return proxySelectorRefresh.refresh(snapshot(2, "beta"));
            });
            assertTrue(newerStarted.await(5, TimeUnit.SECONDS));
            assertThrows(TimeoutException.class, () -> newer.get(500, TimeUnit.MILLISECONDS));
            subscriber.releaseCallback.countDown();
            assertTrue(older.get(5, TimeUnit.SECONDS));
            assertTrue(newer.get(5, TimeUnit.SECONDS));
            assertEquals(Set.of("beta"), subscriber.names);
            assertEquals(List.of("blocked"), subscriber.removed);
        } finally {
            subscriber.releaseCallback.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    private JsonObject snapshot(final int version, final String... names) {
        JsonArray entries = new JsonArray();
        for (String name : names) {
            JsonObject selector = new JsonObject();
            selector.addProperty("id", name);
            selector.addProperty("name", name);
            selector.addProperty("pluginName", "tcp");
            entries.add(selector);
        }
        JsonObject group = new JsonObject();
        group.addProperty("md5", "v" + version);
        group.addProperty("lastModifyTime", version);
        group.add("data", entries);
        JsonObject response = new JsonObject();
        response.add(ConfigGroupEnum.PROXY_SELECTOR.name(), group);
        return response;
    }

    private static final class StubProxySelectorDataSubscriber implements ProxySelectorDataSubscriber {

        private final Set<String> names = new HashSet<>();

        private final List<String> removed = new ArrayList<>();

        private final CountDownLatch callbackEntered = new CountDownLatch(1);

        private final CountDownLatch releaseCallback = new CountDownLatch(1);

        private boolean refreshed;

        private boolean subscribed;

        @Override
        public void onSubscribe(final ProxySelectorData proxySelectorData) {
            if ("blocked".equals(proxySelectorData.getName())) {
                callbackEntered.countDown();
                try {
                    assertTrue(releaseCallback.await(5, TimeUnit.SECONDS));
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new AssertionError(e);
                }
            }
            names.add(proxySelectorData.getName());
            subscribed = true;
        }

        @Override
        public void unSubscribe(final ProxySelectorData proxySelectorData) {
            removed.add(proxySelectorData.getName());
            names.remove(proxySelectorData.getName());
        }

        @Override
        public void refresh() {
            names.clear();
            refreshed = true;
        }
    }
}
