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

package org.apache.shenyu.disruptor;

import org.apache.shenyu.disruptor.consumer.QueueConsumerFactory;
import org.apache.shenyu.disruptor.provider.DisruptorProvider;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class DisruptorProviderManagerTest {

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testRepeatedStartupRetainsProviderAndMode(final boolean orderly) {
        QueueConsumerFactory<String> factory = mock(QueueConsumerFactory.class);
        DisruptorProviderManage<String> manager = new DisruptorProviderManage<>(factory, 1, 16);
        Set<DisruptorProvider<String>> providers = new HashSet<>();
        try {
            manager.startup(orderly);
            DisruptorProvider<String> provider = manager.getProvider();
            providers.add(provider);
            assertNotNull(provider);
            manager.startup();
            providers.add(manager.getProvider());
            assertSame(provider, manager.getProvider());
            manager.startup(true);
            providers.add(manager.getProvider());
            assertSame(provider, manager.getProvider());
            if (orderly) {
                assertThrows(IllegalArgumentException.class, () -> provider.onData("data"));
            } else {
                assertThrows(IllegalArgumentException.class, () -> provider.onOrderlyData("data", "key"));
            }
            verify(factory, times(1)).fixName();
        } finally {
            providers.forEach(DisruptorProvider::shutdown);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testConcurrentStartup(final boolean orderly) throws Exception {
        QueueConsumerFactory<String> factory = mock(QueueConsumerFactory.class);
        DisruptorProviderManage<String> manager = new DisruptorProviderManage<>(factory, 1, 16);
        int callers = 8;
        ExecutorService executor = Executors.newFixedThreadPool(callers);
        CountDownLatch ready = new CountDownLatch(callers);
        CountDownLatch start = new CountDownLatch(1);
        List<Future<DisruptorProvider<String>>> results = new ArrayList<>();
        Set<DisruptorProvider<String>> providers = new HashSet<>();
        try {
            for (int i = 0; i < callers; i++) {
                results.add(executor.submit(() -> {
                    ready.countDown();
                    assertTrue(start.await(5, TimeUnit.SECONDS));
                    manager.startup(orderly);
                    return manager.getProvider();
                }));
            }
            assertTrue(ready.await(5, TimeUnit.SECONDS));
            start.countDown();
            for (Future<DisruptorProvider<String>> result : results) {
                providers.add(result.get(5, TimeUnit.SECONDS));
            }
            assertNotNull(manager.getProvider());
            for (DisruptorProvider<String> provider : providers) {
                assertSame(manager.getProvider(), provider);
            }
            verify(factory, times(1)).fixName();
        } finally {
            start.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
            providers.forEach(DisruptorProvider::shutdown);
        }
    }

    @Test
    void testStartupCanRetryAfterFailure() {
        QueueConsumerFactory<String> factory = mock(QueueConsumerFactory.class);
        when(factory.fixName()).thenThrow(new IllegalStateException("startup failed")).thenReturn("retry");
        DisruptorProviderManage<String> manager = new DisruptorProviderManage<>(factory, 1, 16);
        try {
            assertThrows(IllegalStateException.class, manager::startup);
            assertNull(manager.getProvider());
            manager.startup();
            assertNotNull(manager.getProvider());
            verify(factory, times(2)).fixName();
        } finally {
            if (Objects.nonNull(manager.getProvider())) {
                manager.getProvider().shutdown();
            }
        }
    }
}
