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

package org.apache.shenyu.plugin.agent.gateway.remote;

import org.apache.shenyu.common.dto.PluginData;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedRemoteMcpCatalogTest {

    @Test
    void resolvesCredentialsOnOwnedWorkerAndDropsSupersededEnableEvents() throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        final AtomicInteger resolutions = new AtomicInteger();
        final AtomicBoolean workerThread = new AtomicBoolean();
        URI endpoint = URI.create("https://192.0.2.10:443/mcp");
        final ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(endpoint), Set.of(), target -> {
            resolutions.incrementAndGet();
            workerThread.set(Thread.currentThread().getName().startsWith("agent-mcp-config"));
            entered.countDown();
            try {
                assertTrue(release.await(10, TimeUnit.SECONDS));
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
            }
            throw new SecurityException("SECRET_STORE_FAILURE");
        });
        try {
            catalog.accept(event(true, config()));
            assertTrue(entered.await(2, TimeUnit.SECONDS));
            catalog.accept(event(true, config()));
            catalog.accept(event(false, config()));
            release.countDown();
            await(() -> (long) catalog.diagnostics().get("currentVersion") > 1L);
            assertTrue(workerThread.get());
            assertEquals(1, resolutions.get());
            assertEquals(0, catalog.diagnostics().get("clients"));
        } finally {
            release.countDown();
            catalog.closeAsync().block(Duration.ofSeconds(5));
        }
    }

    @Test
    void boundsQueueAndRequiresExplicitRefreshAfterOverflow() throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        final ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(URI.create("https://192.0.2.10:443/mcp")), Set.of(), target -> {
            entered.countDown();
            try {
                assertTrue(release.await(10, TimeUnit.SECONDS));
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
            }
            throw new SecurityException("injected");
        });
        try {
            catalog.accept(event(true, config()));
            assertTrue(entered.await(2, TimeUnit.SECONDS));
            for (int index = 0; index < 32; index++) {
                catalog.accept(event(true, config()));
            }
            assertEquals("ConfigurationQueueRejected", catalog.diagnostics().get("lastFailure"));
            assertEquals(0L, catalog.diagnostics().get("currentVersion"));
            release.countDown();
            await(() -> {
                if ((long) catalog.diagnostics().get("currentVersion") > 1L) {
                    return true;
                }
                // Recovery remains explicit while the worker drains superseded events.
                catalog.accept(event(true, "{}"));
                return false;
            });
            assertEquals(0, catalog.diagnostics().get("clients"));
        } finally {
            release.countDown();
            catalog.closeAsync().block(Duration.ofSeconds(5));
        }
    }

    private static PluginData event(final boolean enabled, final String source) {
        PluginData event = new PluginData();
        event.setEnabled(enabled);
        event.setConfig(source);
        return event;
    }

    private static String config() {
        return "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[{\"name\":\"orders\",\"endpoint\":\"https://192.0.2.10:443/mcp\","
                + "\"credentialRef\":\"service/orders\",\"credentialVersion\":\"v1\"}]}}";
    }

    private static void await(final java.util.function.BooleanSupplier condition) throws InterruptedException {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
        while (!condition.getAsBoolean() && System.nanoTime() < deadline) {
            Thread.sleep(5);
        }
        assertTrue(condition.getAsBoolean());
    }
}
