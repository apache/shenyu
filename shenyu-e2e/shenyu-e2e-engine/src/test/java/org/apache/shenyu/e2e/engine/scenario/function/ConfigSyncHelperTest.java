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

package org.apache.shenyu.e2e.engine.scenario.function;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.model.data.PluginData;
import org.apache.shenyu.e2e.model.data.SelectorCacheData;
import org.awaitility.core.ConditionTimeoutException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ConfigSyncHelperTest {

    private HttpServer server;

    private GatewayClient gateway;

    private Supplier<String> selectors = () -> "[]";

    private Supplier<String> rules = () -> "[]";

    @BeforeEach
    void setUp() throws IOException {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/actuator/selectorData", exchange -> respond(exchange, "[{\"divide\":" + selectors.get() + "}]"));
        server.createContext("/actuator/ruleData", exchange -> respond(exchange, "[{\"selector\":" + rules.get() + "}]"));
        server.createContext("/actuator/pluginData", exchange -> respond(exchange,
                "[{\"gateway-cache-key\":{\"id\":\"29\",\"name\":\"loggingRocketMQ\",\"enabled\":true,\"sort\":170,"
                        + "\"namespaceId\":\"namespace\",\"config\":\"{\\\"topic\\\":\\\"logs\\\"}\",\"pluginJar\":null}}]"));
        server.start();
        gateway = new GatewayClient("convergence", "gateway", "http://127.0.0.1:" + server.getAddress().getPort(), new Properties());
    }

    @AfterEach
    void tearDown() {
        server.stop(0);
    }

    @Test
    void readsEverySelectorAndAppliedPluginConfiguration() throws Exception {
        selectors = () -> "[{\"id\":\"old-selector\"},{\"id\":\"new-selector\"}]";
        assertEquals(List.of("old-selector", "new-selector"), gateway.getSelectorCache().stream().map(SelectorCacheData::getId).collect(Collectors.toList()));
        PluginData plugin = gateway.getPluginCache().get("gateway-cache-key");
        assertEquals("loggingRocketMQ", plugin.getName());
        assertEquals("namespace", plugin.getNamespaceId());
        assertEquals(Boolean.TRUE, plugin.getEnabled());
        assertEquals("{\"topic\":\"logs\"}", plugin.getConfig());
    }

    @Test
    void waitsForBothCreatedIdsInsteadOfEqualCacheSizes() {
        AtomicInteger selectorReads = new AtomicInteger();
        AtomicInteger ruleReads = new AtomicInteger();
        selectors = () -> "[{\"id\":\"unrelated-selector\"},{\"id\":\"" + (selectorReads.incrementAndGet() < 2 ? "old-selector" : "new-selector") + "\"}]";
        rules = () -> "[{\"id\":\"unrelated-rule\"},{\"id\":\"" + (ruleReads.incrementAndGet() < 3 ? "old-rule" : "new-rule") + "\"}]";
        ConfigSyncHelper.awaitCreated(gateway, List.of("new-selector"), List.of("new-rule"));
        assertTrue(selectorReads.get() >= 3);
        assertTrue(ruleReads.get() >= 3);
    }

    @Test
    void waitsForRulesToDisappearAfterSelectorDeletion() {
        AtomicInteger ruleReads = new AtomicInteger();
        selectors = () -> "[{\"id\":\"unrelated-selector\"}]";
        rules = () -> ruleReads.incrementAndGet() < 3 ? "[{\"id\":\"deleted-rule\"}]" : "[{\"id\":\"unrelated-rule\"}]";
        ConfigSyncHelper.awaitDeleted(gateway, List.of("deleted-selector"), List.of("deleted-rule"));
        assertTrue(ruleReads.get() >= 3);
    }

    @Test
    void stopsBeforeBusinessAssertionsWhenConfigurationDoesNotConverge() {
        selectors = () -> "[{\"id\":\"old-selector\"}]";
        rules = () -> "[{\"id\":\"old-rule\"}]";
        AtomicBoolean businessExecuted = new AtomicBoolean();
        ConditionTimeoutException failure = assertThrows(ConditionTimeoutException.class, () -> {
            ConfigSyncHelper.awaitCreated(gateway, List.of("missing-selector"), List.of("missing-rule"), Duration.ofMillis(400));
            businessExecuted.set(true);
        });
        assertFalse(businessExecuted.get());
        assertTrue(failure.getMessage().contains("missing-selector"));
    }

    private void respond(final HttpExchange exchange, final String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(200, bytes.length);
        exchange.getResponseBody().write(bytes);
        exchange.close();
    }
}
