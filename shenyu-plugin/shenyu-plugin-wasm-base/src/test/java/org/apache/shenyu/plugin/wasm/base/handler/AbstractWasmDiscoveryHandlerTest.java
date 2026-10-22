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

package org.apache.shenyu.plugin.wasm.base.handler;

import com.dylibso.chicory.runtime.Memory;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.cache.CommonDiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamKey;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AbstractWasmDiscoveryHandlerTest {

    private DiscoverySyncData discoverySyncData;

    @BeforeEach
    public void setUp() {
        discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setPluginName("SHENYU_TEST");
        discoverySyncData.setSelectorId("selector-id");
        discoverySyncData.setSelectorName("upstream-测试");
        discoverySyncData.setUpstreamDataList(List.of(new DiscoveryUpstreamData()));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void handlerDiscoveryUpstreamDataTest(final boolean go) {
        try (WasmTestHandler handler = newHandler(go)) {
            handler.handlerDiscoveryUpstreamData(discoverySyncData);
            assertEquals(List.of(go ? "go result" : "rust result"), handler.results);
            assertSame(discoverySyncData, handler.arguments.get(0));
            assertArgumentsRemoved(handler);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void removeDiscoveryUpstreamDataTest(final boolean go) {
        try (WasmTestHandler handler = newHandler(go)) {
            DiscoveryUpstreamKey key = DiscoveryUpstreamKey.from(discoverySyncData);
            handler.removeDiscoveryUpstreamData(key);
            assertEquals(1, handler.results.size());
            assertDeletionResult(handler, key, 0);
            DiscoveryUpstreamKey unnamedKey = new DiscoveryUpstreamKey("SHENYU_TEST", "another-selector-id", null);
            handler.removeDiscoveryUpstreamData(unnamedKey);
            assertEquals(2, handler.results.size());
            assertDeletionResult(handler, unnamedKey, 1);
            assertArgumentsRemoved(handler);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void subscriberForwardsUpdateAndDeletionTest(final boolean go) {
        try (WasmTestHandler handler = newHandler(go)) {
            CommonDiscoveryUpstreamDataSubscriber subscriber = new CommonDiscoveryUpstreamDataSubscriber(List.of(handler));
            subscriber.onSubscribe(discoverySyncData);
            DiscoveryUpstreamKey key = DiscoveryUpstreamKey.from(discoverySyncData);
            subscriber.unSubscribe(key);
            assertEquals(2, handler.results.size());
            assertEquals(go ? "go result" : "rust result", handler.results.get(0));
            assertDeletionResult(handler, key, 1);
            assertArgumentsRemoved(handler);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void deletionFailureRemovesArgumentsTest(final boolean go) {
        try (WasmTestHandler handler = newHandler(go)) {
            handler.failOnResult = true;
            DiscoveryUpstreamKey key = DiscoveryUpstreamKey.from(discoverySyncData);
            assertThrows(RuntimeException.class, () -> handler.removeDiscoveryUpstreamData(key));
            assertEquals(1, handler.results.size());
            assertDeletionResult(handler, key, 0);
            assertArgumentsRemoved(handler);
        }
    }

    @Test
    void missingOptionalDeletionExportTest() {
        try (WasmTestHandler handler = new TestLegacyWasmPluginDiscoveryHandler()) {
            assertTrue(handler.getWasmExtern("removeDiscoveryUpstreamData").isEmpty());
            DiscoveryUpstreamKey key = DiscoveryUpstreamKey.from(discoverySyncData);
            assertDoesNotThrow(() -> handler.removeDiscoveryUpstreamData(key));
            CommonDiscoveryUpstreamDataSubscriber subscriber = new CommonDiscoveryUpstreamDataSubscriber(List.of(handler));
            assertDoesNotThrow(() -> subscriber.unSubscribe(key));
            assertTrue(handler.arguments.isEmpty());
            assertTrue(handler.argumentIds.isEmpty());
            assertTrue(handler.results.isEmpty());
            subscriber.onSubscribe(discoverySyncData);
            assertEquals(List.of("go result"), handler.results);
            assertArgumentsRemoved(handler);
        }
    }

    private static WasmTestHandler newHandler(final boolean go) {
        return go ? new TestGoWasmPluginDiscoveryHandler() : new TestWasmPluginDiscoveryHandler();
    }

    private static void assertDeletionResult(final WasmTestHandler handler, final DiscoveryUpstreamKey key, final int index) {
        String result = handler.results.get(index);
        assertTrue(result.startsWith("removed:"));
        DiscoverySyncData guestData = GsonUtils.getInstance().fromJson(result.substring("removed:".length()), DiscoverySyncData.class);
        assertEquals(key, DiscoveryUpstreamKey.from(guestData));
        assertNull(guestData.getUpstreamDataList());
        assertNull(guestData.getNamespaceId());
        assertNull(guestData.getDiscoveryHandlerId());
        assertEquals(key, DiscoveryUpstreamKey.from(handler.arguments.get(index)));
    }

    private static void assertArgumentsRemoved(final WasmTestHandler handler) {
        assertTrue(AbstractWasmDiscoveryHandler.ARGUMENTS.isEmpty());
        handler.argumentIds.forEach(id -> assertNull(AbstractWasmDiscoveryHandler.ARGUMENTS.get(id)));
    }

    abstract static class WasmTestHandler extends AbstractWasmDiscoveryHandler {

        private static final AtomicLong ID = new AtomicLong();

        private final List<String> results = new ArrayList<>();

        private final List<DiscoverySyncData> arguments = new ArrayList<>();

        private final List<Long> argumentIds = new ArrayList<>();

        private boolean failOnResult;

        @Override
        protected long onGetArgs(final long argId, final long addr, final int len) {
            DiscoverySyncData argument = ARGUMENTS.get(argId);
            assertNotNull(argument);
            arguments.add(argument);
            byte[] data = GsonUtils.getInstance().toJson(argument).getBytes(StandardCharsets.UTF_8);
            assertTrue(data.length <= len);
            Memory memory = super.getMemory();
            memory.write((int) addr, data, 0, data.length);
            return data.length;
        }

        @Override
        protected long onPutResult(final long argId, final long addr, final int len) {
            byte[] bytes = super.getMemory().readBytes((int) addr, len);
            results.add(new String(bytes, StandardCharsets.UTF_8));
            if (failOnResult) {
                throw new IllegalStateException("discovery callback failed");
            }
            return 0;
        }

        @Override
        public String pluginName() {
            return "SHENYU_TEST";
        }

        @Override
        protected Long getArgumentId(final DiscoverySyncData discoverySyncData) {
            long argumentId = ID.incrementAndGet();
            argumentIds.add(argumentId);
            return argumentId;
        }
    }

    /** Loads $TestGoWasmPluginDiscoveryHandler.wasm. */
    static class TestGoWasmPluginDiscoveryHandler extends WasmTestHandler {
    }

    /** Loads $TestWasmPluginDiscoveryHandler.wasm. */
    static class TestWasmPluginDiscoveryHandler extends WasmTestHandler {
    }

    /** Loads the original Go guest, which only exports the update callback. */
    static class TestLegacyWasmPluginDiscoveryHandler extends WasmTestHandler {
    }
}
