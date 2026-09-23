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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AbstractWasmDiscoveryHandlerTest {

    private DiscoverySyncData discoverySyncData;

    @BeforeEach
    public void setUp() {
        this.discoverySyncData = mock(DiscoverySyncData.class);
        when(discoverySyncData.getSelectorId()).thenReturn("SHENYU");
    }

    /** Go WASM test. */
    @Test
    public void goHandlerDiscoveryUpstreamDataTest() {
        final TestGoWasmPluginDiscoveryHandler goHandler = new TestGoWasmPluginDiscoveryHandler("go result");
        goHandler.handlerDiscoveryUpstreamData(discoverySyncData);
        assertEquals("SHENYU_TEST", goHandler.pluginName());
    }

    /** Rust WASM test. */
    @Test
    public void rustHandlerDiscoveryUpstreamDataTest() {
        final TestWasmPluginDiscoveryHandler rustHandler = new TestWasmPluginDiscoveryHandler("rust result");
        rustHandler.handlerDiscoveryUpstreamData(discoverySyncData);
        assertEquals("SHENYU_TEST", rustHandler.pluginName());
    }

    abstract static class WasmTestHandler extends AbstractWasmDiscoveryHandler {

        private static final java.util.concurrent.atomic.AtomicLong ID =
                new java.util.concurrent.atomic.AtomicLong();

        private final Map<Long, String> results = new ConcurrentHashMap<>();

        private final String expectedResult;

        WasmTestHandler(final String expectedResult) {
            this.expectedResult = expectedResult;
        }

        @Override
        protected long onGetArgs(final long argId, final long addr, final int len) {
            String config = "hello from java " + argId;
            assertTrue(config.startsWith("hello from java "));
            Memory memory = super.getMemory();
            byte[] data = config.getBytes(StandardCharsets.UTF_8);
            int writeLen = Math.min(len, data.length);
            memory.write((int) addr, data, 0, writeLen);
            return writeLen;
        }

        @Override
        protected long onPutResult(final long argId, final long addr, final int len) {
            Memory memory = super.getMemory();
            byte[] bytes = memory.readBytes((int) addr, len);
            String result = new String(bytes, StandardCharsets.UTF_8);
            assertEquals(expectedResult, result);
            results.put(argId, result);
            return 0;
        }

        @Override
        public String pluginName() {
            return "SHENYU_TEST";
        }

        @Override
        protected Long getArgumentId(final DiscoverySyncData discoverySyncData) {
            return ID.incrementAndGet();
        }
    }

    /** Loads $TestGoWasmPluginDiscoveryHandler.wasm. */
    static class TestGoWasmPluginDiscoveryHandler extends WasmTestHandler {
        TestGoWasmPluginDiscoveryHandler(final String expected) {
            super(expected);
        }
    }

    /** Loads $TestWasmPluginDiscoveryHandler.wasm. */
    static class TestWasmPluginDiscoveryHandler extends WasmTestHandler {
        TestWasmPluginDiscoveryHandler(final String expected) {
            super(expected);
        }
    }
}
