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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
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

/**
 * Test cases for AbstractWasmPluginDataHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class AbstractWasmPluginDataHandlerTest {

    private PluginData pluginData;

    @BeforeEach
    public void setUp() {
        this.pluginData = mock(PluginData.class);
        when(pluginData.getId()).thenReturn("SHENYU");
    }

    /** Go WASM test. */
    @Test
    public void goHandlerPluginTest() {
        final TestGoWasmPluginDataHandler goHandler = new TestGoWasmPluginDataHandler("go result");
        goHandler.handlerPlugin(pluginData);
    }

    /** Rust WASM test. */
    @Test
    public void rustHandlerPluginTest() {
        final TestWasmPluginDataHandler rustHandler = new TestWasmPluginDataHandler("rust result");
        rustHandler.handlerPlugin(pluginData);
    }

    abstract static class WasmTestHandler extends AbstractWasmPluginDataHandler {

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
        protected Long getPluginArgumentId(final PluginData pluginData) {
            return ID.incrementAndGet();
        }

        @Override
        protected Long getRuleArgumentId(final RuleData ruleData) {
            return ID.incrementAndGet();
        }

        @Override
        protected Long getSelectorArgumentId(final SelectorData selectorData) {
            return ID.incrementAndGet();
        }

        @Override
        public String pluginNamed() {
            return "SHENYU_TEST";
        }
    }

    /** Loads $TestGoWasmPluginDataHandler.wasm. */
    static class TestGoWasmPluginDataHandler extends WasmTestHandler {
        TestGoWasmPluginDataHandler(final String expected) {
            super(expected);
        }
    }

    /** Loads $TestWasmPluginDataHandler.wasm. */
    static class TestWasmPluginDataHandler extends WasmTestHandler {
        TestWasmPluginDataHandler(final String expected) {
            super(expected);
        }
    }
}
