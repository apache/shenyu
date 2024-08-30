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

import io.github.kawamuray.wasmtime.Func;
import io.github.kawamuray.wasmtime.Store;
import io.github.kawamuray.wasmtime.WasmFunctions;
import io.github.kawamuray.wasmtime.WasmValType;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.apache.shenyu.plugin.api.ShenyuPlugin.LOG;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AbstractWasmDiscoveryHandlerTest {

    private DiscoverySyncData discoverySyncData;

    private TestWasmPluginDiscoveryHandler testWasmPluginDiscoveryHandler;

    private DiscoveryUpstreamDataHandler discoveryUpstreamDataHandler;

    @BeforeEach
    public void setUp() {

        this.discoverySyncData = mock(DiscoverySyncData.class);
        this.testWasmPluginDiscoveryHandler = new TestWasmPluginDiscoveryHandler();
        this.discoveryUpstreamDataHandler = testWasmPluginDiscoveryHandler;
        when(discoverySyncData.getSelectorId()).thenReturn("SHENYU");
    }

    /**
     * The handlerDiscoveryUpstreamData test.
     */
    @Test
    public void handlerDiscoveryUpstreamDataTest() {
        discoveryUpstreamDataHandler = mock(DiscoveryUpstreamDataHandler.class);
        discoveryUpstreamDataHandler.handlerDiscoveryUpstreamData(discoverySyncData);
        testWasmPluginDiscoveryHandler.handlerDiscoveryUpstreamData(discoverySyncData);
        verify(discoveryUpstreamDataHandler).handlerDiscoveryUpstreamData(discoverySyncData);

    }

    /**
     * The plugin name test.
     */
    @Test
    public void pluginNameTest() {
        assertEquals("SHENYU_TEST", discoveryUpstreamDataHandler.pluginName());
        assertEquals("SHENYU_TEST", testWasmPluginDiscoveryHandler.pluginName());
    }

    static class TestWasmPluginDiscoveryHandler extends AbstractWasmDiscoveryHandler {

        private static final Map<Long, String> RESULTS = new ConcurrentHashMap<>();

        @Override
        protected Map<String, Func> initWasmCallJavaFunc(final Store<Void> store) {
            Map<String, Func> funcMap = new HashMap<>();
            funcMap.put("get_args", WasmFunctions.wrap(store, WasmValType.I64, WasmValType.I64, WasmValType.I32, WasmValType.I32,
                (argId, addr, len) -> {
                    String config = "hello from java " + argId;
                    LOG.info("java side->{}", config);
                    assertEquals("hello from java 0", config);
                    ByteBuffer buf = super.getBuffer();
                    for (int i = 0; i < len && i < config.length(); i++) {
                        buf.put(addr.intValue() + i, (byte) config.charAt(i));
                    }
                    return Math.min(config.length(), len);
                }));
            funcMap.put("put_result", WasmFunctions.wrap(store, WasmValType.I64, WasmValType.I64, WasmValType.I32, WasmValType.I32,
                (argId, addr, len) -> {
                    ByteBuffer buf = super.getBuffer();
                    byte[] bytes = new byte[len];
                    for (int i = 0; i < len; i++) {
                        bytes[i] = buf.get(addr.intValue() + i);
                    }
                    String result = new String(bytes, StandardCharsets.UTF_8);
                    assertEquals("rust result", result);
                    RESULTS.put(argId, result);
                    LOG.info("java side->{}", result);
                    return 0;
                }));
            return funcMap;
        }

        @Override
        public String pluginName() {
            return "SHENYU_TEST";
        }

        @Override
        protected Long getArgumentId(final DiscoverySyncData discoverySyncData) {
            return 0L;
        }
    }
}
