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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for AbstractWasmPluginDataHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class AbstractWasmPluginDataHandlerTest {
    
    private RuleData ruleData;
    
    private PluginData pluginData;
    
    private SelectorData selectorData;
    
    private TestWasmPluginDataHandler testWasmPluginDataHandler;
    
    private PluginDataHandler pluginDataHandler;
    
    @BeforeEach
    public void setUp() {
        this.ruleData = mock(RuleData.class);
        this.pluginData = mock(PluginData.class);
        this.selectorData = mock(SelectorData.class);
        this.testWasmPluginDataHandler = new TestWasmPluginDataHandler();
        this.pluginDataHandler = () -> "SHENYU";
        when(ruleData.getId()).thenReturn("SHENYU");
        when(pluginData.getId()).thenReturn("SHENYU");
        when(selectorData.getId()).thenReturn("SHENYU");
    }
    
    /**
     * The handler plugin test.
     */
    @Test
    public void handlerPluginTest() {
        pluginDataHandler.handlerPlugin(pluginData);
        testWasmPluginDataHandler.handlerPlugin(pluginData);
    }
    
    /**
     * The remove plugin test.
     */
    @Test
    public void removePluginTest() {
        pluginDataHandler.removePlugin(pluginData);
        testWasmPluginDataHandler.handlerPlugin(pluginData);
        testWasmPluginDataHandler.removePlugin(pluginData);
    }
    
    /**
     * The handler selector test.
     */
    @Test
    public void handlerSelectorTest() {
        pluginDataHandler.handlerSelector(selectorData);
        testWasmPluginDataHandler.handlerSelector(selectorData);
    }
    
    /**
     * The remove selector test.
     */
    @Test
    public void removeSelectorTest() {
        pluginDataHandler.removeSelector(selectorData);
        testWasmPluginDataHandler.handlerSelector(selectorData);
        testWasmPluginDataHandler.removeSelector(selectorData);
    }
    
    /**
     * The handler rule test.
     */
    @Test
    public void handlerRuleTest() {
        pluginDataHandler.handlerRule(ruleData);
        testWasmPluginDataHandler.handlerRule(ruleData);
    }
    
    /**
     * The remove rule test.
     */
    @Test
    public void removeRuleTest() {
        pluginDataHandler.removeRule(ruleData);
        testWasmPluginDataHandler.handlerRule(ruleData);
        testWasmPluginDataHandler.removeRule(ruleData);
    }
    
    /**
     * The plugin named test.
     */
    @Test
    public void pluginNamedTest() {
        assertEquals("SHENYU", pluginDataHandler.pluginNamed());
        assertEquals("SHENYU_TEST", testWasmPluginDataHandler.pluginNamed());
    }
    
    static class TestWasmPluginDataHandler extends AbstractWasmPluginDataHandler {
        
        private static final Logger LOG = LoggerFactory.getLogger(TestWasmPluginDataHandler.class);
        
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
                    LOG.info("java side->{}", result);
                    return 0;
                }));
            return funcMap;
        }
        
        @Override
        public String pluginNamed() {
            return "SHENYU_TEST";
        }
    }
}
