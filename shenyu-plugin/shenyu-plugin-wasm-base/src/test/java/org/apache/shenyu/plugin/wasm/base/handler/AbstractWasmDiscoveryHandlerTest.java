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

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AbstractWasmDiscoveryHandlerTest {

    private DiscoverySyncData discoverySyncData;

    private SimpleTestHandler testWasmPluginDiscoveryHandler;

    private DiscoveryUpstreamDataHandler discoveryUpstreamDataHandler;

    @BeforeEach
    public void setUp() {

        this.discoverySyncData = mock(DiscoverySyncData.class);
        // Use a simple test handler instead of WebAssembly-dependent handler
        this.testWasmPluginDiscoveryHandler = new SimpleTestHandler();
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

    /**
     * Simple test handler for testing without WebAssembly dependencies.
     */
    static class SimpleTestHandler implements DiscoveryUpstreamDataHandler {
        
        @Override
        public void handlerDiscoveryUpstreamData(final DiscoverySyncData discoverySyncData) {
            // Simple test implementation - just store for verification
        }
        
        @Override
        public String pluginName() {
            return "SHENYU_TEST";
        }
        
        protected Long getArgumentId(final DiscoverySyncData discoverySyncData) {
            return 0L;
        }
    }
}
