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

package org.apache.shenyu.plugin.logging.common.handler;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Test case for {@link AbstractLogPluginDataHandler}.
 */
public final class AbstractLogPluginDataHandlerTest {

    private static final String SELECTOR_ID = "selector-1";

    private AbstractLogPluginDataHandler<GenericGlobalConfig, GenericApiConfig> handler;

    @BeforeEach
    public void setUp() {
        handler = new MockLogPluginDataHandler();
    }

    @AfterEach
    public void cleanUp() {
        AbstractLogPluginDataHandler.getSelectApiConfigMap().clear();
        AbstractLogPluginDataHandler.getPluginGlobalConfigMap().clear();
    }

    @Test
    public void testHandlerSelectorClearsStaleEntryOnEmptyHandle() {
        SelectorData selectorData = buildSelector("{}");
        AbstractLogPluginDataHandler.getSelectApiConfigMap().put(SELECTOR_ID, new GenericApiConfig());

        handler.handlerSelector(selectorData);

        assertFalse(AbstractLogPluginDataHandler.getSelectApiConfigMap().containsKey(SELECTOR_ID),
                "a cleared handle must drop the previous selector override");
    }

    @Test
    public void testHandlerSelectorClearsStaleEntryOnBlankHandle() {
        SelectorData selectorData = buildSelector("");
        AbstractLogPluginDataHandler.getSelectApiConfigMap().put(SELECTOR_ID, new GenericApiConfig());

        handler.handlerSelector(selectorData);

        assertFalse(AbstractLogPluginDataHandler.getSelectApiConfigMap().containsKey(SELECTOR_ID));
    }

    @Test
    public void testHandlerSelectorStillStoresConfiguredHandle() {
        SelectorData selectorData = buildSelector("{\"sampleRate\":\"1\"}");

        handler.handlerSelector(selectorData);

        GenericApiConfig config = AbstractLogPluginDataHandler.getSelectApiConfigMap().get(SELECTOR_ID);
        assertNotNull(config);
        assertEquals("1", config.getSampleRate());
        assertTrue(AbstractLogPluginDataHandler.getSelectApiConfigMap().containsKey(SELECTOR_ID));
    }

    private SelectorData buildSelector(final String handle) {
        SelectorData selectorData = new SelectorData();
        selectorData.setId(SELECTOR_ID);
        selectorData.setPluginId("logging-mock");
        selectorData.setHandle(handle);
        return selectorData;
    }

    private static final class MockLogPluginDataHandler extends AbstractLogPluginDataHandler<GenericGlobalConfig, GenericApiConfig> {

        @Override
        public String pluginNamed() {
            return "logging-mock";
        }

        @Override
        protected LogCollector<org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog> logCollector() {
            return mock(LogCollector.class);
        }

        @Override
        protected void doRefreshConfig(final GenericGlobalConfig globalLogConfig) {
            // no-op
        }
    }
}
