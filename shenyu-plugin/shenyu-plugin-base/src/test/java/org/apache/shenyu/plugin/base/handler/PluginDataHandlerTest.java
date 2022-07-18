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

package org.apache.shenyu.plugin.base.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for PluginDataHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class PluginDataHandlerTest {

    private RuleData ruleData;

    private PluginData pluginData;

    private SelectorData selectorData;

    private TestPluginDataHandler testPluginDataHandler;

    private PluginDataHandler pluginDataHandler;

    @BeforeEach
    public void setUp() {
        this.ruleData = mock(RuleData.class);
        this.pluginData = mock(PluginData.class);
        this.selectorData = mock(SelectorData.class);
        this.testPluginDataHandler = new TestPluginDataHandler();
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
        testPluginDataHandler.handlerPlugin(pluginData);
        assertEquals(testPluginDataHandler.getPluginDataMap().get(pluginData.getId()).getId(), pluginData.getId());
    }

    /**
     * The remove plugin test.
     */
    @Test
    public void removePluginTest() {
        pluginDataHandler.removePlugin(pluginData);
        testPluginDataHandler.handlerPlugin(pluginData);
        testPluginDataHandler.removePlugin(pluginData);
        assertNull(testPluginDataHandler.getPluginDataMap().get(pluginData.getId()));
    }

    /**
     * The handler selector test.
     */
    @Test
    public void handlerSelectorTest() {
        pluginDataHandler.handlerSelector(selectorData);
        testPluginDataHandler.handlerSelector(selectorData);
        assertEquals(testPluginDataHandler.getSelectorDataMap().get(selectorData.getId()).getId(), selectorData.getId());
    }

    /**
     * The remove selector test.
     */
    @Test
    public void removeSelectorTest() {
        pluginDataHandler.removeSelector(selectorData);
        testPluginDataHandler.handlerSelector(selectorData);
        testPluginDataHandler.removeSelector(selectorData);
        assertNull(testPluginDataHandler.getSelectorDataMap().get(selectorData.getId()));
    }

    /**
     * The handler rule test.
     */
    @Test
    public void handlerRuleTest() {
        pluginDataHandler.handlerRule(ruleData);
        testPluginDataHandler.handlerRule(ruleData);
        assertEquals(testPluginDataHandler.getRuleDataMap().get(ruleData.getId()).getId(), ruleData.getId());
    }

    /**
     * The remove rule test.
     */
    @Test
    public void removeRuleTest() {
        pluginDataHandler.removeRule(ruleData);
        testPluginDataHandler.handlerRule(ruleData);
        testPluginDataHandler.removeRule(ruleData);
        assertNull(testPluginDataHandler.getRuleDataMap().get(ruleData.getId()));
    }

    /**
     * The plugin named test.
     */
    @Test
    public void pluginNamedTest() {
        assertEquals("SHENYU", pluginDataHandler.pluginNamed());
        assertEquals("SHENYU_TEST", testPluginDataHandler.pluginNamed());
    }

    static class TestPluginDataHandler implements PluginDataHandler {

        private Map<String, PluginData> pluginDataMap = new HashMap<>();

        private Map<String, SelectorData> selectorDataMap = new HashMap<>();

        private Map<String, RuleData> ruleDataMap = new HashMap<>();

        public Map<String, PluginData> getPluginDataMap() {
            return pluginDataMap;
        }

        public void setPluginDataMap(final Map<String, PluginData> pluginDataMap) {
            this.pluginDataMap = pluginDataMap;
        }

        public Map<String, SelectorData> getSelectorDataMap() {
            return selectorDataMap;
        }

        public void setSelectorDataMap(final Map<String, SelectorData> selectorDataMap) {
            this.selectorDataMap = selectorDataMap;
        }

        public Map<String, RuleData> getRuleDataMap() {
            return ruleDataMap;
        }

        public void setRuleDataMap(final Map<String, RuleData> ruleDataMap) {
            this.ruleDataMap = ruleDataMap;
        }

        @Override
        public void handlerPlugin(final PluginData pluginData) {
            pluginDataMap.put(pluginData.getId(), pluginData);
        }

        @Override
        public void removePlugin(final PluginData pluginData) {
            pluginDataMap.remove(pluginData.getId());
        }

        @Override
        public void handlerSelector(final SelectorData selectorData) {
            selectorDataMap.put(selectorData.getId(), selectorData);
        }

        @Override
        public void removeSelector(final SelectorData selectorData) {
            selectorDataMap.remove(selectorData.getId());
        }

        @Override
        public void handlerRule(final RuleData ruleData) {
            ruleDataMap.put(ruleData.getId(), ruleData);
        }

        @Override
        public void removeRule(final RuleData ruleData) {
            ruleDataMap.remove(ruleData.getId());
        }

        @Override
        public String pluginNamed() {
            return "SHENYU_TEST";
        }
    }
}
