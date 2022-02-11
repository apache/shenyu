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

package org.apache.shenyu.plugin.param.mapping.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * Test case for {@link ParamMappingPluginDataHandler}.
 */
@ExtendWith(MockitoExtension.class)
public class ParamMappingPluginDataTest {

    private ParamMappingPluginDataHandler paramMappingPluginDataHandler;

    private RuleData ruleData;

    @BeforeEach
    public void setUp() {
        this.paramMappingPluginDataHandler = new ParamMappingPluginDataHandler();
        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test");
        this.ruleData.setName("test-param-mappin-plugin");
        this.ruleData.setHandle("{\"addParameterKeys\":[{\"path\":\"1\",\"key\":\"2\",\"value\":\"3\"},{\"path\":\"h\",\"key\":\"j\",\"value\":\"k\"}],"
                + "\"replaceParameterKeys\":[{\"path\":\"4\",\"key\":\"5\",\"value\":\"6\"},{\"path\":\"q\",\"key\":\"wt\",\"value\":\"ei\"}],"
                + "\"removeParameterKeys\":[\"7\",\"82e\",\"82fd\"]}");
    }

    @Test
    public void testHandlerRule() {
        this.paramMappingPluginDataHandler.handlerRule(this.ruleData);
        assertNotNull(ParamMappingPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(this.ruleData)));
    }

    @Test
    public void testRemoveRule() {
        this.paramMappingPluginDataHandler.handlerRule(this.ruleData);
        RuleData ruleData = new RuleData();
        ruleData.setSelectorId("test");
        ruleData.setName("test-param-mappin-plugin");
        this.paramMappingPluginDataHandler.removeRule(this.ruleData);
        assertNull(ParamMappingPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(this.ruleData)));
        this.paramMappingPluginDataHandler.removeRule(ruleData);
        assertNull(ParamMappingPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Test
    public void testPluginNamed() {
        assertEquals(this.paramMappingPluginDataHandler.pluginNamed(), "paramMapping");
    }
}
