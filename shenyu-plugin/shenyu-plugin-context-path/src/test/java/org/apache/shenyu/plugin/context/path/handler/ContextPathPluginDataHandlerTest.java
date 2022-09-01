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

package org.apache.shenyu.plugin.context.path.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * ContextPathPluginDataHandler Test.
 */
public class ContextPathPluginDataHandlerTest {

    private ContextPathPluginDataHandler contextPathPluginDataHandler;

    @BeforeEach
    public void setup() {
        contextPathPluginDataHandler = new ContextPathPluginDataHandler();
    }

    @Test
    public void namedTest() {
        assertEquals(PluginEnum.CONTEXT_PATH.getName(), contextPathPluginDataHandler.pluginNamed());
    }

    @Test
    public void removeRuleTest() {
        contextPathPluginDataHandler.removeRule(RuleData.builder().handle("{}").build());
    }

    @Test
    public void handlerRuleTest() {
        contextPathPluginDataHandler.handlerRule(RuleData.builder().handle("{}").build());
    }
}
