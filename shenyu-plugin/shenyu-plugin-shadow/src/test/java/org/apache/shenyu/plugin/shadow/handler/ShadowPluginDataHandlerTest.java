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

package org.apache.shenyu.plugin.shadow.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowRuntimeRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

/**
 * test case for {@link ShadowPluginDataHandler}.
 */
@ExtendWith(MockitoExtension.class)
public final class ShadowPluginDataHandlerTest {

    private static final String KEY = "selector_rule";

    @Mock
    private ShadowRuntimeRegistry runtimeRegistry;

    private ShadowPluginDataHandler handler;

    private RuleData rule;

    @BeforeEach
    public void setUp() {
        handler = new ShadowPluginDataHandler(runtimeRegistry);
        rule = new RuleData();
        rule.setSelectorId("selector");
        rule.setId("rule");
    }

    @Test
    public void testRegistersValidRule() {
        rule.setHandle("{\"enabled\":true,\"target\":\"http://shadow:8080\"}");
        handler.handlerRule(rule);
        verify(runtimeRegistry).register(eq(KEY), any(ShadowHandle.class));
    }

    @Test
    public void testRemovesInvalidRule() {
        rule.setHandle("{\"enabled\":true,\"target\":\"relative\"}");
        handler.handlerRule(rule);
        verify(runtimeRegistry).remove(KEY);
    }

    @Test
    public void testRemovesRuleWithoutHandle() {
        handler.handlerRule(rule);
        verify(runtimeRegistry).remove(KEY);
    }

    @Test
    public void testRemoveRule() {
        handler.removeRule(rule);
        verify(runtimeRegistry).remove(KEY);
    }
}
