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

package org.apache.shenyu.plugin.logging.console.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.logging.console.entity.LoggingConsoleRuleHandle;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link LoggingConsolePluginDataHandler}.
 */
public final class LoggingConsolePluginDataHandlerTest {

    private final LoggingConsolePluginDataHandler handler = new LoggingConsolePluginDataHandler();

    private final RuleData ruleData = new RuleData();

    @BeforeEach
    public void setUp() {
        ruleData.setSelectorId("selector");
        ruleData.setId("rule");
        ruleData.setHandle("{\"keyword\":\"requestBody;responseBody\",\"maskStatus\":true}");
    }

    @AfterEach
    public void tearDown() {
        handler.removeRule(ruleData);
    }

    @Test
    public void testCachesKeyWordMatchWithRule() {
        handler.handlerRule(ruleData);
        String cacheKey = CacheKeyUtils.INST.getKey(ruleData);
        LoggingConsoleRuleHandle ruleHandle = LoggingConsolePluginDataHandler.CACHED_HANDLE.get().obtainHandle(cacheKey);

        assertNotNull(ruleHandle);
        assertSame(ruleHandle, LoggingConsolePluginDataHandler.CACHED_HANDLE.get().obtainHandle(cacheKey));
        assertTrue(ruleHandle.getKeyWordMatch().matches("requestBody"));
    }

    @Test
    public void testRemovesKeyWordMatchWithRule() {
        handler.handlerRule(ruleData);
        String cacheKey = CacheKeyUtils.INST.getKey(ruleData);

        handler.removeRule(ruleData);

        assertNull(LoggingConsolePluginDataHandler.CACHED_HANDLE.get().obtainHandle(cacheKey));
    }

    @Test
    public void testDisabledMaskDoesNotCompileKeywords() {
        ruleData.setHandle("{\"keyword\":\"[\",\"maskStatus\":false}");

        assertDoesNotThrow(() -> handler.handlerRule(ruleData));
        LoggingConsoleRuleHandle ruleHandle = LoggingConsolePluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        assertNotNull(ruleHandle);
        assertFalse(ruleHandle.isDesensitized());
    }
}
