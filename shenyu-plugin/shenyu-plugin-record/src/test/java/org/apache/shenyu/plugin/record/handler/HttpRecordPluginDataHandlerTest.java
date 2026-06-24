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

package org.apache.shenyu.plugin.record.handler;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.HttpRecordHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

public final class HttpRecordPluginDataHandlerTest {

    private HttpRecordPluginDataHandler handler;

    @BeforeEach
    public void setUp() {
        handler = new HttpRecordPluginDataHandler();
    }

    @Test
    public void testPluginNamed() {
        assertEquals(PluginEnum.HTTP_RECORD.getName(), handler.pluginNamed());
    }

    @Test
    public void testHandlerRule() {
        RuleData ruleData = new RuleData();
        ruleData.setId("ruleId");
        ruleData.setSelectorId("selectorId");
        long startTime = System.currentTimeMillis();
        long endTime = startTime + 60000;
        HttpRecordHandle httpRecordHandle = new HttpRecordHandle();
        httpRecordHandle.setStartTime(startTime);
        httpRecordHandle.setEndTime(endTime);
        ruleData.setHandle(GsonUtils.getInstance().toJson(httpRecordHandle));
        handler.handlerRule(ruleData);
        HttpRecordHandle cachedHandle = HttpRecordPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        assertNotNull(cachedHandle);
        String expectedTaskId = startTime + "_" + endTime + "_" + ruleData.getId();
        assertEquals(expectedTaskId, cachedHandle.getTaskId());
        assertEquals(startTime, cachedHandle.getStartTime());
        assertEquals(endTime, cachedHandle.getEndTime());

        handler.removeRule(ruleData);
        assertNull(HttpRecordPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Test
    public void testHandlerRuleWithNullHandle() {
        RuleData ruleData = new RuleData();
        ruleData.setId("ruleId2");
        ruleData.setSelectorId("selectorId2");
        ruleData.setHandle(null);
        handler.handlerRule(ruleData);
        assertNull(HttpRecordPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Test
    public void testHandlerSelector() {
        SelectorData selectorData = SelectorData.builder()
                .id("selectorId")
                .build();
        handler.handlerSelector(selectorData);
        HttpRecordHandle handle = HttpRecordPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE));
        assertNotNull(handle);
        assertNotNull(handle.getStartTime());
        assertNotNull(handle.getEndTime());

        handler.removeSelector(selectorData);
        assertNull(HttpRecordPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE)));
    }
}