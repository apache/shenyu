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

package org.apache.shenyu.plugin.resilience4j.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.Resilience4JHandle;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.function.Supplier;

/**
 * The Test Case For Resilience4JHandler.
 */
public final class Resilience4JHandlerTest {

    private Resilience4JHandler resilience4JHandler = new Resilience4JHandler();

    private RuleData ruleData = new RuleData();

    @Test
    public void testHandlerRule() {
        ruleData.setSelectorId("1");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        ruleData.setName("test");
        resilience4JHandler.handlerRule(ruleData);
        Supplier<CommonHandleCache<String, Resilience4JHandle>> cache = resilience4JHandler.CACHED_HANDLE;
        Assertions.assertNotNull(cache.get().obtainHandle("1_test"));
    }

    @Test
    public void testRemoveRule() {
        ruleData.setSelectorId("1");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        ruleData.setName("test");
        Supplier<CommonHandleCache<String, Resilience4JHandle>> cache = resilience4JHandler.CACHED_HANDLE;
        cache.get().cachedHandle("1_test", new Resilience4JHandle());
        Assertions.assertNotNull(cache.get().obtainHandle("1_test"));
        resilience4JHandler.removeRule(ruleData);
        Assertions.assertNull(cache.get().obtainHandle("1_test"));
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(resilience4JHandler.pluginNamed(), "resilience4j");
    }

}
