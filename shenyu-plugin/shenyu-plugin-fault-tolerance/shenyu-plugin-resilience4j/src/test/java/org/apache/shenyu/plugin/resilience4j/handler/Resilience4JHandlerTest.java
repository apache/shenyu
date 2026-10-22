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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.util.function.Supplier;

/**
 * The Test Case For Resilience4JHandler.
 */
public final class Resilience4JHandlerTest {

    private final Resilience4JHandler resilience4JHandler = new Resilience4JHandler();

    private final RuleData ruleData = new RuleData();

    @Test
    public void testHandlerRule() {
        ruleData.setSelectorId("1");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        ruleData.setId("test");
        resilience4JHandler.handlerRule(ruleData);
        Supplier<CommonHandleCache<String, Resilience4JHandle>> cache = Resilience4JHandler.CACHED_HANDLE;
        Assertions.assertNotNull(cache.get().obtainHandle("1_test"));
    }

    @Test
    public void testRemoveRule() {
        ruleData.setSelectorId("1");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        ruleData.setId("test");
        Supplier<CommonHandleCache<String, Resilience4JHandle>> cache = Resilience4JHandler.CACHED_HANDLE;
        cache.get().cachedHandle("1_test", new Resilience4JHandle());
        Assertions.assertNotNull(cache.get().obtainHandle("1_test"));
        resilience4JHandler.removeRule(ruleData);
        Assertions.assertNull(cache.get().obtainHandle("1_test"));
    }

    /**
     * An explicitly cleared handle must evict the previous rule, whitespace
     * counts as a clear, and a subsequent valid update re-caches (#7914).
     */
    @Test
    public void testHandlerRuleExplicitClearEvictsPreviousRule() {
        ruleData.setSelectorId("1");
        ruleData.setId("clear-test");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        resilience4JHandler.handlerRule(ruleData);
        final Supplier<CommonHandleCache<String, Resilience4JHandle>> cache = Resilience4JHandler.CACHED_HANDLE;
        Assertions.assertNotNull(cache.get().obtainHandle("1_clear-test"));

        ruleData.setHandle("");
        resilience4JHandler.handlerRule(ruleData);
        Assertions.assertNull(cache.get().obtainHandle("1_clear-test"),
                "an empty handle must evict the previously cached rule");

        ruleData.setHandle("   ");
        resilience4JHandler.handlerRule(ruleData);
        Assertions.assertNull(cache.get().obtainHandle("1_clear-test"));

        ruleData.setHandle("{\"urlPath\":\"test\"}");
        resilience4JHandler.handlerRule(ruleData);
        Assertions.assertNotNull(cache.get().obtainHandle("1_clear-test"),
                "a subsequent valid update must re-cache the rule");
    }

    /**
     * A handle that disappeared entirely (null, e.g. the column is NULL instead
     * of an empty string) is a clear too: the previous rule must stop enforcing (#7914).
     */
    @Test
    public void testHandlerRuleNullHandleEvictsPreviousRule() {
        ruleData.setSelectorId("1");
        ruleData.setId("null-clear-test");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        resilience4JHandler.handlerRule(ruleData);
        final Supplier<CommonHandleCache<String, Resilience4JHandle>> cache = Resilience4JHandler.CACHED_HANDLE;
        Assertions.assertNotNull(cache.get().obtainHandle("1_null-clear-test"));

        ruleData.setHandle(null);
        resilience4JHandler.handlerRule(ruleData);
        Assertions.assertNull(cache.get().obtainHandle("1_null-clear-test"),
                "a null handle must evict the previously cached rule");
    }

    /**
     * Deleting a rule whose handle is already null must still evict the cached
     * entry, so the paths cannot disagree about what a removal means (#7914).
     */
    @Test
    public void testRemoveRuleWithoutHandleStillEvicts() {
        ruleData.setSelectorId("1");
        ruleData.setId("remove-null-test");
        ruleData.setHandle(null);
        final Supplier<CommonHandleCache<String, Resilience4JHandle>> cache = Resilience4JHandler.CACHED_HANDLE;
        cache.get().cachedHandle("1_remove-null-test", new Resilience4JHandle());

        resilience4JHandler.removeRule(ruleData);

        Assertions.assertNull(cache.get().obtainHandle("1_remove-null-test"));
    }

    @AfterEach
    public void tearDown() {
        Resilience4JHandler.CACHED_HANDLE.get().removeHandle("1_clear-test");
        Resilience4JHandler.CACHED_HANDLE.get().removeHandle("1_null-clear-test");
        Resilience4JHandler.CACHED_HANDLE.get().removeHandle("1_remove-null-test");
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(resilience4JHandler.pluginNamed(), "resilience4j");
    }

}
