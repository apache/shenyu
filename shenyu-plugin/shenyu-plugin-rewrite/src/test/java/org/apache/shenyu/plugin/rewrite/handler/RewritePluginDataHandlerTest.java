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

package org.apache.shenyu.plugin.rewrite.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.RewriteHandle;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.function.Supplier;

/**
 * The Test Case For RewritePluginDataHandler.
 */
public final class RewritePluginDataHandlerTest {

    private final RewritePluginDataHandler rewritePluginDataHandler = new RewritePluginDataHandler();

    private final RuleData ruleData = new RuleData();

    @Test
    public void testHandlerRule() {
        ruleData.setSelectorId("1");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        ruleData.setId("test");
        rewritePluginDataHandler.handlerRule(ruleData);
        Supplier<CommonHandleCache<String, RewriteHandle>> cache = RewritePluginDataHandler.CACHED_HANDLE;
        Assertions.assertNotNull(cache.get().obtainHandle("1_test"));
    }

    @Test
    public void testRemoveRule() {
        ruleData.setSelectorId("1");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        ruleData.setId("test");
        Supplier<CommonHandleCache<String, RewriteHandle>> cache = RewritePluginDataHandler.CACHED_HANDLE;
        cache.get().cachedHandle("1_test", new RewriteHandle());
        Assertions.assertNotNull(cache.get().obtainHandle("1_test"));
        ruleData.setHandle(null);
        rewritePluginDataHandler.removeRule(ruleData);
        Assertions.assertNull(cache.get().obtainHandle("1_test"));
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(rewritePluginDataHandler.pluginNamed(), "rewrite");
    }

    @ParameterizedTest
    @EmptySource
    @ValueSource(strings = {" ", "\t\r\n"})
    public void testBlankHandleClearsCachedRule(final String handle) {
        ruleData.setSelectorId("clear-selector");
        ruleData.setId("clear-rule");
        ruleData.setHandle("{\"regex\":\"/old\",\"replace\":\"/new\"}");
        rewritePluginDataHandler.handlerRule(ruleData);
        Assertions.assertNotNull(RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("clear-selector_clear-rule"));

        ruleData.setHandle(handle);
        rewritePluginDataHandler.handlerRule(ruleData);
        Assertions.assertNull(RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("clear-selector_clear-rule"));

        rewritePluginDataHandler.handlerRule(ruleData);
        Assertions.assertNull(RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("clear-selector_clear-rule"));
    }

    @Test
    public void testNullHandleRetainsCachedRule() {
        ruleData.setSelectorId("null-selector");
        ruleData.setId("null-rule");
        ruleData.setHandle("{\"regex\":\"/old\",\"replace\":\"/new\"}");
        rewritePluginDataHandler.handlerRule(ruleData);
        RewriteHandle cached = RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("null-selector_null-rule");

        ruleData.setHandle(null);
        rewritePluginDataHandler.handlerRule(ruleData);

        Assertions.assertSame(cached, RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("null-selector_null-rule"));
    }

    @Test
    public void testEmptyObjectReplacesCachedRule() {
        ruleData.setSelectorId("empty-selector");
        ruleData.setId("empty-rule");
        ruleData.setHandle("{\"regex\":\"/old\",\"replace\":\"/new\"}");
        rewritePluginDataHandler.handlerRule(ruleData);
        ruleData.setHandle("{}");

        rewritePluginDataHandler.handlerRule(ruleData);

        RewriteHandle cached = RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("empty-selector_empty-rule");
        Assertions.assertNotNull(cached);
        Assertions.assertNull(cached.getRegex());
        Assertions.assertNull(cached.getReplace());
    }

    @Test
    public void testInvalidRegexIsNotCached() {
        ruleData.setSelectorId("invalid-regex-selector");
        ruleData.setId("invalid-regex-rule");
        ruleData.setHandle("{\"regex\":\"[a\",\"replace\":\"/new\"}");

        rewritePluginDataHandler.handlerRule(ruleData);

        Assertions.assertNull(RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("invalid-regex-selector_invalid-regex-rule"));
    }

    @Test
    public void testInvalidReplacementIsNotCached() {
        ruleData.setSelectorId("invalid-replace-selector");
        ruleData.setId("invalid-replace-rule");
        ruleData.setHandle("{\"regex\":\"test\",\"replace\":\"/new/$1\"}");

        rewritePluginDataHandler.handlerRule(ruleData);

        Assertions.assertNull(RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("invalid-replace-selector_invalid-replace-rule"));
    }

    @Test
    public void testInvalidHandleEvictsPreviouslyCachedRule() {
        ruleData.setSelectorId("stale-selector");
        ruleData.setId("stale-rule");
        ruleData.setHandle("{\"regex\":\"(test)\",\"replace\":\"/new/$1\"}");
        rewritePluginDataHandler.handlerRule(ruleData);
        Assertions.assertNotNull(RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("stale-selector_stale-rule"));

        ruleData.setHandle("{\"regex\":\"test\",\"replace\":\"/new/$1\"}");
        rewritePluginDataHandler.handlerRule(ruleData);

        Assertions.assertNull(RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("stale-selector_stale-rule"));
    }

    @Test
    public void testPlaceholderHandleIsCached() {
        ruleData.setSelectorId("placeholder-selector");
        ruleData.setId("placeholder-rule");
        ruleData.setHandle("{\"regex\":\"/shenyu/{id}\",\"replace\":\"/target/{id}\"}");

        rewritePluginDataHandler.handlerRule(ruleData);

        Assertions.assertNotNull(RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle("placeholder-selector_placeholder-rule"));
    }
}
