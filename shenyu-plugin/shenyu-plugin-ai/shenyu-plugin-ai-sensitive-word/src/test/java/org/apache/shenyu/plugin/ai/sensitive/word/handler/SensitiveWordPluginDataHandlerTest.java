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

package org.apache.shenyu.plugin.ai.sensitive.word.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.SensitiveWordHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.ai.sensitive.word.ac.AhoCorasick;
import org.apache.shenyu.plugin.ai.sensitive.word.handler.SensitiveWordPluginDataHandler.CachedDictionary;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link SensitiveWordPluginDataHandler}.
 */
public final class SensitiveWordPluginDataHandlerTest {

    private SensitiveWordPluginDataHandler handler;

    @BeforeEach
    public void setUp() {
        handler = new SensitiveWordPluginDataHandler();
    }

    @AfterEach
    public void tearDown() {
        SensitiveWordPluginDataHandler.CACHED_HANDLE.get()
                .removeHandle(CacheKeyUtils.INST.getKey("selector-1", "rule-1"));
        SensitiveWordPluginDataHandler.DICTIONARIES.get().removeHandle("my:sensitive:words");
        SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get().removeHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME);
        SensitiveWordPluginDataHandler.REDIS_PROPERTIES.get().removeHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME);
    }

    @Test
    public void testPluginNamed() {
        assertEquals(PluginEnum.SENSITIVE_WORD.getName(), handler.pluginNamed());
    }

    @Test
    public void testHandlerRuleCachesTheHandle() {
        RuleData ruleData = ruleData("{\"redisKey\":\"my:sensitive:words\",\"refreshIntervalSeconds\":10}");
        handler.handlerRule(ruleData);
        SensitiveWordHandle handle = SensitiveWordPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        assertNotNull(handle);
        assertEquals("my:sensitive:words", handle.getRedisKey());
        assertEquals(10L, handle.getRefreshIntervalSeconds());
    }

    @Test
    public void testHandlerRuleWithoutRedisKeyFallsBackToTheDefaultOne() {
        RuleData ruleData = ruleData("{}");
        handler.handlerRule(ruleData);
        SensitiveWordHandle handle = SensitiveWordPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        assertNotNull(handle);
        assertEquals(SensitiveWordHandle.DEFAULT_REDIS_KEY, handle.getRedisKey());
        assertEquals(300L, handle.getRefreshIntervalSeconds());
    }

    @Test
    public void testHandlerRuleInvalidatesTheCachedDictionary() {
        String redisKey = "my:sensitive:words";
        SensitiveWordPluginDataHandler.DICTIONARIES.get()
                .cachedHandle(redisKey, new CachedDictionary(AhoCorasick.empty()));
        handler.handlerRule(ruleData("{\"redisKey\":\"my:sensitive:words\"}"));
        assertNull(SensitiveWordPluginDataHandler.DICTIONARIES.get().obtainHandle(redisKey));
    }

    @Test
    public void testRemoveRuleRemovesTheHandle() {
        RuleData ruleData = ruleData("{\"redisKey\":\"my:sensitive:words\"}");
        handler.handlerRule(ruleData);
        assertNotNull(SensitiveWordPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
        handler.removeRule(ruleData);
        assertNull(SensitiveWordPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Test
    public void testHandlerPluginWithoutRedisConfiguration() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig("{}");
        handler.handlerPlugin(pluginData);
        assertNull(SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME));
    }

    @Test
    public void testHandlerPluginDisabled() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(false);
        pluginData.setConfig("{\"url\":\"127.0.0.1:6379\"}");
        handler.handlerPlugin(pluginData);
        assertNull(SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME));
    }

    @Test
    public void testHandlerPluginCachesTheRedisTemplate() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"url\":\"127.0.0.1:6379\",\"database\":0,\"maxIdle\":8,\"minIdle\":0,\"maxActive\":8}");
        handler.handlerPlugin(pluginData);
        assertNotNull(SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME));
        assertNotNull(SensitiveWordPluginDataHandler.REDIS_PROPERTIES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME));
    }

    @Test
    public void testRemovePluginReleasesTheRedisTemplate() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"url\":\"127.0.0.1:6379\"}");
        handler.handlerPlugin(pluginData);
        assertNotNull(SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME));
        handler.removePlugin(pluginData);
        assertNull(SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME));
        assertNull(SensitiveWordPluginDataHandler.REDIS_PROPERTIES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME));
    }

    @Test
    public void testHandlerPluginDestroysTheClientItReplaces() {
        handler.handlerPlugin(pluginData("127.0.0.1:6379"));
        ReactiveRedisTemplate<String, String> first = SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME);
        assertNotNull(first);
        assertTrue(lettuceFactory(first).isRunning());

        handler.handlerPlugin(pluginData("127.0.0.1:6380"));
        ReactiveRedisTemplate<String, String> second = SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME);
        assertNotSame(first, second);
        // the client that was replaced must not keep its connection pool and its threads alive
        assertFalse(lettuceFactory(first).isRunning());
        assertTrue(lettuceFactory(second).isRunning());
    }

    @Test
    public void testHandlerPluginKeepsTheClientWhenTheConfigurationIsUnchanged() {
        handler.handlerPlugin(pluginData("127.0.0.1:6379"));
        ReactiveRedisTemplate<String, String> first = SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME);
        handler.handlerPlugin(pluginData("127.0.0.1:6379"));
        assertSame(first, SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME));
        assertTrue(lettuceFactory(first).isRunning());
    }

    @Test
    public void testRemovePluginDestroysTheClient() {
        handler.handlerPlugin(pluginData("127.0.0.1:6379"));
        ReactiveRedisTemplate<String, String> cached = SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .obtainHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME);
        assertNotNull(cached);
        handler.removePlugin(new PluginData());
        assertFalse(lettuceFactory(cached).isRunning());
    }

    @Test
    public void testCachedDictionaryExpires() {
        CachedDictionary dictionary = new CachedDictionary(AhoCorasick.empty());
        assertTrue(dictionary.isExpired(0L));
        assertTrue(dictionary.isExpired(-1L));
        assertTrue(!dictionary.isExpired(300L));
    }

    private PluginData pluginData(final String url) {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"url\":\"" + url + "\"}");
        return pluginData;
    }

    private LettuceConnectionFactory lettuceFactory(final ReactiveRedisTemplate<String, String> template) {
        return (LettuceConnectionFactory) template.getConnectionFactory();
    }

    private RuleData ruleData(final String handle) {
        RuleData ruleData = new RuleData();
        ruleData.setId("rule-1");
        ruleData.setName("rule-1");
        ruleData.setSelectorId("selector-1");
        ruleData.setPluginName(PluginEnum.SENSITIVE_WORD.getName());
        ruleData.setHandle(handle);
        return ruleData;
    }
}
