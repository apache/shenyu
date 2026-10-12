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

package org.apache.shenyu.plugin.ai.token.limiter.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link AiTokenLimiterPluginHandler}.
 */
public final class AiTokenLimiterPluginHandlerTest {

    @AfterEach
    public void tearDown() {
        AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().removeHandle(PluginEnum.AI_TOKEN_LIMITER.getName());
        AiTokenLimiterPluginHandler.REDIS_PROPERTIES_CACHED_HANDLE.get()
                .removeHandle(PluginEnum.AI_TOKEN_LIMITER.getName());
    }

    @Test
    public void testHandlerPluginCachesTheRedisTemplate() {
        new AiTokenLimiterPluginHandler().handlerPlugin(pluginData("127.0.0.1:6379"));
        ReactiveRedisTemplate<?, ?> template = redisTemplate();
        assertNotNull(template);
        assertTrue(lettuceFactory(template).isRunning());
    }

    @Test
    public void testHandlerPluginDestroysTheClientItReplaces() {
        AiTokenLimiterPluginHandler handler = new AiTokenLimiterPluginHandler();
        handler.handlerPlugin(pluginData("127.0.0.1:6379"));
        ReactiveRedisTemplate<?, ?> first = redisTemplate();
        assertNotNull(first);

        handler.handlerPlugin(pluginData("127.0.0.1:6380"));
        ReactiveRedisTemplate<?, ?> second = redisTemplate();
        assertNotSame(first, second);
        // the client that was replaced must not keep its connection pool and its threads alive
        assertFalse(lettuceFactory(first).isRunning());
        assertTrue(lettuceFactory(second).isRunning());
    }

    @Test
    public void testHandlerPluginKeepsTheClientWhenTheConfigurationIsUnchanged() {
        AiTokenLimiterPluginHandler handler = new AiTokenLimiterPluginHandler();
        handler.handlerPlugin(pluginData("127.0.0.1:6379"));
        ReactiveRedisTemplate<?, ?> first = redisTemplate();
        handler.handlerPlugin(pluginData("127.0.0.1:6379"));
        assertSame(first, redisTemplate());
        assertTrue(lettuceFactory(first).isRunning());
    }

    @Test
    public void testHandlerPluginDisabledDoesNothing() {
        PluginData pluginData = pluginData("127.0.0.1:6379");
        pluginData.setEnabled(false);
        new AiTokenLimiterPluginHandler().handlerPlugin(pluginData);
        assertNull(redisTemplate());
    }

    @Test
    public void testRemovePluginReleasesTheClient() {
        AiTokenLimiterPluginHandler handler = new AiTokenLimiterPluginHandler();
        handler.handlerPlugin(pluginData("127.0.0.1:6379"));
        ReactiveRedisTemplate<?, ?> cached = redisTemplate();
        assertNotNull(cached);

        handler.removePlugin(new PluginData());
        assertFalse(lettuceFactory(cached).isRunning());
        assertNull(redisTemplate());
    }

    private ReactiveRedisTemplate<?, ?> redisTemplate() {
        return AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get()
                .obtainHandle(PluginEnum.AI_TOKEN_LIMITER.getName());
    }

    private LettuceConnectionFactory lettuceFactory(final ReactiveRedisTemplate<?, ?> template) {
        return (LettuceConnectionFactory) template.getConnectionFactory();
    }

    private PluginData pluginData(final String url) {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"url\":\"" + url + "\"}");
        return pluginData;
    }
}
