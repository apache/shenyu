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

package org.apache.shenyu.plugin.ai.proxy.enhanced.subscriber;

import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.ChatClientCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.mock;

class CommonAiProxyApiKeyDataSubscriberTest {

    private final ChatClientCache chatClientCache = mock(ChatClientCache.class);

    @AfterEach
    void cleanup() {
        AiProxyApiKeyCache.getInstance().refresh();
    }

    @Test
    void testOnSubscribeCachesEnabled() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey("proxy-sub")
                .realApiKey("real-sub")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        subscriber.onSubscribe(data);
        Assertions.assertEquals("real-sub", AiProxyApiKeyCache.getInstance().getRealApiKey("test-selector", "proxy-sub"));
    }

    @Test
    void testOnSubscribeIgnoresDisabled() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey("proxy-disabled")
                .realApiKey("real-disabled")
                .enabled(Boolean.FALSE)
                .namespaceId("default")
                .build();
        subscriber.onSubscribe(data);
        Assertions.assertNull(AiProxyApiKeyCache.getInstance().getRealApiKey("test-selector", "proxy-disabled"));
    }

    @Test
    void testUnSubscribeRemoves() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey("proxy-unsub")
                .realApiKey("real-unsub")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        subscriber.onSubscribe(data);
        Assertions.assertEquals("real-unsub", AiProxyApiKeyCache.getInstance().getRealApiKey("test-selector", "proxy-unsub"));
        subscriber.unSubscribe(data);
        Assertions.assertNull(AiProxyApiKeyCache.getInstance().getRealApiKey("test-selector", "proxy-unsub"));
    }

    @Test
    void testRefreshClearsCache() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey("proxy-refresh")
                .realApiKey("real-refresh")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        subscriber.onSubscribe(data);
        Assertions.assertEquals(1, AiProxyApiKeyCache.getInstance().size());
        subscriber.refresh();
        Assertions.assertEquals(0, AiProxyApiKeyCache.getInstance().size());
        Assertions.assertNull(AiProxyApiKeyCache.getInstance().getRealApiKey("test-selector", "proxy-refresh"));
    }

    @Test
    void testOnSubscribeNullDataNoThrow() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        subscriber.onSubscribe(null);
        Assertions.assertEquals(0, AiProxyApiKeyCache.getInstance().size());
    }

    @Test
    void testOnSubscribeNullKeyIgnored() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey(null)
                .realApiKey("real")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        subscriber.onSubscribe(data);
        Assertions.assertEquals(0, AiProxyApiKeyCache.getInstance().size());
    }

    @Test
    void testUnSubscribeNullDataNoThrow() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        subscriber.unSubscribe(null);
        Assertions.assertEquals(0, AiProxyApiKeyCache.getInstance().size());
    }

    @Test
    void testUnSubscribeNullKeyNoThrow() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey(null)
                .realApiKey("real")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        subscriber.unSubscribe(data);
        Assertions.assertEquals(0, AiProxyApiKeyCache.getInstance().size());
    }

    @Test
    void testDuplicateSubscribeOverrides() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        ProxyApiKeyData v1 = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey("dup-key")
                .realApiKey("real-1")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        subscriber.onSubscribe(v1);
        Assertions.assertEquals("real-1", AiProxyApiKeyCache.getInstance().getRealApiKey("test-selector", "dup-key"));

        ProxyApiKeyData v2 = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey("dup-key")
                .realApiKey("real-2")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        subscriber.onSubscribe(v2);
        Assertions.assertEquals("real-2", AiProxyApiKeyCache.getInstance().getRealApiKey("test-selector", "dup-key"));
    }

    @Test
    void testSubscribeDisabledDoesNotOverrideExisting() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        ProxyApiKeyData enabled = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey("keep-key")
                .realApiKey("real-keep")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        subscriber.onSubscribe(enabled);
        Assertions.assertEquals("real-keep", AiProxyApiKeyCache.getInstance().getRealApiKey("test-selector", "keep-key"));

        ProxyApiKeyData disabled = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey("keep-key")
                .realApiKey("real-new")
                .enabled(Boolean.FALSE)
                .namespaceId("default")
                .build();
        subscriber.onSubscribe(disabled);
        // disabled subscribe should not override existing cached mapping
        Assertions.assertEquals("real-keep", AiProxyApiKeyCache.getInstance().getRealApiKey("test-selector", "keep-key"));
    }

    @Test
    void testVeryLongProxyKey() {
        CommonAiProxyApiKeyDataSubscriber subscriber = new CommonAiProxyApiKeyDataSubscriber(chatClientCache);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 300; i++) {
            sb.append('A' + (i % 26));
        }
        String longKey = sb.toString();
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("test-selector")
                .proxyApiKey(longKey)
                .realApiKey("real-long")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        subscriber.onSubscribe(data);
        Assertions.assertEquals("real-long", AiProxyApiKeyCache.getInstance().getRealApiKey("test-selector", longKey));
    }
} 