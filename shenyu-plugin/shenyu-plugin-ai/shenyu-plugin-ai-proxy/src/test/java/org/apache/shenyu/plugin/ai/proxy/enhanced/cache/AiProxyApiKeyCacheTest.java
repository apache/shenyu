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

package org.apache.shenyu.plugin.ai.proxy.enhanced.cache;

import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class AiProxyApiKeyCacheTest {

    @AfterEach
    void cleanup() {
        AiProxyApiKeyCache.getInstance().refresh();
    }

    @Test
    void testCacheAndGetRealApiKeyWhenEnabled() {
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("selector-1")
                .proxyApiKey("proxy-1")
                .realApiKey("real-1")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        AiProxyApiKeyCache.getInstance().cache(data);
        String real = AiProxyApiKeyCache.getInstance().getRealApiKey("selector-1", "proxy-1");
        Assertions.assertEquals("real-1", real);
    }

    @Test
    void testCacheIgnoredWhenDisabled() {
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("selector-2")
                .proxyApiKey("proxy-2")
                .realApiKey("real-2")
                .enabled(Boolean.FALSE)
                .namespaceId("default")
                .build();
        AiProxyApiKeyCache.getInstance().cache(data);
        String real = AiProxyApiKeyCache.getInstance().getRealApiKey("selector-2", "proxy-2");
        Assertions.assertNull(real);
    }

    @Test
    void testRemove() {
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("selector-3")
                .proxyApiKey("proxy-3")
                .realApiKey("real-3")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        AiProxyApiKeyCache.getInstance().cache(data);
        AiProxyApiKeyCache.getInstance().remove(data);
        Assertions.assertNull(AiProxyApiKeyCache.getInstance().getRealApiKey("selector-3", "proxy-3"));
    }

    @Test
    void testCacheNullEnabledUpdateKeepsEnabledEntry() {
        ProxyApiKeyData enabled = ProxyApiKeyData.builder()
                .selectorId("selector-5")
                .proxyApiKey("proxy-5")
                .realApiKey("real-5")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        AiProxyApiKeyCache.getInstance().cache(enabled);

        ProxyApiKeyData update = ProxyApiKeyData.builder()
                .selectorId("selector-5")
                .proxyApiKey("proxy-5")
                .realApiKey("updated-real-5")
                .enabled(null)
                .namespaceId("default")
                .build();
        AiProxyApiKeyCache.getInstance().cache(update);

        Assertions.assertEquals("real-5", AiProxyApiKeyCache.getInstance().getRealApiKey("selector-5", "proxy-5"));
        Assertions.assertEquals(1, AiProxyApiKeyCache.getInstance().size());
    }

    @Test
    void testCacheDisabledUpdateEvictsEnabledEntry() {
        ProxyApiKeyData enabled = ProxyApiKeyData.builder()
                .selectorId("selector-4")
                .proxyApiKey("proxy-4")
                .realApiKey("real-4")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        AiProxyApiKeyCache.getInstance().cache(enabled);
        Assertions.assertEquals("real-4", AiProxyApiKeyCache.getInstance().getRealApiKey("selector-4", "proxy-4"));

        ProxyApiKeyData disabled = ProxyApiKeyData.builder()
                .selectorId("selector-4")
                .proxyApiKey("proxy-4")
                .realApiKey("real-4")
                .enabled(Boolean.FALSE)
                .namespaceId("default")
                .build();
        AiProxyApiKeyCache.getInstance().cache(disabled);
        Assertions.assertNull(AiProxyApiKeyCache.getInstance().getRealApiKey("selector-4", "proxy-4"));
        Assertions.assertEquals(0, AiProxyApiKeyCache.getInstance().size());
    }
}
