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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;

class AiProxyApiKeyCacheTest {

    private Logger cacheLogger;

    private Level previousLevel;

    private ListAppender<ILoggingEvent> appender;

    @BeforeEach
    void attachLogCapture() {
        cacheLogger = (Logger) LoggerFactory.getLogger(AiProxyApiKeyCache.class);
        previousLevel = cacheLogger.getLevel();
        cacheLogger.setLevel(Level.DEBUG);
        appender = new ListAppender<>();
        appender.setContext(cacheLogger.getLoggerContext());
        appender.start();
        cacheLogger.addAppender(appender);
    }

    @AfterEach
    void cleanup() {
        if (Objects.nonNull(cacheLogger)) {
            cacheLogger.detachAppender(appender);
            cacheLogger.setLevel(previousLevel);
        }
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

    @Test
    void testSameProxyKeyIsIsolatedPerSelector() {
        AiProxyApiKeyCache.getInstance().cache(ProxyApiKeyData.builder()
                .selectorId("selector-a")
                .proxyApiKey("shared-proxy")
                .realApiKey("real-a")
                .enabled(Boolean.TRUE)
                .build());
        AiProxyApiKeyCache.getInstance().cache(ProxyApiKeyData.builder()
                .selectorId("selector-b")
                .proxyApiKey("shared-proxy")
                .realApiKey("real-b")
                .enabled(Boolean.TRUE)
                .build());

        Assertions.assertEquals("real-a", AiProxyApiKeyCache.getInstance().getRealApiKey("selector-a", "shared-proxy"));
        Assertions.assertEquals("real-b", AiProxyApiKeyCache.getInstance().getRealApiKey("selector-b", "shared-proxy"));

        AiProxyApiKeyCache.getInstance().removeBySelectorId("selector-a");
        Assertions.assertNull(AiProxyApiKeyCache.getInstance().getRealApiKey("selector-a", "shared-proxy"));
        Assertions.assertEquals("real-b", AiProxyApiKeyCache.getInstance().getRealApiKey("selector-b", "shared-proxy"));
    }

    @Test
    void testCacheAndRemoveLogsRedactProxyAndRealCredentials() {
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("selector-log")
                .proxyApiKey("proxy-secret-value")
                .realApiKey("real-secret-value")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        AiProxyApiKeyCache.getInstance().cache(data);
        AiProxyApiKeyCache.getInstance().remove(data);

        String logs = appender.list.stream()
                .map(ILoggingEvent::getFormattedMessage)
                .collect(Collectors.joining("\n"));
        String arguments = appender.list.stream()
                .filter(event -> Objects.nonNull(event.getArgumentArray()))
                .flatMap(event -> Arrays.stream(event.getArgumentArray()))
                .map(String::valueOf)
                .collect(Collectors.joining("\n"));
        Assertions.assertFalse(logs.contains("proxy-secret-value"));
        Assertions.assertFalse(logs.contains("real-secret-value"));
        Assertions.assertFalse(arguments.contains("proxy-secret-value"));
        Assertions.assertFalse(arguments.contains("real-secret-value"));
        Assertions.assertTrue(logs.contains("[AiProxyApiKeyCache] cache key=selector-log::"));
        Assertions.assertTrue(logs.contains("[AiProxyApiKeyCache] remove key=selector-log::"));
    }
}
