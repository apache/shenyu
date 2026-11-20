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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.common.config.ShenyuConfig.RuleMatchCache;
import org.apache.shenyu.common.config.ShenyuConfig.SelectorMatchCache;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.InitialSyncApplication;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;

import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

class AiProxyPluginDataSubscriberTest {

    private final Logger handlerLogger = (Logger) LoggerFactory.getLogger(AiProxyPluginHandler.class);

    private final Logger subscriberLogger = (Logger) LoggerFactory.getLogger(CommonPluginDataSubscriber.class);

    private final ListAppender<ILoggingEvent> handlerLogs = new ListAppender<>();

    private final ListAppender<ILoggingEvent> subscriberLogs = new ListAppender<>();

    private PluginData previousPlugin;

    private Level previousHandlerLevel;

    private Level previousSubscriberLevel;

    private AiProxyProviderFactory providers;

    private AiProxyProtocolFactory protocols;

    private AiProxyPluginHandler handler;

    private CommonPluginDataSubscriber subscriber;

    @BeforeEach
    void setUp() {
        previousPlugin = BaseDataCache.getInstance().obtainPluginData(PluginEnum.AI_PROXY.getName());
        BaseDataCache.getInstance().removePluginDataByPluginName(PluginEnum.AI_PROXY.getName());
        previousHandlerLevel = handlerLogger.getLevel();
        previousSubscriberLevel = subscriberLogger.getLevel();
        handlerLogs.setContext(handlerLogger.getLoggerContext());
        handlerLogs.start();
        handlerLogger.setLevel(Level.INFO);
        handlerLogger.addAppender(handlerLogs);
        subscriberLogs.setContext(subscriberLogger.getLoggerContext());
        subscriberLogs.start();
        subscriberLogger.setLevel(Level.INFO);
        subscriberLogger.addAppender(subscriberLogs);
        providers = spy(new AiProxyProviderFactory(List.of(new OpenAi())));
        protocols = spy(new AiProxyProtocolFactory(List.of(new OpenAiChat())));
        handler = new AiProxyPluginHandler(providers, protocols);
        SelectorMatchCache selectors = new SelectorMatchCache();
        selectors.getCache().setEnabled(false);
        RuleMatchCache rules = new RuleMatchCache();
        rules.getCache().setEnabled(false);
        subscriber = new CommonPluginDataSubscriber(List.of(handler), mock(ApplicationEventPublisher.class), selectors, rules);
    }

    @AfterEach
    void tearDown() {
        handlerLogger.detachAppender(handlerLogs);
        handlerLogger.setLevel(previousHandlerLevel);
        handlerLogs.stop();
        subscriberLogger.detachAppender(subscriberLogs);
        subscriberLogger.setLevel(previousSubscriberLevel);
        subscriberLogs.stop();
        if (Objects.isNull(previousPlugin)) {
            BaseDataCache.getInstance().removePluginDataByPluginName(PluginEnum.AI_PROXY.getName());
        } else {
            BaseDataCache.getInstance().cachePluginData(previousPlugin);
        }
    }

    @Test
    void testRejectedDefaultsDoNotPreventRawCacheUpdate() {
        subscriber.onSubscribe(plugin("{\"maxTokens\":100}"));
        AiCommonConfig original = handler.getGlobalConfig();
        PluginData rejected = plugin("{\"maxTokens\":-1,\"apiKey\":\"test-secret\"}");
        subscriber.onSubscribe(rejected);
        assertSame(rejected, BaseDataCache.getInstance().obtainPluginData(PluginEnum.AI_PROXY.getName()));
        assertEquals(original, handler.getGlobalConfig());
        assertEquals(1, handlerLogs.list.size());
        assertSafeRejection(handlerLogs.list.get(0), Level.WARN, "maxTokens.nonPositive");
        PluginData recovered = plugin("{\"maxTokens\":200}");
        subscriber.onSubscribe(recovered);
        assertSame(recovered, BaseDataCache.getInstance().obtainPluginData(PluginEnum.AI_PROXY.getName()));
        assertEquals(200, handler.getGlobalConfig().getMaxTokens());
    }

    @ParameterizedTest
    @CsvSource({"false, provider", "true, provider", "false, protocol", "true, protocol"})
    void testUnexpectedValidationFailureStaysInsideHandler(final boolean initialSync, final String factory) {
        if ("provider".equals(factory)) {
            doThrow(new NullPointerException("test-secret")).when(providers).getProvider("broken");
        } else {
            doThrow(new IllegalStateException("test-secret")).when(protocols).getProtocol("broken");
        }
        final PluginData rejected = plugin("{\"" + factory + "\":\"broken\",\"apiKey\":\"test-secret\"}");
        subscribe(rejected, initialSync);
        assertNull(handler.getGlobalConfig());
        subscribe(plugin("{\"maxTokens\":100}"), initialSync);
        AiCommonConfig original = handler.getGlobalConfig();
        subscribe(rejected, initialSync);
        assertEquals(original, handler.getGlobalConfig());
        assertSame(rejected, BaseDataCache.getInstance().obtainPluginData(PluginEnum.AI_PROXY.getName()));
        assertEquals(2, handlerLogs.list.size());
        for (ILoggingEvent event : handlerLogs.list) {
            assertSafeRejection(event, Level.ERROR, "unexpectedValidationFailure");
            assertTrue(event.getFormattedMessage().contains("exceptionType="
                    + ("provider".equals(factory) ? "NullPointerException" : "IllegalStateException")));
        }
        // The subscriber's existing INFO message contains the raw config; only its error path is checked here.
        assertTrue(subscriberLogs.list.stream().noneMatch(event -> event.getLevel() == Level.ERROR));
        subscribe(plugin("{\"maxTokens\":200}"), initialSync);
        assertEquals(200, handler.getGlobalConfig().getMaxTokens());
    }

    private void subscribe(final PluginData data, final boolean initialSync) {
        assertDoesNotThrow(() -> {
            if (initialSync) {
                InitialSyncApplication.run(() -> subscriber.onSubscribe(data)).join();
            } else {
                subscriber.onSubscribe(data);
            }
        });
    }

    private void assertSafeRejection(final ILoggingEvent event, final Level level, final String reason) {
        assertEquals(level, event.getLevel());
        assertTrue(event.getFormattedMessage().contains("action=rejected reason=" + reason));
        assertTrue(event.getFormattedMessage().contains("pluginId=plugin-test namespaceId=namespace-test"));
        assertFalse(event.getFormattedMessage().contains("test-secret"));
        assertNull(event.getThrowableProxy());
    }

    private PluginData plugin(final String config) {
        PluginData data = new PluginData();
        data.setId("plugin-test");
        data.setNamespaceId("namespace-test");
        data.setName(PluginEnum.AI_PROXY.getName());
        data.setEnabled(true);
        data.setSort(1);
        data.setConfig(config);
        return data;
    }
}
