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

package org.apache.shenyu.plugin.ai.proxy.enhanced.handler;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocol;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

class AiProxyPluginHandlerTest {

    private static final String FIRST_CONFIG = "{\"provider\":\"OpenAI\",\"baseUrl\":\"https://old.example\",\"apiKey\":\"old-key\","
            + "\"model\":\"old-model\",\"temperature\":0.2,\"maxTokens\":16,\"stream\":true,\"protocol\":\"openai-chat\"}";

    private static final String SECOND_CONFIG = "{\"provider\":\"OPEN_AI\",\"baseUrl\":\"https://new.example\",\"apiKey\":\"new-key\","
            + "\"model\":\"new-model\",\"temperature\":1.5,\"maxTokens\":32,\"stream\":false,\"protocol\":\"openai-responses\"}";

    private AiProxyPluginHandler handler;

    @BeforeEach
    void setUp() {
        handler = new AiProxyPluginHandler();
    }

    @AfterEach
    void cleanupCache() {
        AiProxyApiKeyCache.getInstance().refresh();
    }

    @Test
    void testPluginNamed() {
        assertEquals(PluginEnum.AI_PROXY.getName(), handler.pluginNamed());
    }

    @Test
    void testHandlerSelectorCachesHandle() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-1");
        selector.setHandle("{\"provider\":\"open_ai\",\"baseUrl\":\"https://api.openai.com\",\"apiKey\":\"sk-test\",\"model\":\"gpt-4\"}");

        handler.handlerSelector(selector);

        String key = CacheKeyUtils.INST.getKey("sel-1", Constants.DEFAULT_RULE);
        AiProxyHandle cached = handler.getSelectorCachedHandle().obtainHandle(key);
        assertNotNull(cached);
        assertEquals("open_ai", cached.getProvider());
        assertEquals("https://api.openai.com", cached.getBaseUrl());
        assertEquals("sk-test", cached.getApiKey());
        assertEquals("gpt-4", cached.getModel());
    }

    @Test
    void testHandlerSelectorWithNullHandle() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-2");
        selector.setHandle(null);

        handler.handlerSelector(selector);

        String key = CacheKeyUtils.INST.getKey("sel-2", Constants.DEFAULT_RULE);
        assertNull(handler.getSelectorCachedHandle().obtainHandle(key));
    }

    @Test
    void testHandlerSelectorWithEmptyHandle() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-3");
        selector.setHandle("");

        handler.handlerSelector(selector);

        String key = CacheKeyUtils.INST.getKey("sel-3", Constants.DEFAULT_RULE);
        assertNull(handler.getSelectorCachedHandle().obtainHandle(key));
    }

    @Test
    void testHandlerSelectorWithFallbackNormalize() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-4");
        selector.setHandle("{\"provider\":\"open_ai\",\"fallbackEnabled\":\"true\",\"fallbackModel\":\"fallback-model\"}");

        handler.handlerSelector(selector);

        String key = CacheKeyUtils.INST.getKey("sel-4", Constants.DEFAULT_RULE);
        AiProxyHandle cached = handler.getSelectorCachedHandle().obtainHandle(key);
        assertNotNull(cached);
        assertNotNull(cached.getFallbackConfig());
        assertEquals("fallback-model", cached.getFallbackConfig().getModel());
    }

    @Test
    void testRemoveSelector() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-5");
        selector.setHandle("{\"provider\":\"open_ai\",\"model\":\"gpt-4\"}");

        handler.handlerSelector(selector);

        String key = CacheKeyUtils.INST.getKey("sel-5", Constants.DEFAULT_RULE);
        assertNotNull(handler.getSelectorCachedHandle().obtainHandle(key));

        handler.removeSelector(selector);
        assertNull(handler.getSelectorCachedHandle().obtainHandle(key));
    }

    @Test
    void testRemoveSelectorClearsProxyApiKeyCache() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-cache");
        selector.setHandle("{\"provider\":\"open_ai\",\"model\":\"gpt-4\"}");
        handler.handlerSelector(selector);

        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .selectorId("sel-cache")
                .proxyApiKey("proxy-cache")
                .realApiKey("real-cache")
                .enabled(Boolean.TRUE)
                .namespaceId("default")
                .build();
        AiProxyApiKeyCache.getInstance().cache(data);
        assertEquals("real-cache", AiProxyApiKeyCache.getInstance().getRealApiKey("sel-cache", "proxy-cache"));

        handler.removeSelector(selector);

        assertNull(AiProxyApiKeyCache.getInstance().getRealApiKey("sel-cache", "proxy-cache"));
        assertEquals(0, AiProxyApiKeyCache.getInstance().size());
    }

    @Test
    void testNullPluginUpdateIsIgnored() {
        handler.handlerPlugin(null);
    }

    @Test
    void testGlobalConfigurationIsReplacedAndCopied() {
        handler.handlerPlugin(plugin("{\"provider\":\"OpenAI\",\"apiKey\":\"first-key\",\"model\":\"first-model\"}"));
        AiCommonConfig first = handler.getGlobalConfig();
        assertEquals("first-model", first.getModel());
        first.setApiKey("request-key");
        assertEquals("first-key", handler.getGlobalConfig().getApiKey());
        handler.handlerPlugin(plugin("{\"model\":\"second-model\"}"));
        assertEquals("second-model", handler.getGlobalConfig().getModel());
        assertNull(handler.getGlobalConfig().getApiKey());
        assertNull(handler.getGlobalConfig().getProvider());
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" ", "null", "{}"})
    void testClearingPluginConfigurationRemovesOldDefaults(final String config) {
        handler.handlerPlugin(plugin("{\"apiKey\":\"old-key\",\"model\":\"old-model\"}"));
        handler.handlerPlugin(plugin(config));
        AiCommonConfig cleared = new AiProxyConfigService(handler).resolvePrimaryConfig(null);
        assertNull(cleared.getApiKey());
        assertNull(cleared.getModel());
    }

    @Test
    void testDisableReenableAndRemovePlugin() {
        PluginData data = plugin("{\"model\":\"global-model\"}");
        handler.handlerPlugin(data);
        data.setEnabled(false);
        handler.handlerPlugin(data);
        assertNull(handler.getGlobalConfig());
        data.setEnabled(true);
        handler.handlerPlugin(data);
        assertEquals("global-model", handler.getGlobalConfig().getModel());
        handler.removePlugin(data);
        assertNull(handler.getGlobalConfig());
    }

    @ParameterizedTest
    @ValueSource(strings = {"not-json", "{", "[]", "{\"maxTokens\":\"invalid\"}",
            "{\"maxTokens\":0}", "{\"maxTokens\":-1}", "{\"temperature\":-0.1}",
            "{\"temperature\":\"NaN\"}", "{\"temperature\":\"Infinity\"}", "{\"temperature\":\"-Infinity\"}",
            "{\"baseUrl\":\"https://example.com/%ZZ\"}"})
    void testInvalidPluginConfigurationPreservesDefaultsAndAllowsRecovery(final String config) {
        handler.handlerPlugin(plugin("{\"apiKey\":\"old-key\",\"model\":\"old-model\"}"));
        assertNotNull(handler.getGlobalConfig());

        assertDoesNotThrow(() -> handler.handlerPlugin(plugin(config)));

        assertNotNull(handler.getGlobalConfig());
        assertEquals("old-key", handler.getGlobalConfig().getApiKey());
        assertEquals("old-model", handler.getGlobalConfig().getModel());

        AiCommonConfig retained = new AiProxyConfigService(handler).resolvePrimaryConfig(null);
        assertEquals("old-key", retained.getApiKey());
        assertEquals("old-model", retained.getModel());

        handler.handlerPlugin(plugin("{\"apiKey\":\"new-key\",\"model\":\"new-model\"}"));
        assertEquals("new-key", handler.getGlobalConfig().getApiKey());
        assertEquals("new-model", handler.getGlobalConfig().getModel());
    }

    @Test
    void testHandlerInstancesDoNotShareGlobalConfiguration() {
        AiProxyPluginHandler other = new AiProxyPluginHandler();
        handler.handlerPlugin(plugin("{\"model\":\"first-model\"}"));
        assertNull(other.getGlobalConfig());
        other.handlerPlugin(plugin("{\"model\":\"second-model\"}"));
        assertEquals("first-model", handler.getGlobalConfig().getModel());
        assertEquals("second-model", other.getGlobalConfig().getModel());
        handler.removePlugin(null);
        assertEquals("second-model", other.getGlobalConfig().getModel());
    }


    @ParameterizedTest
    @ValueSource(strings = {"{", "[]", "{\"maxTokens\":0}", "{\"temperature\":-1}", "{\"baseUrl\":\"%ZZ\"}",
            "{\"baseUrl\":\"ftp://example.org\"}", "{\"baseUrl\":\"relative\"}", "{\"baseUrl\":\"https:///v1\"}",
            "{\"baseUrl\":\"https://user:secret@example.org\"}", "{\"baseUrl\":\"https://example.org/?key=secret#fragment\"}",
            "{\"baseUrl\":\"https://example.org:0\"}", "{\"baseUrl\":\"https://example.org:65536\"}",
            "{\"provider\":\"opneai\"}", "{\"protocol\":\"unknown\"}", "{\"protocol\":\"OPENAI-CHAT\"}", "{\"protocol\":\" openai-chat \"}"})
    void testRejectedCandidatesKeepEveryPublishedField(final String config) {
        AiProxyPluginHandler registered = registeredHandler();
        registered.handlerPlugin(plugin(config));
        assertNull(registered.getGlobalConfig());
        registered.handlerPlugin(plugin(FIRST_CONFIG));
        AiCommonConfig original = registered.getGlobalConfig();
        registered.handlerPlugin(plugin(config));
        assertEquals(original, registered.getGlobalConfig());
        registered.handlerPlugin(plugin(SECOND_CONFIG));
        assertEquals("new-key", registered.getGlobalConfig().getApiKey());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{}", "{\"provider\":\" Open-AI \"}", "{\"provider\":\"OPEN_AI\"}",
            "{\"provider\":\"tenant\",\"protocol\":\"tenant-chat\"}", "{\"provider\":null,\"protocol\":null}",
            "{\"provider\":\" \",\"protocol\":\" \"}", "{\"temperature\":3,\"maxTokens\":1,\"baseUrl\":\" \"}",
            "{\"baseUrl\":\"https://example.org/a%20b\",\"stream\":\"abc\"}"})
    void testRegisteredNamesAndPartialDefaultsAreAccepted(final String config) {
        AiProxyPluginHandler registered = registeredHandler();
        registered.handlerPlugin(plugin(FIRST_CONFIG));
        registered.handlerPlugin(plugin(config));
        assertNotNull(registered.getGlobalConfig());
        assertNull(registered.getGlobalConfig().getApiKey());
        assertNull(registered.getGlobalConfig().getModel());
    }

    @Test
    void testNoArgumentConstructorPreservesProgrammaticNameHandling() {
        handler.handlerPlugin(plugin("{\"provider\":\"external-provider\",\"protocol\":\"external-protocol\"}"));
        AiCommonConfig original = handler.getGlobalConfig();
        assertEquals("external-provider", original.getProvider());
        assertEquals("external-protocol", original.getProtocol());
        handler.handlerPlugin(plugin("{\"maxTokens\":0}"));
        assertEquals(original, handler.getGlobalConfig());
    }

    @Test
    void testMissingRegistriesWarnOncePerHandler() {
        Logger logger = (Logger) LoggerFactory.getLogger(AiProxyPluginHandler.class);
        Level previous = logger.getLevel();
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.setContext(logger.getLoggerContext());
        appender.start();
        logger.setLevel(Level.INFO);
        logger.addAppender(appender);
        try {
            final AiProxyPluginHandler compatibility = new AiProxyPluginHandler();
            assertEquals(1, appender.list.size());
            assertEquals(Level.WARN, appender.list.get(0).getLevel());
            assertTrue(appender.list.get(0).getFormattedMessage().contains("validationMode=withoutRegistries"));
            assertNull(appender.list.get(0).getThrowableProxy());
            compatibility.handlerPlugin(plugin(FIRST_CONFIG));
            compatibility.handlerPlugin(plugin(SECOND_CONFIG));
            assertEquals(1, appender.list.size());
            appender.list.clear();
            registeredHandler().handlerPlugin(plugin(FIRST_CONFIG));
            assertTrue(appender.list.isEmpty());
        } finally {
            logger.detachAppender(appender);
            logger.setLevel(previous);
            appender.stop();
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"runtime", "nullPointer", "illegalArgument", "nullMessage"})
    void testUnexpectedValidationFailuresPreserveDefaultsAndAllowRecovery(final String failure) {
        RuntimeException error = switch (failure) {
            case "runtime" -> new IllegalStateException("test-secret");
            case "nullPointer" -> new NullPointerException("test-secret");
            case "illegalArgument" -> new IllegalArgumentException("maxTokens.nonPositive test-secret");
            default -> new IllegalArgumentException();
        };
        AiProxyProviderFactory factory = spy(new AiProxyProviderFactory(List.of(new OpenAi())));
        doThrow(error).when(factory).getProvider("broken");
        AiProxyPluginHandler registered = new AiProxyPluginHandler(factory, protocols());
        PluginData invalid = plugin("{\"provider\":\"broken\",\"apiKey\":\"test-secret\"}");
        invalid.setId("plugin-test");
        invalid.setNamespaceId("namespace-test");
        Logger logger = (Logger) LoggerFactory.getLogger(AiProxyPluginHandler.class);
        Level previous = logger.getLevel();
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.setContext(logger.getLoggerContext());
        appender.start();
        logger.setLevel(Level.INFO);
        logger.addAppender(appender);
        try {
            assertDoesNotThrow(() -> registered.handlerPlugin(invalid));
            assertNull(registered.getGlobalConfig());
            registered.handlerPlugin(plugin(FIRST_CONFIG));
            AiCommonConfig original = registered.getGlobalConfig();
            assertDoesNotThrow(() -> registered.handlerPlugin(invalid));
            assertEquals(original, registered.getGlobalConfig());
            assertEquals(2, appender.list.size());
            for (ILoggingEvent event : appender.list) {
                assertEquals(Level.ERROR, event.getLevel());
                assertTrue(event.getFormattedMessage().contains("reason=unexpectedValidationFailure"));
                assertTrue(event.getFormattedMessage().contains("pluginId=plugin-test namespaceId=namespace-test"));
                assertFalse(event.getFormattedMessage().contains("test-secret"));
                assertNull(event.getThrowableProxy());
            }
            registered.handlerPlugin(plugin(SECOND_CONFIG));
            assertEquals("new-key", registered.getGlobalConfig().getApiKey());
        } finally {
            logger.detachAppender(appender);
            logger.setLevel(previous);
            appender.stop();
        }
    }

    @Test
    void testClearDisableAndRemovalCannotRestoreOldDefaults() {
        AiProxyPluginHandler registered = registeredHandler();
        registered.handlerPlugin(plugin(FIRST_CONFIG));
        PluginData disabled = plugin("{");
        disabled.setEnabled(false);
        registered.handlerPlugin(disabled);
        assertNull(registered.getGlobalConfig());
        disabled.setEnabled(true);
        registered.handlerPlugin(disabled);
        assertNull(registered.getGlobalConfig());
        registered.handlerPlugin(plugin(SECOND_CONFIG));
        assertEquals("new-key", registered.getGlobalConfig().getApiKey());
        registered.removePlugin(plugin("{"));
        registered.handlerPlugin(plugin("{\"temperature\":-1}"));
        assertNull(registered.getGlobalConfig());
        registered.handlerPlugin(plugin(FIRST_CONFIG));
        registered.handlerPlugin(plugin("{}"));
        assertEquals(new AiCommonConfig(), registered.getGlobalConfig());
        AiCommonConfig before = registered.getGlobalConfig();
        registered.handlerPlugin(null);
        assertEquals(before, registered.getGlobalConfig());
    }

    @Test
    void testReadersKeepCompleteIsolatedSnapshotsDuringPublication() throws Exception {
        CountDownLatch validating = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        AiProxyPluginHandler registered = pausedHandler("OPEN_AI", validating, release);
        registered.handlerPlugin(plugin(FIRST_CONFIG));
        AiCommonConfig first = registered.getGlobalConfig();
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            final Future<?> update = executor.submit(() -> registered.handlerPlugin(plugin(SECOND_CONFIG)));
            assertTrue(validating.await(5, TimeUnit.SECONDS));
            assertEquals(first, registered.getGlobalConfig());
            AiCommonConfig local = registered.getGlobalConfig();
            local.setApiKey("request-only");
            assertEquals(first, registered.getGlobalConfig());
            release.countDown();
            update.get(5, TimeUnit.SECONDS);
            AiProxyPluginHandler expected = registeredHandler();
            expected.handlerPlugin(plugin(SECOND_CONFIG));
            assertEquals(expected.getGlobalConfig(), registered.getGlobalConfig());
            expected.handlerPlugin(plugin(FIRST_CONFIG));
            assertEquals(expected.getGlobalConfig(), first);
        } finally {
            release.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"update", "clear", "disable", "remove"})
    void testDelayedRejectionDoesNotRestoreAnOlderSnapshot(final String operation) throws Exception {
        CountDownLatch validating = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        AiProxyPluginHandler registered = pausedHandler("unknown", validating, release);
        registered.handlerPlugin(plugin(FIRST_CONFIG));
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            final Future<?> rejected = executor.submit(() -> registered.handlerPlugin(plugin("{\"provider\":\"unknown\"}")));
            assertTrue(validating.await(5, TimeUnit.SECONDS));
            switch (operation) {
                case "update":
                    registered.handlerPlugin(plugin(SECOND_CONFIG));
                    break;
                case "clear":
                    registered.handlerPlugin(plugin("null"));
                    break;
                case "disable":
                    PluginData disabled = plugin("{");
                    disabled.setEnabled(false);
                    registered.handlerPlugin(disabled);
                    break;
                default:
                    registered.removePlugin(null);
                    break;
            }
            AiCommonConfig latest = registered.getGlobalConfig();
            release.countDown();
            rejected.get(5, TimeUnit.SECONDS);
            assertEquals(latest, registered.getGlobalConfig());
        } finally {
            release.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"update", "clear"})
    void testDelayedUnexpectedFailureDoesNotRestoreAnOlderSnapshot(final String operation) throws Exception {
        CountDownLatch validating = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        AiProxyProviderFactory factory = spy(new AiProxyProviderFactory(List.of(new OpenAi())));
        doAnswer(invocation -> {
            validating.countDown();
            assertTrue(release.await(5, TimeUnit.SECONDS));
            throw new IllegalStateException("test-secret");
        }).when(factory).getProvider("broken");
        AiProxyPluginHandler registered = new AiProxyPluginHandler(factory, protocols());
        registered.handlerPlugin(plugin(FIRST_CONFIG));
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            final Future<?> rejected = executor.submit(() -> registered.handlerPlugin(plugin("{\"provider\":\"broken\"}")));
            assertTrue(validating.await(5, TimeUnit.SECONDS));
            registered.handlerPlugin(plugin("update".equals(operation) ? SECOND_CONFIG : "null"));
            AiCommonConfig latest = registered.getGlobalConfig();
            release.countDown();
            rejected.get(5, TimeUnit.SECONDS);
            assertEquals(latest, registered.getGlobalConfig());
        } finally {
            release.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test
    void testDiagnosticsDistinguishOperationsWithoutExposingInputs() {
        Logger logger = (Logger) LoggerFactory.getLogger(AiProxyPluginHandler.class);
        Level previous = logger.getLevel();
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.setContext(logger.getLoggerContext());
        appender.start();
        logger.setLevel(Level.INFO);
        logger.addAppender(appender);
        try {
            AiProxyPluginHandler registered = registeredHandler();
            registered.handlerPlugin(plugin(FIRST_CONFIG));
            String[][] rejected = {
                {"{\"apiKey\":\"test-secret\",", "malformedConfig"},
                {"{\"maxTokens\":0,\"apiKey\":\"test-secret\"}", "maxTokens.nonPositive"},
                {"{\"baseUrl\":\"https://test-secret@example.org/%ZZ\"}", "baseUrl.invalidSyntax"},
                {"{\"baseUrl\":\"https://example.org?token=test-secret\"}", "baseUrl.invalidEndpoint"},
                {"{\"provider\":\"test-secret\"}", "provider.unregistered"},
                {"{\"protocol\":\"test-secret\"}", "protocol.unregistered"},
            };
            for (String[] candidate : rejected) {
                appender.list.clear();
                PluginData data = plugin(candidate[0]);
                data.setId("plugin-test");
                data.setNamespaceId("namespace-test");
                registered.handlerPlugin(data);
                assertEquals(1, appender.list.size());
                ILoggingEvent event = appender.list.get(0);
                assertEquals(Level.WARN, event.getLevel());
                assertTrue(event.getFormattedMessage().contains("action=rejected reason=" + candidate[1]));
                assertTrue(event.getFormattedMessage().contains("pluginId=plugin-test namespaceId=namespace-test"));
                assertFalse(event.getFormattedMessage().contains("test-secret"));
                assertFalse(event.getFormattedMessage().contains(candidate[0]));
                assertNull(event.getThrowableProxy());
                assertEquals("old-key", registered.getGlobalConfig().getApiKey());
            }
            appender.list.clear();
            registered.handlerPlugin(plugin(" "));
            registered.handlerPlugin(plugin("null"));
            PluginData disabled = plugin("test-secret");
            disabled.setEnabled(false);
            registered.handlerPlugin(disabled);
            registered.removePlugin(disabled);
            assertEquals(4, appender.list.size());
            assertTrue(appender.list.get(0).getFormattedMessage().contains("action=cleared reason=blankConfig"));
            assertTrue(appender.list.get(1).getFormattedMessage().contains("action=cleared reason=jsonNull"));
            assertTrue(appender.list.get(2).getFormattedMessage().contains("action=disabled"));
            assertTrue(appender.list.get(3).getFormattedMessage().contains("action=removed"));
            for (ILoggingEvent event : appender.list) {
                assertEquals(Level.INFO, event.getLevel());
                assertFalse(event.getFormattedMessage().contains("test-secret"));
                assertNull(event.getThrowableProxy());
            }
        } finally {
            logger.detachAppender(appender);
            logger.setLevel(previous);
            appender.stop();
        }
    }

    private AiProxyPluginHandler registeredHandler() {
        return new AiProxyPluginHandler(new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("tenant"))), protocols());
    }

    private AiProxyProtocolFactory protocols() {
        AiProxyProtocol custom = mock(AiProxyProtocol.class);
        when(custom.name()).thenReturn("tenant-chat");
        return new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses(), custom));
    }

    private AiProxyPluginHandler pausedHandler(final String name, final CountDownLatch validating, final CountDownLatch release) {
        AiProxyProviderFactory factory = spy(new AiProxyProviderFactory(List.of(new OpenAi())));
        doAnswer(invocation -> {
            validating.countDown();
            assertTrue(release.await(5, TimeUnit.SECONDS));
            return invocation.callRealMethod();
        }).when(factory).getProvider(name);
        return new AiProxyPluginHandler(factory, protocols());
    }

    private PluginData plugin(final String config) {
        PluginData data = new PluginData();
        data.setName(PluginEnum.AI_PROXY.getName());
        data.setConfig(config);
        return data;
    }
}
