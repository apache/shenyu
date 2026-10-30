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

package org.apache.shenyu.plugin.sync.data.websocket.handler;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import com.google.gson.Gson;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * Test cases for {@link AiProxyApiKeyDataHandler}.
 */
public final class AiProxyApiKeyDataHandlerTest {

    private final List<AiProxyApiKeyDataSubscriber> subscribers;

    private final AiProxyApiKeyDataHandler dataHandler;

    public AiProxyApiKeyDataHandlerTest() {
        subscribers = new LinkedList<>();
        subscribers.add(mock(AiProxyApiKeyDataSubscriber.class));
        subscribers.add(mock(AiProxyApiKeyDataSubscriber.class));
        dataHandler = new AiProxyApiKeyDataHandler(subscribers);
    }

    @Test
    public void testConvert() {
        ProxyApiKeyData apiKeyData = ProxyApiKeyData.builder()
                .realApiKey("real-key")
                .proxyApiKey("proxy-key")
                .description("description")
                .enabled(true)
                .namespaceId("namespace")
                .selectorId("selector")
                .build();
        List<ProxyApiKeyData> sources = Collections.singletonList(apiKeyData);

        List<ProxyApiKeyData> result = dataHandler.convert(new Gson().toJson(sources));

        assertEquals(sources, result);
    }

    @Test
    public void testDoRefresh() {
        List<ProxyApiKeyData> apiKeyDataList = createFakeApiKeyData(3);

        dataHandler.doRefresh(apiKeyDataList);

        subscribers.forEach(subscriber -> verify(subscriber).refresh());
        apiKeyDataList.forEach(data ->
                subscribers.forEach(subscriber -> verify(subscriber).onSubscribe(data)));
    }

    @Test
    public void testDoUpdate() {
        List<ProxyApiKeyData> apiKeyDataList = createFakeApiKeyData(4);

        dataHandler.doUpdate(apiKeyDataList);

        apiKeyDataList.forEach(data ->
                subscribers.forEach(subscriber -> verify(subscriber).onSubscribe(data)));
    }

    @Test
    public void testDoUpdateDoesNotLogProxyKey() {
        ProxyApiKeyData apiKeyData = ProxyApiKeyData.builder()
                .proxyApiKey("proxy-key-secret")
                .namespaceId("namespace-id")
                .selectorId("selector-id")
                .enabled(true)
                .build();

        Logger logger = (Logger) LoggerFactory.getLogger(AiProxyApiKeyDataHandler.class);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.setContext(logger.getLoggerContext());
        appender.start();
        logger.addAppender(appender);
        try {
            dataHandler.doUpdate(Collections.singletonList(apiKeyData));
            String logs = appender.list.stream().map(ILoggingEvent::getFormattedMessage).collect(Collectors.joining("\n"));
            assertFalse(logs.contains("proxy-key-secret"), "the proxy API key must not be logged");
            assertTrue(logs.contains("selector-id"));
        } finally {
            logger.detachAppender(appender);
            appender.stop();
        }
    }

    @Test
    public void testDoDelete() {
        List<ProxyApiKeyData> apiKeyDataList = createFakeApiKeyData(3);

        dataHandler.doDelete(apiKeyDataList);

        apiKeyDataList.forEach(data ->
                subscribers.forEach(subscriber -> verify(subscriber).unSubscribe(data)));
    }

    @Test
    public void testNullDataListShouldBeIgnored() {
        assertDoesNotThrow(() -> dataHandler.doUpdate(null));
        assertDoesNotThrow(() -> dataHandler.doDelete(null));
    }

    @Test
    public void testNullSubscribersShouldBeIgnored() {
        AiProxyApiKeyDataHandler handlerWithoutSubscribers = new AiProxyApiKeyDataHandler(null);

        assertDoesNotThrow(() -> handlerWithoutSubscribers.doRefresh(createFakeApiKeyData(1)));
        assertDoesNotThrow(() -> handlerWithoutSubscribers.doUpdate(createFakeApiKeyData(1)));
        assertDoesNotThrow(() -> handlerWithoutSubscribers.doDelete(createFakeApiKeyData(1)));
    }

    private List<ProxyApiKeyData> createFakeApiKeyData(final int count) {
        List<ProxyApiKeyData> result = new LinkedList<>();
        for (int i = 1; i <= count; i++) {
            result.add(ProxyApiKeyData.builder()
                    .realApiKey("real-key-" + i)
                    .proxyApiKey("proxy-key-" + i)
                    .enabled(true)
                    .build());
        }
        return result;
    }
}
