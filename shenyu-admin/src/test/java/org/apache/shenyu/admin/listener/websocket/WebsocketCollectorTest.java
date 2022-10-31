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

package org.apache.shenyu.admin.listener.websocket;

import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ThreadLocalUtils;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import javax.websocket.RemoteEndpoint;
import javax.websocket.Session;
import java.io.IOException;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The TestCase for {@link WebsocketCollector}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class WebsocketCollectorTest {

    private static Logger loggerSpy;

    private static MockedStatic<LoggerFactory> loggerFactoryMockedStatic;

    private WebsocketCollector websocketCollector;

    @Mock
    private Session session;

    @Mock
    private SyncDataService syncDataService;

    @BeforeAll
    public static void beforeClass() {
        loggerSpy = spy(LoggerFactory.getLogger(WebsocketCollector.class));
        loggerFactoryMockedStatic = mockStatic(LoggerFactory.class);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(WebsocketCollector.class)).thenReturn(loggerSpy);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(anyString())).thenReturn(loggerSpy);
    }

    @AfterAll
    public static void close() {
        loggerFactoryMockedStatic.close();
    }

    @BeforeEach
    public void setUp() {
        websocketCollector = new WebsocketCollector();
    }

    @Test
    public void testOnOpen() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        doNothing().when(loggerSpy).warn(anyString(), anyString());
        websocketCollector.onClose(session);
    }

    @Test
    public void testOnMessage() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(SpringBeanUtils.getInstance().getBean(SyncDataService.class)).thenReturn(syncDataService);
        when(syncDataService.syncAll(DataEventTypeEnum.MYSELF)).thenReturn(true);
        websocketCollector.onOpen(session);
        websocketCollector.onMessage(DataEventTypeEnum.MYSELF.name(), session);
        assertEquals(1L, getSessionSetSize());
        verify(syncDataService, times(1)).syncAll(DataEventTypeEnum.MYSELF);
        doNothing().when(loggerSpy).warn(anyString(), anyString());
        websocketCollector.onClose(session);
    }

    @Test
    public void testOnClose() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        doNothing().when(loggerSpy).warn(anyString(), anyString());
        websocketCollector.onClose(session);
        assertEquals(0L, getSessionSetSize());
        assertNull(getSession());
    }

    @Test
    public void testOnError() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        doNothing().when(loggerSpy).error(anyString(), anyString(), isA(Throwable.class));
        Throwable throwable = new Throwable();
        websocketCollector.onError(session, throwable);
        assertEquals(0L, getSessionSetSize());
        assertNull(getSession());
    }

    @Test
    public void testSend() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        WebsocketCollector.send(null, DataEventTypeEnum.MYSELF);
        verify(basic, times(0)).sendText(null);
        ThreadLocalUtils.put("sessionKey", session);
        WebsocketCollector.send("test_message_1", DataEventTypeEnum.MYSELF);
        verify(basic, times(1)).sendText("test_message_1");
        WebsocketCollector.send("test_message_2", DataEventTypeEnum.CREATE);
        verify(basic, times(1)).sendText("test_message_2");
        doNothing().when(loggerSpy).warn(anyString(), anyString());
        websocketCollector.onClose(session);
        ThreadLocalUtils.remove("sessionKey");
    }

    private long getSessionSetSize() {
        Set sessionSet = (Set) ReflectionTestUtils.getField(WebsocketCollector.class, "SESSION_SET");
        return sessionSet == null ? -1 : sessionSet.size();
    }

    private Session getSession() {
        return (Session) ThreadLocalUtils.get("sessionKey");
    }
}
