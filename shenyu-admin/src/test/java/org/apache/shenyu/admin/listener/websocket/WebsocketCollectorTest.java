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

import jakarta.websocket.RemoteEndpoint;
import jakarta.websocket.SendHandler;
import jakarta.websocket.SendResult;
import jakarta.websocket.Session;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.service.publish.InstanceInfoReportEventPublisher;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ThreadLocalUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
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
    void setUp() {
        websocketCollector = new WebsocketCollector();
        // Clear shared static state between tests
        clearStaticSessionState();
        when(session.isOpen()).thenReturn(true);
        Map<String, Object> userProperties = new HashMap<>();
        userProperties.put(Constants.SHENYU_NAMESPACE_ID, Constants.SYS_DEFAULT_NAMESPACE_ID);
        when(session.getUserProperties()).thenReturn(userProperties);

        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        ThreadLocalUtils.remove("sessionKey");
    }

    @SuppressWarnings("unchecked")
    private void clearStaticSessionState() {
        Set<Session> sessionSet = (Set<Session>) ReflectionTestUtils.getField(WebsocketCollector.class, "SESSION_SET");
        if (Objects.nonNull(sessionSet)) {
            sessionSet.clear();
        }
        Map<String, Set<Session>> namespaceMap =
                (Map<String, Set<Session>>) ReflectionTestUtils.getField(WebsocketCollector.class, "NAMESPACE_SESSION_MAP");
        if (Objects.nonNull(namespaceMap)) {
            namespaceMap.clear();
        }
        Map<Session, ?> sessionSendQueues =
                (Map<Session, ?>) ReflectionTestUtils.getField(WebsocketCollector.class, "SESSION_SEND_QUEUES");
        if (Objects.nonNull(sessionSendQueues)) {
            sessionSendQueues.clear();
        }
    }

    @Test
    void testOnOpen() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        doNothing().when(loggerSpy).warn(anyString(), anyString());
        websocketCollector.onClose(session);
    }

    @Test
    void testOnOpenWithBlankNamespaceIdThrows() {
        Map<String, Object> userProperties = new HashMap<>();
        // no SHENYU_NAMESPACE_ID set → getNamespaceId returns null → throws ShenyuException
        when(session.getUserProperties()).thenReturn(userProperties);
        assertThrows(ShenyuException.class, () -> websocketCollector.onOpen(session));
        // clean up the session that was added before throw
        websocketCollector.onClose(session);
    }

    @Test
    void testOnOpenWithClientIp() {
        Map<String, Object> userProperties = new HashMap<>();
        userProperties.put(Constants.SHENYU_NAMESPACE_ID, Constants.SYS_DEFAULT_NAMESPACE_ID);
        userProperties.put(WebsocketListener.CLIENT_IP_NAME, "192.168.1.1");
        when(session.getUserProperties()).thenReturn(userProperties);

        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        websocketCollector.onClose(session);
    }

    @Test
    void testOnMessage() {
        when(SpringBeanUtils.getInstance().getBean(SyncDataService.class)).thenReturn(syncDataService);
        when(syncDataService.syncAllByNamespaceId(DataEventTypeEnum.MYSELF, Constants.SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        websocketCollector.onOpen(session);
        websocketCollector.onMessage(DataEventTypeEnum.MYSELF.name(), session);
        assertEquals(1L, getSessionSetSize());
        verify(syncDataService, times(1)).syncAllByNamespaceId(DataEventTypeEnum.MYSELF, Constants.SYS_DEFAULT_NAMESPACE_ID);
        doNothing().when(loggerSpy).warn(anyString(), anyString());
        websocketCollector.onClose(session);
    }

    @Test
    void testOnMessageUnknownMessageReturnsEarly() {
        websocketCollector.onOpen(session);
        // Unknown message — early return, no service calls
        websocketCollector.onMessage("UNKNOWN_EVENT", session);
        verify(syncDataService, never()).syncAllByNamespaceId(DataEventTypeEnum.MYSELF, Constants.SYS_DEFAULT_NAMESPACE_ID);
        websocketCollector.onClose(session);
    }

    @Test
    void testOnMessageRunningModeStandalone() {
        ClusterProperties clusterProperties = mock(ClusterProperties.class);
        when(clusterProperties.isEnabled()).thenReturn(false);
        when(SpringBeanUtils.getInstance().getBean(ClusterProperties.class)).thenReturn(clusterProperties);

        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);

        websocketCollector.onOpen(session);
        websocketCollector.onMessage(DataEventTypeEnum.RUNNING_MODE.name(), session);

        verify(async, times(1)).sendText(anyString(), any(SendHandler.class));
        websocketCollector.onClose(session);
        ThreadLocalUtils.remove("sessionKey");
    }

    @Test
    void testOnMessageRunningModeCluster() {
        ClusterProperties clusterProperties = mock(ClusterProperties.class);
        when(clusterProperties.isEnabled()).thenReturn(true);
        ClusterSelectMasterService masterService = mock(ClusterSelectMasterService.class);
        when(masterService.isMaster()).thenReturn(true);
        when(masterService.getMasterUrl()).thenReturn("http://localhost:9095");
        when(SpringBeanUtils.getInstance().getBean(ClusterProperties.class)).thenReturn(clusterProperties);
        when(SpringBeanUtils.getInstance().getBean(ClusterSelectMasterService.class)).thenReturn(masterService);

        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);

        websocketCollector.onOpen(session);
        websocketCollector.onMessage(DataEventTypeEnum.RUNNING_MODE.name(), session);

        verify(async, times(1)).sendText(anyString(), any(SendHandler.class));
        websocketCollector.onClose(session);
        ThreadLocalUtils.remove("sessionKey");
    }

    @Test
    void testOnMessageBootstrapInstanceInfo() {
        InstanceInfoReportEventPublisher publisher = mock(InstanceInfoReportEventPublisher.class);
        when(SpringBeanUtils.getInstance().getBean(InstanceInfoReportEventPublisher.class)).thenReturn(publisher);

        Map<String, Object> userProperties = new HashMap<>();
        userProperties.put(Constants.SHENYU_NAMESPACE_ID, Constants.SYS_DEFAULT_NAMESPACE_ID);
        userProperties.put(Constants.CLIENT_PORT_NAME, "8080");
        when(session.getUserProperties()).thenReturn(userProperties);

        websocketCollector.onOpen(session);
        String bootstrapMsg = "{\"" + InstanceTypeConstants.BOOTSTRAP_INSTANCE_INFO + "\":{\"key\":\"val\"}}";
        websocketCollector.onMessage(bootstrapMsg, session);

        verify(publisher, times(1)).publish(isA(org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent.class));
        websocketCollector.onClose(session);
    }

    @Test
    void testOnClose() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        doNothing().when(loggerSpy).warn(anyString(), anyString());
        websocketCollector.onClose(session);
        assertEquals(0L, getSessionSetSize());
        assertNull(getSession());
    }

    @Test
    void testOnCloseWithBlankNamespaceId() {
        // Session with no namespace — clearSession should still work (blank namespaceId branch)
        Map<String, Object> props = new HashMap<>();
        props.put(Constants.SHENYU_NAMESPACE_ID, Constants.SYS_DEFAULT_NAMESPACE_ID);
        when(session.getUserProperties()).thenReturn(props);
        websocketCollector.onOpen(session);

        // Now change namespace to blank before close
        Session session2 = mock(Session.class);
        when(session2.isOpen()).thenReturn(false);
        when(session2.getUserProperties()).thenReturn(new HashMap<>());
        websocketCollector.onClose(session2);
        // original session still tracked
        assertEquals(1L, getSessionSetSize());
        websocketCollector.onClose(session);
    }

    @Test
    void testOnError() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        doNothing().when(loggerSpy).error(anyString(), anyString(), isA(Throwable.class));
        Throwable throwable = new Throwable();
        websocketCollector.onError(session, throwable);
        assertEquals(0L, getSessionSetSize());
        assertNull(getSession());
    }

    @Test
    void testSendOldApi() {
        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        when(session.isOpen()).thenReturn(true);
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        WebsocketCollector.send(null, DataEventTypeEnum.MYSELF);
        verify(async, times(0)).sendText(eq(null), any(SendHandler.class));
        ThreadLocalUtils.put("sessionKey", session);
        WebsocketCollector.send("test_message_1", DataEventTypeEnum.MYSELF);
        verify(async, times(1)).sendText(eq("test_message_1"), any(SendHandler.class));
        WebsocketCollector.send("test_message_2", DataEventTypeEnum.CREATE);
        verify(async, times(1)).sendText(eq("test_message_2"), any(SendHandler.class));
        doNothing().when(loggerSpy).warn(anyString(), anyString());
        websocketCollector.onClose(session);
        ThreadLocalUtils.remove("sessionKey");
    }

    @Test
    void testSendOldApiMyselfClosedSession() {
        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        websocketCollector.onOpen(session);

        // Mark session as closed
        when(session.isOpen()).thenReturn(false);
        ThreadLocalUtils.put("sessionKey", session);
        WebsocketCollector.send("msg", DataEventTypeEnum.MYSELF);
        // closed session → removed from SESSION_SET, no sendText
        verify(async, never()).sendText(eq("msg"), any(SendHandler.class));
        assertEquals(0L, getSessionSetSize());
        ThreadLocalUtils.remove("sessionKey");
    }

    @Test
    void testSendOldApiMyselfNullSession() {
        final RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        // No session in ThreadLocal
        ThreadLocalUtils.remove("sessionKey");
        WebsocketCollector.send("msg", DataEventTypeEnum.MYSELF);
        verify(async, never()).sendText(anyString(), any(SendHandler.class));
    }

    @Test
    void testSendWithNamespaceIdBlankMessageNoOp() {
        final RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "", DataEventTypeEnum.CREATE);
        verify(async, never()).sendText(anyString(), any(SendHandler.class));
    }

    @Test
    void testSendWithNamespaceIdBlankNamespaceThrows() {
        assertThrows(ShenyuException.class,
                () -> WebsocketCollector.send("", "some-message", DataEventTypeEnum.CREATE));
    }

    @Test
    void testSendWithNamespaceIdMyself() {
        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        when(session.isOpen()).thenReturn(true);
        websocketCollector.onOpen(session);

        ThreadLocalUtils.put("sessionKey", session);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "ns-msg", DataEventTypeEnum.MYSELF);
        verify(async, times(1)).sendText(eq("ns-msg"), any(SendHandler.class));

        websocketCollector.onClose(session);
        ThreadLocalUtils.remove("sessionKey");
    }

    @Test
    void testSendWithNamespaceIdMyselfClosedSession() {
        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        websocketCollector.onOpen(session);

        when(session.isOpen()).thenReturn(false);
        ThreadLocalUtils.put("sessionKey", session);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "ns-msg", DataEventTypeEnum.MYSELF);
        verify(async, never()).sendText(eq("ns-msg"), any(SendHandler.class));

        ThreadLocalUtils.remove("sessionKey");
    }

    @Test
    void testSendWithNamespaceIdMyselfNullSession() {
        final RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        ThreadLocalUtils.remove("sessionKey");
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "ns-msg", DataEventTypeEnum.MYSELF);
        verify(async, never()).sendText(anyString(), any(SendHandler.class));
    }

    @Test
    void testSendWithNamespaceIdNonMyselfBroadcast() {
        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        when(session.isOpen()).thenReturn(true);
        websocketCollector.onOpen(session);

        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "broadcast-msg", DataEventTypeEnum.CREATE);
        verify(async, times(1)).sendText(eq("broadcast-msg"), any(SendHandler.class));

        websocketCollector.onClose(session);
    }

    @Test
    void testSendBySessionFailure() {
        final RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        when(session.getAsyncRemote()).thenReturn(async);
        when(session.isOpen()).thenReturn(true);
        websocketCollector.onOpen(session);

        doAnswer(invocation -> {
            SendHandler handler = invocation.getArgument(1);
            handler.onResult(new SendResult(new IllegalStateException("send error")));
            return null;
        }).when(async).sendText(anyString(), any(SendHandler.class));
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "fail-msg", DataEventTypeEnum.CREATE);
        verify(async, times(1)).sendText(eq("fail-msg"), any(SendHandler.class));

        websocketCollector.onClose(session);
    }

    @Test
    void testSendDoesNotWaitForOtherSession() {
        Session anotherSession = mock(Session.class);
        Map<String, Object> userProperties = session.getUserProperties();
        when(anotherSession.isOpen()).thenReturn(true);
        when(anotherSession.getUserProperties()).thenReturn(userProperties);
        RemoteEndpoint.Async firstAsync = mock(RemoteEndpoint.Async.class);
        RemoteEndpoint.Async secondAsync = mock(RemoteEndpoint.Async.class);
        when(session.getAsyncRemote()).thenReturn(firstAsync);
        when(anotherSession.getAsyncRemote()).thenReturn(secondAsync);
        websocketCollector.onOpen(session);
        websocketCollector.onOpen(anotherSession);

        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "broadcast-msg", DataEventTypeEnum.CREATE);

        verify(firstAsync).sendText(eq("broadcast-msg"), any(SendHandler.class));
        verify(secondAsync).sendText(eq("broadcast-msg"), any(SendHandler.class));
        websocketCollector.onClose(session);
        websocketCollector.onClose(anotherSession);
    }

    @Test
    void testSendMessagesSequentiallyForSameSession() {
        final RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        when(session.getAsyncRemote()).thenReturn(async);
        websocketCollector.onOpen(session);

        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "first-message", DataEventTypeEnum.CREATE);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "second-message", DataEventTypeEnum.CREATE);

        ArgumentCaptor<SendHandler> handlerCaptor = ArgumentCaptor.forClass(SendHandler.class);
        verify(async).sendText(eq("first-message"), handlerCaptor.capture());
        verify(async, never()).sendText(eq("second-message"), any(SendHandler.class));

        handlerCaptor.getValue().onResult(new SendResult());

        verify(async).sendText(eq("second-message"), any(SendHandler.class));
        websocketCollector.onClose(session);
    }

    private RemoteEndpoint.Async mockSuccessfulAsyncRemote(final Session targetSession) {
        final RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        when(targetSession.getAsyncRemote()).thenReturn(async);
        doAnswer(invocation -> {
            SendHandler handler = invocation.getArgument(1);
            handler.onResult(new SendResult());
            return null;
        }).when(async).sendText(anyString(), any(SendHandler.class));
        return async;
    }

    private long getSessionSetSize() {
        Set sessionSet = (Set) ReflectionTestUtils.getField(WebsocketCollector.class, "SESSION_SET");
        return Objects.isNull(sessionSet) ? -1 : sessionSet.size();
    }

    private Session getSession() {
        return (Session) ThreadLocalUtils.get("sessionKey");
    }
}
