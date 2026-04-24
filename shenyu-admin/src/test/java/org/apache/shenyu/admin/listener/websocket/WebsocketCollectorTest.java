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
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.isA;
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
    void testOnMessageRunningModeStandalone() throws IOException {
        ClusterProperties clusterProperties = mock(ClusterProperties.class);
        when(clusterProperties.isEnabled()).thenReturn(false);
        when(SpringBeanUtils.getInstance().getBean(ClusterProperties.class)).thenReturn(clusterProperties);

        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);

        websocketCollector.onOpen(session);
        websocketCollector.onMessage(DataEventTypeEnum.RUNNING_MODE.name(), session);

        verify(basic, times(1)).sendText(anyString());
        websocketCollector.onClose(session);
        ThreadLocalUtils.remove("sessionKey");
    }

    @Test
    void testOnMessageRunningModeCluster() throws IOException {
        ClusterProperties clusterProperties = mock(ClusterProperties.class);
        when(clusterProperties.isEnabled()).thenReturn(true);
        ClusterSelectMasterService masterService = mock(ClusterSelectMasterService.class);
        when(masterService.isMaster()).thenReturn(true);
        when(masterService.getMasterUrl()).thenReturn("http://localhost:9095");
        when(SpringBeanUtils.getInstance().getBean(ClusterProperties.class)).thenReturn(clusterProperties);
        when(SpringBeanUtils.getInstance().getBean(ClusterSelectMasterService.class)).thenReturn(masterService);

        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);

        websocketCollector.onOpen(session);
        websocketCollector.onMessage(DataEventTypeEnum.RUNNING_MODE.name(), session);

        verify(basic, times(1)).sendText(anyString());
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
    void testSendOldApi() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);
        when(session.isOpen()).thenReturn(true);
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

    @Test
    void testSendOldApiMyselfClosedSession() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);
        websocketCollector.onOpen(session);

        // Mark session as closed
        when(session.isOpen()).thenReturn(false);
        ThreadLocalUtils.put("sessionKey", session);
        WebsocketCollector.send("msg", DataEventTypeEnum.MYSELF);
        // closed session → removed from SESSION_SET, no sendText
        verify(basic, never()).sendText("msg");
        assertEquals(0L, getSessionSetSize());
        ThreadLocalUtils.remove("sessionKey");
    }

    @Test
    void testSendOldApiMyselfNullSession() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        // No session in ThreadLocal
        ThreadLocalUtils.remove("sessionKey");
        WebsocketCollector.send("msg", DataEventTypeEnum.MYSELF);
        verify(basic, never()).sendText(anyString());
    }

    @Test
    void testSendWithNamespaceIdBlankMessageNoOp() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "", DataEventTypeEnum.CREATE);
        verify(basic, never()).sendText(anyString());
    }

    @Test
    void testSendWithNamespaceIdBlankNamespaceThrows() {
        assertThrows(ShenyuException.class,
                () -> WebsocketCollector.send("", "some-message", DataEventTypeEnum.CREATE));
    }

    @Test
    void testSendWithNamespaceIdMyself() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);
        when(session.isOpen()).thenReturn(true);
        websocketCollector.onOpen(session);

        ThreadLocalUtils.put("sessionKey", session);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "ns-msg", DataEventTypeEnum.MYSELF);
        verify(basic, times(1)).sendText("ns-msg");

        websocketCollector.onClose(session);
        ThreadLocalUtils.remove("sessionKey");
    }

    @Test
    void testSendWithNamespaceIdMyselfClosedSession() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);
        websocketCollector.onOpen(session);

        when(session.isOpen()).thenReturn(false);
        ThreadLocalUtils.put("sessionKey", session);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "ns-msg", DataEventTypeEnum.MYSELF);
        verify(basic, never()).sendText("ns-msg");

        ThreadLocalUtils.remove("sessionKey");
    }

    @Test
    void testSendWithNamespaceIdMyselfNullSession() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        ThreadLocalUtils.remove("sessionKey");
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "ns-msg", DataEventTypeEnum.MYSELF);
        verify(basic, never()).sendText(anyString());
    }

    @Test
    void testSendWithNamespaceIdNonMyselfBroadcast() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);
        when(session.isOpen()).thenReturn(true);
        websocketCollector.onOpen(session);

        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "broadcast-msg", DataEventTypeEnum.CREATE);
        verify(basic, times(1)).sendText("broadcast-msg");

        websocketCollector.onClose(session);
    }

    @Test
    void testSendBySessionIOException() throws IOException {
        RemoteEndpoint.Basic basic = mock(RemoteEndpoint.Basic.class);
        when(session.getBasicRemote()).thenReturn(basic);
        when(session.isOpen()).thenReturn(true);
        websocketCollector.onOpen(session);

        // IOException on sendText should be caught internally
        org.mockito.Mockito.doThrow(new IOException("io error")).when(basic).sendText(anyString());
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "fail-msg", DataEventTypeEnum.CREATE);
        // no exception propagated; verify attempted send
        verify(basic, times(1)).sendText("fail-msg");

        websocketCollector.onClose(session);
    }

    private long getSessionSetSize() {
        Set sessionSet = (Set) ReflectionTestUtils.getField(WebsocketCollector.class, "SESSION_SET");
        return Objects.isNull(sessionSet) ? -1 : sessionSet.size();
    }

    private Session getSession() {
        return (Session) ThreadLocalUtils.get("sessionKey");
    }
}
