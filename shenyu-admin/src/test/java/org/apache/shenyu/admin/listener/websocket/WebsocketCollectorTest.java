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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import com.google.gson.JsonObject;
import jakarta.websocket.RemoteEndpoint;
import jakarta.websocket.SendHandler;
import jakarta.websocket.SendResult;
import jakarta.websocket.Session;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.service.publish.InstanceInfoReportEventPublisher;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ThreadLocalUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.dto.WebsocketSyncFrame;
import org.apache.shenyu.common.dto.WebsocketSyncFrameV2;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
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
        loggerFactoryMockedStatic = mockStatic(LoggerFactory.class, CALLS_REAL_METHODS);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(WebsocketCollector.class)).thenReturn(loggerSpy);
    }

    @AfterAll
    public static void close() {
        loggerFactoryMockedStatic.close();
    }

    @Test
    public void testUnrelatedClassLoggerIsPreserved() {
        Logger logger = LoggerFactory.getLogger(JsonUtils.class);
        assertNotNull(logger);
        assertEquals(JsonUtils.class.getName(), logger.getName());
        assertNull(JsonUtils.jsonToObject("", Map.class));
    }

    @BeforeEach
    void setUp() {
        websocketCollector = new WebsocketCollector();
        // Clear shared static state between tests
        clearStaticSessionState();
        WebsocketCollector.resetSendGuards();
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
        Map<Session, ?> sessionNamespaces =
                (Map<Session, ?>) ReflectionTestUtils.getField(WebsocketCollector.class, "SESSION_NAMESPACE_IDS");
        if (Objects.nonNull(sessionNamespaces)) {
            sessionNamespaces.clear();
        }
    }

    @Test
    public void testNestedApiKeysAreRedacted() {
        String message = "{\"data\":[{\"proxyApiKey\":\"proxy-secret\","
                + "\"nested\":{\"realApiKey\":\"real-secret\",\"apiKey\":\"api-secret\"}}]}";
        String masked = ReflectionTestUtils.invokeMethod(WebsocketCollector.class, "maskSensitive", message);
        assertFalse(masked.contains("proxy-secret"));
        assertFalse(masked.contains("real-secret"));
        assertFalse(masked.contains("api-secret"));
        assertTrue(masked.contains("******"));
    }

    @Test
    void testOnOpen() {
        websocketCollector.onOpen(session);
        assertEquals(1L, getSessionSetSize());
        doNothing().when(loggerSpy).warn(anyString(), anyString());
        websocketCollector.onClose(session);
    }

    @Test
    void testInvalidInitialSyncRequestIsIgnored() {
        when(SpringBeanUtils.getInstance().getBean(ClusterProperties.class)).thenReturn(new ClusterProperties());
        when(SpringBeanUtils.getInstance().getBean(SyncDataService.class)).thenReturn(syncDataService);
        RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        for (String id : new String[]{"", "not-a-uuid", "00000000-0000-0000-0000-00000000000z"}) {
            assertDoesNotThrow(() -> websocketCollector.onMessage(WebsocketSyncFrame.REQUEST_PREFIX + id, session));
        }
        verify(syncDataService, never()).syncAllByNamespaceId(any(), anyString());
        verify(async, never()).sendText(anyString(), any(SendHandler.class));
        assertNull(ThreadLocalUtils.get("sessionKey"));
        when(syncDataService.syncAllByNamespaceId(DataEventTypeEnum.MYSELF, Constants.SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        websocketCollector.onMessage(WebsocketSyncFrame.REQUEST_PREFIX + UUID.randomUUID(), session);
        verify(async).sendText(anyString(), any(SendHandler.class));
    }

    @Test
    void testInitialSyncFramesAndEmptyCompletion() {
        when(SpringBeanUtils.getInstance().getBean(ClusterProperties.class)).thenReturn(new ClusterProperties());
        when(SpringBeanUtils.getInstance().getBean(SyncDataService.class)).thenReturn(syncDataService);
        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        websocketCollector.onOpen(session);
        String id = UUID.randomUUID().toString();
        when(syncDataService.syncAllByNamespaceId(DataEventTypeEnum.MYSELF, Constants.SYS_DEFAULT_NAMESPACE_ID))
                .thenAnswer(invocation -> {
                    WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "configuration", DataEventTypeEnum.MYSELF);
                    return true;
                });
        websocketCollector.onMessage(WebsocketSyncFrame.REQUEST_PREFIX + id, session);
        ArgumentCaptor<String> messages = ArgumentCaptor.forClass(String.class);
        verify(async, times(2)).sendText(messages.capture(), any(SendHandler.class));
        WebsocketSyncFrame data = GsonUtils.getInstance().fromJson(messages.getAllValues().get(0), WebsocketSyncFrame.class);
        final WebsocketSyncFrame end = GsonUtils.getInstance().fromJson(messages.getAllValues().get(1), WebsocketSyncFrame.class);
        assertEquals(id, data.getRequestId());
        assertEquals(0, data.getSequence());
        assertEquals("configuration", data.getPayload());
        assertEquals(id, end.getRequestId());
        assertEquals(1, end.getSequence());
        assertNull(end.getPayload());
        when(syncDataService.syncAllByNamespaceId(DataEventTypeEnum.MYSELF, Constants.SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        websocketCollector.onMessage(WebsocketSyncFrame.REQUEST_PREFIX + UUID.randomUUID(), session);
        verify(async, times(3)).sendText(messages.capture(), any(SendHandler.class));
        WebsocketSyncFrame empty = GsonUtils.getInstance().fromJson(messages.getValue(), WebsocketSyncFrame.class);
        assertEquals(0, empty.getSequence());
        assertNull(empty.getPayload());
        websocketCollector.onClose(session);
    }

    @Test
    void testFailedInitialSyncDoesNotSendCompletion() {
        when(SpringBeanUtils.getInstance().getBean(ClusterProperties.class)).thenReturn(new ClusterProperties());
        when(SpringBeanUtils.getInstance().getBean(SyncDataService.class)).thenReturn(syncDataService);
        RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        when(syncDataService.syncAllByNamespaceId(DataEventTypeEnum.MYSELF, Constants.SYS_DEFAULT_NAMESPACE_ID))
                .thenThrow(new IllegalStateException("snapshot unavailable"));
        assertThrows(IllegalStateException.class,
                () -> websocketCollector.onMessage(WebsocketSyncFrame.REQUEST_PREFIX + UUID.randomUUID(), session));
        verify(async, never()).sendText(anyString(), any(SendHandler.class));
        assertNull(ThreadLocalUtils.get("sessionKey"));
    }

    @Test
    void testStructuredInitialSyncEmbedsConfigurationAsJsonValue() {
        when(SpringBeanUtils.getInstance().getBean(ClusterProperties.class)).thenReturn(new ClusterProperties());
        when(SpringBeanUtils.getInstance().getBean(SyncDataService.class)).thenReturn(syncDataService);
        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        websocketCollector.onOpen(session);
        websocketCollector.onMessage(WebsocketSyncFrame.STRUCTURED_SYNC_CAPABILITY, session);
        String id = UUID.randomUUID().toString();
        when(syncDataService.syncAllByNamespaceId(DataEventTypeEnum.MYSELF, Constants.SYS_DEFAULT_NAMESPACE_ID))
                .thenAnswer(invocation -> {
                    WebsocketData<PluginData> data = new WebsocketData<>("PLUGIN", "MYSELF",
                            Collections.singletonList(new PluginData()));
                    data.setFullSnapshot(true);
                    WebsocketCollector.sendConfiguration(Constants.SYS_DEFAULT_NAMESPACE_ID, data, DataEventTypeEnum.MYSELF);
                    return true;
                });
        websocketCollector.onMessage(WebsocketSyncFrame.REQUEST_PREFIX + id, session);

        ArgumentCaptor<String> messages = ArgumentCaptor.forClass(String.class);
        verify(async, times(2)).sendText(messages.capture(), any(SendHandler.class));
        String dataWire = messages.getAllValues().get(0);
        assertTrue(dataWire.contains("\"payload\":{\"groupType\":\"PLUGIN\""));
        assertFalse(dataWire.contains("\\\"groupType\\\""));
        WebsocketSyncFrameV2 data = GsonUtils.getInstance().fromJson(dataWire, WebsocketSyncFrameV2.class);
        assertEquals(id, data.getRequestId());
        assertEquals(0, data.getSequence());
        assertEquals("PLUGIN", data.getPayload().getAsJsonObject().get("groupType").getAsString());
        assertTrue(data.getPayload().getAsJsonObject().get("data").isJsonArray());

        WebsocketSyncFrameV2 end = GsonUtils.getInstance().fromJson(messages.getAllValues().get(1), WebsocketSyncFrameV2.class);
        assertEquals(id, end.getRequestId());
        assertEquals(1, end.getSequence());
        assertNull(end.getPayload());
        websocketCollector.onClose(session);
    }

    @Test
    void testStructuredCapabilityIsDroppedWhenSessionCloses() {
        websocketCollector.onOpen(session);
        websocketCollector.onMessage(WebsocketSyncFrame.STRUCTURED_SYNC_CAPABILITY, session);
        Set<Session> structured = (Set<Session>) ReflectionTestUtils.getField(
                WebsocketCollector.class, "STRUCTURED_SYNC_SESSIONS");
        assertTrue(structured.contains(session));
        websocketCollector.onClose(session);
        assertFalse(structured.contains(session));
    }

    @Test
    void testInitialSyncWithoutCapabilityKeepsStringPayload() {
        when(SpringBeanUtils.getInstance().getBean(ClusterProperties.class)).thenReturn(new ClusterProperties());
        when(SpringBeanUtils.getInstance().getBean(SyncDataService.class)).thenReturn(syncDataService);
        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        websocketCollector.onOpen(session);
        String id = UUID.randomUUID().toString();
        when(syncDataService.syncAllByNamespaceId(DataEventTypeEnum.MYSELF, Constants.SYS_DEFAULT_NAMESPACE_ID))
                .thenAnswer(invocation -> {
                    WebsocketCollector.sendConfiguration(Constants.SYS_DEFAULT_NAMESPACE_ID,
                            new WebsocketData<>("PLUGIN", "MYSELF", Collections.emptyList()), DataEventTypeEnum.MYSELF);
                    return true;
                });
        websocketCollector.onMessage(WebsocketSyncFrame.REQUEST_PREFIX + id, session);

        ArgumentCaptor<String> messages = ArgumentCaptor.forClass(String.class);
        verify(async, times(2)).sendText(messages.capture(), any(SendHandler.class));
        assertTrue(messages.getAllValues().get(0).contains("\"eventType\":\"INITIAL_SYNC_V1\""));
        WebsocketSyncFrame data = GsonUtils.getInstance().fromJson(messages.getAllValues().get(0), WebsocketSyncFrame.class);
        assertEquals("{\"groupType\":\"PLUGIN\",\"eventType\":\"MYSELF\",\"data\":[],\"fullSnapshot\":false}",
                data.getPayload());
        websocketCollector.onClose(session);
    }

    @Test
    void testFollowerCannotCompleteInitialSync() {
        ClusterProperties properties = new ClusterProperties();
        properties.setEnabled(true);
        when(SpringBeanUtils.getInstance().getBean(ClusterProperties.class)).thenReturn(properties);
        ClusterSelectMasterService master = mock(ClusterSelectMasterService.class);
        when(SpringBeanUtils.getInstance().getBean(ClusterSelectMasterService.class)).thenReturn(master);
        RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        websocketCollector.onMessage(WebsocketSyncFrame.REQUEST_PREFIX + UUID.randomUUID(), session);
        verify(syncDataService, never()).syncAllByNamespaceId(any(), anyString());
        verify(async, never()).sendText(anyString(), any(SendHandler.class));
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

        verify(publisher, times(1)).publish(isA(InstanceInfoReportEvent.class));
        websocketCollector.onClose(session);
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"cpu\":4,\"os\":\"Windows\"}", "{\"note\":\"{\\\"nested\\\":true}\"}"})
    void testBootstrapInstanceInfoPreservesJsonString(final String information) {
        InstanceInfoReportEventPublisher publisher = mock(InstanceInfoReportEventPublisher.class);
        when(SpringBeanUtils.getInstance().getBean(InstanceInfoReportEventPublisher.class)).thenReturn(publisher);
        String message = GsonUtils.getInstance().toJson(Map.of(InstanceTypeConstants.BOOTSTRAP_INSTANCE_INFO, information));

        websocketCollector.onMessage(message, session);

        ArgumentCaptor<InstanceInfoReportEvent> event = ArgumentCaptor.forClass(InstanceInfoReportEvent.class);
        verify(publisher).publish(event.capture());
        assertEquals(information, event.getValue().getInstanceInfo());
    }

    @Test
    @SuppressWarnings("unchecked")
    void testBootstrapObjectInformationIsPreserved() {
        InstanceInfoReportEventPublisher publisher = mock(InstanceInfoReportEventPublisher.class);
        when(SpringBeanUtils.getInstance().getBean(InstanceInfoReportEventPublisher.class)).thenReturn(publisher);
        String message = "{\"bootstrapInstanceInfo\":{\"note\":\"{\\\"nested\\\":true}\"}}";

        websocketCollector.onMessage(message, session);

        ArgumentCaptor<InstanceInfoReportEvent> event = ArgumentCaptor.forClass(InstanceInfoReportEvent.class);
        verify(publisher).publish(event.capture());
        Map<String, Object> information = GsonUtils.getInstance().fromJson(event.getValue().getInstanceInfo(), Map.class);
        assertEquals("{\"nested\":true}", information.get("note"));
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"{broken}", "not-json", "[]"})
    void testMalformedOptionalInformationStillPublishesHeartbeat(final String information) {
        InstanceInfoReportEventPublisher publisher = mock(InstanceInfoReportEventPublisher.class);
        when(SpringBeanUtils.getInstance().getBean(InstanceInfoReportEventPublisher.class)).thenReturn(publisher);
        JsonObject message = new JsonObject();
        message.addProperty(InstanceTypeConstants.BOOTSTRAP_INSTANCE_INFO, information);

        assertDoesNotThrow(() -> websocketCollector.onMessage(message.toString(), session));

        verify(publisher).publish(isA(InstanceInfoReportEvent.class));
    }

    @Test
    void testMalformedBootstrapFrameDoesNotPreventNextHeartbeat() {
        InstanceInfoReportEventPublisher publisher = mock(InstanceInfoReportEventPublisher.class);
        when(SpringBeanUtils.getInstance().getBean(InstanceInfoReportEventPublisher.class)).thenReturn(publisher);
        String[] messages = {"{\"bootstrapInstanceInfo\":", "[\"bootstrapInstanceInfo\"]", "\"bootstrapInstanceInfo\"", "null /* bootstrapInstanceInfo */"};
        for (String message : messages) {
            assertDoesNotThrow(() -> websocketCollector.onMessage(message, session));
        }
        verify(publisher, never()).publish(any(InstanceInfoReportEvent.class));

        websocketCollector.onMessage("{\"bootstrapInstanceInfo\":\"{\\\"cpu\\\":4}\"}", session);

        ArgumentCaptor<InstanceInfoReportEvent> event = ArgumentCaptor.forClass(InstanceInfoReportEvent.class);
        verify(publisher).publish(event.capture());
        assertEquals("{\"cpu\":4}", event.getValue().getInstanceInfo());
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
    void testOnCloseRemovesAlreadyClosedSessionFromNamespaceMap() {
        websocketCollector.onOpen(session);
        assertEquals(1, namespaceSessionCount(Constants.SYS_DEFAULT_NAMESPACE_ID));
        when(session.isOpen()).thenReturn(false);
        doNothing().when(loggerSpy).warn(anyString(), anyString());

        websocketCollector.onClose(session);

        assertEquals(0L, getSessionSetSize());
        assertFalse(namespaceMap().containsKey(Constants.SYS_DEFAULT_NAMESPACE_ID));
        assertEquals(0, sendQueueSize());
    }

    @Test
    void testOnErrorRemovesAlreadyClosedSessionFromNamespaceMap() {
        websocketCollector.onOpen(session);
        when(session.isOpen()).thenReturn(false);
        doNothing().when(loggerSpy).error(anyString(), anyString(), isA(Throwable.class));

        websocketCollector.onError(session, new IllegalStateException("closed"));

        assertEquals(0L, getSessionSetSize());
        assertFalse(namespaceMap().containsKey(Constants.SYS_DEFAULT_NAMESPACE_ID));
    }

    @Test
    void testRepeatedCloseOfAlreadyClosedSessionIsIdempotent() {
        websocketCollector.onOpen(session);
        when(session.isOpen()).thenReturn(false);
        doNothing().when(loggerSpy).warn(anyString(), anyString());

        websocketCollector.onClose(session);
        websocketCollector.onClose(session);
        websocketCollector.onError(session, new IllegalStateException("closed again"));

        assertEquals(0L, getSessionSetSize());
        assertTrue(namespaceMap().isEmpty());
    }

    @Test
    void testMissingNamespaceLeavesNoPartialRegistration() {
        Map<String, Object> userProperties = new HashMap<>();
        when(session.getUserProperties()).thenReturn(userProperties);

        assertThrows(ShenyuException.class, () -> websocketCollector.onOpen(session));

        assertEquals(0L, getSessionSetSize());
        assertTrue(namespaceMap().isEmpty());
        assertEquals(0, sendQueueSize());
    }

    @Test
    void testReconnectDoesNotAccumulateClosedSessions() {
        doNothing().when(loggerSpy).warn(anyString(), anyString());
        for (int i = 0; i < 3; i++) {
            Session reconnect = mock(Session.class);
            Map<String, Object> props = new HashMap<>();
            props.put(Constants.SHENYU_NAMESPACE_ID, Constants.SYS_DEFAULT_NAMESPACE_ID);
            when(reconnect.isOpen()).thenReturn(true);
            when(reconnect.getUserProperties()).thenReturn(props);
            websocketCollector.onOpen(reconnect);
            when(reconnect.isOpen()).thenReturn(false);
            websocketCollector.onClose(reconnect);
        }

        assertEquals(0L, getSessionSetSize());
        assertTrue(namespaceMap().isEmpty());
    }

    @Test
    void testNamespaceBroadcastSkipsClosedSession() {
        RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        when(session.getAsyncRemote()).thenReturn(async);
        websocketCollector.onOpen(session);
        when(session.isOpen()).thenReturn(false);

        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "stale-msg", DataEventTypeEnum.CREATE);

        verify(async, never()).sendText(anyString(), any(SendHandler.class));
        assertFalse(namespaceMap().containsKey(Constants.SYS_DEFAULT_NAMESPACE_ID));
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
    void testSendWithNamespaceIdLogsBoundedMetadataNotThePayload() {
        final RemoteEndpoint.Async async = mockSuccessfulAsyncRemote(session);
        websocketCollector.onOpen(session);
        // a realistic envelope carrying a secret; the collector must log bounded metadata, never the payload
        final String message = "{\"groupType\":\"AI_PROXY_API_KEY\",\"eventType\":\"UPDATE\",\"data\":[{\"apiKey\":\"api-secret\"}]}";
        // Attach to the root logger: the collector's events reach it through additivity whether the collector's
        // static LOG is the real logger or the @BeforeAll spy, whose copied parent chain still points at the root.
        final ch.qos.logback.classic.Logger root =
                (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME);
        final ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        final Level original = root.getLevel();
        root.setLevel(Level.INFO);
        root.addAppender(appender);
        try {
            WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, message, DataEventTypeEnum.CREATE);
        } finally {
            root.detachAppender(appender);
            root.setLevel(original);
        }
        // delivery is preserved even though the diagnostic no longer serializes the payload
        verify(async, times(1)).sendText(eq(message), any(SendHandler.class));
        // exactly one bounded INFO diagnostic is emitted, and no DEBUG redaction/serialization runs at INFO
        final String collectorName = WebsocketCollector.class.getName();
        assertEquals(1L, appender.list.stream()
                .filter(event -> collectorName.equals(event.getLoggerName()))
                .count(), "expected exactly one bounded collector diagnostic");
        final String logged = appender.list.stream()
                .filter(event -> collectorName.equals(event.getLoggerName()))
                .map(ILoggingEvent::getFormattedMessage)
                .findFirst()
                .orElse("");
        assertTrue(logged.contains(Constants.SYS_DEFAULT_NAMESPACE_ID), logged);
        assertTrue(logged.contains(DataEventTypeEnum.CREATE.name()), logged);
        assertTrue(logged.contains(String.valueOf(message.length())), logged);
        // neither the payload shape, nor the secret, nor the redaction marker may be serialized into the log
        assertFalse(logged.contains("groupType"), logged);
        assertFalse(logged.contains("api-secret"), logged);
        assertFalse(logged.contains("******"), logged);
    }

    @Test
    void testSendBySessionFailure() throws Exception {
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
        // a failed send closes the session so the gateway reconnects and resynchronizes
        verify(session, times(1)).close();
        assertEquals(0, sendQueueSize());

        websocketCollector.onClose(session);
    }

    @Test
    void testCloseCancelsInFlightWatchdog() {
        WebsocketCollector.setSendTimeoutMillis(60000L);
        final RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        when(session.getAsyncRemote()).thenReturn(async);
        websocketCollector.onOpen(session);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "stalled-message", DataEventTypeEnum.CREATE);
        Map<?, ?> queues = (Map<?, ?>) ReflectionTestUtils.getField(WebsocketCollector.class, "SESSION_SEND_QUEUES");
        Object queue = java.util.Objects.requireNonNull(queues).get(session);
        java.util.concurrent.ScheduledFuture<?> future = (java.util.concurrent.ScheduledFuture<?>)
                ReflectionTestUtils.getField(queue, "timeoutFuture");
        org.junit.jupiter.api.Assertions.assertNotNull(future);
        assertFalse(future.isCancelled());
        websocketCollector.onClose(session);
        assertTrue(future.isCancelled());
        assertNull(ReflectionTestUtils.getField(queue, "timeoutFuture"));
        WebsocketCollector.resetSendGuards();
    }

    @Test
    void testMissingSendCallbackTimesOutAndClosesSession() throws Exception {
        WebsocketCollector.setSendTimeoutMillis(150);
        final RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        when(session.getAsyncRemote()).thenReturn(async);
        // container never invokes the send handler — the stalled-connection case
        doAnswer(invocation -> null).when(async).sendText(anyString(), any(SendHandler.class));
        websocketCollector.onOpen(session);

        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "stuck-message", DataEventTypeEnum.CREATE);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "second-message", DataEventTypeEnum.CREATE);
        // only the first message reaches sendText; the second stays queued
        verify(async, times(1)).sendText(eq("stuck-message"), any(SendHandler.class));
        verify(async, never()).sendText(eq("second-message"), any(SendHandler.class));

        // the watchdog closes the broken session within a bounded time
        waitUntil(() -> getSessionSetSize() == 0L);
        verify(session, times(1)).close();
        assertEquals(0, sendQueueSize());
    }

    @Test
    void testSynchronousSendExceptionClosesSession() throws Exception {
        final RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        when(session.getAsyncRemote()).thenReturn(async);
        when(session.isOpen()).thenReturn(true);
        websocketCollector.onOpen(session);

        doAnswer(invocation -> {
            throw new IllegalStateException("broken pipe");
        }).when(async).sendText(anyString(), any(SendHandler.class));
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "boom-message", DataEventTypeEnum.CREATE);

        verify(session, times(1)).close();
        assertEquals(0, sendQueueSize());
    }

    @Test
    void testQueueOverflowClosesSession() throws Exception {
        WebsocketCollector.setMaxQueuedMessages(2);
        final RemoteEndpoint.Async async = mock(RemoteEndpoint.Async.class);
        when(session.getAsyncRemote()).thenReturn(async);
        doAnswer(invocation -> null).when(async).sendText(anyString(), any(SendHandler.class));
        websocketCollector.onOpen(session);

        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "m1", DataEventTypeEnum.CREATE);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "m2", DataEventTypeEnum.CREATE);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "m3", DataEventTypeEnum.CREATE);
        WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, "m4", DataEventTypeEnum.CREATE);

        verify(async, times(1)).sendText(eq("m1"), any(SendHandler.class));
        verify(session, times(1)).close();
        assertEquals(0, sendQueueSize());
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

    @SuppressWarnings("unchecked")
    private Map<String, Set<Session>> namespaceMap() {
        return (Map<String, Set<Session>>) ReflectionTestUtils.getField(WebsocketCollector.class, "NAMESPACE_SESSION_MAP");
    }

    private int namespaceSessionCount(final String namespaceId) {
        Set<Session> sessions = namespaceMap().get(namespaceId);
        return Objects.isNull(sessions) ? 0 : sessions.size();
    }

    private int sendQueueSize() {
        Map<?, ?> queues = (Map<?, ?>) ReflectionTestUtils.getField(WebsocketCollector.class, "SESSION_SEND_QUEUES");
        return Objects.isNull(queues) ? 0 : queues.size();
    }

    private Session getSession() {
        return (Session) ThreadLocalUtils.get("sessionKey");
    }

    private void waitUntil(final java.util.function.Supplier<Boolean> condition) throws InterruptedException {
        long deadline = System.currentTimeMillis() + 3000L;
        while (!condition.get() && System.currentTimeMillis() < deadline) {
            Thread.sleep(20L);
        }
    }
}
