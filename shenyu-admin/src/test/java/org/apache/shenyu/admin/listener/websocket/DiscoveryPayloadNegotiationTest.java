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

import com.google.gson.JsonObject;
import jakarta.websocket.RemoteEndpoint;
import jakarta.websocket.SendHandler;
import jakarta.websocket.SendResult;
import jakarta.websocket.Session;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ThreadLocalUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.WebsocketSyncFrame;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Mixed gateway connections must receive only their supported Discovery representation.
 */
class DiscoveryPayloadNegotiationTest {

    private final WebsocketCollector collector = new WebsocketCollector();

    private final List<Session> sessions = new ArrayList<>();

    private ApplicationContext previousContext;

    @BeforeEach
    void setUp() {
        previousContext = (ApplicationContext) ReflectionTestUtils.getField(SpringBeanUtils.getInstance(), "applicationContext");
    }

    @AfterEach
    void tearDown() {
        ThreadLocalUtils.clear();
        sessions.forEach(collector::onClose);
        SpringBeanUtils.getInstance().setApplicationContext(previousContext);
    }

    @ParameterizedTest
    @EnumSource(value = DataEventTypeEnum.class, names = {"CREATE", "UPDATE", "DELETE", "REFRESH"})
    void mixedFleetRetainsLegacyFieldsAndNamespaceIsolation(final DataEventTypeEnum eventType) {
        List<String> legacy = new ArrayList<>();
        List<String> compact = new ArrayList<>();
        List<String> otherNamespace = new ArrayList<>();
        register("namespace", false, legacy);
        register("namespace", true, compact);
        register("other-namespace", true, otherNamespace);
        DiscoverySyncData data = discovery();
        final String original = GsonUtils.getInstance().toJson(data);

        new WebsocketDataChangedListener(true).onDiscoveryUpstreamChanged(List.of(data), eventType);

        assertEquals(1, legacy.size());
        assertEquals(1, compact.size());
        assertTrue(otherNamespace.isEmpty());
        assertTrue(payload(legacy).has("discoveryHandlerId"));
        assertFalse(payload(compact).has("discoveryHandlerId"));
        assertEquals(original, GsonUtils.getInstance().toJson(data));
        assertEquals("tcp-route", payload(compact).get("selectorName").getAsString());
        assertEquals("namespace", payload(compact).get("namespaceId").getAsString());
        assertTrue(payload(compact).getAsJsonArray("upstreamDataList").isEmpty());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void initialSnapshotUsesOnlyTheRequestingSessionsCapability(final boolean capable) {
        List<String> target = new ArrayList<>();
        List<String> sibling = new ArrayList<>();
        Session session = register("namespace", capable, target);
        register("namespace", !capable, sibling);
        ThreadLocalUtils.put("sessionKey", session);

        new WebsocketDataChangedListener(true).onDiscoveryUpstreamChanged(List.of(discovery()), DataEventTypeEnum.MYSELF);

        assertEquals(1, target.size());
        assertTrue(sibling.isEmpty());
        assertEquals(!capable, payload(target).has("discoveryHandlerId"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void disabledAdminAlwaysUsesLegacyPayload(final boolean capable) {
        List<String> messages = new ArrayList<>();
        register("namespace", capable, messages);
        new WebsocketDataChangedListener(false).onDiscoveryUpstreamChanged(List.of(discovery()), DataEventTypeEnum.UPDATE);
        assertEquals(1, messages.size());
        assertTrue(payload(messages).has("discoveryHandlerId"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void serializesOnlyTheUsedVariantOnce(final boolean capable) {
        List<String> messages = new ArrayList<>();
        register("namespace", capable, messages);
        register("namespace", capable, messages);
        AtomicInteger legacy = new AtomicInteger();
        AtomicInteger compact = new AtomicInteger();

        WebsocketCollector.sendDiscovery("namespace", () -> "legacy-" + legacy.incrementAndGet(),
                () -> "compact-" + compact.incrementAndGet(), DataEventTypeEnum.UPDATE);

        assertEquals(capable ? 0 : 1, legacy.get());
        assertEquals(capable ? 1 : 0, compact.get());
        assertEquals(List.of(capable ? "compact-1" : "legacy-1", capable ? "compact-1" : "legacy-1"), messages);
    }

    @Test
    void doesNotSerializeWithoutRecipients() {
        AtomicInteger serializations = new AtomicInteger();
        WebsocketCollector.sendDiscovery("unused-namespace", () -> "legacy-" + serializations.incrementAndGet(),
                () -> "compact-" + serializations.incrementAndGet(), DataEventTypeEnum.UPDATE);
        assertEquals(0, serializations.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void negotiatedSnapshotPrecedesItsCompletionAndClearsFrameState(final boolean capable) {
        ApplicationContext context = mock(ApplicationContext.class);
        SyncDataService sync = mock(SyncDataService.class);
        when(context.getBean(ClusterProperties.class)).thenReturn(new ClusterProperties());
        when(context.getBean(SyncDataService.class)).thenReturn(sync);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        List<String> messages = new ArrayList<>();
        List<String> sibling = new ArrayList<>();
        Session session = register("namespace", capable, messages);
        register("namespace", !capable, sibling);
        WebsocketDataChangedListener listener = new WebsocketDataChangedListener(true);
        when(sync.syncAllByNamespaceId(DataEventTypeEnum.MYSELF, "namespace")).thenAnswer(invocation -> {
            listener.onDiscoveryUpstreamChanged(List.of(discovery()), DataEventTypeEnum.MYSELF);
            return true;
        });
        String requestId = UUID.randomUUID().toString();

        collector.onMessage(WebsocketSyncFrame.REQUEST_PREFIX + requestId, session);

        assertEquals(2, messages.size());
        assertTrue(sibling.isEmpty());
        WebsocketSyncFrame data = GsonUtils.getInstance().fromJson(messages.get(0), WebsocketSyncFrame.class);
        assertEquals(requestId, data.getRequestId());
        assertEquals(0, data.getSequence());
        assertEquals(!capable, payload(List.of(data.getPayload())).has("discoveryHandlerId"));
        WebsocketSyncFrame end = GsonUtils.getInstance().fromJson(messages.get(1), WebsocketSyncFrame.class);
        assertEquals(requestId, end.getRequestId());
        assertEquals(1, end.getSequence());
        assertNull(end.getPayload());
        assertNull(ThreadLocalUtils.get("sessionKey"));
        listener.onDiscoveryUpstreamChanged(List.of(discovery()), DataEventTypeEnum.UPDATE);
        assertEquals(3, messages.size());
        JsonObject update = GsonUtils.getInstance().fromJson(messages.get(2), JsonObject.class);
        assertEquals("DISCOVER_UPSTREAM", update.get("groupType").getAsString());
        assertFalse(update.has("requestId"));
    }

    @Test
    void initialSnapshotCannotTargetAnotherNamespace() {
        List<String> messages = new ArrayList<>();
        Session other = register("other-namespace", true, messages);
        ThreadLocalUtils.put("sessionKey", other);
        new WebsocketDataChangedListener(true).onDiscoveryUpstreamChanged(List.of(discovery()), DataEventTypeEnum.MYSELF);
        assertTrue(messages.isEmpty());
    }

    private DiscoverySyncData discovery() {
        DiscoverySyncData data = new DiscoverySyncData();
        data.setSelectorId("selector");
        data.setSelectorName("tcp-route");
        data.setPluginName("divide");
        data.setNamespaceId("namespace");
        data.setDiscoveryHandlerId("admin-binding");
        data.setUpstreamDataList(new ArrayList<DiscoveryUpstreamData>());
        return data;
    }

    private JsonObject payload(final List<String> messages) {
        return GsonUtils.getInstance().fromJson(messages.get(0), JsonObject.class).getAsJsonArray("data").get(0).getAsJsonObject();
    }

    private Session register(final String namespace, final boolean capable, final List<String> messages) {
        Session session = mock(Session.class);
        RemoteEndpoint.Async remote = mock(RemoteEndpoint.Async.class);
        when(session.isOpen()).thenReturn(true);
        when(session.getAsyncRemote()).thenReturn(remote);
        Map<String, Object> properties = new HashMap<>();
        properties.put(Constants.SHENYU_NAMESPACE_ID, namespace);
        properties.put("X-Shenyu-Discovery-Payload", capable);
        when(session.getUserProperties()).thenReturn(properties);
        doAnswer(invocation -> {
            messages.add(invocation.getArgument(0));
            invocation.<SendHandler>getArgument(1).onResult(new SendResult());
            return null;
        }).when(remote).sendText(anyString(), any(SendHandler.class));
        sessions.add(session);
        collector.onOpen(session);
        return session;
    }
}
