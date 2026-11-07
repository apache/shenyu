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

package org.apache.shenyu.admin.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterDataChangedEventPayload;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ClusterDataChangedEventController}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ClusterDataChangedEventControllerTest {

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private ClusterSelectMasterService clusterSelectMasterService;

    private ClusterDataChangedEventController controller;

    @BeforeEach
    public void setUp() {
        controller = new ClusterDataChangedEventController(eventPublisher, clusterSelectMasterService,
                new ObjectMapper());
    }

    /**
     * The master accepts a forwarded event and re-publishes it locally with typed source data.
     */
    @Test
    public void receiveOnMasterPublishesEventTest() {
        when(clusterSelectMasterService.isMaster()).thenReturn(true);
        PluginData pluginData = new PluginData();
        pluginData.setName("mockPlugin");
        String sourceJson = GsonUtils.getInstance().toJson(Collections.singletonList(pluginData));
        ClusterDataChangedEventPayload payload =
                new ClusterDataChangedEventPayload(ConfigGroupEnum.PLUGIN.name(), DataEventTypeEnum.UPDATE.name(), sourceJson);
        payload.setNamespaceId("namespace-a");

        ResponseEntity<?> response = controller.receive(payload);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher, times(1)).publishEvent(captor.capture());
        assertEquals(ConfigGroupEnum.PLUGIN, captor.getValue().getGroupKey());
        assertEquals("namespace-a", captor.getValue().getNamespaceId());
        assertEquals(1, captor.getValue().getSource().size());
        assertInstanceOf(PluginData.class, captor.getValue().getSource().get(0));
    }

    /**
     * A non-master node rejects the forwarded event with 409 and does not publish.
     */
    @Test
    public void receiveOnNonMasterRejectsWithConflictTest() {
        when(clusterSelectMasterService.isMaster()).thenReturn(false);
        ClusterDataChangedEventPayload payload =
                new ClusterDataChangedEventPayload(ConfigGroupEnum.PLUGIN.name(), DataEventTypeEnum.UPDATE.name(), "[]");

        ResponseEntity<?> response = controller.receive(payload);

        assertEquals(HttpStatus.CONFLICT, response.getStatusCode());
        verify(eventPublisher, never()).publishEvent(any(DataChangedEvent.class));
    }

    /**
     * An unknown group key is rejected with a debuggable 400 before publishing.
     */
    @Test
    public void receiveWithUnknownGroupKeyReturnsBadRequestTest() {
        when(clusterSelectMasterService.isMaster()).thenReturn(true);
        ClusterDataChangedEventPayload payload =
                new ClusterDataChangedEventPayload("UNKNOWN_GROUP", DataEventTypeEnum.UPDATE.name(), "[]");

        ResponseEntity<?> response = controller.receive(payload);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        verify(eventPublisher, never()).publishEvent(any(DataChangedEvent.class));
    }

    /**
     * All config groups map to a typed source deserialization.
     */
    @Test
    public void receiveMapsEveryConfigGroupTest() {
        when(clusterSelectMasterService.isMaster()).thenReturn(true);
        List<ConfigGroupEnum> groups = List.of(ConfigGroupEnum.values());
        for (ConfigGroupEnum group : groups) {
            ClusterDataChangedEventPayload payload =
                    new ClusterDataChangedEventPayload(group.name(), DataEventTypeEnum.UPDATE.name(), "[]");
            ResponseEntity<?> response = controller.receive(payload);
            assertEquals(HttpStatus.OK, response.getStatusCode());
        }
        verify(eventPublisher, times(groups.size())).publishEvent(any(DataChangedEvent.class));
    }

    /**
     * Every config group's structured source is read back into its typed record class.
     */
    @Test
    public void receiveStructuredSourceMapsEveryConfigGroupTest() {
        when(clusterSelectMasterService.isMaster()).thenReturn(true);
        ObjectMapper mapper = new ObjectMapper();
        java.util.Map<ConfigGroupEnum, Object> samples = new java.util.LinkedHashMap<>();
        PluginData pluginData = new PluginData();
        pluginData.setName("plugin");
        samples.put(ConfigGroupEnum.PLUGIN, pluginData);
        RuleData ruleData = RuleData.builder().name("rule").build();
        samples.put(ConfigGroupEnum.RULE, ruleData);
        SelectorData selectorData = new SelectorData();
        selectorData.setName("selector");
        samples.put(ConfigGroupEnum.SELECTOR, selectorData);
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("app-key");
        samples.put(ConfigGroupEnum.APP_AUTH, appAuthData);
        MetaData metaData = new MetaData();
        metaData.setPath("/app/execute");
        samples.put(ConfigGroupEnum.META_DATA, metaData);
        ProxySelectorData proxySelectorData = new ProxySelectorData();
        proxySelectorData.setName("proxy");
        samples.put(ConfigGroupEnum.PROXY_SELECTOR, proxySelectorData);
        ProxyApiKeyData proxyApiKeyData = ProxyApiKeyData.builder().proxyApiKey("proxy-key").build();
        samples.put(ConfigGroupEnum.AI_PROXY_API_KEY, proxyApiKeyData);
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setSelectorId("selector-id");
        samples.put(ConfigGroupEnum.DISCOVER_UPSTREAM, discoverySyncData);

        for (java.util.Map.Entry<ConfigGroupEnum, Object> entry : samples.entrySet()) {
            ClusterDataChangedEventPayload payload = new ClusterDataChangedEventPayload();
            payload.setGroupKey(entry.getKey().name());
            payload.setEventType(DataEventTypeEnum.UPDATE.name());
            payload.setPayloadVersion(2);
            payload.setStructuredSource(mapper.valueToTree(Collections.singletonList(entry.getValue())));
            assertEquals(HttpStatus.OK, controller.receive(payload).getStatusCode(), entry.getKey().name());
        }

        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher, times(samples.size())).publishEvent(captor.capture());
        List<DataChangedEvent> events = captor.getAllValues();
        assertEquals(samples.size(), events.size());
        for (DataChangedEvent event : events) {
            assertEquals(1, event.getSource().size());
            assertEquals(samples.get(event.getGroupKey()).getClass(), event.getSource().get(0).getClass());
        }
    }

    @Test
    public void testNullAndMalformedFieldsReturnBadRequest() {
        when(clusterSelectMasterService.isMaster()).thenReturn(true);
        assertEquals(HttpStatus.BAD_REQUEST, controller.receive(null).getStatusCode());
        assertEquals(HttpStatus.BAD_REQUEST, controller.receive(new ClusterDataChangedEventPayload()).getStatusCode());
        for (String source : java.util.List.of("null", "[null]", "not-json", "{}")) {
            assertEquals(HttpStatus.BAD_REQUEST,
                    controller.receive(new ClusterDataChangedEventPayload("PLUGIN", "UPDATE", source)).getStatusCode());
        }
        org.mockito.Mockito.verifyNoInteractions(eventPublisher);
    }

    /**
     * A structured source is re-published as typed records without a string round trip.
     */
    @Test
    public void receiveStructuredSourcePublishesTypedRecordsTest() {
        when(clusterSelectMasterService.isMaster()).thenReturn(true);
        PluginData pluginData = new PluginData();
        pluginData.setName("structuredPlugin");
        ClusterDataChangedEventPayload payload = new ClusterDataChangedEventPayload();
        payload.setGroupKey(ConfigGroupEnum.PLUGIN.name());
        payload.setEventType(DataEventTypeEnum.UPDATE.name());
        payload.setPayloadVersion(2);
        payload.setStructuredSource(new ObjectMapper()
                .valueToTree(Collections.singletonList(pluginData)));

        ResponseEntity<?> response = controller.receive(payload);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(captor.capture());
        assertEquals(1, captor.getValue().getSource().size());
        PluginData published = assertInstanceOf(PluginData.class, captor.getValue().getSource().get(0));
        assertEquals("structuredPlugin", published.getName());
    }

    /**
     * A legacy envelope without a version field keeps working for older senders.
     */
    @Test
    public void receiveLegacySourceWithoutVersionPublishesTest() {
        when(clusterSelectMasterService.isMaster()).thenReturn(true);
        PluginData pluginData = new PluginData();
        pluginData.setName("legacyPlugin");
        ClusterDataChangedEventPayload payload = new ClusterDataChangedEventPayload(
                ConfigGroupEnum.PLUGIN.name(), DataEventTypeEnum.UPDATE.name(),
                GsonUtils.getInstance().toJson(Collections.singletonList(pluginData)));

        assertEquals(HttpStatus.OK, controller.receive(payload).getStatusCode());
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(captor.capture());
        PluginData published = assertInstanceOf(PluginData.class, captor.getValue().getSource().get(0));
        assertEquals("legacyPlugin", published.getName());
    }

    /**
     * Exactly one source representation is accepted, so a conflicting payload is rejected.
     */
    @Test
    public void receiveBothSourceRepresentationsReturnsBadRequestTest() {
        when(clusterSelectMasterService.isMaster()).thenReturn(true);
        ClusterDataChangedEventPayload payload =
                new ClusterDataChangedEventPayload(ConfigGroupEnum.PLUGIN.name(), DataEventTypeEnum.UPDATE.name(), "[]");
        payload.setStructuredSource(new ObjectMapper().createArrayNode());

        assertEquals(HttpStatus.BAD_REQUEST, controller.receive(payload).getStatusCode());
        verify(eventPublisher, never()).publishEvent(any(DataChangedEvent.class));
    }

    /**
     * Unknown envelope versions and non-array structured sources are rejected.
     */
    @Test
    public void receiveUnknownVersionOrShapeReturnsBadRequestTest() {
        when(clusterSelectMasterService.isMaster()).thenReturn(true);
        ObjectMapper objectMapper = new ObjectMapper();
        ClusterDataChangedEventPayload unknownVersion = new ClusterDataChangedEventPayload();
        unknownVersion.setGroupKey(ConfigGroupEnum.PLUGIN.name());
        unknownVersion.setEventType(DataEventTypeEnum.UPDATE.name());
        unknownVersion.setPayloadVersion(3);
        unknownVersion.setStructuredSource(objectMapper.createArrayNode());
        assertEquals(HttpStatus.BAD_REQUEST, controller.receive(unknownVersion).getStatusCode());

        ClusterDataChangedEventPayload objectSource = new ClusterDataChangedEventPayload();
        objectSource.setGroupKey(ConfigGroupEnum.PLUGIN.name());
        objectSource.setEventType(DataEventTypeEnum.UPDATE.name());
        objectSource.setPayloadVersion(2);
        objectSource.setStructuredSource(objectMapper.createObjectNode());
        assertEquals(HttpStatus.BAD_REQUEST, controller.receive(objectSource).getStatusCode());

        ClusterDataChangedEventPayload legacyUnknownVersion = new ClusterDataChangedEventPayload(
                ConfigGroupEnum.PLUGIN.name(), DataEventTypeEnum.UPDATE.name(), "[]");
        legacyUnknownVersion.setPayloadVersion(2);
        assertEquals(HttpStatus.BAD_REQUEST, controller.receive(legacyUnknownVersion).getStatusCode());
        verify(eventPublisher, never()).publishEvent(any(DataChangedEvent.class));
    }

}
