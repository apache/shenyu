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

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterDataChangedEventPayload;
import org.apache.shenyu.common.dto.PluginData;
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
        controller = new ClusterDataChangedEventController(eventPublisher, clusterSelectMasterService);
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

        ResponseEntity<?> response = controller.receive(payload);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher, times(1)).publishEvent(captor.capture());
        assertEquals(ConfigGroupEnum.PLUGIN, captor.getValue().getGroupKey());
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
}
