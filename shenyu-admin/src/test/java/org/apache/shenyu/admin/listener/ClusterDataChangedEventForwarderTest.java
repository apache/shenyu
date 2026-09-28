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

package org.apache.shenyu.admin.listener;

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterDataChangedEventPayload;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ClusterDataChangedEventForwarder}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ClusterDataChangedEventForwarderTest {

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private ClusterProperties clusterProperties;

    @Mock
    private ClusterSelectMasterService clusterSelectMasterService;

    private ClusterDataChangedEventForwarder forwarder;

    @BeforeEach
    public void setUp() {
        forwarder = new ClusterDataChangedEventForwarder(restTemplate, clusterProperties, clusterSelectMasterService);
        when(clusterProperties.getSchema()).thenReturn("http");
    }

    /**
     * Forward posts the serialized event to the master url built from the master dto.
     */
    @Test
    public void forwardPostsPayloadToMasterUrlTest() {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        master.setContextPath("/admin");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok("ok"));

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(new PluginData()));
        boolean forwarded = forwarder.forward(event);

        assertTrue(forwarded);
        ArgumentCaptor<ClusterDataChangedEventPayload> captor = ArgumentCaptor.forClass(ClusterDataChangedEventPayload.class);
        verify(restTemplate).postForEntity(eq("http://10.0.0.2:9095/admin/cluster/data-change-event"),
                captor.capture(), eq(String.class));
        assertEquals(ConfigGroupEnum.PLUGIN.name(), captor.getValue().getGroupKey());
        assertEquals(DataEventTypeEnum.UPDATE.name(), captor.getValue().getEventType());
        assertTrue(captor.getValue().getSource().contains("pluginName") || captor.getValue().getSource().startsWith("["));
    }

    /**
     * Forward without a master available returns false without posting.
     */
    @Test
    public void forwardWithoutMasterReturnsFalseTest() {
        when(clusterSelectMasterService.getMaster()).thenReturn(null);

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(new PluginData()));
        boolean forwarded = forwarder.forward(event);

        assertFalse(forwarded);
        verify(restTemplate, org.mockito.Mockito.never()).postForEntity(any(String.class), any(Object.class), eq(String.class));
    }

    /**
     * Forward rejected by the master (non-2xx) returns false.
     */
    @Test
    public void forwardRejectedByMasterReturnsFalseTest() {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenReturn(ResponseEntity.status(HttpStatus.CONFLICT).body("not master"));

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.UPDATE,
                Collections.singletonList(new org.apache.shenyu.common.dto.RuleData()));
        boolean forwarded = forwarder.forward(event);

        assertFalse(forwarded);
    }

    /**
     * Forward failure (connection error) returns false instead of throwing.
     */
    @Test
    public void forwardConnectionFailureReturnsFalseTest() {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenThrow(new RestClientException("connection refused"));

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.META_DATA, DataEventTypeEnum.UPDATE,
                Collections.singletonList(new org.apache.shenyu.common.dto.MetaData()));
        boolean forwarded = forwarder.forward(event);

        assertFalse(forwarded);
    }

    /**
     * The source list is serialized into the payload as JSON.
     */
    @Test
    public void forwardSerializesSourceAsJsonTest() {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok("ok"));

        PluginData pluginData = new PluginData();
        pluginData.setName("mockPlugin");
        List<PluginData> source = Collections.singletonList(pluginData);
        forwarder.forward(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE, source));

        ArgumentCaptor<ClusterDataChangedEventPayload> captor = ArgumentCaptor.forClass(ClusterDataChangedEventPayload.class);
        verify(restTemplate).postForEntity(any(String.class), captor.capture(), eq(String.class));
        assertTrue(captor.getValue().getSource().contains("mockPlugin"));
    }
}
