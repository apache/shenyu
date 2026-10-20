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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.converter.DivideSelectorHandleConverter;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverterFactor;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Tests the discovery snapshot published after upstream health changes.
 */
@ExtendWith(MockitoExtension.class)
class UpstreamCheckServiceSyncTest {

    private static final String SELECTOR_ID = "sync-selector";

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private SelectorConditionMapper selectorConditionMapper;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private DiscoveryUpstreamService discoveryUpstreamService;

    private UpstreamCheckService service;

    @BeforeEach
    void setUp() {
        SelectorHandleConverterFactor converters = new SelectorHandleConverterFactor(
                Collections.singletonMap(PluginEnum.DIVIDE.getName(), new DivideSelectorHandleConverter()));
        service = new UpstreamCheckService(selectorMapper, eventPublisher, pluginMapper, selectorConditionMapper,
                new ShenyuRegisterCenterConfig(), converters, discoveryUpstreamService);
        when(selectorMapper.selectById(SELECTOR_ID)).thenReturn(SelectorDO.builder().id(SELECTOR_ID)
                .selectorName("/divide").namespaceId("namespace").pluginId("divide-id").handle("[]").build());
        when(pluginMapper.selectById("divide-id"))
                .thenReturn(PluginDO.builder().id("divide-id").name(PluginEnum.DIVIDE.getName()).build());
    }

    @Test
    void testRecoveredUpstreamPublishesCurrentStatus() {
        DiscoveryUpstreamData recovered = DiscoveryUpstreamData.builder().id("recovered")
                .url("127.0.0.1:8080").protocol("http://").status(1).weight(80).props("{}").build();
        DiscoveryUpstreamData offline = DiscoveryUpstreamData.builder().id("offline").url("127.0.0.1:8081").status(1).build();
        when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(new ArrayList<>(List.of(recovered, offline)));
        DivideUpstream alive = DivideUpstream.builder().upstreamUrl(recovered.getUrl()).protocol("http://").status(true).build();

        ReflectionTestUtils.invokeMethod(service, "updateSelectorHandler", SELECTOR_ID, Collections.singletonList(alive));

        verify(discoveryUpstreamService).changeStatusBySelectorIdAndUrl(SELECTOR_ID, recovered.getUrl(), Boolean.TRUE);
        DiscoverySyncData snapshot = publishedSnapshot();
        assertEquals(1, snapshot.getUpstreamDataList().size());
        DiscoveryUpstreamData upstream = snapshot.getUpstreamDataList().get(0);
        assertEquals(0, upstream.getStatus(), "The published status must match the database update");
        assertEquals("recovered", upstream.getId());
        assertEquals(80, upstream.getWeight());
        assertEquals("{}", upstream.getProps());
    }

    @Test
    void testAllOfflinePublishesEmptySnapshot() {
        DiscoveryUpstreamData offline = DiscoveryUpstreamData.builder().url("127.0.0.1:8080").status(1).build();
        when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(new ArrayList<>(Collections.singletonList(offline)));

        ReflectionTestUtils.invokeMethod(service, "updateSelectorHandler", SELECTOR_ID, Collections.emptyList());

        assertTrue(publishedSnapshot().getUpstreamDataList().isEmpty());
    }

    @Test
    void testUpstreamWithoutDiscoveryRecordPublishesHealthyStatus() {
        DivideUpstream alive = DivideUpstream.builder().upstreamUrl("127.0.0.1:8080").protocol("http://").status(true).build();

        ReflectionTestUtils.invokeMethod(service, "updateSelectorHandler", SELECTOR_ID, Collections.singletonList(alive));

        DiscoverySyncData snapshot = publishedSnapshot();
        assertEquals(1, snapshot.getUpstreamDataList().size());
        assertEquals(alive.getUpstreamUrl(), snapshot.getUpstreamDataList().get(0).getUrl());
        assertEquals(0, snapshot.getUpstreamDataList().get(0).getStatus());
    }

    private DiscoverySyncData publishedSnapshot() {
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher, times(2)).publishEvent(captor.capture());
        DataChangedEvent event = captor.getAllValues().stream()
                .filter(value -> value.getGroupKey() == ConfigGroupEnum.DISCOVER_UPSTREAM).findFirst().orElseThrow();
        DiscoverySyncData snapshot = GsonUtils.getInstance().fromJson(GsonUtils.getInstance().toJson(event.getSource().get(0)), DiscoverySyncData.class);
        assertEquals(SELECTOR_ID, snapshot.getSelectorId());
        assertEquals("/divide", snapshot.getSelectorName());
        assertEquals("namespace", snapshot.getNamespaceId());
        assertEquals(PluginEnum.DIVIDE.getName(), snapshot.getPluginName());
        return snapshot;
    }
}
