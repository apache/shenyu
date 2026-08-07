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

package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.discovery.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.admin.discovery.parse.KeyValueParser;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DiscoveryDataChangedEventSyncListenerTest {

    @InjectMocks
    private DiscoveryDataChangedEventSyncListener discoveryDataChangedEventSyncListener;

    @Mock
    private KeyValueParser keyValueParser;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Mock
    private DiscoverySyncData contextInfo;

    @BeforeEach
    public void setUp() {
        String discoveryHandlerId = "discoveryHandlerId";
        discoveryDataChangedEventSyncListener = new DiscoveryDataChangedEventSyncListener(eventPublisher, discoveryUpstreamMapper, keyValueParser, contextInfo, discoveryHandlerId);
    }

    private void setUpCommonMocks() {
        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        when(contextInfo.getNamespaceId()).thenReturn(SYS_DEFAULT_NAMESPACE_ID);
        when(contextInfo.getDiscoveryHandlerId()).thenReturn("discoveryHandlerId");
        when(contextInfo.getPluginName()).thenReturn("divide");
    }

    private List<DiscoveryUpstreamData> buildUpstreamDataList(final String url) {
        DiscoveryUpstreamData data = new DiscoveryUpstreamData();
        data.setProtocol("http");
        data.setUrl(url);
        data.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        data.setDiscoveryHandlerId("discoveryHandlerId");
        List<DiscoveryUpstreamData> list = new ArrayList<>();
        list.add(data);
        return list;
    }

    @Test
    public void testOnChange() {
        setUpCommonMocks();
        when(keyValueParser.parseValue(anyString())).thenReturn(buildUpstreamDataList("1111"));
        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.ADDED);
        discoveryDataChangedEventSyncListener.onChange(event);
        verify(discoveryUpstreamMapper).insert(any(DiscoveryUpstreamDO.class));
        DiscoveryDataChangedEvent event2 = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.UPDATED);
        discoveryDataChangedEventSyncListener.onChange(event2);
        verify(discoveryUpstreamMapper).updateDiscoveryHandlerIdAndUrl(any(DiscoveryUpstreamDO.class));
        DiscoveryDataChangedEvent event3 = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.DELETED);
        discoveryDataChangedEventSyncListener.onChange(event3);
        verify(discoveryUpstreamMapper).deleteByUrl(anyString(), anyString());
    }

    @Test
    public void testAddedFallbackMatchMigratesOldUrl() {
        setUpCommonMocks();
        String normalizedUrl = "192.168.1.1:80";
        when(keyValueParser.parseValue(anyString())).thenReturn(buildUpstreamDataList("192.168.1.1"));
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl("discoveryHandlerId", normalizedUrl)).thenReturn(null);

        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl("192.168.1.1");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("discoveryHandlerId"))
                .thenReturn(Collections.singletonList(oldRecord));

        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.ADDED);
        discoveryDataChangedEventSyncListener.onChange(event);

        verify(discoveryUpstreamMapper, never()).insert(any(DiscoveryUpstreamDO.class));
        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(captor.capture());
        assertEquals(normalizedUrl, captor.getValue().getUpstreamUrl());
    }

    @Test
    public void testUpdatedFallbackMatchMigratesOldUrl() {
        setUpCommonMocks();
        when(keyValueParser.parseValue(anyString())).thenReturn(buildUpstreamDataList("::1:9090"));
        when(discoveryUpstreamMapper.updateDiscoveryHandlerIdAndUrl(any(DiscoveryUpstreamDO.class))).thenReturn(0);

        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl("::1:9090");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("discoveryHandlerId"))
                .thenReturn(Collections.singletonList(oldRecord));

        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.UPDATED);
        discoveryDataChangedEventSyncListener.onChange(event);

        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(captor.capture());
        assertEquals("[::1]:9090", captor.getValue().getUpstreamUrl());
    }

    @Test
    public void testDeletedFallbackMatchDeletesById() {
        setUpCommonMocks();
        String normalizedUrl = "[2001:db8::1]:80";
        when(keyValueParser.parseValue(anyString())).thenReturn(buildUpstreamDataList("[2001:db8::1]"));
        when(discoveryUpstreamMapper.deleteByUrl("discoveryHandlerId", normalizedUrl)).thenReturn(0);

        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl("[2001:db8::1]");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("discoveryHandlerId"))
                .thenReturn(Collections.singletonList(oldRecord));

        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.DELETED);
        discoveryDataChangedEventSyncListener.onChange(event);

        verify(discoveryUpstreamMapper).deleteByIds(Collections.singletonList("old-id"));
    }

    @Test
    public void testDeletedExactMatchSucceeds() {
        setUpCommonMocks();
        String normalizedUrl = "192.168.1.1:8080";
        when(keyValueParser.parseValue(anyString())).thenReturn(buildUpstreamDataList("192.168.1.1:8080"));
        when(discoveryUpstreamMapper.deleteByUrl("discoveryHandlerId", normalizedUrl)).thenReturn(1);

        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.DELETED);
        discoveryDataChangedEventSyncListener.onChange(event);

        verify(discoveryUpstreamMapper).deleteByUrl("discoveryHandlerId", normalizedUrl);
        verify(discoveryUpstreamMapper, never()).selectByDiscoveryHandlerId(anyString());
        verify(discoveryUpstreamMapper, never()).deleteByIds(anyList());
    }

}
