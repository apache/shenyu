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
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;

import java.util.ArrayList;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
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

    @Test
    public void testOnChange() {
        final List<DiscoveryUpstreamData> discoveryUpstreamDTOS = new ArrayList<>();
        DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
        discoveryUpstreamData.setProtocol("http");
        discoveryUpstreamData.setUrl("1111");
        discoveryUpstreamData.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        discoveryUpstreamData.setDiscoveryHandlerId("discoveryHandlerId");
        discoveryUpstreamDTOS.add(discoveryUpstreamData);
        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        when(keyValueParser.parseValue(anyString())).thenReturn(discoveryUpstreamDTOS);
        when(contextInfo.getNamespaceId()).thenReturn(SYS_DEFAULT_NAMESPACE_ID);
        when(contextInfo.getDiscoveryHandlerId()).thenReturn("discoveryHandlerId");
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

}
