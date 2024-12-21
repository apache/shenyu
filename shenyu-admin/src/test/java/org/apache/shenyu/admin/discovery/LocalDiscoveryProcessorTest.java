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

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class LocalDiscoveryProcessorTest {

    @InjectMocks
    private LocalDiscoveryProcessor localDiscoveryProcessor;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @BeforeEach
    public void setup() {
        localDiscoveryProcessor = new LocalDiscoveryProcessor(discoveryUpstreamMapper);
        localDiscoveryProcessor.setApplicationEventPublisher(eventPublisher);
    }

    @Test
    public void testCreateProxySelector() {
        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        localDiscoveryProcessor.createProxySelector(new DiscoveryHandlerDTO(), new ProxySelectorDTO());
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @Test
    public void testRemoveProxySelector() {
        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        localDiscoveryProcessor.removeProxySelector(new DiscoveryHandlerDTO(), new ProxySelectorDTO());
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @Test
    public void testChangeUpstream() {
        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        localDiscoveryProcessor.changeUpstream(new ProxySelectorDTO(), new ArrayList<>());
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @Test
    public void testRemoveSelectorUpstream() {
        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        localDiscoveryProcessor.removeSelectorUpstream(new ProxySelectorDTO());
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @Test
    public void testFetchAll() {
        List<DiscoveryUpstreamDO> discoveryUpstreamDOS = new ArrayList<>();
        DiscoveryHandlerDTO discoveryHandlerDTO = new DiscoveryHandlerDTO();
        ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId(anyString())).thenReturn(discoveryUpstreamDOS);
        localDiscoveryProcessor.fetchAll(discoveryHandlerDTO, proxySelectorDTO);
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

}
