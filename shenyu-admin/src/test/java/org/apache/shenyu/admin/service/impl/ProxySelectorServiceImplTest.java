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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.exception.ValidFailException;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.model.dto.ProxySelectorAddDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ProxySelectorServiceImpl} namespace validation on create.
 */
@ExtendWith(MockitoExtension.class)
public final class ProxySelectorServiceImplTest {

    @Mock
    private ProxySelectorMapper proxySelectorMapper;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private DiscoveryRelMapper discoveryRelMapper;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private DiscoveryProcessorHolder discoveryProcessorHolder;

    @Mock
    private DiscoveryProcessor discoveryProcessor;

    @InjectMocks
    private ProxySelectorServiceImpl proxySelectorService;

    @Test
    public void createRejectsDiscoveryFromAnotherNamespace() {
        final ProxySelectorAddDTO dto = buildAddDto("namespace-a", "discovery-1");
        DiscoveryDO foreignDiscovery = new DiscoveryDO();
        foreignDiscovery.setId("discovery-1");
        foreignDiscovery.setNamespaceId("namespace-b");
        when(proxySelectorMapper.insert(any())).thenReturn(1);
        when(discoveryMapper.selectById("discovery-1")).thenReturn(foreignDiscovery);

        assertThrows(ValidFailException.class, () -> proxySelectorService.create(dto));
    }

    @Test
    public void createRejectsUnknownDiscoveryId() {
        final ProxySelectorAddDTO dto = buildAddDto("namespace-a", "missing-discovery");
        when(proxySelectorMapper.insert(any())).thenReturn(1);
        when(discoveryMapper.selectById("missing-discovery")).thenReturn(null);

        assertThrows(ValidFailException.class, () -> proxySelectorService.create(dto));
    }

    @Test
    public void createAcceptsDiscoveryFromSameNamespace() {
        final ProxySelectorAddDTO dto = buildAddDto("namespace-a", "discovery-1");
        DiscoveryDO ownDiscovery = new DiscoveryDO();
        ownDiscovery.setId("discovery-1");
        ownDiscovery.setNamespaceId("namespace-a");
        when(proxySelectorMapper.insert(any())).thenReturn(1);
        when(discoveryMapper.selectById("discovery-1")).thenReturn(ownDiscovery);
        lenient().when(discoveryProcessorHolder.chooseProcessor(anyString())).thenReturn(discoveryProcessor);
        lenient().when(discoveryHandlerMapper.insertSelective(any())).thenReturn(1);
        lenient().when(discoveryRelMapper.insertSelective(any())).thenReturn(1);

        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, proxySelectorService.create(dto));
    }

    private ProxySelectorAddDTO buildAddDto(final String namespaceId, final String discoveryId) {
        ProxySelectorAddDTO dto = new ProxySelectorAddDTO();
        dto.setName("test-proxy-selector");
        dto.setPluginName("tcp");
        dto.setNamespaceId(namespaceId);
        dto.setForwardPort(9295);
        dto.setListenerNode("/shenyu/discovery");
        dto.setProps("{}");
        dto.setHandler("");
        ProxySelectorAddDTO.Discovery discovery = new ProxySelectorAddDTO.Discovery();
        discovery.setId(discoveryId);
        discovery.setDiscoveryType("zookeeper");
        dto.setDiscovery(discovery);
        return dto;
    }
}
