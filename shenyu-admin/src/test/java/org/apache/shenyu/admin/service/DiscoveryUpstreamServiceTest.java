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

import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.impl.DiscoveryUpstreamServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DiscoveryUpstreamServiceTest {

    @InjectMocks
    private DiscoveryUpstreamServiceImpl discoveryUpstreamService;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private ProxySelectorMapper proxySelectorMapper;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private DiscoveryProcessorHolder discoveryProcessorHolder;

    @Mock
    private DiscoveryProcessor discoveryProcessor;

    @Mock
    private DiscoveryRelMapper discoveryRelMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @BeforeEach
    void setUp() {
        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setDiscoveryId("1");
        discoveryHandlerDO.setId("1");
        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setType("zookeeper");
        when(discoveryHandlerMapper.selectById(anyString())).thenReturn(discoveryHandlerDO);
        when(discoveryMapper.selectById(anyString())).thenReturn(discoveryDO);
        when(discoveryProcessorHolder.chooseProcessor(anyString())).thenReturn(discoveryProcessor);
        when(proxySelectorMapper.selectByHandlerId(anyString())).thenReturn(new ProxySelectorDO());
        discoveryUpstreamService = new DiscoveryUpstreamServiceImpl(discoveryUpstreamMapper, discoveryHandlerMapper, proxySelectorMapper, discoveryMapper,
                discoveryRelMapper, selectorMapper, discoveryProcessorHolder);
    }

    @Test
    void createOrUpdate() {

        DiscoveryUpstreamDTO discoveryUpstreamDTO = new DiscoveryUpstreamDTO();
        discoveryUpstreamDTO.setDiscoveryHandlerId("1");
        discoveryUpstreamDTO.setProps("test");
        discoveryUpstreamDTO.setProtocol("test");
        discoveryUpstreamDTO.setUrl("test");
        discoveryUpstreamDTO.setWeight(1);
        discoveryUpstreamDTO.setStatus(1);
        given(discoveryUpstreamMapper.insert(DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO)))
                .willReturn(1);
        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, discoveryUpstreamService.createOrUpdate(discoveryUpstreamDTO));
        discoveryUpstreamDTO.setId("1");
        given(discoveryUpstreamMapper.update(DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO)))
                .willReturn(1);
        assertEquals(ShenyuResultMessage.UPDATE_SUCCESS, discoveryUpstreamService.createOrUpdate(discoveryUpstreamDTO));

    }

    @Test
    void delete() {

        given(discoveryUpstreamMapper.deleteByIds(Collections.singletonList("1"))).willReturn(1);
        assertEquals(ShenyuResultMessage.DELETE_SUCCESS, discoveryUpstreamService.delete(Collections.singletonList("1")));
    }

    @Test
    void nativeCreateOrUpdate() {
        DiscoveryUpstreamDTO discoveryUpstreamDTO = new DiscoveryUpstreamDTO();
        discoveryUpstreamDTO.setDiscoveryHandlerId("1");
        discoveryUpstreamDTO.setProps("test");
        discoveryUpstreamDTO.setProtocol("test");
        discoveryUpstreamDTO.setUrl("test");
        discoveryUpstreamDTO.setWeight(1);
        discoveryUpstreamDTO.setStatus(1);
        discoveryUpstreamService.nativeCreateOrUpdate(discoveryUpstreamDTO);
        discoveryUpstreamDTO.setId("1");
        discoveryUpstreamService.nativeCreateOrUpdate(discoveryUpstreamDTO);
        verify(discoveryUpstreamMapper).insert(any(DiscoveryUpstreamDO.class));
        verify(discoveryUpstreamMapper).updateSelective(any(DiscoveryUpstreamDO.class));
    }

    @Test
    void listAll() {
        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setDiscoveryId("1");
        discoveryHandlerDO.setId("1");

        List<DiscoveryHandlerDO> discoveryHandlerDOS = new ArrayList<>();
        discoveryHandlerDOS.add(discoveryHandlerDO);

        DiscoveryRelDO discoveryRelDO = new DiscoveryRelDO();
        discoveryRelDO.setSelectorId("11");

        SelectorDO selectorDO = new SelectorDO();
        selectorDO.setName("name");

        when(selectorMapper.selectById(anyString())).thenReturn(selectorDO);
        when(discoveryHandlerMapper.selectAll()).thenReturn(discoveryHandlerDOS);
        when(discoveryRelMapper.selectByDiscoveryHandlerId(anyString())).thenReturn(discoveryRelDO);

        List<DiscoverySyncData> discoverySyncData = discoveryUpstreamService.listAll();
        assertThat(discoverySyncData.size(), greaterThan(0));

        DiscoveryRelDO discoveryRelDO2 = new DiscoveryRelDO();
        discoveryRelDO2.setSelectorId("");
        discoveryRelDO2.setProxySelectorId("222");

        ProxySelectorDO proxySelectorDO = new ProxySelectorDO();
        proxySelectorDO.setName("name");
        proxySelectorDO.setId("111");

        when(discoveryRelMapper.selectByDiscoveryHandlerId(anyString())).thenReturn(discoveryRelDO2);
        when(proxySelectorMapper.selectById(anyString())).thenReturn(proxySelectorDO);

        List<DiscoverySyncData> discoverySyncData2 = discoveryUpstreamService.listAll();
        assertThat(discoverySyncData2.size(), greaterThan(0));
    }

    @Test
    void findBySelectorId() {
        List<DiscoveryUpstreamDO> discoveryUpstreamDOS = new ArrayList<>();
        discoveryUpstreamDOS.add(new DiscoveryUpstreamDO());

        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setId("1");

        when(discoveryHandlerMapper.selectBySelectorId(anyString())).thenReturn(discoveryHandlerDO);
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId(anyString())).thenReturn(discoveryUpstreamDOS);
        List<DiscoveryUpstreamData> bySelectorId = discoveryUpstreamService.findBySelectorId("111");
        assertThat(bySelectorId.size(), greaterThan(0));
    }

    @Test
    void deleteBySelectorIdAndUrl() {
        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setId("1");
        when(discoveryHandlerMapper.selectBySelectorId(anyString())).thenReturn(discoveryHandlerDO);
        discoveryUpstreamService.deleteBySelectorIdAndUrl("11", "2");
        verify(discoveryUpstreamMapper).deleteByUrl(any(String.class), any(String.class));
    }
}
