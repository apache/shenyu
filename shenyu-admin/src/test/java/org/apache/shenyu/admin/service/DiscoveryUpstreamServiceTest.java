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
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.DiscoveryUpstreamVO;
import org.apache.shenyu.admin.service.impl.DiscoveryUpstreamServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;

/**
 * Test cases for DiscoveryUpstreamService.
 */
@ExtendWith(MockitoExtension.class)
public final class DiscoveryUpstreamServiceTest {

    @InjectMocks
    private DiscoveryUpstreamServiceImpl discoveryUpstreamService;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private DiscoveryRelMapper discoveryRelMapper;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private ProxySelectorMapper proxySelectorMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private DiscoveryProcessorHolder discoveryProcessorHolder;

    @Mock
    private DiscoveryProcessor discoveryProcessor;

    @BeforeEach
    public void setUp() {

        discoveryUpstreamService = new DiscoveryUpstreamServiceImpl(discoveryUpstreamMapper,
                discoveryHandlerMapper,
                proxySelectorMapper,
                discoveryMapper,
                discoveryRelMapper,
                selectorMapper,
                pluginMapper,
                discoveryProcessorHolder
        );
    }

    @Test
    public void testCreateOrUpdate() {
        when(discoveryUpstreamMapper.insert(any())).thenReturn(1);
        when(selectorMapper.selectByDiscoveryHandlerId(any())).thenReturn(buildSelectorDO());
        when(discoveryHandlerMapper.selectById(any())).thenReturn(buildDiscoveryHandlerDO());
        when(pluginMapper.selectById(any())).thenReturn(buildPluginDO());
        when(discoveryMapper.selectById(any())).thenReturn(buildDiscoveryDO());
        when(discoveryProcessorHolder.chooseProcessor(anyString())).thenReturn(discoveryProcessor);
        testCreate();
        testUpdate();
    }

    @Test
    public void testNativeCreateOrUpdate() {
        when(discoveryUpstreamMapper.insert(any())).thenReturn(1);
        testNativeCreate();
        testNativeUpdate();
    }

    @Test
    public void testDelete() {
        when(discoveryUpstreamMapper.deleteByIds(any())).thenReturn(1);
        String delete = discoveryUpstreamService.delete(Collections.singletonList("123"));
        assertEquals(ShenyuResultMessage.DELETE_SUCCESS, delete);
    }

    @Test
    public void testListAll() {
        List<DiscoveryHandlerDO> list = Collections.singletonList(buildDiscoveryHandlerDO());

        when(discoveryHandlerMapper.selectAll()).thenReturn(list);
        when(discoveryRelMapper.selectByDiscoveryHandlerId(any())).thenReturn(buildDiscoveryRelDO());
        when(proxySelectorMapper.selectById(any())).thenReturn(buildProxySelectorDO());
        List<DiscoverySyncData> dataList = discoveryUpstreamService.listAll();
        assertEquals(dataList.size(), list.size());
    }

    @Test
    public void testListAllData() {
        List<DiscoveryUpstreamDO> list = Collections.singletonList(buildDiscoveryUpstreamDO(""));
        when(discoveryUpstreamMapper.selectAll()).thenReturn(list);
        List<DiscoveryUpstreamVO> upstreamVOList = discoveryUpstreamService.listAllData();
        assertEquals(upstreamVOList.size(), list.size());
    }

    @Test
    public void testImportData() {
        List<DiscoveryUpstreamDO> list = Collections.singletonList(buildDiscoveryUpstreamDO("", "123", "url1"));
        when(discoveryUpstreamMapper.selectAll()).thenReturn(list);
        given(this.discoveryUpstreamMapper.insert(any())).willReturn(1);

        final List<DiscoveryUpstreamDTO> upstreamDTOList = Collections.singletonList(buildDiscoveryUpstreamDTO("", "123", "url2"));
        ConfigImportResult configImportResult = this.discoveryUpstreamService.importData(upstreamDTOList);
        assertNotNull(configImportResult);
        Assertions.assertEquals(configImportResult.getSuccessCount(), upstreamDTOList.size());

        final List<DiscoveryUpstreamDTO> upstreamDTOList1 = Collections.singletonList(buildDiscoveryUpstreamDTO("", "123", "url1"));
        ConfigImportResult configImportResult1 = this.discoveryUpstreamService.importData(upstreamDTOList1);
        assertNotNull(configImportResult1);
        Assertions.assertEquals(0, configImportResult1.getSuccessCount());

    }

    @Test
    public void testUpdateBatch() {
        when(discoveryUpstreamMapper.insert(any())).thenReturn(1);
        when(discoveryProcessorHolder.chooseProcessor(anyString())).thenReturn(discoveryProcessor);
        when(selectorMapper.selectByDiscoveryHandlerId(any())).thenReturn(buildSelectorDO());
        when(discoveryHandlerMapper.selectById(any())).thenReturn(buildDiscoveryHandlerDO());
        when(pluginMapper.selectById(any())).thenReturn(buildPluginDO());
        when(discoveryMapper.selectById(any())).thenReturn(buildDiscoveryDO());
        when(discoveryUpstreamMapper.deleteByDiscoveryHandlerId(anyString())).thenReturn(0);
        discoveryUpstreamService.updateBatch("123", Collections.singletonList(buildDiscoveryUpstreamDTO("")));
    }

    private void testUpdate() {
        when(discoveryUpstreamMapper.update(any())).thenReturn(1);
        DiscoveryUpstreamDTO discoveryUpstreamDTO = buildDiscoveryUpstreamDTO("123");
        discoveryUpstreamService.createOrUpdate(discoveryUpstreamDTO);
    }

    private void testCreate() {
        DiscoveryUpstreamDTO discoveryUpstreamDTO = buildDiscoveryUpstreamDTO("");
        discoveryUpstreamService.createOrUpdate(discoveryUpstreamDTO);
    }

    private void testNativeUpdate() {
        DiscoveryUpstreamDTO discoveryUpstreamDTO = buildDiscoveryUpstreamDTO("123");
        discoveryUpstreamService.nativeCreateOrUpdate(discoveryUpstreamDTO);
    }

    private void testNativeCreate() {
        DiscoveryUpstreamDTO discoveryUpstreamDTO = buildDiscoveryUpstreamDTO("");
        discoveryUpstreamService.nativeCreateOrUpdate(discoveryUpstreamDTO);
    }

    private DiscoveryDO buildDiscoveryDO() {
        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setName("123");
        discoveryDO.setId("123");
        discoveryDO.setType("local");
        return discoveryDO;
    }

    private PluginDO buildPluginDO() {
        PluginDO pluginDO = new PluginDO();
        pluginDO.setId("123");
        pluginDO.setName("234");
        return pluginDO;
    }

    private DiscoveryUpstreamDO buildDiscoveryUpstreamDO(final String id) {
        DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
        discoveryUpstreamDO.setId(id);
        discoveryUpstreamDO.setStatus(1);
        discoveryUpstreamDO.setWeight(50);
        discoveryUpstreamDO.setDiscoveryHandlerId("123");
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        discoveryUpstreamDO.setDateCreated(now);
        discoveryUpstreamDO.setDateUpdated(now);
        return discoveryUpstreamDO;
    }

    private DiscoveryUpstreamDO buildDiscoveryUpstreamDO(final String id, final String discoveryHandlerId, final String url) {
        DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
        discoveryUpstreamDO.setId(id);
        discoveryUpstreamDO.setStatus(1);
        discoveryUpstreamDO.setWeight(50);
        discoveryUpstreamDO.setUrl(url);
        discoveryUpstreamDO.setDiscoveryHandlerId(discoveryHandlerId);
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        discoveryUpstreamDO.setDateCreated(now);
        discoveryUpstreamDO.setDateUpdated(now);
        return discoveryUpstreamDO;
    }

    private DiscoveryUpstreamDTO buildDiscoveryUpstreamDTO(final String id) {
        DiscoveryUpstreamDTO discoveryUpstreamDTO = new DiscoveryUpstreamDTO();
        discoveryUpstreamDTO.setId(id);
        discoveryUpstreamDTO.setStatus(1);
        discoveryUpstreamDTO.setWeight(50);
        discoveryUpstreamDTO.setDiscoveryHandlerId("123");
        return discoveryUpstreamDTO;
    }

    private DiscoveryUpstreamDTO buildDiscoveryUpstreamDTO(final String id, final String discoveryHandlerId, final String url) {
        DiscoveryUpstreamDTO discoveryUpstreamDTO = new DiscoveryUpstreamDTO();
        discoveryUpstreamDTO.setId(id);
        discoveryUpstreamDTO.setStatus(1);
        discoveryUpstreamDTO.setWeight(50);
        discoveryUpstreamDTO.setUrl(url);
        discoveryUpstreamDTO.setDiscoveryHandlerId(discoveryHandlerId);
        return discoveryUpstreamDTO;
    }

    private DiscoveryHandlerDO buildDiscoveryHandlerDO() {
        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setDiscoveryId("123");
        discoveryHandlerDO.setId("123");
        discoveryHandlerDO.setHandler("123");
        return discoveryHandlerDO;
    }

    private SelectorDO buildSelectorDO() {
        SelectorDO selectorDO = new SelectorDO();
        selectorDO.setId("selector_1");
        selectorDO.setName("selector_1");
        return selectorDO;
    }

    private ProxySelectorDO buildProxySelectorDO() {
        ProxySelectorDO selectorDO = new ProxySelectorDO();
        selectorDO.setId("selector_1");
        selectorDO.setName("selector_1");
        return selectorDO;
    }

    private DiscoveryRelDO buildDiscoveryRelDO() {
        DiscoveryRelDO discoveryRelDO = new DiscoveryRelDO();
        discoveryRelDO.setId("123");
        discoveryRelDO.setPluginName("123");
        return discoveryRelDO;
    }

}
