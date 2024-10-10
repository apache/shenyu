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

import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.ProxySelectorAddDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ProxySelectorQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.ProxySelectorVO;
import org.apache.shenyu.admin.service.impl.ProxySelectorServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ProxySelectorServiceTest {

    @InjectMocks
    private ProxySelectorServiceImpl proxySelectorService;

    @Mock
    private ProxySelectorMapper proxySelectorMapper;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private DiscoveryRelMapper discoveryRelMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private DiscoveryProcessorHolder discoveryProcessorHolder;

    @BeforeEach
    void testSetUp() {

        proxySelectorService = new ProxySelectorServiceImpl(proxySelectorMapper, discoveryMapper, discoveryUpstreamMapper,
                discoveryHandlerMapper, discoveryRelMapper, selectorMapper, discoveryProcessorHolder);
    }

    @Test
    void testListByPage() {

        final ProxySelectorQuery proxySelectorQuery = new ProxySelectorQuery("test", new PageParameter(), SYS_DEFAULT_NAMESPACE_ID);
        final List<ProxySelectorDO> list = new ArrayList<>();
        ProxySelectorDO proxySelectorDO = new ProxySelectorDO();
        proxySelectorDO.setId("123");
        proxySelectorDO.setName("test");
        proxySelectorDO.setPluginName("test");
        proxySelectorDO.setForwardPort(8080);
        proxySelectorDO.setProps("test");
        proxySelectorDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        proxySelectorDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        list.add(proxySelectorDO);
        given(this.proxySelectorMapper.selectByQuery(proxySelectorQuery)).willReturn(list);
        assertEquals(proxySelectorService.listByPage(proxySelectorQuery).getDataList().size(), list.size());
    }

    @Test
    void testCreateOrUpdate() {

        ProxySelectorAddDTO proxySelectorDTO = new ProxySelectorAddDTO();
        proxySelectorDTO.setName("test");
        proxySelectorDTO.setForwardPort(8080);
        proxySelectorDTO.setProps("test");
        given(proxySelectorMapper.nameExisted("test")).willReturn(null);
        given(proxySelectorMapper.insert(ProxySelectorDO.buildProxySelectorDO(proxySelectorDTO))).willReturn(1);
        assertEquals(proxySelectorService.createOrUpdate(proxySelectorDTO), ShenyuResultMessage.CREATE_SUCCESS);
    }

    @Test
    void testDelete() {

        List<String> ids = new ArrayList<>();
        ids.add("123");
        given(proxySelectorMapper.deleteByIds(ids)).willReturn(1);
        assertEquals(proxySelectorService.delete(ids), ShenyuResultMessage.DELETE_SUCCESS);
    }

    @Test
    void testListAllData() {
        List<ProxySelectorDO> selectorDOList = Collections.singletonList(buildProxySelectorDO());
        given(proxySelectorMapper.selectAll()).willReturn(selectorDOList);
        given(discoveryRelMapper.selectByProxySelectorId(any())).willReturn(buildDiscoveryRelDO());
        List<ProxySelectorVO> selectorVOList = proxySelectorService.listAllData();
        assertNotNull(selectorVOList);
        assertEquals(selectorVOList.size(), selectorDOList.size());
    }

    @Test
    void testImportData() {
        final List<ProxySelectorDO> selectorDOs = Collections.singletonList(buildProxySelectorDO());
        given(this.proxySelectorMapper.selectAll()).willReturn(selectorDOs);

        final List<ProxySelectorData> proxySelectorDataList = Collections.singletonList(buildProxySelectorData());
        given(this.proxySelectorMapper.insert(any())).willReturn(1);

        ConfigImportResult configImportResult = this.proxySelectorService.importData(proxySelectorDataList);

        assertNotNull(configImportResult);
        assertEquals(configImportResult.getSuccessCount(), proxySelectorDataList.size());
    }

    private ProxySelectorDO buildProxySelectorDO() {
        ProxySelectorDO proxySelectorDO = new ProxySelectorDO();
        proxySelectorDO.setId("123");
        proxySelectorDO.setName("test");
        proxySelectorDO.setPluginName("test");
        proxySelectorDO.setForwardPort(8080);
        proxySelectorDO.setProps("test");
        proxySelectorDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        proxySelectorDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        return proxySelectorDO;
    }

    private ProxySelectorData buildProxySelectorData() {
        ProxySelectorData selectorData = new ProxySelectorData();
        selectorData.setId("123");
        selectorData.setName("test123456");
        selectorData.setPluginName("test");
        selectorData.setForwardPort(8080);
        selectorData.setProps(null);
        return selectorData;
    }

    private DiscoveryRelDO buildDiscoveryRelDO() {
        DiscoveryRelDO discoveryRelDO = new DiscoveryRelDO();
        discoveryRelDO.setSelectorId("123");
        discoveryRelDO.setDiscoveryHandlerId("123");
        discoveryRelDO.setProxySelectorId("123");
        discoveryRelDO.setPluginName("test");
        discoveryRelDO.setId("456");
        discoveryRelDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        discoveryRelDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        return discoveryRelDO;
    }
}
