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

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.AiProxyApiKeyMapper;
import org.apache.shenyu.admin.model.dto.ProxyApiKeyDTO;
import org.apache.shenyu.admin.model.entity.ProxyApiKeyDO;
import org.apache.shenyu.admin.model.query.ProxyApiKeyQuery;
import org.apache.shenyu.admin.model.vo.ProxyApiKeyVO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import org.apache.shenyu.admin.service.support.AiProxyRealKeyResolver;

class AiProxyApiKeyServiceImplTest {

    @Mock
    private AiProxyApiKeyMapper mapper;

    @Mock
    private ApplicationEventPublisher publisher;

    @Mock
    private AiProxyRealKeyResolver realKeyResolver;

    @InjectMocks
    private AiProxyApiKeyServiceImpl service;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        service = new AiProxyApiKeyServiceImpl(mapper, publisher, realKeyResolver);
    }

    @Test
    void testCreatePublishesEvent() {
        ProxyApiKeyDTO dto = new ProxyApiKeyDTO();
        dto.setNamespaceId("default");
        when(mapper.insert(any())).thenReturn(1);
        service.create(dto, "sel-1");
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(publisher).publishEvent(captor.capture());
        assertEquals(ConfigGroupEnum.AI_PROXY_API_KEY, captor.getValue().getGroupKey());
    }

    @Test
    void testUpdatePublishesEvent() {
        ProxyApiKeyDTO dto = new ProxyApiKeyDTO();
        dto.setId("id-1");
        dto.setNamespaceId("default");
        when(mapper.updateSelective(any())).thenReturn(1);
        service.update(dto);
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(publisher).publishEvent(captor.capture());
        assertEquals(ConfigGroupEnum.AI_PROXY_API_KEY, captor.getValue().getGroupKey());
    }

    @Test
    void testEnabledPublishesEvent() {
        when(mapper.updateEnableBatch(any(), any())).thenReturn(1);
        ProxyApiKeyDO e = new ProxyApiKeyDO();
        e.setId("1");
        e.setProxyApiKey("p");
        e.setNamespaceId("default");
        e.setEnabled(Boolean.TRUE);
        when(mapper.selectByIds(any())).thenReturn(Collections.singletonList(e));
        service.enabled(Arrays.asList("1"), true);
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(publisher).publishEvent(captor.capture());
        assertEquals(ConfigGroupEnum.AI_PROXY_API_KEY, captor.getValue().getGroupKey());
    }

    @Test
    void testDeletePublishesEvent() {
        ProxyApiKeyDO e = new ProxyApiKeyDO();
        e.setId("1");
        e.setProxyApiKey("p");
        e.setNamespaceId("default");
        e.setEnabled(Boolean.TRUE);
        when(mapper.selectByIds(any())).thenReturn(Collections.singletonList(e));
        when(mapper.deleteByIds(any())).thenReturn(1);
        service.delete(Collections.singletonList("1"));
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(publisher).publishEvent(captor.capture());
        assertEquals(ConfigGroupEnum.AI_PROXY_API_KEY, captor.getValue().getGroupKey());
    }

    @Test
    void testCreateBackfillFields() {
        ProxyApiKeyDTO dto = new ProxyApiKeyDTO();
        dto.setNamespaceId("default");
        when(mapper.insert(any())).thenReturn(1);
        service.create(dto, "sel-1");
        assertNotNull(dto.getId());
        assertNotNull(dto.getProxyApiKey());
        // enabled will be defaulted to true
        // can't assert equals(Boolean.TRUE) as dto default might be null before backfill
        // but after create it should be non-null
        assertNotNull(dto.getEnabled());
    }

    @Test
    void testListByPage() {
        ProxyApiKeyQuery query = new ProxyApiKeyQuery();
        query.setNamespaceId("default");
        query.setPageParameter(new PageParameter(1, 10));
        ProxyApiKeyVO vo = new ProxyApiKeyVO();
        vo.setId("1");
        vo.setProxyApiKey("p");
        vo.setDescription("d");
        vo.setEnabled(Boolean.TRUE);
        vo.setNamespaceId("default");
        when(mapper.selectByCondition(any())).thenReturn(Collections.singletonList(vo));
        CommonPager<ProxyApiKeyVO> pager = service.listByPage(query);
        assertEquals(1, pager.getDataList().size());
        assertEquals("p", pager.getDataList().get(0).getProxyApiKey());
    }

    @Test
    void testSearchByCondition() {
        ProxyApiKeyQuery query = new ProxyApiKeyQuery();
        query.setNamespaceId("default");
        ProxyApiKeyVO vo = new ProxyApiKeyVO();
        vo.setId("2");
        vo.setProxyApiKey("px");
        when(mapper.selectByCondition(any())).thenReturn(Collections.singletonList(vo));
        List<ProxyApiKeyVO> list = service.searchByCondition(query);
        assertEquals(1, list.size());
        assertEquals("px", list.get(0).getProxyApiKey());
    }

    @Test
    void testUpdateReturnsZeroWhenIdMissing() {
        ProxyApiKeyDTO dto = new ProxyApiKeyDTO();
        int res = service.update(dto);
        assertEquals(0, res);
        verify(mapper, never()).updateSelective(any());
    }

    @Test
    void testDeleteNoEventWhenNoRows() {
        when(mapper.selectByIds(any())).thenReturn(Collections.emptyList());
        when(mapper.deleteByIds(any())).thenReturn(0);
        service.delete(Collections.singletonList("1"));
        verify(publisher, never()).publishEvent(any());
    }
} 