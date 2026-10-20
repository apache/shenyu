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
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.exception.ValidFailException;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.ProxySelectorAddDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ProxySelectorQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.ProxySelectorVO;
import org.apache.shenyu.admin.service.impl.ProxySelectorServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.times;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.mockito.ArgumentCaptor;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @BeforeEach
    void testSetUp() {

        proxySelectorService = new ProxySelectorServiceImpl(proxySelectorMapper, discoveryMapper, discoveryUpstreamMapper,
                discoveryHandlerMapper, discoveryRelMapper, selectorMapper, discoveryProcessorHolder, eventPublisher);
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
    void testListByPageBatchesSharedRelations() {
        final ProxySelectorQuery query = new ProxySelectorQuery("test", new PageParameter(), SYS_DEFAULT_NAMESPACE_ID);
        ProxySelectorDO first = new ProxySelectorDO();
        first.setId("first");
        ProxySelectorDO second = new ProxySelectorDO();
        second.setId("second");
        ProxySelectorDO missing = new ProxySelectorDO();
        missing.setId("missing");
        given(proxySelectorMapper.selectByQuery(query)).willReturn(Arrays.asList(first, second, missing));
        DiscoveryRelDO firstRel = new DiscoveryRelDO();
        firstRel.setProxySelectorId("first");
        firstRel.setDiscoveryHandlerId("handler");
        DiscoveryRelDO secondRel = new DiscoveryRelDO();
        secondRel.setProxySelectorId("second");
        secondRel.setDiscoveryHandlerId("handler");
        given(discoveryRelMapper.selectByProxySelectorIds(Arrays.asList("first", "second", "missing"))).willReturn(Arrays.asList(firstRel, secondRel));
        DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId("handler");
        handler.setDiscoveryId("discovery");
        given(discoveryHandlerMapper.selectByIds(Collections.singletonList("handler"))).willReturn(Collections.singletonList(handler));
        DiscoveryDO discovery = new DiscoveryDO();
        discovery.setId("discovery");
        given(discoveryMapper.selectByIds(Collections.singletonList("discovery"))).willReturn(Collections.singletonList(discovery));
        DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setId("upstream");
        upstream.setDateCreated(new Timestamp(0));
        upstream.setDateUpdated(new Timestamp(0));
        upstream.setDiscoveryHandlerId("handler");
        given(discoveryUpstreamMapper.selectByDiscoveryHandlerIds(Collections.singletonList("handler"))).willReturn(Collections.singletonList(upstream));

        List<ProxySelectorVO> result = proxySelectorService.listByPage(query).getDataList();

        assertEquals(3, result.size());
        for (int index = 0; index < 2; index++) {
            assertEquals("handler", result.get(index).getDiscoveryHandlerId());
            assertEquals("discovery", result.get(index).getDiscovery().getId());
            assertEquals("upstream", result.get(index).getDiscoveryUpstreams().get(0).getId());
        }
        verify(discoveryRelMapper).selectByProxySelectorIds(Arrays.asList("first", "second", "missing"));
        verify(discoveryHandlerMapper).selectByIds(Collections.singletonList("handler"));
        verify(discoveryMapper).selectByIds(Collections.singletonList("discovery"));
        verify(discoveryUpstreamMapper).selectByDiscoveryHandlerIds(Collections.singletonList("handler"));
        verifyNoMoreInteractions(discoveryRelMapper, discoveryHandlerMapper, discoveryMapper, discoveryUpstreamMapper);
    }

    @Test
    void testEmptyPageSkipsRelations() {
        ProxySelectorQuery query = new ProxySelectorQuery("test", new PageParameter(), SYS_DEFAULT_NAMESPACE_ID);
        given(proxySelectorMapper.selectByQuery(query)).willReturn(Collections.emptyList());
        assertEquals(0, proxySelectorService.listByPage(query).getDataList().size());
        verifyNoInteractions(discoveryRelMapper, discoveryHandlerMapper, discoveryMapper, discoveryUpstreamMapper);
    }

    @Test
    void chunksEveryRelationQueryForLargePages() {
        final ProxySelectorQuery query = new ProxySelectorQuery("test", new PageParameter(1, 1201), SYS_DEFAULT_NAMESPACE_ID);
        List<ProxySelectorDO> selectors = IntStream.range(0, 1201).mapToObj(index -> {
            ProxySelectorDO selector = new ProxySelectorDO();
            selector.setId(String.valueOf(index));
            return selector;
        }).collect(Collectors.toList());
        given(proxySelectorMapper.selectByQuery(query)).willReturn(selectors);
        given(discoveryRelMapper.selectByProxySelectorIds(any())).willAnswer(invocation -> invocation.<List<String>>getArgument(0).stream().map(id -> {
            DiscoveryRelDO relation = new DiscoveryRelDO();
            relation.setProxySelectorId(id);
            relation.setDiscoveryHandlerId(id);
            return relation;
        }).collect(Collectors.toList()));
        given(discoveryHandlerMapper.selectByIds(any())).willAnswer(invocation -> invocation.<List<String>>getArgument(0).stream().map(id -> {
            DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
            handler.setId(id);
            handler.setDiscoveryId(id);
            return handler;
        }).collect(Collectors.toList()));
        given(discoveryMapper.selectByIds(any())).willAnswer(invocation -> invocation.<List<String>>getArgument(0).stream().map(id -> {
            DiscoveryDO discovery = new DiscoveryDO();
            discovery.setId(id);
            return discovery;
        }).collect(Collectors.toList()));
        given(discoveryUpstreamMapper.selectByDiscoveryHandlerIds(any())).willAnswer(invocation -> invocation.<List<String>>getArgument(0).stream().map(id -> {
            DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
            upstream.setId(id);
            upstream.setDiscoveryHandlerId(id);
            upstream.setDateCreated(new Timestamp(0));
            upstream.setDateUpdated(new Timestamp(0));
            return upstream;
        }).collect(Collectors.toList()));
        List<ProxySelectorVO> result = proxySelectorService.listByPage(query).getDataList();
        assertEquals(1201, result.size());
        for (int index = 0; index < result.size(); index++) {
            String id = String.valueOf(index);
            assertEquals(id, result.get(index).getId());
            assertEquals(id, result.get(index).getDiscoveryHandlerId());
            assertEquals(id, result.get(index).getDiscovery().getId());
            assertEquals(id, result.get(index).getDiscoveryUpstreams().get(0).getId());
        }
        ArgumentCaptor<List<String>> batches = ArgumentCaptor.forClass(List.class);
        verify(discoveryRelMapper, times(3)).selectByProxySelectorIds(batches.capture());
        verify(discoveryHandlerMapper, times(3)).selectByIds(batches.capture());
        verify(discoveryMapper, times(3)).selectByIds(batches.capture());
        verify(discoveryUpstreamMapper, times(3)).selectByDiscoveryHandlerIds(batches.capture());
        assertTrue(batches.getAllValues().stream().allMatch(batch -> !batch.isEmpty() && batch.size() <= 500));
        assertEquals(4 * 1201, batches.getAllValues().stream().mapToInt(List::size).sum());
        verifyNoMoreInteractions(discoveryRelMapper, discoveryHandlerMapper, discoveryMapper, discoveryUpstreamMapper);
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

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testUpdatePublishesListenerConfigurationAfterCommit(final boolean commit) {

        ProxySelectorAddDTO proxySelectorDTO = new ProxySelectorAddDTO();
        proxySelectorDTO.setId("proxy-1");
        proxySelectorDTO.setName("test");
        proxySelectorDTO.setPluginName("tcp");
        proxySelectorDTO.setForwardPort(8080);
        proxySelectorDTO.setNamespaceId("namespace-1");
        proxySelectorDTO.setProps("{\"loadBalance\":\"roundRobin\"}");
        proxySelectorDTO.setDiscovery(new ProxySelectorAddDTO.Discovery());

        DiscoveryRelDO discoveryRelDO = new DiscoveryRelDO();
        discoveryRelDO.setDiscoveryHandlerId("handler-1");
        given(discoveryRelMapper.selectByProxySelectorId("proxy-1")).willReturn(discoveryRelDO);

        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setId("handler-1");
        discoveryHandlerDO.setDiscoveryId("discovery-1");
        given(discoveryHandlerMapper.selectById("handler-1")).willReturn(discoveryHandlerDO);

        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setNamespaceId("namespace-1");
        discoveryDO.setDiscoveryType("local");
        given(discoveryMapper.selectById("discovery-1")).willReturn(discoveryDO);

        DiscoveryProcessor discoveryProcessor = mock(DiscoveryProcessor.class);
        given(discoveryProcessorHolder.chooseProcessor("local")).willReturn(discoveryProcessor);
        given(discoveryUpstreamMapper.selectByDiscoveryHandlerId("handler-1")).willReturn(Collections.emptyList());

        TransactionSynchronizationManager.initSynchronization();
        try {
            assertEquals(proxySelectorService.update(proxySelectorDTO), ShenyuResultMessage.UPDATE_SUCCESS);
            verify(discoveryUpstreamMapper, never()).deleteByDiscoveryHandlerId(any());
            verifyNoInteractions(eventPublisher);
            int status = commit ? TransactionSynchronization.STATUS_COMMITTED : TransactionSynchronization.STATUS_ROLLED_BACK;
            TransactionSynchronizationManager.getSynchronizations().forEach(synchronization -> synchronization.afterCompletion(status));
            if (!commit) {
                verifyNoInteractions(eventPublisher);
                return;
            }
            ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
            verify(eventPublisher).publishEvent(captor.capture());
            DataChangedEvent event = captor.getValue();
            assertEquals(ConfigGroupEnum.PROXY_SELECTOR, event.getGroupKey());
            List<?> payload = (List<?>) event.getSource();
            assertEquals(1, payload.size());
            ProxySelectorData data = (ProxySelectorData) payload.get(0);
            assertEquals("test", data.getName());
            assertEquals(8080, data.getForwardPort());
            assertEquals("roundRobin", data.getProps().getProperty("loadBalance"));
        } finally {
            TransactionSynchronizationManager.clearSynchronization();
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"configuration", "relation", "handler", "discovery"})
    void validatesAllBindingsBeforeUpdatingAnyRecord(final String missing) {
        ProxySelectorAddDTO dto = new ProxySelectorAddDTO();
        dto.setId("proxy");
        dto.setName("proxy");
        dto.setPluginName("tcp");
        dto.setForwardPort(8080);
        dto.setHandler("new-handler");
        if (!"configuration".equals(missing)) {
            dto.setDiscovery(new ProxySelectorAddDTO.Discovery());
        }
        DiscoveryRelDO relation = new DiscoveryRelDO();
        relation.setDiscoveryHandlerId("handler");
        DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId("handler");
        handler.setDiscoveryId("discovery");
        handler.setHandler("original");
        given(discoveryRelMapper.selectByProxySelectorId("proxy")).willReturn("relation".equals(missing) ? null : relation);
        given(discoveryHandlerMapper.selectById("handler")).willReturn("handler".equals(missing) ? null : handler);
        given(discoveryMapper.selectById("discovery")).willReturn("discovery".equals(missing) ? null : new DiscoveryDO());

        assertThrows(ValidFailException.class, () -> proxySelectorService.update(dto));

        verify(proxySelectorMapper, never()).update(any());
        verify(discoveryHandlerMapper, never()).updateSelective(any());
        verify(discoveryMapper, never()).updateSelective(any());
        verifyNoInteractions(discoveryUpstreamMapper, discoveryProcessorHolder);
        assertEquals("original", handler.getHandler());
    }

    @Test
    void bindingRejectsMissingConfigurationBeforeLookingUpProcessorOrWriting() {
        ProxySelectorAddDTO dto = new ProxySelectorAddDTO();
        dto.setSelectorId("selector-without-discovery");

        ValidFailException failure = assertThrows(ValidFailException.class, () -> proxySelectorService.bindingDiscoveryHandler(dto));

        assertEquals("Discovery configuration is required for selector: selector-without-discovery", failure.getMessage());
        verifyNoInteractions(discoveryProcessorHolder, discoveryMapper, discoveryHandlerMapper, discoveryRelMapper, discoveryUpstreamMapper);
    }

    @Test
    void testFetchDataWithProxySelector() {
        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setId("handler-1");
        discoveryHandlerDO.setDiscoveryId("discovery-1");
        given(discoveryHandlerMapper.selectById("handler-1")).willReturn(discoveryHandlerDO);

        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setDiscoveryType("local");
        given(discoveryMapper.selectById("discovery-1")).willReturn(discoveryDO);

        ProxySelectorDO proxySelectorDO = buildProxySelectorDO();
        given(proxySelectorMapper.selectByHandlerId("handler-1")).willReturn(proxySelectorDO);
        DiscoveryProcessor discoveryProcessor = mock(DiscoveryProcessor.class);
        given(discoveryProcessorHolder.chooseProcessor("local")).willReturn(discoveryProcessor);

        proxySelectorService.fetchData("handler-1");

        verify(discoveryProcessor).fetchAll(any(), any());
    }

    @Test
    void testFetchDataWithMissingDiscoveryHandler() {
        given(discoveryHandlerMapper.selectById("missing-handler")).willReturn(null);

        assertDoesNotThrow(() -> proxySelectorService.fetchData("missing-handler"));

        verify(discoveryMapper, never()).selectById(any());
        verify(proxySelectorMapper, never()).selectByHandlerId(any());
        verify(selectorMapper, never()).selectByDiscoveryHandlerId(any());
        verify(discoveryProcessorHolder, never()).chooseProcessor(any());
    }

    @Test
    void testFetchDataWithMissingDiscovery() {
        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setDiscoveryId("missing-discovery");
        given(discoveryHandlerMapper.selectById("handler-1")).willReturn(discoveryHandlerDO);
        given(discoveryMapper.selectById("missing-discovery")).willReturn(null);

        assertDoesNotThrow(() -> proxySelectorService.fetchData("handler-1"));

        verify(proxySelectorMapper, never()).selectByHandlerId(any());
        verify(selectorMapper, never()).selectByDiscoveryHandlerId(any());
        verify(discoveryProcessorHolder, never()).chooseProcessor(any());
    }

    @Test
    void testFetchDataWithoutBoundSelector() {
        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setId("handler-1");
        discoveryHandlerDO.setDiscoveryId("discovery-1");
        given(discoveryHandlerMapper.selectById("handler-1")).willReturn(discoveryHandlerDO);

        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setDiscoveryType("local");
        given(discoveryMapper.selectById("discovery-1")).willReturn(discoveryDO);

        assertDoesNotThrow(() -> proxySelectorService.fetchData("handler-1"));

        verify(discoveryProcessorHolder, never()).chooseProcessor(any());
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
