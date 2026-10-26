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
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryRelDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.DiscoveryVO;
import org.apache.shenyu.admin.service.impl.DiscoveryServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.Mock;
import org.mockito.quality.Strictness;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.SimpleTransactionStatus;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test cases for DiscoveryServiceImpl.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class DiscoveryServiceImplTest {

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private ProxySelectorMapper proxySelectorMapper;

    @Mock
    private DiscoveryRelMapper discoveryRelMapper;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private SelectorService selectorService;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private DiscoveryProcessorHolder discoveryProcessorHolder;

    @Mock
    private DiscoveryProcessor discoveryProcessor;

    @Mock
    private PlatformTransactionManager transactionManager;

    private DiscoveryServiceImpl discoveryService;

    @BeforeEach
    public void setUp() {
        discoveryService = new DiscoveryServiceImpl(discoveryMapper, proxySelectorMapper, discoveryRelMapper,
                discoveryHandlerMapper, selectorService, selectorMapper, discoveryProcessorHolder, transactionManager);
        given(discoveryProcessorHolder.chooseProcessor(anyString())).willReturn(discoveryProcessor);
    }

    @Test
    public void testBindingActivatesAfterCommit() {
        given(transactionManager.getTransaction(any())).willReturn(new SimpleTransactionStatus());
        TransactionSynchronizationManager.initSynchronization();
        try {
            discoveryService.registerDiscoveryConfig(bindingConfig());
            verifyNoInteractions(discoveryProcessor);
            TransactionSynchronizationManager.getSynchronizations().forEach(TransactionSynchronization::afterCommit);
            org.mockito.InOrder order = inOrder(discoveryProcessor);
            order.verify(discoveryProcessor).createDiscovery(any());
            order.verify(discoveryProcessor).createProxySelector(any(), any());
        } finally {
            TransactionSynchronizationManager.clearSynchronization();
        }
    }

    @Test
    public void testRolledBackBindingDoesNotActivate() {
        TransactionSynchronizationManager.initSynchronization();
        try {
            discoveryService.registerDiscoveryConfig(bindingConfig());
            TransactionSynchronizationManager.getSynchronizations().forEach(sync -> sync.afterCompletion(TransactionSynchronization.STATUS_ROLLED_BACK));
            verifyNoInteractions(discoveryProcessor, transactionManager);
        } finally {
            TransactionSynchronizationManager.clearSynchronization();
        }
    }

    @Test
    public void testBindingWithoutTransactionActivatesImmediately() {
        discoveryService.registerDiscoveryConfig(bindingConfig());
        verify(discoveryProcessor).createDiscovery(any());
        verify(discoveryProcessor).createProxySelector(any(), any());
    }

    @Test
    public void testBindingRetriesAndFailsWhenSelectorIsMissing() {
        DiscoveryConfigRegisterDTO config = bindingConfig();
        given(selectorService.findByNameAndPluginNameAndNamespaceIdForUpdate("test", "divide", SYS_DEFAULT_NAMESPACE_ID))
                .willReturn(null);
        assertThrows(ShenyuException.class, () -> discoveryService.registerDiscoveryConfig(config));
        verify(selectorService, org.mockito.Mockito.times(3))
                .findByNameAndPluginNameAndNamespaceIdForUpdate("test", "divide", SYS_DEFAULT_NAMESPACE_ID);
        verifyNoInteractions(discoveryMapper, discoveryHandlerMapper, discoveryRelMapper, discoveryProcessor);
    }

    @Test
    public void testBindingReusesExistingDiscoveryAndHandler() {
        SelectorDO selector = new SelectorDO();
        selector.setId("selector");
        selector.setSelectorName("test");
        selector.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        given(selectorService.findByNameAndPluginNameAndNamespaceIdForUpdate("test", "divide", SYS_DEFAULT_NAMESPACE_ID))
                .willReturn(selector);
        DiscoveryDO existing = DiscoveryDO.builder().id("discovery").discoveryType("local").build();
        DiscoveryHandlerDO handler = DiscoveryHandlerDO.builder().id("handler").discoveryId("discovery").build();
        given(discoveryMapper.selectByPluginNameAndLevelAndNamespaceIdAndType(anyString(), anyString(), anyString(), anyString()))
                .willReturn(existing);
        given(discoveryHandlerMapper.selectBySelectorId("selector")).willReturn(handler);
        discoveryService.registerDiscoveryConfig(bindingConfig());
        verify(discoveryMapper, never()).insertSelective(any());
        verify(discoveryHandlerMapper, never()).insertSelective(any());
        verify(discoveryRelMapper, never()).insertSelective(any());
        verify(discoveryProcessor).createDiscovery(existing);
    }

    private DiscoveryConfigRegisterDTO bindingConfig() {
        SelectorDO selector = new SelectorDO();
        selector.setId("selector");
        selector.setSelectorName("test");
        selector.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        given(selectorService.findByNameAndPluginNameAndNamespaceIdForUpdate("test", "divide", SYS_DEFAULT_NAMESPACE_ID)).willReturn(selector);
        DiscoveryConfigRegisterDTO config = new DiscoveryConfigRegisterDTO();
        config.setSelectorName("test");
        config.setPluginName("divide");
        config.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        config.setDiscoveryType("local");
        return config;
    }

    @Test
    public void testDelete() {
        DiscoveryDO discoveryDO = DiscoveryDO.builder()
                .id("discovery-1")
                .discoveryType("local")
                .namespaceId(SYS_DEFAULT_NAMESPACE_ID)
                .build();
        given(discoveryHandlerMapper.selectByDiscoveryId("discovery-1")).willReturn(Collections.emptyList());
        given(discoveryMapper.selectById("discovery-1")).willReturn(discoveryDO);
        given(discoveryMapper.delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID)).willReturn(1);

        assertEquals(ShenyuResultMessage.DELETE_SUCCESS, discoveryService.delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID));
        verify(discoveryProcessor).removeDiscovery(discoveryDO);
        verify(discoveryMapper).delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID);
    }

    @Test
    public void testDeleteWithMismatchedNamespace() {
        DiscoveryDO discoveryDO = DiscoveryDO.builder()
                .id("discovery-1")
                .discoveryType("local")
                .namespaceId("another-namespace-id")
                .build();
        given(discoveryHandlerMapper.selectByDiscoveryId("discovery-1")).willReturn(Collections.emptyList());
        given(discoveryMapper.selectById("discovery-1")).willReturn(discoveryDO);

        assertThrows(ShenyuException.class, () -> discoveryService.delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID));
        verify(discoveryMapper, never()).delete(any(), any());
        verify(discoveryProcessor, never()).removeDiscovery(any());
    }

    @Test
    public void testDeleteWithDiscoveryNotFound() {
        given(discoveryHandlerMapper.selectByDiscoveryId("discovery-1")).willReturn(Collections.emptyList());
        given(discoveryMapper.selectById("discovery-1")).willReturn(null);

        assertThrows(ShenyuException.class, () -> discoveryService.delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID));
        verify(discoveryMapper, never()).delete(any(), any());
        verify(discoveryProcessor, never()).removeDiscovery(any());
    }

    @Test
    public void testImportSkipsRelWhenSelectorMappingMissing() {
        final ConfigsImportContext context = new ConfigsImportContext();
        DiscoveryDTO discoveryDTO = buildImportDiscovery();
        DiscoveryRelDTO rel = new DiscoveryRelDTO();
        rel.setSelectorId("selector-exported");
        discoveryDTO.setDiscoveryRel(rel);

        given(discoveryMapper.selectAllByNamespaceId("ns-import")).willReturn(Collections.emptyList());
        given(discoveryMapper.insert(any(DiscoveryDO.class))).willReturn(1);

        discoveryService.importData("ns-import", Collections.singletonList(discoveryDTO), context);

        verify(discoveryRelMapper, never()).insertSelective(any(DiscoveryRelDO.class));
    }

    @Test
    public void testImportSkipsRelWhenProxySelectorMappingMissing() {
        final ConfigsImportContext context = new ConfigsImportContext();
        DiscoveryDTO discoveryDTO = buildImportDiscovery();
        DiscoveryRelDTO rel = new DiscoveryRelDTO();
        rel.setProxySelectorId("proxy-selector-exported");
        discoveryDTO.setDiscoveryRel(rel);

        given(discoveryMapper.selectAllByNamespaceId("ns-import")).willReturn(Collections.emptyList());
        given(discoveryMapper.insert(any(DiscoveryDO.class))).willReturn(1);

        discoveryService.importData("ns-import", Collections.singletonList(discoveryDTO), context);

        verify(discoveryRelMapper, never()).insertSelective(any(DiscoveryRelDO.class));
    }

    @Test
    public void testImportRemapsRelReferencesWhenMappingsExist() {
        final ConfigsImportContext context = new ConfigsImportContext();
        context.getSelectorIdMapping().put("selector-exported", "selector-new");
        DiscoveryDTO discoveryDTO = buildImportDiscovery();
        DiscoveryRelDTO rel = new DiscoveryRelDTO();
        rel.setSelectorId("selector-exported");
        discoveryDTO.setDiscoveryRel(rel);

        given(discoveryMapper.selectAllByNamespaceId("ns-import")).willReturn(Collections.emptyList());
        given(discoveryMapper.insert(any(DiscoveryDO.class))).willReturn(1);

        discoveryService.importData("ns-import", Collections.singletonList(discoveryDTO), context);

        ArgumentCaptor<DiscoveryRelDO> relCaptor = ArgumentCaptor.forClass(DiscoveryRelDO.class);
        verify(discoveryRelMapper).insertSelective(relCaptor.capture());
        assertEquals("selector-new", relCaptor.getValue().getSelectorId());
    }

    private DiscoveryDTO buildImportDiscovery() {
        DiscoveryDTO discoveryDTO = new DiscoveryDTO();
        discoveryDTO.setName("discovery-import");
        discoveryDTO.setPluginName("tcp");
        discoveryDTO.setType("local");
        DiscoveryHandlerDTO handlerDTO = new DiscoveryHandlerDTO();
        handlerDTO.setId("handler-exported");
        handlerDTO.setListenerNode("/shenyu/discovery");
        handlerDTO.setHandler("");
        discoveryDTO.setDiscoveryHandler(handlerDTO);
        return discoveryDTO;
    }

    @Test
    public void testImportKeepsBlankSelectorIdShapeAndRemapsProxySelector() {
        ConfigsImportContext context = new ConfigsImportContext();
        context.getProxySelectorIdMapping().put("proxy-selector-exported", "proxy-selector-new");
        DiscoveryDTO discoveryDTO = buildImportDiscovery();
        DiscoveryRelDTO rel = new DiscoveryRelDTO();
        rel.setSelectorId("");
        rel.setProxySelectorId("proxy-selector-exported");
        discoveryDTO.setDiscoveryRel(rel);

        given(discoveryMapper.selectAllByNamespaceId("ns-import")).willReturn(Collections.emptyList());
        given(discoveryMapper.insert(any(DiscoveryDO.class))).willReturn(1);

        discoveryService.importData("ns-import", Collections.singletonList(discoveryDTO), context);

        ArgumentCaptor<DiscoveryRelDO> relCaptor = ArgumentCaptor.forClass(DiscoveryRelDO.class);
        verify(discoveryRelMapper).insertSelective(relCaptor.capture());
        assertEquals("", relCaptor.getValue().getSelectorId());
        assertEquals("proxy-selector-new", relCaptor.getValue().getProxySelectorId());
    }

    @Test
    public void testDeleteWithHandlerBound() {
        given(discoveryHandlerMapper.selectByDiscoveryId("discovery-1"))
                .willReturn(Collections.singletonList(DiscoveryHandlerDO.builder().id("handler").build()));
        assertThrows(ShenyuException.class, () -> discoveryService.delete("discovery-1", SYS_DEFAULT_NAMESPACE_ID));
        verify(discoveryMapper, never()).selectById("discovery-1");
    }

    @Test
    public void testTypeCreateUpdateAndDiscoveryLookup() {
        assertEquals(org.apache.shenyu.admin.model.enums.DiscoveryTypeEnum.types(), discoveryService.typeEnums());
        DiscoveryDTO dto = new DiscoveryDTO();
        dto.setName("d");
        dto.setPluginName("divide");
        dto.setType("local");
        dto.setLevel("plugin");
        dto.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        when(discoveryMapper.insert(any())).thenReturn(1);
        assertEquals("local", discoveryService.createOrUpdate(dto).getDiscoveryType());
        dto.setId("id");
        when(discoveryMapper.updateSelective(any())).thenReturn(1);
        assertEquals("local", discoveryService.createOrUpdate(dto).getDiscoveryType());
        dto.setId(null);
        when(discoveryMapper.insert(any())).thenReturn(0);
        assertNull(discoveryService.createOrUpdate(dto));
        discoveryService.discovery("divide", "plugin", SYS_DEFAULT_NAMESPACE_ID);
        verify(discoveryMapper).selectByPluginNameAndLevelAndNamespaceId("divide", "plugin", SYS_DEFAULT_NAMESPACE_ID);
    }

    @Test
    public void testSyncAndListData() {
        SelectorDO selector = new SelectorDO();
        selector.setId("s");
        selector.setSelectorName("selector");
        selector.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        when(proxySelectorMapper.selectByDiscoveryId("d")).thenReturn(Collections.emptyList());
        when(selectorMapper.selectByDiscoveryId("d")).thenReturn(Collections.singletonList(selector));
        when(discoveryHandlerMapper.selectBySelectorId("s")).thenReturn(DiscoveryHandlerDO.builder().id("h").discoveryId("d").build());
        DiscoveryDO discovery = DiscoveryDO.builder().id("d").pluginName("divide").discoveryType("local").namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
        discoveryService.syncData(Collections.singletonList(discovery));
        verify(discoveryProcessor).createDiscovery(discovery);
        verify(discoveryProcessor).fetchAll(any(), any());
        when(discoveryMapper.selectAll()).thenReturn(Collections.singletonList(discovery));
        when(discoveryHandlerMapper.selectAll()).thenReturn(Collections.emptyList());
        when(discoveryRelMapper.selectAll()).thenReturn(Collections.emptyList());
        assertEquals(1, discoveryService.listAllData().size());
        when(discoveryMapper.selectAllByNamespaceId(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(Collections.emptyList());
        assertEquals(0, discoveryService.listAllDataByNamespaceId(SYS_DEFAULT_NAMESPACE_ID).size());
        discoveryService.syncData();
        discoveryService.syncDataByNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
    }

    @Test
    public void testSyncDataWithProxySelector() {
        final DiscoveryDO discovery = DiscoveryDO.builder().id("d").pluginName("divide").discoveryType("local").build();
        ProxySelectorDO proxySelector = ProxySelectorDO.builder().id("proxy").name("proxy-name")
                .pluginName("divide").namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
        DiscoveryHandlerDO handler = DiscoveryHandlerDO.builder().id("handler").discoveryId("d").build();
        when(proxySelectorMapper.selectByDiscoveryId("d")).thenReturn(Collections.singletonList(proxySelector));
        when(discoveryHandlerMapper.selectByProxySelectorId("proxy")).thenReturn(handler);
        when(selectorMapper.selectByDiscoveryId("d")).thenReturn(Collections.emptyList());
        discoveryService.syncData(Collections.singletonList(discovery));
        ArgumentCaptor<DiscoveryHandlerDTO> handlerCaptor = ArgumentCaptor.forClass(DiscoveryHandlerDTO.class);
        ArgumentCaptor<ProxySelectorDTO> selectorCaptor = ArgumentCaptor.forClass(ProxySelectorDTO.class);
        verify(discoveryProcessor).createProxySelector(handlerCaptor.capture(), selectorCaptor.capture());
        assertEquals("handler", handlerCaptor.getValue().getId());
        assertEquals("proxy", selectorCaptor.getValue().getId());
        assertEquals("proxy-name", selectorCaptor.getValue().getName());
        assertEquals(SYS_DEFAULT_NAMESPACE_ID, selectorCaptor.getValue().getNamespaceId());
        verify(discoveryProcessor).fetchAll(argThat(dto -> "handler".equals(dto.getId())), eq(selectorCaptor.getValue()));
    }

    @Test
    public void testListDataWithRelationsAndFindHandler() {
        DiscoveryDO discovery = DiscoveryDO.builder().id("d").discoveryName("name").discoveryType("local").build();
        DiscoveryHandlerDO handler = DiscoveryHandlerDO.builder().id("h").discoveryId("d").build();
        DiscoveryRelDO relation = DiscoveryRelDO.builder().id("r").discoveryHandlerId("h").pluginName("divide").build();
        when(discoveryMapper.selectAll()).thenReturn(Collections.singletonList(discovery));
        when(discoveryHandlerMapper.selectAll()).thenReturn(Collections.singletonList(handler));
        when(discoveryRelMapper.selectAll()).thenReturn(Collections.singletonList(relation));
        List<DiscoveryVO> all = discoveryService.listAllData();
        assertEquals("h", all.get(0).getDiscoveryHandler().getId());
        assertEquals("r", all.get(0).getDiscoveryRel().getId());
        when(discoveryHandlerMapper.selectBySelectorId("selector")).thenReturn(handler);
        assertEquals("h", discoveryService.findDiscoveryHandlerBySelectorId("selector").getId());
    }

    @Test
    public void testListDataByNamespaceWithHandlerAndRelation() {
        DiscoveryDO discovery = DiscoveryDO.builder().id("d").discoveryType("local").build();
        DiscoveryHandlerDO handler = DiscoveryHandlerDO.builder().id("h").discoveryId("d").build();
        DiscoveryRelDO relation = DiscoveryRelDO.builder().id("r").discoveryHandlerId("h").build();
        when(discoveryMapper.selectAllByNamespaceId("ns")).thenReturn(Collections.singletonList(discovery));
        when(discoveryHandlerMapper.selectByDiscoveryIds(any())).thenReturn(Collections.singletonList(handler));
        when(discoveryRelMapper.selectAll()).thenReturn(Collections.singletonList(relation));
        assertEquals("r", discoveryService.listAllDataByNamespaceId("ns").get(0).getDiscoveryRel().getId());
    }

    @Test
    public void testRegisterDefaultAndImport() {
        when(discoveryHandlerMapper.selectBySelectorId("s")).thenReturn(null);
        String handlerId = discoveryService.registerDefaultDiscovery("s", "divide", SYS_DEFAULT_NAMESPACE_ID);
        org.junit.jupiter.api.Assertions.assertNotNull(handlerId);
        verify(discoveryMapper).insertSelective(any());
        when(discoveryHandlerMapper.selectBySelectorId("existing")).thenReturn(DiscoveryHandlerDO.builder().id("h").build());
        assertEquals("h", discoveryService.registerDefaultDiscovery("existing", "divide", SYS_DEFAULT_NAMESPACE_ID));
        assertEquals(0, discoveryService.importData(Collections.emptyList()).getSuccessCount());
        DiscoveryDTO dto = new DiscoveryDTO();
        dto.setName("new");
        dto.setPluginName("divide");
        dto.setType("local");
        dto.setLevel("plugin");
        when(discoveryMapper.selectAll()).thenReturn(Collections.emptyList());
        when(discoveryMapper.insert(any())).thenReturn(1);
        ConfigImportResult result = discoveryService.importData(Collections.singletonList(dto));
        assertEquals(1, result.getSuccessCount());
        DiscoveryDO existing = DiscoveryDO.builder().pluginName("divide").discoveryName("duplicate").build();
        when(discoveryMapper.selectAll()).thenReturn(Collections.singletonList(existing));
        DiscoveryDTO duplicate = new DiscoveryDTO();
        duplicate.setPluginName("divide");
        duplicate.setName("duplicate");
        assertEquals(0, discoveryService.importData(Collections.singletonList(duplicate)).getSuccessCount());
        ConfigsImportContext context = new ConfigsImportContext();
        when(discoveryMapper.selectAllByNamespaceId("ns")).thenReturn(Collections.emptyList());
        assertEquals(1, discoveryService.importData("ns", Collections.singletonList(dto), context).getSuccessCount());
    }

    @Test
    public void testImportDataWithHandlerAndRelation() {
        DiscoveryDTO dto = new DiscoveryDTO();
        dto.setName("new");
        dto.setPluginName("divide");
        dto.setType("local");
        DiscoveryHandlerDTO handler = new DiscoveryHandlerDTO();
        handler.setId("handler");
        handler.setHandler("{}");
        dto.setDiscoveryHandler(handler);
        DiscoveryRelDTO relation = new DiscoveryRelDTO();
        relation.setId("relation");
        relation.setDiscoveryHandlerId("handler");
        relation.setSelectorId("selector");
        dto.setDiscoveryRel(relation);
        when(discoveryMapper.selectAll()).thenReturn(Collections.emptyList());
        when(discoveryMapper.insert(any())).thenReturn(1);
        assertEquals(1, discoveryService.importData(Collections.singletonList(dto)).getSuccessCount());
        ArgumentCaptor<DiscoveryHandlerDO> handlerCaptor = ArgumentCaptor.forClass(DiscoveryHandlerDO.class);
        ArgumentCaptor<DiscoveryRelDO> relationCaptor = ArgumentCaptor.forClass(DiscoveryRelDO.class);
        verify(discoveryHandlerMapper).insertSelective(handlerCaptor.capture());
        verify(discoveryRelMapper).insertSelective(relationCaptor.capture());
        assertEquals(dto.getId(), handlerCaptor.getValue().getDiscoveryId());
        assertEquals("handler", handlerCaptor.getValue().getId());
        assertEquals("{}", handlerCaptor.getValue().getHandler());
        assertEquals("relation", relationCaptor.getValue().getId());
        assertEquals("handler", relationCaptor.getValue().getDiscoveryHandlerId());
        assertEquals("selector", relationCaptor.getValue().getSelectorId());
    }

    @Test
    public void testNamespaceImportMapsHandlerAndSelectorIds() {
        DiscoveryDTO dto = new DiscoveryDTO();
        dto.setName("new");
        dto.setPluginName("divide");
        dto.setType("local");
        DiscoveryHandlerDTO handler = new DiscoveryHandlerDTO();
        handler.setId("old-handler");
        dto.setDiscoveryHandler(handler);
        DiscoveryRelDTO relation = new DiscoveryRelDTO();
        relation.setSelectorId("old-selector");
        relation.setProxySelectorId("old-proxy");
        dto.setDiscoveryRel(relation);
        ConfigsImportContext context = new ConfigsImportContext();
        context.getSelectorIdMapping().put("old-selector", "new-selector");
        context.getProxySelectorIdMapping().put("old-proxy", "new-proxy");
        when(discoveryMapper.selectAllByNamespaceId("ns")).thenReturn(Collections.emptyList());
        when(discoveryMapper.insert(any())).thenReturn(1);
        assertEquals(1, discoveryService.importData("ns", Collections.singletonList(dto), context).getSuccessCount());
        ArgumentCaptor<DiscoveryHandlerDO> handlerCaptor = ArgumentCaptor.forClass(DiscoveryHandlerDO.class);
        ArgumentCaptor<DiscoveryRelDO> relationCaptor = ArgumentCaptor.forClass(DiscoveryRelDO.class);
        verify(discoveryHandlerMapper).insertSelective(handlerCaptor.capture());
        verify(discoveryRelMapper).insertSelective(relationCaptor.capture());
        String newHandlerId = context.getDiscoveryHandlerIdMapping().get("old-handler");
        assertNotNull(newHandlerId);
        assertEquals(newHandlerId, handlerCaptor.getValue().getId());
        assertEquals(dto.getId(), handlerCaptor.getValue().getDiscoveryId());
        assertEquals(newHandlerId, relationCaptor.getValue().getDiscoveryHandlerId());
        assertEquals("new-selector", relationCaptor.getValue().getSelectorId());
        assertEquals("new-proxy", relationCaptor.getValue().getProxySelectorId());
        assertEquals("ns", dto.getNamespaceId());
    }

    @Test
    public void testUpdateReturnsNullWhenNoRecordIsUpdated() {
        DiscoveryDTO dto = new DiscoveryDTO();
        dto.setId("missing");
        when(discoveryMapper.updateSelective(any())).thenReturn(0);
        assertNull(discoveryService.createOrUpdate(dto));
        verifyNoInteractions(discoveryProcessor);
    }

    @Test
    public void testFindDiscoveryHandlerWhenSelectorIsNotBound() {
        assertNull(discoveryService.findDiscoveryHandlerBySelectorId("unbound"));
        verify(discoveryHandlerMapper).selectBySelectorId("unbound");
    }

    @Test
    public void testDiscoveryLookupMapsRecord() {
        DiscoveryDO discovery = DiscoveryDO.builder().id("d").discoveryType("local")
                .pluginName("divide").namespaceId("ns").discoveryLevel("0").serverList("localhost:8848").props("{}").build();
        when(discoveryMapper.selectByPluginNameAndLevelAndNamespaceId("divide", "0", "ns")).thenReturn(discovery);
        DiscoveryVO result = discoveryService.discovery("divide", "0", "ns");
        assertEquals("d", result.getId());
        assertEquals("local", result.getDiscoveryType());
        assertEquals("divide", result.getPluginName());
        assertEquals("ns", result.getNamespaceId());
        assertEquals("0", result.getLevel());
        assertEquals("localhost:8848", result.getServerList());
        assertEquals("{}", result.getProps());
    }

    @Test
    public void testNamespaceImportRejectsDuplicateName() {
        DiscoveryDTO dto = new DiscoveryDTO();
        dto.setName("duplicate");
        dto.setPluginName("divide");
        when(discoveryMapper.selectAllByNamespaceId("ns")).thenReturn(Collections.singletonList(
                DiscoveryDO.builder().pluginName("divide").discoveryName("duplicate").build()));
        ConfigImportResult result = discoveryService.importData("ns", Collections.singletonList(dto), new ConfigsImportContext());
        assertEquals(0, result.getSuccessCount());
        assertEquals("import fail discovery: duplicate", result.getFailMessage());
        verify(discoveryMapper, never()).insert(any());
        verifyNoInteractions(discoveryHandlerMapper, discoveryRelMapper, discoveryProcessor);
    }
}
