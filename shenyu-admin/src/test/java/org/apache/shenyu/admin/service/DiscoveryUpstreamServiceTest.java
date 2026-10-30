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

import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.exception.ValidFailException;
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
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.service.impl.DiscoveryUpstreamServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import org.springframework.test.util.ReflectionTestUtils;

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
    void testHasDiscoveryHandlerDistinguishesEmptyDiscoveryFromLegacyRegistration() {
        when(discoveryHandlerMapper.selectBySelectorId("managed")).thenReturn(new DiscoveryHandlerDO());

        org.junit.jupiter.api.Assertions.assertTrue(discoveryUpstreamService.hasDiscoveryHandler("managed"));
        org.junit.jupiter.api.Assertions.assertFalse(discoveryUpstreamService.hasDiscoveryHandler("legacy"));
    }

    @Test
    public void testMarkDrainingMergesPropsAndLeavesStatusUntouched() {
        final DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId("handler-1");
        when(discoveryHandlerMapper.selectBySelectorId("selector-1")).thenReturn(handler);
        final DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setDiscoveryHandlerId("handler-1");
        upstream.setUpstreamUrl("127.0.0.1:8080");
        upstream.setProps("{\"warmup\":\"20\"}");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl("handler-1", "127.0.0.1:8080")).thenReturn(upstream);

        discoveryUpstreamService.markDrainingBySelectorIdAndUrl("selector-1", "127.0.0.1:8080", true);

        final ArgumentCaptor<String> propsCaptor = ArgumentCaptor.forClass(String.class);
        verify(discoveryUpstreamMapper).updatePropsByUrl(eq("handler-1"), eq("127.0.0.1:8080"), propsCaptor.capture());
        verify(discoveryUpstreamMapper, never()).updateStatusByUrl(anyString(), anyString(), org.mockito.ArgumentMatchers.anyInt());
        final Map<String, Object> merged = GsonUtils.getInstance().convertToMap(propsCaptor.getValue());
        assertEquals(Boolean.TRUE, merged.get("draining"));
        assertEquals("20", merged.get("warmup"));

        // idempotent: an existing marker is overwritten with the same value, not duplicated
        upstream.setProps(propsCaptor.getValue());
        discoveryUpstreamService.markDrainingBySelectorIdAndUrl("selector-1", "127.0.0.1:8080", true);
        verify(discoveryUpstreamMapper, times(2)).updatePropsByUrl(anyString(), anyString(), propsCaptor.capture());
        assertEquals(Boolean.TRUE, GsonUtils.getInstance().convertToMap(propsCaptor.getValue()).get("draining"));
    }

    @Test
    public void testMarkDrainingFalseReleasesTheMarker() {
        final DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId("handler-1");
        when(discoveryHandlerMapper.selectBySelectorId("selector-1")).thenReturn(handler);
        final DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setDiscoveryHandlerId("handler-1");
        upstream.setUpstreamUrl("127.0.0.1:8080");
        upstream.setProps("{\"draining\":true,\"warmup\":\"20\"}");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl("handler-1", "127.0.0.1:8080")).thenReturn(upstream);

        discoveryUpstreamService.markDrainingBySelectorIdAndUrl("selector-1", "127.0.0.1:8080", false);

        final ArgumentCaptor<String> propsCaptor = ArgumentCaptor.forClass(String.class);
        verify(discoveryUpstreamMapper).updatePropsByUrl(eq("handler-1"), eq("127.0.0.1:8080"), propsCaptor.capture());
        final Map<String, Object> merged = GsonUtils.getInstance().convertToMap(propsCaptor.getValue());
        assertEquals(Boolean.FALSE, merged.get("draining"));
        assertEquals("20", merged.get("warmup"));
    }

    @Test
    public void testMarkDrainingIgnoresUnknownHandlerOrUrl() {
        when(discoveryHandlerMapper.selectBySelectorId("selector-1")).thenReturn(null);
        discoveryUpstreamService.markDrainingBySelectorIdAndUrl("selector-1", "127.0.0.1:8080", true);
        verify(discoveryUpstreamMapper, never()).updatePropsByUrl(anyString(), anyString(), anyString());

        final DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId("handler-1");
        when(discoveryHandlerMapper.selectBySelectorId("selector-2")).thenReturn(handler);
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl("handler-1", "127.0.0.1:8080")).thenReturn(null);
        discoveryUpstreamService.markDrainingBySelectorIdAndUrl("selector-2", "127.0.0.1:8080", true);
        verify(discoveryUpstreamMapper, never()).updatePropsByUrl(anyString(), anyString(), anyString());
    }

    @Test
    public void testMarkNotReadyMergesPropsAndLeavesStatusUntouched() {
        final DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId("handler-1");
        when(discoveryHandlerMapper.selectBySelectorId("selector-1")).thenReturn(handler);
        final DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setDiscoveryHandlerId("handler-1");
        upstream.setUpstreamUrl("127.0.0.1:8080");
        upstream.setProps("{\"warmup\":\"20\"}");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl("handler-1", "127.0.0.1:8080")).thenReturn(upstream);

        discoveryUpstreamService.markNotReadyBySelectorIdAndUrl("selector-1", "127.0.0.1:8080", true);

        final ArgumentCaptor<String> propsCaptor = ArgumentCaptor.forClass(String.class);
        verify(discoveryUpstreamMapper).updatePropsByUrl(eq("handler-1"), eq("127.0.0.1:8080"), propsCaptor.capture());
        verify(discoveryUpstreamMapper, never()).updateStatusByUrl(anyString(), anyString(), org.mockito.ArgumentMatchers.anyInt());
        final Map<String, Object> merged = GsonUtils.getInstance().convertToMap(propsCaptor.getValue());
        assertEquals(Boolean.TRUE, merged.get("notReady"));
        assertEquals("20", merged.get("warmup"));
    }

    @Test
    public void testMarkReadyReleasesBothMarkers() {
        final DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId("handler-1");
        when(discoveryHandlerMapper.selectBySelectorId("selector-1")).thenReturn(handler);
        final DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setDiscoveryHandlerId("handler-1");
        upstream.setUpstreamUrl("127.0.0.1:8080");
        upstream.setProps("{\"draining\":true,\"notReady\":true,\"warmup\":\"20\"}");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl("handler-1", "127.0.0.1:8080")).thenReturn(upstream);

        discoveryUpstreamService.markReadyBySelectorIdAndUrl("selector-1", "127.0.0.1:8080");

        final ArgumentCaptor<String> propsCaptor = ArgumentCaptor.forClass(String.class);
        verify(discoveryUpstreamMapper).updatePropsByUrl(eq("handler-1"), eq("127.0.0.1:8080"), propsCaptor.capture());
        final Map<String, Object> merged = GsonUtils.getInstance().convertToMap(propsCaptor.getValue());
        Assertions.assertNull(merged.get("draining"));
        Assertions.assertNull(merged.get("notReady"));
        assertEquals("20", merged.get("warmup"));
    }

    @Test
    public void testReadinessMarkersIgnoreUnknownHandlerOrUrl() {
        when(discoveryHandlerMapper.selectBySelectorId("selector-1")).thenReturn(null);
        discoveryUpstreamService.markNotReadyBySelectorIdAndUrl("selector-1", "127.0.0.1:8080", true);
        discoveryUpstreamService.markReadyBySelectorIdAndUrl("selector-1", "127.0.0.1:8080");
        verify(discoveryUpstreamMapper, never()).updatePropsByUrl(anyString(), anyString(), anyString());
    }

    @Test
    public void testHeartbeatDoesNotTouchDrainingProps() {
        // the drain marker lives in props; the heartbeat path only flips the status column,
        // so a draining instance must stay excluded even though it keeps beating (#8042)
        final DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId("handler-1");
        when(discoveryHandlerMapper.selectBySelectorId("selector-1")).thenReturn(handler);

        discoveryUpstreamService.changeStatusBySelectorIdAndUrl("selector-1", "127.0.0.1:8080", Boolean.TRUE);

        verify(discoveryUpstreamMapper).updateStatusByUrl("handler-1", "127.0.0.1:8080", 0);
        verify(discoveryUpstreamMapper, never()).updatePropsByUrl(anyString(), anyString(), anyString());
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
    public void testDeleteNotifiesAffectedHandlersWithRemainingUpstreams() {
        final DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setId("upstream-1");
        upstream.setDiscoveryHandlerId("handler-1");
        when(discoveryUpstreamMapper.selectByIds(Collections.singletonList("upstream-1"))).thenReturn(Collections.singletonList(upstream));
        when(discoveryUpstreamMapper.deleteByIds(any())).thenReturn(1);
        // fetchAll dependencies for handler-1
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("handler-1")).thenReturn(Collections.emptyList());
        final DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId("handler-1");
        when(discoveryHandlerMapper.selectById("handler-1")).thenReturn(handler);
        when(proxySelectorMapper.selectByHandlerId("handler-1")).thenReturn(null);
        final SelectorDO selector = new SelectorDO();
        selector.setId("selector-1");
        selector.setPluginId("plugin-1");
        when(selectorMapper.selectByDiscoveryHandlerId("handler-1")).thenReturn(selector);
        final PluginDO plugin = new PluginDO();
        plugin.setName("divide");
        when(pluginMapper.selectById("plugin-1")).thenReturn(plugin);
        handler.setDiscoveryId("discovery-1");
        final DiscoveryDO discovery = new DiscoveryDO();
        discovery.setId("discovery-1");
        discovery.setDiscoveryType("local");
        when(discoveryMapper.selectById("discovery-1")).thenReturn(discovery);
        when(discoveryProcessorHolder.chooseProcessor(anyString())).thenReturn(discoveryProcessor);

        String delete = discoveryUpstreamService.delete(Collections.singletonList("upstream-1"));

        assertEquals(ShenyuResultMessage.DELETE_SUCCESS, delete);
        // the empty remaining snapshot must be published for the affected handler
        verify(discoveryProcessor, Mockito.timeout(1000).times(1)).changeUpstream(any(), eq(Collections.emptyList()));
    }

    @ParameterizedTest
    @ValueSource(strings = {"handler", "selector", "plugin", "discovery"})
    void rejectsMissingDiscoveryBindingsWithDomainErrors(final String missing) {
        if (!"handler".equals(missing)) {
            when(discoveryHandlerMapper.selectById("123")).thenReturn(buildDiscoveryHandlerDO());
        }
        if ("plugin".equals(missing) || "discovery".equals(missing)) {
            when(selectorMapper.selectByDiscoveryHandlerId("123")).thenReturn(buildSelectorDO());
        }
        if ("discovery".equals(missing)) {
            when(pluginMapper.selectById(any())).thenReturn(buildPluginDO());
        }
        ValidFailException error = Assertions.assertThrows(ValidFailException.class,
                () -> ReflectionTestUtils.invokeMethod(discoveryUpstreamService, "fetchAll", "123"));
        Assertions.assertTrue(error.getMessage().toLowerCase(Locale.ROOT).contains(missing));
        verifyNoInteractions(discoveryProcessorHolder, discoveryProcessor);
    }

    @Test
    public void testListAll() {
        List<DiscoveryHandlerDO> list = Collections.singletonList(buildDiscoveryHandlerDO());

        when(discoveryHandlerMapper.selectAll()).thenReturn(list);
        DiscoveryRelDO relation = buildDiscoveryRelDO();
        relation.setDiscoveryHandlerId("123");
        relation.setProxySelectorId("selector_1");
        when(discoveryRelMapper.selectByDiscoveryHandlerIds(any())).thenReturn(Collections.singletonList(relation));
        when(proxySelectorMapper.selectByIds(any())).thenReturn(Collections.singletonList(buildProxySelectorDO()));
        List<DiscoverySyncData> dataList = discoveryUpstreamService.listAll();
        assertEquals(dataList.size(), list.size());
    }

    @ParameterizedTest
    @ValueSource(ints = {20, 501})
    void batchesQueriesAndPreservesHandlerOrder(final int count) {
        List<DiscoveryHandlerDO> handlers = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            DiscoveryHandlerDO handler = buildDiscoveryHandlerDO();
            handler.setId(String.valueOf(i));
            handlers.add(handler);
        }
        when(discoveryHandlerMapper.selectAll()).thenReturn(handlers);
        when(discoveryRelMapper.selectByDiscoveryHandlerIds(any())).thenAnswer(invocation -> {
            List<String> ids = invocation.getArgument(0);
            Assertions.assertTrue(ids.size() <= 500);
            return ids.stream().map(id -> {
                DiscoveryRelDO relation = buildDiscoveryRelDO();
                relation.setDiscoveryHandlerId(id);
                if (Integer.parseInt(id) % 2 == 0) {
                    relation.setSelectorId(id);
                } else {
                    relation.setProxySelectorId(id);
                }
                return relation;
            }).collect(Collectors.toList());
        });
        when(selectorMapper.selectByIdSet(any())).thenAnswer(invocation -> {
            java.util.Set<String> ids = invocation.getArgument(0);
            return ids.stream().map(id -> {
                SelectorDO selector = buildSelectorDO();
                selector.setId(id);
                return selector;
            }).collect(Collectors.toList());
        });
        when(proxySelectorMapper.selectByIds(any())).thenAnswer(invocation -> {
            List<String> ids = invocation.getArgument(0);
            return ids.stream().map(id -> {
                ProxySelectorDO proxy = buildProxySelectorDO();
                proxy.setId(id);
                return proxy;
            }).collect(Collectors.toList());
        });
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerIds(any())).thenAnswer(invocation -> {
            List<String> ids = invocation.getArgument(0);
            return ids.stream().map(id -> buildDiscoveryUpstreamDO("u-" + id, id, "localhost:8080")).collect(Collectors.toList());
        });
        List<DiscoverySyncData> data = discoveryUpstreamService.listAll();
        assertEquals(count, data.size());
        for (int i = 0; i < count; i++) {
            assertEquals(String.valueOf(i), data.get(i).getSelectorId());
            assertEquals(1, data.get(i).getUpstreamDataList().size());
        }
        int batches = (count + 499) / 500;
        verify(discoveryHandlerMapper).selectAll();
        verify(discoveryRelMapper, times(batches)).selectByDiscoveryHandlerIds(any());
        verify(discoveryUpstreamMapper, times(batches)).selectByDiscoveryHandlerIds(any());
        verify(selectorMapper, times(batches)).selectByIdSet(any());
        verify(proxySelectorMapper, times(count / 500 + (count % 500 > 1 ? 1 : 0))).selectByIds(any());
        verify(discoveryRelMapper, never()).selectByDiscoveryHandlerId(any());
        verify(discoveryUpstreamMapper, never()).selectByDiscoveryHandlerId(any());
        verify(selectorMapper, never()).selectById(any());
        verify(proxySelectorMapper, never()).selectById(any());
    }

    @Test
    public void duplicateAndMissingBindingsDoNotAbortOtherHandlers() {
        List<DiscoveryHandlerDO> handlers = new ArrayList<>();
        for (String id : List.of("valid", "missing-relation", "missing-selector", "missing-proxy")) {
            DiscoveryHandlerDO handler = buildDiscoveryHandlerDO();
            handler.setId(id);
            handlers.add(handler);
        }
        DiscoveryRelDO valid = buildDiscoveryRelDO();
        valid.setDiscoveryHandlerId("valid");
        valid.setSelectorId("selector_1");
        DiscoveryRelDO duplicate = buildDiscoveryRelDO();
        duplicate.setDiscoveryHandlerId("valid");
        duplicate.setSelectorId("missing");
        DiscoveryRelDO missingSelector = buildDiscoveryRelDO();
        missingSelector.setDiscoveryHandlerId("missing-selector");
        missingSelector.setSelectorId("missing");
        DiscoveryRelDO missingProxy = buildDiscoveryRelDO();
        missingProxy.setDiscoveryHandlerId("missing-proxy");
        missingProxy.setProxySelectorId("missing");
        when(discoveryHandlerMapper.selectAll()).thenReturn(handlers);
        when(discoveryRelMapper.selectByDiscoveryHandlerIds(any())).thenReturn(List.of(valid, duplicate, missingSelector, missingProxy));
        when(selectorMapper.selectByIdSet(any())).thenReturn(List.of(buildSelectorDO()));
        when(proxySelectorMapper.selectByIds(any())).thenReturn(Collections.emptyList());
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerIds(any())).thenReturn(Collections.emptyList());

        List<DiscoverySyncData> data = discoveryUpstreamService.listAll();

        assertEquals(1, data.size());
        assertEquals("selector_1", data.get(0).getSelectorId());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void batchesNamespaceSyncAndRetainsNamespace(final boolean proxy) {
        final DiscoveryRelDO relation = buildDiscoveryRelDO();
        relation.setDiscoveryHandlerId("123");
        when(discoveryHandlerMapper.selectAllByNamespaceId("tenant")).thenReturn(List.of(buildDiscoveryHandlerDO()));
        if (proxy) {
            relation.setSelectorId("");
            relation.setProxySelectorId("selector_1");
            ProxySelectorDO selector = buildProxySelectorDO();
            selector.setNamespaceId("tenant");
            when(proxySelectorMapper.selectByIds(List.of("selector_1"))).thenReturn(List.of(selector));
        } else {
            relation.setSelectorId("selector_1");
            SelectorDO selector = buildSelectorDO();
            selector.setNamespaceId("tenant");
            when(selectorMapper.selectByIdSet(Set.of("selector_1"))).thenReturn(List.of(selector));
        }
        when(discoveryRelMapper.selectByDiscoveryHandlerIds(List.of("123"))).thenReturn(List.of(relation));
        List<DiscoverySyncData> data = discoveryUpstreamService.listAllByNamespaceId("tenant");
        assertEquals(1, data.size());
        assertEquals("tenant", data.get(0).getNamespaceId());
        verify(discoveryHandlerMapper, never()).selectAll();
        verify(discoveryRelMapper, never()).selectByDiscoveryHandlerId(any());
    }

    @Test
    public void testListAllSkipsOrphansAndKeepsValidBindings() {
        DiscoveryHandlerDO noRelation = buildDiscoveryHandlerDO();
        noRelation.setId("no-relation");
        DiscoveryHandlerDO missingSelector = buildDiscoveryHandlerDO();
        missingSelector.setId("missing-selector");
        DiscoveryHandlerDO missingProxy = buildDiscoveryHandlerDO();
        missingProxy.setId("missing-proxy");
        DiscoveryHandlerDO validSelector = buildDiscoveryHandlerDO();
        validSelector.setId("valid-selector");
        DiscoveryHandlerDO validProxy = buildDiscoveryHandlerDO();
        validProxy.setId("valid-proxy");
        when(discoveryHandlerMapper.selectAll()).thenReturn(List.of(noRelation, missingSelector, validSelector, missingProxy, validProxy));
        DiscoveryRelDO staleSelectorRel = buildDiscoveryRelDO();
        staleSelectorRel.setSelectorId("deleted-selector");
        staleSelectorRel.setDiscoveryHandlerId("missing-selector");
        DiscoveryRelDO staleProxyRel = buildDiscoveryRelDO();
        staleProxyRel.setProxySelectorId("deleted-proxy");
        staleProxyRel.setDiscoveryHandlerId("missing-proxy");
        DiscoveryRelDO selectorRel = buildDiscoveryRelDO();
        selectorRel.setSelectorId("selector_1");
        selectorRel.setDiscoveryHandlerId("valid-selector");
        when(selectorMapper.selectByIdSet(Set.of("selector_1", "deleted-selector"))).thenReturn(List.of(buildSelectorDO()));
        DiscoveryRelDO proxyRel = buildDiscoveryRelDO();
        proxyRel.setProxySelectorId("proxy_1");
        proxyRel.setDiscoveryHandlerId("valid-proxy");
        when(discoveryRelMapper.selectByDiscoveryHandlerIds(any())).thenReturn(List.of(staleSelectorRel, staleProxyRel, selectorRel, proxyRel));
        ProxySelectorDO proxy = buildProxySelectorDO();
        proxy.setId("proxy_1");
        when(proxySelectorMapper.selectByIds(List.of("deleted-proxy", "proxy_1"))).thenReturn(List.of(proxy));
        List<DiscoverySyncData> result = discoveryUpstreamService.listAll();
        assertEquals(2, result.size());
        assertEquals("selector_1", result.get(0).getSelectorId());
        assertEquals("proxy_1", result.get(1).getSelectorId());
        verify(discoveryUpstreamMapper, never()).selectByDiscoveryHandlerId("no-relation");
        verify(discoveryUpstreamMapper, never()).selectByDiscoveryHandlerId("missing-selector");
        verify(discoveryUpstreamMapper, never()).selectByDiscoveryHandlerId("missing-proxy");
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
    public void testImportDataWithNamespaceAndContext() {
        String namespace = "ns1";
        String oldHandlerId = "old_handler_id";
        String newHandlerId = "new_handler_id";

        ConfigsImportContext context = new ConfigsImportContext();
        context.getDiscoveryHandlerIdMapping().put(oldHandlerId, newHandlerId);

        List<DiscoveryUpstreamDO> existingList = Collections.singletonList(buildDiscoveryUpstreamDO("", newHandlerId, "url1"));
        when(discoveryUpstreamMapper.selectByNamespaceId(namespace)).thenReturn(existingList);
        given(this.discoveryUpstreamMapper.insert(any())).willReturn(1);

        final List<DiscoveryUpstreamDTO> upstreamDTOList = Collections.singletonList(buildDiscoveryUpstreamDTO("", oldHandlerId, "url2"));
        ConfigImportResult successResult = this.discoveryUpstreamService.importData(namespace, upstreamDTOList, context);
        assertNotNull(successResult);
        Assertions.assertEquals(1, successResult.getSuccessCount());

        final List<DiscoveryUpstreamDTO> duplicateDTOList = Collections.singletonList(buildDiscoveryUpstreamDTO("", oldHandlerId, "url1"));
        ConfigImportResult duplicateResult = this.discoveryUpstreamService.importData(namespace, duplicateDTOList, context);
        assertNotNull(duplicateResult);
        Assertions.assertEquals(0, duplicateResult.getSuccessCount());
    }

    @Test
    public void testImportDataWithNamespaceUnmappedHandlerId() {
        String namespace = "ns1";
        String unmappedHandlerId = "unmapped_handler_id";

        ConfigsImportContext context = new ConfigsImportContext();

        List<DiscoveryUpstreamDO> existingList = Collections.singletonList(buildDiscoveryUpstreamDO("", unmappedHandlerId, "url1"));
        when(discoveryUpstreamMapper.selectByNamespaceId(namespace)).thenReturn(existingList);
        given(this.discoveryUpstreamMapper.insert(any())).willReturn(1);

        final List<DiscoveryUpstreamDTO> upstreamDTOList = Collections.singletonList(buildDiscoveryUpstreamDTO("", unmappedHandlerId, "url2"));
        ConfigImportResult successResult = this.discoveryUpstreamService.importData(namespace, upstreamDTOList, context);
        assertNotNull(successResult);
        Assertions.assertEquals(1, successResult.getSuccessCount());

        final List<DiscoveryUpstreamDTO> duplicateDTOList = Collections.singletonList(buildDiscoveryUpstreamDTO("", unmappedHandlerId, "url1"));
        ConfigImportResult duplicateResult = this.discoveryUpstreamService.importData(namespace, duplicateDTOList, context);
        assertNotNull(duplicateResult);
        Assertions.assertEquals(0, duplicateResult.getSuccessCount());
    }

    @Test
    public void testImportDataSkipsNullDiscoveryHandlerId() {
        when(discoveryUpstreamMapper.selectAll()).thenReturn(Collections.emptyList());

        final List<DiscoveryUpstreamDTO> upstreamDTOList = Collections.singletonList(buildDiscoveryUpstreamDTO("", null, "url-null"));
        ConfigImportResult result = this.discoveryUpstreamService.importData(upstreamDTOList);
        assertNotNull(result);
        Assertions.assertEquals(0, result.getSuccessCount());
        verify(discoveryUpstreamMapper, never()).insert(any());
    }

    @Test
    public void testImportDataWithNamespaceSkipsNullDiscoveryHandlerId() {
        String namespace = "ns1";
        ConfigsImportContext context = new ConfigsImportContext();
        when(discoveryUpstreamMapper.selectByNamespaceId(namespace)).thenReturn(Collections.emptyList());

        final List<DiscoveryUpstreamDTO> upstreamDTOList = Collections.singletonList(buildDiscoveryUpstreamDTO("", null, "url-null"));
        ConfigImportResult result = this.discoveryUpstreamService.importData(namespace, upstreamDTOList, context);
        assertNotNull(result);
        Assertions.assertEquals(0, result.getSuccessCount());
        verify(discoveryUpstreamMapper, never()).insert(any());
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
        verify(discoveryProcessor).changeUpstream(any(), any());
    }

    @Test
    public void testUpdateBatchPublishesOnlyAfterCommit() {
        when(discoveryProcessorHolder.chooseProcessor(anyString())).thenReturn(discoveryProcessor);
        when(selectorMapper.selectByDiscoveryHandlerId(any())).thenReturn(buildSelectorDO());
        when(discoveryHandlerMapper.selectById(any())).thenReturn(buildDiscoveryHandlerDO());
        when(pluginMapper.selectById(any())).thenReturn(buildPluginDO());
        when(discoveryMapper.selectById(any())).thenReturn(buildDiscoveryDO());
        TransactionSynchronizationManager.initSynchronization();
        try {
            discoveryUpstreamService.updateBatch("123", Collections.singletonList(buildDiscoveryUpstreamDTO("")));
            verifyNoInteractions(discoveryProcessor);
            verify(discoveryUpstreamMapper, never()).selectByDiscoveryHandlerId(any());
            TransactionSynchronizationManager.getSynchronizations().forEach(TransactionSynchronization::afterCommit);
            verify(discoveryProcessor).changeUpstream(any(), any());
        } finally {
            TransactionSynchronizationManager.clearSynchronization();
        }
    }

    @Test
    public void testRolledBackUpdateBatchDoesNotPublish() {
        TransactionSynchronizationManager.initSynchronization();
        try {
            discoveryUpstreamService.updateBatch("123", Collections.emptyList());
            TransactionSynchronizationManager.getSynchronizations().forEach(sync -> sync.afterCompletion(TransactionSynchronization.STATUS_ROLLED_BACK));
            verifyNoInteractions(discoveryProcessor);
            verify(discoveryUpstreamMapper, never()).selectByDiscoveryHandlerId(any());
        } finally {
            TransactionSynchronizationManager.clearSynchronization();
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"labels\":{}}", "{\"labels\":{\"release\":\"canary\"}}", "{\"warmup\":20}", "{}", "{\"warmup\":20,\"extension\":{\"x\":1},\"labels\":{}}"})
    public void testSingleUpdateReplacesPropsAndSynchronizesLabels(final String props) {
        AtomicReference<DiscoveryUpstreamDO> stored = new AtomicReference<>(existingLabeledUpstream());
        configureSync();
        when(discoveryUpstreamMapper.update(any())).thenAnswer(invocation -> {
            stored.set(invocation.getArgument(0));
            return 1;
        });
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("123")).thenAnswer(invocation -> Collections.singletonList(stored.get()));
        DiscoveryUpstreamDTO dto = buildDiscoveryUpstreamDTO("123", "123", "localhost:8080");
        dto.setProtocol("http://");
        dto.setProps(props);
        discoveryUpstreamService.createOrUpdate(dto);
        assertEquals(props, stored.get().getProps());
        ArgumentCaptor<List<DiscoveryUpstreamDTO>> synced = ArgumentCaptor.forClass(List.class);
        verify(discoveryProcessor).changeUpstream(any(), synced.capture());
        assertEquals(stored.get().getProps(), synced.getValue().get(0).getProps());
        when(discoveryHandlerMapper.selectBySelectorId("selector_1")).thenReturn(buildDiscoveryHandlerDO());
        assertEquals(stored.get().getProps(), discoveryUpstreamService.findBySelectorId("selector_1").get(0).getProps());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"labels\":{}}", "{\"labels\":{\"release\":\"canary\"}}", "{\"warmup\":20}", "{}", "{\"warmup\":20,\"extension\":{\"x\":1},\"labels\":{}}"})
    public void testBatchUpdateReplacesPropsAndSynchronizesLabels(final String props) {
        AtomicReference<DiscoveryUpstreamDO> stored = new AtomicReference<>(existingLabeledUpstream());
        configureSync();
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("123")).thenAnswer(invocation -> Collections.singletonList(stored.get()));
        when(discoveryUpstreamMapper.insert(any())).thenAnswer(invocation -> {
            stored.set(invocation.getArgument(0));
            return 1;
        });
        DiscoveryUpstreamDTO dto = buildDiscoveryUpstreamDTO("", "123", "localhost:8080");
        dto.setProtocol("http://");
        dto.setProps(props);
        discoveryUpstreamService.updateBatch("123", Collections.singletonList(dto));
        assertEquals(props, stored.get().getProps());
        ArgumentCaptor<List<DiscoveryUpstreamDTO>> synced = ArgumentCaptor.forClass(List.class);
        verify(discoveryProcessor).changeUpstream(any(), synced.capture());
        assertEquals(stored.get().getProps(), synced.getValue().get(0).getProps());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testBatchReplacementRemovesOmittedInstances(final boolean removeAll) {
        List<DiscoveryUpstreamDO> stored = new ArrayList<>();
        stored.add(existingLabeledUpstream());
        stored.add(buildDiscoveryUpstreamDO("other", "123", "localhost:8081"));
        configureSync();
        doAnswer(invocation -> {
            stored.clear();
            return 2;
        }).when(discoveryUpstreamMapper).deleteByDiscoveryHandlerId("123");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("123")).thenAnswer(invocation -> new ArrayList<>(stored));
        List<DiscoveryUpstreamDTO> submitted = Collections.emptyList();
        if (!removeAll) {
            DiscoveryUpstreamDTO remaining = buildDiscoveryUpstreamDTO("", "123", "localhost:8080");
            remaining.setProtocol("http://");
            remaining.setProps("{}");
            submitted = Collections.singletonList(remaining);
            when(discoveryUpstreamMapper.insert(any())).thenAnswer(invocation -> {
                stored.add(invocation.getArgument(0));
                return 1;
            });
        }
        discoveryUpstreamService.updateBatch("123", submitted);
        assertEquals(removeAll ? 0 : 1, stored.size());
        ArgumentCaptor<List<DiscoveryUpstreamDTO>> synced = ArgumentCaptor.forClass(List.class);
        verify(discoveryProcessor).changeUpstream(any(), synced.capture());
        assertEquals(stored.size(), synced.getValue().size());
        if (!removeAll) {
            assertEquals("localhost:8080", synced.getValue().get(0).getUrl());
            assertEquals("{}", synced.getValue().get(0).getProps());
        } else {
            verify(discoveryUpstreamMapper, never()).insert(any());
        }
    }

    @Test
    public void testNativeUpdateReplacesSubmittedProps() {
        DiscoveryUpstreamDTO dto = buildDiscoveryUpstreamDTO("123");
        dto.setProps("{\"warmup\":20}");
        discoveryUpstreamService.nativeCreateOrUpdate(dto);
        ArgumentCaptor<DiscoveryUpstreamDO> saved = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(saved.capture());
        assertEquals(dto.getProps(), saved.getValue().getProps());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"labels\":null}", "{\"labels\":[]}", "{\"labels\":{\"release\":123}}", "[]"})
    public void testInvalidPropsNeverWriteOrSynchronize(final String props) {
        DiscoveryUpstreamDTO dto = buildDiscoveryUpstreamDTO("");
        dto.setProps(props);
        assertThrows(ShenyuAdminException.class, () -> discoveryUpstreamService.createOrUpdate(dto));
        DiscoveryUpstreamDTO valid = buildDiscoveryUpstreamDTO("");
        valid.setProps("{\"labels\":{}}");
        assertThrows(ShenyuAdminException.class, () -> discoveryUpstreamService.createOrUpdateBatch(Arrays.asList(valid, dto)));
        assertThrows(ShenyuAdminException.class, () -> discoveryUpstreamService.updateBatch("123", Arrays.asList(valid, dto)));
        assertThrows(ShenyuAdminException.class, () -> discoveryUpstreamService.importData(Arrays.asList(valid, dto)));
        assertThrows(ShenyuAdminException.class, () -> discoveryUpstreamService.nativeCreateOrUpdate(dto));
        dto.setId("123");
        assertThrows(ShenyuAdminException.class, () -> discoveryUpstreamService.createOrUpdate(dto));
        verify(discoveryUpstreamMapper, never()).insert(any());
        verify(discoveryUpstreamMapper, never()).update(any());
        verify(discoveryUpstreamMapper, never()).updateSelective(any());
        verify(discoveryUpstreamMapper, never()).deleteByDiscoveryHandlerId(any());
        verifyNoInteractions(discoveryProcessor, discoveryProcessorHolder);
    }

    private DiscoveryUpstreamDO existingLabeledUpstream() {
        DiscoveryUpstreamDO upstream = buildDiscoveryUpstreamDO("123", "123", "localhost:8080");
        upstream.setProtocol("http://");
        upstream.setProps("{\"warmup\":10,\"gray\":true,\"healthCheckEnabled\":false,\"extension\":{\"x\":1},"
                + "\"labels\":{\"release\":\"stable\",\"region\":\"east\"}}");
        return upstream;
    }

    private void configureSync() {
        when(selectorMapper.selectByDiscoveryHandlerId(any())).thenReturn(buildSelectorDO());
        when(discoveryHandlerMapper.selectById(any())).thenReturn(buildDiscoveryHandlerDO());
        when(pluginMapper.selectById(any())).thenReturn(buildPluginDO());
        when(discoveryMapper.selectById(any())).thenReturn(buildDiscoveryDO());
        when(discoveryProcessorHolder.chooseProcessor(anyString())).thenReturn(discoveryProcessor);
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
        discoveryDO.setDiscoveryName("123");
        discoveryDO.setId("123");
        discoveryDO.setDiscoveryType("local");
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
        discoveryUpstreamDO.setUpstreamStatus(1);
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
        discoveryUpstreamDO.setUpstreamStatus(1);
        discoveryUpstreamDO.setWeight(50);
        discoveryUpstreamDO.setUpstreamUrl(url);
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
        selectorDO.setSelectorName("selector_1");
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
