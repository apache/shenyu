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
import org.apache.shenyu.admin.exception.ValidFailException;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingGuard;
import org.apache.shenyu.admin.service.impl.DiscoveryUpstreamServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Namespace consistency at the public discovery upstream write boundary.
 */
class DiscoveryUpstreamNamespaceTest {

    private static final String HANDLER = "handler-a";

    private static final String NAMESPACE = "namespace-a";

    private final DiscoveryUpstreamMapper upstreamMapper = mock(DiscoveryUpstreamMapper.class);

    private final DiscoveryHandlerMapper handlerMapper = mock(DiscoveryHandlerMapper.class);

    private final ProxySelectorMapper proxyMapper = mock(ProxySelectorMapper.class);

    private final DiscoveryMapper discoveryMapper = mock(DiscoveryMapper.class);

    private final SelectorMapper selectorMapper = mock(SelectorMapper.class);

    private final PluginMapper pluginMapper = mock(PluginMapper.class);

    private final DiscoveryProcessorHolder processorHolder = mock(DiscoveryProcessorHolder.class);

    private final DiscoveryProcessor processor = mock(DiscoveryProcessor.class);

    private final DiscoveryUpstreamService service = new DiscoveryUpstreamServiceImpl(upstreamMapper, handlerMapper,
            proxyMapper, discoveryMapper, mock(DiscoveryRelMapper.class), selectorMapper, pluginMapper,
            processorHolder, mock(RegistrationFencingGuard.class));

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void rejectsMismatchedCreateBeforeInsert(final boolean proxy) {
        configureHandler(proxy);
        assertNamespaceRejected(() -> service.createOrUpdate(upstream(null, "namespace-b")));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void rejectsMismatchedUpdateBeforeChangingStoredRow(final boolean proxy) {
        configureHandler(proxy);
        configureExistingUpstream();
        assertNamespaceRejected(() -> service.createOrUpdate(upstream("upstream-a", "namespace-b")));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void validatesEntireCreateBatchBeforeFirstInsert(final boolean proxy) {
        configureHandler(proxy);
        assertNamespaceRejected(() -> service.createOrUpdateBatch(List.of(
                upstream(null, NAMESPACE), upstream(null, "namespace-b"))));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void validatesEntireMixedBatchBeforeFirstUpdate(final boolean proxy) {
        configureHandler(proxy);
        configureExistingUpstream();
        assertNamespaceRejected(() -> service.createOrUpdateBatch(List.of(
                upstream("upstream-a", NAMESPACE), upstream(null, "namespace-b"))));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void validatesEachBatchHandlerAgainstItsOwnNamespace(final boolean proxy) {
        configureHandler(proxy);
        configureHandler(proxy, "handler-b", "namespace-b");
        DiscoveryUpstreamDTO second = upstream(null, NAMESPACE);
        second.setDiscoveryHandlerId("handler-b");
        assertNamespaceRejected(() -> service.createOrUpdateBatch(List.of(upstream(null, NAMESPACE), second)));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void matchingBatchHandlersKeepTheirRespectiveNamespaces(final boolean proxy) {
        configureHandler(proxy);
        configureHandler(proxy, "handler-b", "namespace-b");
        DiscoveryUpstreamDTO second = upstream(null, "namespace-b");
        second.setDiscoveryHandlerId("handler-b");
        service.createOrUpdateBatch(List.of(upstream(null, NAMESPACE), second));

        ArgumentCaptor<DiscoveryUpstreamDO> inserted = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(upstreamMapper, times(2)).insert(inserted.capture());
        Map<String, String> expected = Map.of(HANDLER, NAMESPACE, "handler-b", "namespace-b");
        inserted.getAllValues().forEach(row -> assertEquals(expected.get(row.getDiscoveryHandlerId()), row.getNamespaceId()));
        verify(processor, times(2)).changeUpstream(any(), any());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void validatesReplacementAgainstPathHandlerBeforeDeletingRows(final boolean proxy) {
        configureHandler(proxy);
        DiscoveryUpstreamDTO mismatched = upstream(null, "namespace-b");
        mismatched.setDiscoveryHandlerId("body-handler");
        assertNamespaceRejected(() -> service.updateBatch(HANDLER, List.of(upstream(null, NAMESPACE), mismatched)));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void matchingNamespaceStillPersistsAndPublishesForEveryCrudWrite(final boolean proxy) {
        configureHandler(proxy);
        configureExistingUpstream();
        service.createOrUpdate(upstream(null, NAMESPACE));
        service.createOrUpdate(upstream("upstream-a", NAMESPACE));
        service.createOrUpdateBatch(List.of(upstream("upstream-a", NAMESPACE), upstream(null, NAMESPACE)));
        DiscoveryUpstreamDTO replacement = upstream(null, NAMESPACE);
        replacement.setDiscoveryHandlerId("body-handler");
        service.updateBatch(HANDLER, List.of(replacement));

        ArgumentCaptor<DiscoveryUpstreamDO> inserted = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(upstreamMapper, times(3)).insert(inserted.capture());
        inserted.getAllValues().forEach(row -> {
            assertEquals(NAMESPACE, row.getNamespaceId());
            assertEquals(HANDLER, row.getDiscoveryHandlerId());
        });
        ArgumentCaptor<DiscoveryUpstreamDO> updated = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(upstreamMapper, times(2)).updateForHandler(updated.capture(), eq(HANDLER));
        updated.getAllValues().forEach(row -> assertEquals(NAMESPACE, row.getNamespaceId()));
        verify(upstreamMapper).deleteByDiscoveryHandlerId(HANDLER);
        ArgumentCaptor<ProxySelectorDTO> destinations = ArgumentCaptor.forClass(ProxySelectorDTO.class);
        verify(processor, times(4)).changeUpstream(destinations.capture(), any());
        destinations.getAllValues().forEach(destination -> assertEquals(NAMESPACE, destination.getNamespaceId()));
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" "})
    void rejectsMissingNamespaceBeforeWriting(final String namespace) {
        configureHandler(false);
        assertNamespaceRejected(() -> service.createOrUpdate(upstream(null, namespace)));
    }

    @Test
    void rejectsMissingHandlerBeforeWriting() {
        assertThrows(ValidFailException.class, () -> service.createOrUpdate(upstream(null, NAMESPACE)));
        verifyNoMutationOrPublication();
    }

    @Test
    void rejectsMissingSelectorBindingBeforeWriting() {
        when(handlerMapper.selectById(HANDLER)).thenReturn(new DiscoveryHandlerDO());
        assertThrows(ValidFailException.class, () -> service.createOrUpdate(upstream(null, NAMESPACE)));
        verifyNoMutationOrPublication();
    }

    private void assertNamespaceRejected(final Runnable write) {
        ValidFailException error = assertThrows(ValidFailException.class, write::run);
        assertTrue(error.getMessage().contains("namespace"));
        verifyNoMutationOrPublication();
    }

    private void verifyNoMutationOrPublication() {
        verify(upstreamMapper, never()).insert(any());
        verify(upstreamMapper, never()).updateForHandler(any(), anyString());
        verify(upstreamMapper, never()).deleteByDiscoveryHandlerId(any());
        verifyNoInteractions(processor, processorHolder);
    }

    private void configureExistingUpstream() {
        DiscoveryUpstreamDO existing = new DiscoveryUpstreamDO();
        existing.setId("upstream-a");
        existing.setDiscoveryHandlerId(HANDLER);
        existing.setNamespaceId(NAMESPACE);
        when(upstreamMapper.selectByIds(List.of("upstream-a"))).thenReturn(List.of(existing));
        when(upstreamMapper.selectByIdsForUpdate(List.of("upstream-a"))).thenReturn(List.of(existing));
        when(upstreamMapper.updateForHandler(any(), eq(HANDLER))).thenReturn(1);
    }

    private void configureHandler(final boolean proxy) {
        configureHandler(proxy, HANDLER, NAMESPACE);
    }

    private void configureHandler(final boolean proxy, final String handlerId, final String namespace) {
        DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId(handlerId);
        handler.setDiscoveryId("discovery-a");
        when(handlerMapper.selectById(handlerId)).thenReturn(handler);
        if (proxy) {
            ProxySelectorDO selector = new ProxySelectorDO();
            selector.setId("proxy-" + handlerId);
            selector.setNamespaceId(namespace);
            selector.setPluginName("tcp");
            when(proxyMapper.selectByHandlerId(handlerId)).thenReturn(selector);
        } else {
            SelectorDO selector = new SelectorDO();
            selector.setId("selector-" + handlerId);
            selector.setNamespaceId(namespace);
            selector.setPluginId("plugin-a");
            when(selectorMapper.selectByDiscoveryHandlerId(handlerId)).thenReturn(selector);
            PluginDO plugin = new PluginDO();
            plugin.setName("divide");
            when(pluginMapper.selectById("plugin-a")).thenReturn(plugin);
        }
        DiscoveryDO discovery = new DiscoveryDO();
        discovery.setDiscoveryType("local");
        when(discoveryMapper.selectById("discovery-a")).thenReturn(discovery);
        when(processorHolder.chooseProcessor("local")).thenReturn(processor);
    }

    private DiscoveryUpstreamDTO upstream(final String id, final String namespace) {
        DiscoveryUpstreamDTO upstream = new DiscoveryUpstreamDTO();
        upstream.setId(id);
        upstream.setDiscoveryHandlerId(HANDLER);
        upstream.setNamespaceId(namespace);
        upstream.setUrl("127.0.0.1:8080");
        upstream.setStatus(0);
        upstream.setWeight(100);
        upstream.setProps("{}");
        return upstream;
    }
}
