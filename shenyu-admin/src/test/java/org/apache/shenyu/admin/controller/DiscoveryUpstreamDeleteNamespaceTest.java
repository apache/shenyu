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

package org.apache.shenyu.admin.controller;

import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.exception.ValidFailException;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingGuard;
import org.apache.shenyu.admin.service.impl.DiscoveryUpstreamServiceImpl;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.List;
import java.util.Objects;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Exercises namespace-scoped HTTP deletion through the real service with mocked persistence.
 */
class DiscoveryUpstreamDeleteNamespaceTest {

    private static final String NAMESPACE = "namespace-a";

    private static final String HANDLER = "handler-a";

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

    private final MockMvc mockMvc = MockMvcBuilders.standaloneSetup(new DiscoveryUpstreamController(service))
            .setControllerAdvice(new ExceptionHandlers(null)).build();

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void rejectsForeignUpstreamBeforeDeleteOrPublication(final boolean proxy) throws Exception {
        configureHandler(proxy, HANDLER, "namespace-b");
        configureRows(List.of(upstream("upstream-a", HANDLER, "namespace-b")));
        assertRejected(NAMESPACE, List.of("upstream-a"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void rejectsMatchingRowWithForeignHandler(final boolean proxy) throws Exception {
        configureHandler(proxy, HANDLER, "namespace-b");
        configureRows(List.of(upstream("upstream-a", HANDLER, NAMESPACE)));
        assertRejected(NAMESPACE, List.of("upstream-a"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void rejectsEntireMixedNamespaceBatchBeforeDeletingValidRows(final boolean proxy) throws Exception {
        configureHandler(proxy, HANDLER, NAMESPACE);
        configureHandler(proxy, "handler-b", "namespace-b");
        configureRows(List.of(upstream("upstream-a", HANDLER, NAMESPACE), upstream("upstream-b", "handler-b", "namespace-b")));
        assertRejected(NAMESPACE, List.of("upstream-a", "upstream-b"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void matchingNamespaceStillDeletesAndPublishesEmptySnapshot(final boolean proxy) throws Exception {
        configureHandler(proxy, HANDLER, NAMESPACE);
        configureRows(List.of(upstream("upstream-a", HANDLER, NAMESPACE)));
        request(NAMESPACE, List.of("upstream-a")).andExpect(jsonPath("$.code").value(200));

        verify(upstreamMapper).deleteByIds(List.of("upstream-a"));
        ArgumentCaptor<ProxySelectorDTO> destination = ArgumentCaptor.forClass(ProxySelectorDTO.class);
        verify(processor).changeUpstream(destination.capture(), eq(List.of()));
        assertEquals(NAMESPACE, destination.getValue().getNamespaceId());
    }

    @Test
    void validatesLockedRowsInsteadOfEarlierObservation() throws Exception {
        configureHandler(false, HANDLER, NAMESPACE);
        configureRows(List.of(upstream("upstream-a", HANDLER, NAMESPACE)));
        when(upstreamMapper.selectByIdsForUpdate(List.of("upstream-a")))
                .thenReturn(List.of(upstream("upstream-a", HANDLER, "namespace-b")));
        assertRejected(NAMESPACE, List.of("upstream-a"));
    }

    @Test
    void rejectsMissingHandlerBeforeDelete() throws Exception {
        configureRows(List.of(upstream("upstream-a", HANDLER, NAMESPACE)));
        assertRejected(NAMESPACE, List.of("upstream-a"));
    }

    @Test
    void rejectsMissingBindingBeforeDelete() throws Exception {
        configureRows(List.of(upstream("upstream-a", HANDLER, NAMESPACE)));
        when(handlerMapper.selectById(HANDLER)).thenReturn(new DiscoveryHandlerDO());
        assertRejected(NAMESPACE, List.of("upstream-a"));
    }

    @Test
    void rejectsWhitespaceNamespaceBeforeDelete() throws Exception {
        configureHandler(false, HANDLER, NAMESPACE);
        configureRows(List.of(upstream("upstream-a", HANDLER, NAMESPACE)));
        assertRejected(" ", List.of("upstream-a"));
    }

    @Test
    void omittedNamespaceKeepsDefaultNamespaceRequestsWorking() throws Exception {
        configureHandler(false, HANDLER, SYS_DEFAULT_NAMESPACE_ID);
        configureRows(List.of(upstream("upstream-a", HANDLER, SYS_DEFAULT_NAMESPACE_ID)));
        request(null, List.of("upstream-a")).andExpect(jsonPath("$.code").value(200));
        verify(upstreamMapper).deleteByIds(List.of("upstream-a"));
        verify(processor).changeUpstream(any(), eq(List.of()));
    }

    @Test
    void omittedNamespaceCannotDeleteOtherNamespaces() throws Exception {
        configureHandler(false, HANDLER, NAMESPACE);
        configureRows(List.of(upstream("upstream-a", HANDLER, NAMESPACE)));
        assertRejected(null, List.of("upstream-a"));
    }

    @Test
    void nonexistentIdsRemainAnIdempotentNoOp() throws Exception {
        request(NAMESPACE, List.of("missing")).andExpect(jsonPath("$.code").value(200));
        verifyNoDeletionOrPublication();
    }

    @Test
    void legacyServiceEntryPointCannotDeleteOtherNamespaces() {
        configureHandler(false, HANDLER, NAMESPACE);
        configureRows(List.of(upstream("upstream-a", HANDLER, NAMESPACE)));
        assertThrows(ValidFailException.class, () -> service.delete(List.of("upstream-a")));
        verifyNoDeletionOrPublication();
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" "})
    void explicitServiceNamespaceMustNotBeBlank(final String namespace) {
        assertThrows(ValidFailException.class, () -> service.delete(List.of("upstream-a"), namespace));
        verifyNoDeletionOrPublication();
    }

    @Test
    void repeatedAndMissingIdsOnlyDeleteTheExistingRow() throws Exception {
        configureHandler(false, HANDLER, NAMESPACE);
        DiscoveryUpstreamDO row = upstream("upstream-a", HANDLER, NAMESPACE);
        when(upstreamMapper.selectByIds(List.of("missing", "upstream-a"))).thenReturn(List.of(row));
        when(upstreamMapper.selectByIdsForUpdate(List.of("missing", "upstream-a"))).thenReturn(List.of(row));
        request(NAMESPACE, List.of("upstream-a", "missing", "upstream-a")).andExpect(jsonPath("$.code").value(200));
        verify(upstreamMapper).deleteByIds(List.of("upstream-a"));
        verify(processor).changeUpstream(any(), eq(List.of()));
    }

    private ResultActions request(final String namespace, final List<String> ids) throws Exception {
        MockHttpServletRequestBuilder request = delete("/discovery-upstream/batch")
                .contentType(MediaType.APPLICATION_JSON).content(GsonUtils.getInstance().toJson(ids));
        if (Objects.nonNull(namespace)) {
            request.param("namespaceId", namespace);
        }
        return mockMvc.perform(request).andExpect(status().isOk());
    }

    private void assertRejected(final String namespace, final List<String> ids) throws Exception {
        request(namespace, ids).andExpect(jsonPath("$.code").value(not(200)));
        verifyNoDeletionOrPublication();
    }

    private void verifyNoDeletionOrPublication() {
        verify(upstreamMapper, never()).deleteByIds(any());
        verifyNoInteractions(processor, processorHolder);
    }

    private void configureRows(final List<DiscoveryUpstreamDO> rows) {
        List<String> ids = rows.stream().map(DiscoveryUpstreamDO::getId).sorted().toList();
        when(upstreamMapper.selectByIds(ids)).thenReturn(rows);
        when(upstreamMapper.selectByIdsForUpdate(ids)).thenReturn(rows);
    }

    private DiscoveryUpstreamDO upstream(final String id, final String handlerId, final String namespace) {
        DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setId(id);
        upstream.setDiscoveryHandlerId(handlerId);
        upstream.setNamespaceId(namespace);
        upstream.setUpstreamUrl("127.0.0.1:8080");
        return upstream;
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
}
