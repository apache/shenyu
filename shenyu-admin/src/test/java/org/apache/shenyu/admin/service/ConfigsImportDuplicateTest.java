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
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AuthParamMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.dto.AppAuthDTO;
import org.apache.shenyu.admin.model.dto.AuthPathDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.service.impl.AppAuthServiceImpl;
import org.apache.shenyu.admin.service.impl.DiscoveryServiceImpl;
import org.apache.shenyu.admin.service.impl.DiscoveryUpstreamServiceImpl;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.service.impl.ProxySelectorServiceImpl;
import org.apache.shenyu.admin.service.impl.RuleServiceImpl;
import org.apache.shenyu.admin.service.impl.SelectorServiceImpl;
import org.apache.shenyu.admin.service.impl.ShenyuDictServiceImpl;
import org.apache.shenyu.admin.service.publish.DictEventPublisher;
import org.apache.shenyu.admin.service.publish.MetaDataEventPublisher;
import org.apache.shenyu.admin.service.publish.RuleEventPublisher;
import org.apache.shenyu.admin.service.publish.SelectorEventPublisher;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.transaction.PlatformTransactionManager;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Regression tests for duplicate business keys in namespace config imports.
 */
@ExtendWith(MockitoExtension.class)
public final class ConfigsImportDuplicateTest {

    private static final String NAMESPACE = "target-namespace";

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private RuleMapper ruleMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private MetaDataMapper metaDataMapper;

    @Mock
    private AppAuthMapper appAuthMapper;

    @Mock
    private ShenyuDictMapper dictMapper;

    @Mock
    private ProxySelectorMapper proxySelectorMapper;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private DiscoveryUpstreamMapper upstreamMapper;

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testSelectorsDeduplicateAfterPluginRemapping(final boolean existing) {
        final SelectorServiceImpl service = new SelectorServiceImpl(selectorMapper, mock(SelectorConditionMapper.class), pluginMapper,
                mock(ApplicationEventPublisher.class), discoveryMapper, mock(DiscoveryHandlerMapper.class), mock(DiscoveryRelMapper.class),
                upstreamMapper, mock(DiscoveryProcessorHolder.class), mock(SelectorEventPublisher.class));
        final ConfigsImportContext context = new ConfigsImportContext();
        context.getPluginTemplateIdMapping().put("source", "target");
        context.getPluginTemplateIdMapping().put("alias", "target");
        context.getPluginTemplateIdMapping().put("other", "other-target");
        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(existing
                ? Arrays.asList(SelectorDO.builder().id("existing").pluginId("target").selectorName("same").build(),
                        SelectorDO.builder().id("legacy-duplicate").pluginId("target").selectorName("same").build()) : Collections.emptyList());
        when(selectorMapper.insertSelective(any())).thenReturn(1);
        when(pluginMapper.selectById(anyString())).thenReturn(PluginDO.builder().name("divide").build());

        final ConfigImportResult result = service.importData(NAMESPACE, Arrays.asList(selector("first", "source", "same"),
                selector("duplicate", "alias", "same"), selector("other-plugin", "other", "same"), selector("other-name", "source", "different")), context);

        assertResult(result, existing ? 2 : 3, "import fail selector: ", existing ? "same,same" : "same");
        assertNotNull(context.getSelectorIdMapping().get("first"));
        assertEquals(context.getSelectorIdMapping().get("first"), context.getSelectorIdMapping().get("duplicate"));
        assertNotEquals(context.getSelectorIdMapping().get("first"), context.getSelectorIdMapping().get("other-plugin"));
        final ArgumentCaptor<SelectorDO> captor = ArgumentCaptor.forClass(SelectorDO.class);
        verify(selectorMapper, times(result.getSuccessCount())).insertSelective(captor.capture());
        captor.getAllValues().forEach(row -> assertEquals(NAMESPACE, row.getNamespaceId()));
        if (existing) {
            assertEquals("existing", context.getSelectorIdMapping().get("first"));
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testRulesDeduplicateAfterSelectorRemapping(final boolean existing) {
        final RuleServiceImpl service = new RuleServiceImpl(ruleMapper, mock(RuleConditionMapper.class), selectorMapper, pluginMapper, mock(RuleEventPublisher.class));
        final ConfigsImportContext context = new ConfigsImportContext();
        context.getSelectorIdMapping().put("source", "target");
        context.getSelectorIdMapping().put("alias", "target");
        context.getSelectorIdMapping().put("other", "other-target");
        when(ruleMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(existing
                ? Collections.singletonList(RuleDO.builder().selectorId("target").ruleName("same").build()) : Collections.emptyList());
        when(ruleMapper.insertSelective(any())).thenReturn(1);
        when(selectorMapper.selectById(anyString())).thenReturn(SelectorDO.builder().pluginId("plugin").build());
        when(pluginMapper.selectById("plugin")).thenReturn(PluginDO.builder().name("divide").build());

        final ConfigImportResult result = service.importData(NAMESPACE, Arrays.asList(rule("source", "same"), rule("alias", "same"),
                rule("other", "same"), rule("source", "different")), context);

        assertResult(result, existing ? 2 : 3, "import fail rule: ", existing ? "same,same" : "same");
        final ArgumentCaptor<RuleDO> captor = ArgumentCaptor.forClass(RuleDO.class);
        verify(ruleMapper, times(result.getSuccessCount())).insertSelective(captor.capture());
        captor.getAllValues().forEach(row -> assertEquals(NAMESPACE, row.getNamespaceId()));
        assertEquals(existing ? 0 : 1, captor.getAllValues().stream()
                .filter(row -> "target".equals(row.getSelectorId()) && "same".equals(row.getRuleName())).count());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testMetadataDeduplicatesPaths(final boolean existing) {
        final MetaDataServiceImpl service = new MetaDataServiceImpl(metaDataMapper, mock(ApplicationEventPublisher.class), mock(MetaDataEventPublisher.class));
        when(metaDataMapper.findAllByNamespaceId(NAMESPACE)).thenReturn(existing
                ? Collections.singletonList(MetaDataDO.builder().path("/same").build()) : Collections.emptyList());
        when(metaDataMapper.insert(any())).thenReturn(1);
        when(metaDataMapper.pathExisted(anyString(), anyString())).thenReturn(null);

        final ConfigImportResult result = service.importData(NAMESPACE, Arrays.asList(metadata("/same"), metadata("/same"), metadata("/different")));

        assertResult(result, existing ? 1 : 2, "import fail meta: ", existing ? "/same,/same" : "/same");
        final ArgumentCaptor<MetaDataDO> captor = ArgumentCaptor.forClass(MetaDataDO.class);
        verify(metaDataMapper, times(result.getSuccessCount())).insert(captor.capture());
        captor.getAllValues().forEach(row -> assertEquals(NAMESPACE, row.getNamespaceId()));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testAuthDeduplicatesAppKeysAndChildRows(final boolean existing) {
        final AuthPathMapper pathMapper = mock(AuthPathMapper.class);
        final AppAuthServiceImpl service = new AppAuthServiceImpl(appAuthMapper, mock(ApplicationEventPublisher.class), mock(AuthParamMapper.class), pathMapper);
        when(appAuthMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(existing
                ? Collections.singletonList(AppAuthDO.builder().appKey("same").build()) : Collections.emptyList());
        when(appAuthMapper.insertSelective(any())).thenReturn(1);

        final ConfigImportResult result = service.importData(NAMESPACE, Arrays.asList(auth("same"), auth("same"), auth("different")));

        assertResult(result, existing ? 1 : 2, "import fail appKey: ", existing ? "same,same," : "same,");
        final ArgumentCaptor<AppAuthDO> captor = ArgumentCaptor.forClass(AppAuthDO.class);
        verify(appAuthMapper, times(result.getSuccessCount())).insertSelective(captor.capture());
        verify(pathMapper, times(result.getSuccessCount())).batchSave(any());
        captor.getAllValues().forEach(row -> assertEquals(NAMESPACE, row.getNamespaceId()));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testDictsDeduplicateWithinType(final boolean existing) {
        final ShenyuDictServiceImpl service = new ShenyuDictServiceImpl(dictMapper, mock(DictEventPublisher.class));
        final Timestamp now = new Timestamp(System.currentTimeMillis());
        when(dictMapper.selectByQuery(any())).thenReturn(existing ? Collections.singletonList(ShenyuDictDO.builder().id("existing")
                .type("type").dictCode("code").dictName("same").dateCreated(now).dateUpdated(now).build()) : Collections.emptyList());
        when(dictMapper.insertSelective(any())).thenReturn(1);

        final ConfigImportResult result = service.importData(Arrays.asList(dict("type", "same"), dict("type", "same"),
                dict("other-type", "same"), dict("type", "different")));

        assertResult(result, existing ? 2 : 3, "import fail dict: ", existing ? "same,same" : "same");
        verify(dictMapper, times(result.getSuccessCount())).insertSelective(any());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testProxySelectorsDeduplicateAndMapIds(final boolean existing) {
        final ProxySelectorServiceImpl service = new ProxySelectorServiceImpl(proxySelectorMapper, discoveryMapper, upstreamMapper,
                mock(DiscoveryHandlerMapper.class), mock(DiscoveryRelMapper.class), selectorMapper, mock(DiscoveryProcessorHolder.class), mock(ApplicationEventPublisher.class));
        final ConfigsImportContext context = new ConfigsImportContext();
        when(proxySelectorMapper.selectByNamespaceId(NAMESPACE)).thenReturn(existing
                ? Arrays.asList(ProxySelectorDO.builder().id("existing").pluginName("tcp").name("same").build(),
                        ProxySelectorDO.builder().id("legacy-duplicate").pluginName("tcp").name("same").build()) : Collections.emptyList());
        when(proxySelectorMapper.insert(any())).thenReturn(1);

        final ConfigImportResult result = service.importData(NAMESPACE, Arrays.asList(proxy("first", "tcp", "same"), proxy("duplicate", "tcp", "same"),
                proxy("other-plugin", "mqtt", "same"), proxy("other-name", "tcp", "different")), context);

        assertResult(result, existing ? 2 : 3, "import fail proxy selector: ", existing ? "same,same" : "same");
        assertNotNull(context.getProxySelectorIdMapping().get("first"));
        assertEquals(context.getProxySelectorIdMapping().get("first"), context.getProxySelectorIdMapping().get("duplicate"));
        assertNotEquals(context.getProxySelectorIdMapping().get("first"), context.getProxySelectorIdMapping().get("other-plugin"));
        verify(proxySelectorMapper, times(result.getSuccessCount())).insert(any());
        if (existing) {
            assertEquals("existing", context.getProxySelectorIdMapping().get("first"));
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testDiscoveriesDeduplicateBeforeCreatingHandlers(final boolean existing) {
        final DiscoveryProcessorHolder holder = mock(DiscoveryProcessorHolder.class);
        final DiscoveryProcessor processor = mock(DiscoveryProcessor.class);
        final DiscoveryHandlerMapper handlerMapper = mock(DiscoveryHandlerMapper.class);
        final DiscoveryServiceImpl service = new DiscoveryServiceImpl(discoveryMapper, proxySelectorMapper, mock(DiscoveryRelMapper.class),
                handlerMapper, mock(SelectorService.class), selectorMapper, holder, mock(PlatformTransactionManager.class));
        when(holder.chooseProcessor("local")).thenReturn(processor);
        when(discoveryMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(existing
                ? Collections.singletonList(DiscoveryDO.builder().pluginName("divide").discoveryName("same").build()) : Collections.emptyList());
        when(discoveryMapper.insert(any())).thenReturn(1);

        final ConfigImportResult result = service.importData(NAMESPACE, Arrays.asList(discovery("divide", "same"), discovery("divide", "same"),
                discovery("tcp", "same"), discovery("divide", "different")), new ConfigsImportContext());

        assertResult(result, existing ? 2 : 3, "import fail discovery: ", existing ? "same,same" : "same");
        verify(discoveryMapper, times(result.getSuccessCount())).insert(any());
        verify(handlerMapper, times(result.getSuccessCount())).insertSelective(any());
        verify(processor, times(result.getSuccessCount())).createDiscovery(any());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testUpstreamsDeduplicateAfterHandlerRemapping(final boolean existing) {
        final DiscoveryUpstreamServiceImpl service = new DiscoveryUpstreamServiceImpl(upstreamMapper, mock(DiscoveryHandlerMapper.class), proxySelectorMapper,
                discoveryMapper, mock(DiscoveryRelMapper.class), selectorMapper, pluginMapper, mock(DiscoveryProcessorHolder.class));
        final ConfigsImportContext context = new ConfigsImportContext();
        context.getDiscoveryHandlerIdMapping().put("source", "target");
        context.getDiscoveryHandlerIdMapping().put("alias", "target");
        context.getDiscoveryHandlerIdMapping().put("other", "other-target");
        when(upstreamMapper.selectByNamespaceId(NAMESPACE)).thenReturn(existing
                ? Collections.singletonList(DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(upstream("target", "same"))) : Collections.emptyList());
        when(upstreamMapper.insert(any())).thenReturn(1);

        final ConfigImportResult result = service.importData(NAMESPACE, Arrays.asList(upstream("source", "same"), upstream("alias", "same"),
                upstream("other", "same"), upstream("source", "different")), context);

        assertResult(result, existing ? 2 : 3, "import fail discovery upstream: ", existing ? "same,same" : "same");
        final ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(upstreamMapper, times(result.getSuccessCount())).insert(captor.capture());
        captor.getAllValues().forEach(row -> assertEquals(NAMESPACE, row.getNamespaceId()));
        assertEquals(existing ? 0 : 1, captor.getAllValues().stream()
                .filter(row -> "target".equals(row.getDiscoveryHandlerId()) && "same".equals(row.getUpstreamUrl())).count());
    }

    private void assertResult(final ConfigImportResult result, final int count, final String prefix, final String names) {
        assertEquals(count, result.getSuccessCount());
        assertEquals(prefix + names, result.getFailMessage());
    }

    @Test
    public void testMetadataOnlyReservesPathsAfterSuccessfulInsert() {
        final MetaDataServiceImpl service = new MetaDataServiceImpl(metaDataMapper, mock(ApplicationEventPublisher.class), mock(MetaDataEventPublisher.class));
        when(metaDataMapper.insert(any())).thenReturn(0, 1);
        when(metaDataMapper.pathExisted(anyString(), anyString())).thenReturn(null);

        final ConfigImportResult result = service.importData(NAMESPACE, Arrays.asList(metadata("/same"), metadata("/same"), metadata("/same")));

        assertResult(result, 1, "import fail meta: ", "/same");
        verify(metaDataMapper, times(2)).insert(any());
    }

    @Test
    public void testDuplicateDiscoveryHandlersRemapUpstreamsToFirstInsertedHandler() {
        final DiscoveryProcessorHolder holder = mock(DiscoveryProcessorHolder.class);
        final DiscoveryProcessor processor = mock(DiscoveryProcessor.class);
        final DiscoveryHandlerMapper handlerMapper = mock(DiscoveryHandlerMapper.class);
        final DiscoveryRelMapper relMapper = mock(DiscoveryRelMapper.class);
        final DiscoveryServiceImpl discoveries = new DiscoveryServiceImpl(discoveryMapper, proxySelectorMapper, relMapper,
                handlerMapper, mock(SelectorService.class), selectorMapper, holder, mock(PlatformTransactionManager.class));
        final DiscoveryUpstreamServiceImpl upstreams = new DiscoveryUpstreamServiceImpl(upstreamMapper, handlerMapper, proxySelectorMapper,
                discoveryMapper, relMapper, selectorMapper, pluginMapper, holder);
        final ConfigsImportContext context = new ConfigsImportContext();
        final DiscoveryDTO first = discovery("divide", "same");
        first.getDiscoveryHandler().setId("first-handler");
        final DiscoveryDTO duplicate = discovery("divide", "same");
        duplicate.getDiscoveryHandler().setId("duplicate-handler");
        when(holder.chooseProcessor("local")).thenReturn(processor);
        when(discoveryMapper.insert(any())).thenReturn(1);
        when(handlerMapper.insertSelective(any())).thenReturn(1);
        when(upstreamMapper.insert(any())).thenReturn(1);

        assertResult(discoveries.importData(NAMESPACE, Arrays.asList(first, duplicate), context), 1, "import fail discovery: ", "same");
        final String targetHandler = context.getDiscoveryHandlerIdMapping().get("first-handler");
        assertNotNull(targetHandler);
        assertEquals(targetHandler, context.getDiscoveryHandlerIdMapping().get("duplicate-handler"));
        assertResult(upstreams.importData(NAMESPACE, Arrays.asList(upstream("first-handler", "same-url"),
                upstream("duplicate-handler", "same-url")), context), 1, "import fail discovery upstream: ", "same-url");
        verify(handlerMapper).insertSelective(any());
        final ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(upstreamMapper).insert(captor.capture());
        assertEquals(targetHandler, captor.getValue().getDiscoveryHandlerId());
    }

    @Test
    public void testDictOnlyReservesNamesAfterSuccessfulInsert() {
        final ShenyuDictServiceImpl service = new ShenyuDictServiceImpl(dictMapper, mock(DictEventPublisher.class));
        when(dictMapper.insertSelective(any())).thenReturn(0, 1);

        final ConfigImportResult result = service.importData(Arrays.asList(dict("type", "same"), dict("type", "same"), dict("type", "same")));

        assertResult(result, 1, "import fail dict: ", "same");
        verify(dictMapper, times(2)).insertSelective(any());
    }

    @Test
    public void testProxySelectorOnlyMapsIdsAfterSuccessfulInsert() {
        final ProxySelectorServiceImpl service = new ProxySelectorServiceImpl(proxySelectorMapper, discoveryMapper, upstreamMapper,
                mock(DiscoveryHandlerMapper.class), mock(DiscoveryRelMapper.class), selectorMapper, mock(DiscoveryProcessorHolder.class), mock(ApplicationEventPublisher.class));
        final ConfigsImportContext context = new ConfigsImportContext();
        when(proxySelectorMapper.insert(any())).thenReturn(0, 1);

        final ConfigImportResult result = service.importData(NAMESPACE, Arrays.asList(proxy("failed", "tcp", "same"),
                proxy("success", "tcp", "same"), proxy("duplicate", "tcp", "same")), context);

        assertResult(result, 1, "import fail proxy selector: ", "same");
        assertEquals(2, context.getProxySelectorIdMapping().size());
        assertNotNull(context.getProxySelectorIdMapping().get("success"));
        assertEquals(context.getProxySelectorIdMapping().get("success"), context.getProxySelectorIdMapping().get("duplicate"));
        verify(proxySelectorMapper, times(2)).insert(any());
    }

    private SelectorDTO selector(final String id, final String pluginId, final String name) {
        final SelectorDTO dto = new SelectorDTO();
        dto.setId(id);
        dto.setPluginId(pluginId);
        dto.setName(name);
        dto.setType(0);
        dto.setSelectorConditions(Collections.emptyList());
        return dto;
    }

    private RuleDTO rule(final String selectorId, final String name) {
        final RuleDTO dto = new RuleDTO();
        dto.setSelectorId(selectorId);
        dto.setName(name);
        dto.setRuleConditions(Collections.emptyList());
        return dto;
    }

    private MetaDataDTO metadata(final String path) {
        final MetaDataDTO dto = new MetaDataDTO();
        dto.setPath(path);
        return dto;
    }

    private AppAuthDTO auth(final String key) {
        final AppAuthDTO dto = new AppAuthDTO();
        dto.setAppKey(key);
        final AuthPathDTO path = new AuthPathDTO();
        path.setPath("/" + key);
        dto.setAuthPathList(Collections.singletonList(path));
        return dto;
    }

    private ShenyuDictDTO dict(final String type, final String name) {
        final ShenyuDictDTO dto = new ShenyuDictDTO();
        dto.setType(type);
        dto.setDictCode("code");
        dto.setDictName(name);
        return dto;
    }

    private ProxySelectorData proxy(final String id, final String plugin, final String name) {
        final ProxySelectorData data = new ProxySelectorData();
        data.setId(id);
        data.setPluginName(plugin);
        data.setName(name);
        return data;
    }

    private DiscoveryDTO discovery(final String plugin, final String name) {
        final DiscoveryDTO dto = new DiscoveryDTO();
        dto.setPluginName(plugin);
        dto.setName(name);
        dto.setType("local");
        final DiscoveryHandlerDTO handler = new DiscoveryHandlerDTO();
        handler.setId(plugin + "-" + name);
        dto.setDiscoveryHandler(handler);
        return dto;
    }

    private DiscoveryUpstreamDTO upstream(final String handler, final String url) {
        final DiscoveryUpstreamDTO dto = new DiscoveryUpstreamDTO();
        dto.setDiscoveryHandlerId(handler);
        dto.setUrl(url);
        dto.setStatus(0);
        dto.setWeight(50);
        return dto;
    }
}
