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

package org.apache.shenyu.admin.listener;

import com.google.common.collect.Lists;
import org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.NamespacePluginService;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import org.apache.shenyu.admin.service.AiProxyApiKeyService;

/**
 * The TestCase for {@link AbstractDataChangedListener}.
 */
public final class AbstractDataChangedListenerTest {

    private MockAbstractDataChangedListener listener;

    private AppAuthService appAuthService;

    private NamespacePluginService namespacePluginService;

    private RuleService ruleService;

    private SelectorService selectorService;

    private MetaDataService metaDataService;

    private ProxySelectorService proxySelectorService;

    private DiscoveryUpstreamService discoveryUpstreamService;

    private NamespaceService namespaceService;

    private AiProxyApiKeyService aiProxyApiKeyService;

    @BeforeEach
    public void setUp() throws Exception {
        listener = new MockAbstractDataChangedListener();
        appAuthService = mock(AppAuthService.class);
        namespacePluginService = mock(NamespacePluginService.class);
        ruleService = mock(RuleService.class);
        selectorService = mock(SelectorService.class);
        metaDataService = mock(MetaDataService.class);
        proxySelectorService = mock(ProxySelectorService.class);
        discoveryUpstreamService = mock(DiscoveryUpstreamService.class);
        namespaceService = mock(NamespaceService.class);
        aiProxyApiKeyService = mock(AiProxyApiKeyService.class);

        Class clazz = MockAbstractDataChangedListener.class.getSuperclass();
        Field appAuthServiceField = clazz.getDeclaredField("appAuthService");
        appAuthServiceField.setAccessible(true);
        appAuthServiceField.set(listener, appAuthService);
        Field namespacePluginServiceField = clazz.getDeclaredField("namespacePluginService");
        namespacePluginServiceField.setAccessible(true);
        namespacePluginServiceField.set(listener, namespacePluginService);
        Field ruleServiceField = clazz.getDeclaredField("ruleService");
        ruleServiceField.setAccessible(true);
        ruleServiceField.set(listener, ruleService);
        Field selectorServiceField = clazz.getDeclaredField("selectorService");
        selectorServiceField.setAccessible(true);
        selectorServiceField.set(listener, selectorService);
        Field metaDataServiceField = clazz.getDeclaredField("metaDataService");
        metaDataServiceField.setAccessible(true);
        metaDataServiceField.set(listener, metaDataService);
        Field proxySelectorServiceField = clazz.getDeclaredField("proxySelectorService");
        proxySelectorServiceField.setAccessible(true);
        proxySelectorServiceField.set(listener, proxySelectorService);
        Field discoveryUpstreamServiceField = clazz.getDeclaredField("discoveryUpstreamService");
        discoveryUpstreamServiceField.setAccessible(true);
        discoveryUpstreamServiceField.set(listener, discoveryUpstreamService);
        Field namespaceServiceField = clazz.getDeclaredField("namespaceService");
        namespaceServiceField.setAccessible(true);
        namespaceServiceField.set(listener, namespaceService);
        Field aiProxyApiKeyServiceField = clazz.getDeclaredField("aiProxyApiKeyService");
        aiProxyApiKeyServiceField.setAccessible(true);
        aiProxyApiKeyServiceField.set(listener, aiProxyApiKeyService);

        List<AppAuthData> appAuthDatas = Lists.newArrayList(mock(AppAuthData.class));
        when(appAuthService.listAllByNamespaceId(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(appAuthDatas);
        List<PluginData> pluginDatas = Lists.newArrayList(mock(PluginData.class));
        when(namespacePluginService.listAll(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(pluginDatas);
        List<RuleData> ruleDatas = Lists.newArrayList(mock(RuleData.class));
        when(ruleService.listAllByNamespaceId(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(ruleDatas);
        List<SelectorData> selectorDatas = Lists.newArrayList(mock(SelectorData.class));
        when(selectorService.listAllByNamespaceId(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(selectorDatas);
        List<MetaData> metaDatas = Lists.newArrayList(mock(MetaData.class));
        when(metaDataService.listAllByNamespaceId(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(metaDatas);
        List<ProxySelectorData> proxySelectorDatas = Lists.newArrayList(mock(ProxySelectorData.class));
        when(proxySelectorService.listAllByNamespaceId(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(proxySelectorDatas);
        List<DiscoverySyncData> discoverySyncDatas = Lists.newArrayList(mock(DiscoverySyncData.class));
        when(discoveryUpstreamService.listAllByNamespaceId(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(discoverySyncDatas);
        List<NamespaceVO> list = new ArrayList<>();
        NamespaceVO namespaceVO = new NamespaceVO();
        namespaceVO.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        list.add(namespaceVO);
        when(namespaceService.listAll()).thenReturn(list);

        // clear first
        listener.getCache().clear();
    }

    @Test
    void refreshesOnlyTheRequestedNamespaceForEverySyncGroup() {
        for (String namespace : new String[]{"namespace-a", "namespace-b"}) {
            when(selectorService.listAllByNamespaceId(namespace)).thenReturn(java.util.Collections.singletonList(
                    SelectorData.builder().id(namespace).namespaceId(namespace).build()));
            listener.updateSelectorCache(namespace);
            listener.updateRuleCache(namespace);
            listener.updateAppAuthCache(namespace);
            listener.updateMetaDataCache(namespace);
            listener.updateProxySelectorDataCache(namespace);
            listener.updateDiscoveryUpstreamDataCache(namespace);
            listener.updateAiProxyApiKeyCache(namespace);
            verify(selectorService).listAllByNamespaceId(namespace);
            verify(ruleService).listAllByNamespaceId(namespace);
            verify(appAuthService).listAllByNamespaceId(namespace);
            verify(metaDataService).listAllByNamespaceId(namespace);
            verify(proxySelectorService).listAllByNamespaceId(namespace);
            verify(discoveryUpstreamService).listAllByNamespaceId(namespace);
            verify(aiProxyApiKeyService).listAllByNamespaceId(namespace);
            SelectorData cached = (SelectorData) listener.fetchConfig(ConfigGroupEnum.SELECTOR, namespace).getData().get(0);
            assertEquals(namespace, cached.getNamespaceId());
        }
        verify(selectorService, never()).listAll();
        verify(ruleService, never()).listAll();
        verify(appAuthService, never()).listAll();
        verify(metaDataService, never()).listAll();
        verify(proxySelectorService, never()).listAll();
        verify(discoveryUpstreamService, never()).listAll();
        verify(aiProxyApiKeyService, never()).listAll();
    }

    @Test
    void splitsMixedNamespaceChangesForEverySyncGroup() {
        when(appAuthService.listAllByNamespaceId("namespace-a")).thenReturn(Collections.emptyList());
        when(appAuthService.listAllByNamespaceId("namespace-b")).thenReturn(Collections.emptyList());
        when(namespacePluginService.listAll("namespace-a")).thenReturn(Collections.emptyList());
        when(namespacePluginService.listAll("namespace-b")).thenReturn(Collections.emptyList());
        when(ruleService.listAllByNamespaceId("namespace-a")).thenReturn(Collections.emptyList());
        when(ruleService.listAllByNamespaceId("namespace-b")).thenReturn(Collections.emptyList());
        when(selectorService.listAllByNamespaceId("namespace-a")).thenReturn(Collections.emptyList());
        when(selectorService.listAllByNamespaceId("namespace-b")).thenReturn(Collections.emptyList());
        when(metaDataService.listAllByNamespaceId("namespace-a")).thenReturn(Collections.emptyList());
        when(metaDataService.listAllByNamespaceId("namespace-b")).thenReturn(Collections.emptyList());
        when(proxySelectorService.listAllByNamespaceId("namespace-a")).thenReturn(Collections.emptyList());
        when(proxySelectorService.listAllByNamespaceId("namespace-b")).thenReturn(Collections.emptyList());
        when(discoveryUpstreamService.listAllByNamespaceId("namespace-a")).thenReturn(Collections.emptyList());
        when(discoveryUpstreamService.listAllByNamespaceId("namespace-b")).thenReturn(Collections.emptyList());

        AppAuthData appAuthA = appAuthData("namespace-a");
        AppAuthData appAuthB = appAuthData("namespace-b");
        AppAuthData appAuthDefault = appAuthData(null);
        MetaData metaDataA = metaData("namespace-a");
        MetaData metaDataB = metaData("namespace-b");
        MetaData metaDataDefault = metaData(null);
        PluginData pluginA = pluginData("namespace-a");
        PluginData pluginB = pluginData("namespace-b");
        PluginData pluginDefault = pluginData(null);
        RuleData ruleA = ruleData("namespace-a");
        RuleData ruleB = ruleData("namespace-b");
        RuleData ruleDefault = ruleData(null);
        SelectorData selectorA = selectorData("namespace-a");
        SelectorData selectorB = selectorData("namespace-b");
        SelectorData selectorDefault = selectorData(null);
        ProxySelectorData proxySelectorA = proxySelectorData("namespace-a");
        ProxySelectorData proxySelectorB = proxySelectorData("namespace-b");
        ProxySelectorData proxySelectorDefault = proxySelectorData(null);
        DiscoverySyncData discoveryA = discoverySyncData("namespace-a");
        DiscoverySyncData discoveryB = discoverySyncData("namespace-b");
        DiscoverySyncData discoveryDefault = discoverySyncData(null);
        DataEventTypeEnum eventType = DataEventTypeEnum.UPDATE;

        listener.onAppAuthChanged(Lists.newArrayList(appAuthB, null, appAuthDefault, appAuthA), eventType);
        listener.onMetaDataChanged(Lists.newArrayList(metaDataB, null, metaDataDefault, metaDataA), eventType);
        listener.onPluginChanged(Lists.newArrayList(pluginB, null, pluginDefault, pluginA), eventType);
        listener.onRuleChanged(Lists.newArrayList(ruleB, null, ruleDefault, ruleA), eventType);
        listener.onSelectorChanged(Lists.newArrayList(selectorB, null, selectorDefault, selectorA), eventType);
        listener.onProxySelectorChanged(Lists.newArrayList(proxySelectorB, null, proxySelectorDefault, proxySelectorA), eventType);
        listener.onDiscoveryUpstreamChanged(Lists.newArrayList(discoveryB, null, discoveryDefault, discoveryA), eventType);

        assertEquals(21, listener.callbackBatches.size());
        assertCallbackBatch(ConfigGroupEnum.APP_AUTH, "namespace-a", appAuthA);
        assertCallbackBatch(ConfigGroupEnum.APP_AUTH, "namespace-b", appAuthB);
        assertCallbackBatch(ConfigGroupEnum.APP_AUTH, SYS_DEFAULT_NAMESPACE_ID, appAuthDefault);
        assertCallbackBatch(ConfigGroupEnum.META_DATA, "namespace-a", metaDataA);
        assertCallbackBatch(ConfigGroupEnum.META_DATA, "namespace-b", metaDataB);
        assertCallbackBatch(ConfigGroupEnum.META_DATA, SYS_DEFAULT_NAMESPACE_ID, metaDataDefault);
        assertCallbackBatch(ConfigGroupEnum.PLUGIN, "namespace-a", pluginA);
        assertCallbackBatch(ConfigGroupEnum.PLUGIN, "namespace-b", pluginB);
        assertCallbackBatch(ConfigGroupEnum.PLUGIN, SYS_DEFAULT_NAMESPACE_ID, pluginDefault);
        assertCallbackBatch(ConfigGroupEnum.RULE, "namespace-a", ruleA);
        assertCallbackBatch(ConfigGroupEnum.RULE, "namespace-b", ruleB);
        assertCallbackBatch(ConfigGroupEnum.RULE, SYS_DEFAULT_NAMESPACE_ID, ruleDefault);
        assertCallbackBatch(ConfigGroupEnum.SELECTOR, "namespace-a", selectorA);
        assertCallbackBatch(ConfigGroupEnum.SELECTOR, "namespace-b", selectorB);
        assertCallbackBatch(ConfigGroupEnum.SELECTOR, SYS_DEFAULT_NAMESPACE_ID, selectorDefault);
        assertCallbackBatch(ConfigGroupEnum.PROXY_SELECTOR, "namespace-a", proxySelectorA);
        assertCallbackBatch(ConfigGroupEnum.PROXY_SELECTOR, "namespace-b", proxySelectorB);
        assertCallbackBatch(ConfigGroupEnum.PROXY_SELECTOR, SYS_DEFAULT_NAMESPACE_ID, proxySelectorDefault);
        assertCallbackBatch(ConfigGroupEnum.DISCOVER_UPSTREAM, "namespace-a", discoveryA);
        assertCallbackBatch(ConfigGroupEnum.DISCOVER_UPSTREAM, "namespace-b", discoveryB);
        assertCallbackBatch(ConfigGroupEnum.DISCOVER_UPSTREAM, SYS_DEFAULT_NAMESPACE_ID, discoveryDefault);
    }

    private AppAuthData appAuthData(final String namespaceId) {
        AppAuthData data = mock(AppAuthData.class);
        when(data.getNamespaceId()).thenReturn(namespaceId);
        return data;
    }

    private MetaData metaData(final String namespaceId) {
        MetaData data = mock(MetaData.class);
        when(data.getNamespaceId()).thenReturn(namespaceId);
        return data;
    }

    private PluginData pluginData(final String namespaceId) {
        PluginData data = mock(PluginData.class);
        when(data.getNamespaceId()).thenReturn(namespaceId);
        return data;
    }

    private RuleData ruleData(final String namespaceId) {
        RuleData data = mock(RuleData.class);
        when(data.getNamespaceId()).thenReturn(namespaceId);
        return data;
    }

    private SelectorData selectorData(final String namespaceId) {
        SelectorData data = mock(SelectorData.class);
        when(data.getNamespaceId()).thenReturn(namespaceId);
        return data;
    }

    private ProxySelectorData proxySelectorData(final String namespaceId) {
        ProxySelectorData data = mock(ProxySelectorData.class);
        when(data.getNamespaceId()).thenReturn(namespaceId);
        return data;
    }

    private DiscoverySyncData discoverySyncData(final String namespaceId) {
        DiscoverySyncData data = mock(DiscoverySyncData.class);
        when(data.getNamespaceId()).thenReturn(namespaceId);
        return data;
    }

    private void assertCallbackBatch(final ConfigGroupEnum group, final String namespaceId, final Object expectedData) {
        List<?> changed = listener.callbackBatches.get(group.name() + "_" + namespaceId);
        assertEquals(1, changed.size());
        assertSame(expectedData, changed.get(0));
    }

    @AfterEach
    public void cleanUp() {
        listener.getCache().clear();
    }

    @Test
    public void testFetchConfig() {
        List<AppAuthData> appAuthDatas = Lists.newArrayList(mock(AppAuthData.class));
        listener.updateCache(ConfigGroupEnum.APP_AUTH, appAuthDatas, SYS_DEFAULT_NAMESPACE_ID);
        ConfigData<?> result1 = listener.fetchConfig(ConfigGroupEnum.APP_AUTH, SYS_DEFAULT_NAMESPACE_ID);
        assertNotNull(result1);

        List<PluginData> pluginDatas = Lists.newArrayList(mock(PluginData.class));
        listener.updateCache(ConfigGroupEnum.PLUGIN, pluginDatas, SYS_DEFAULT_NAMESPACE_ID);
        ConfigData<?> result2 = listener.fetchConfig(ConfigGroupEnum.PLUGIN, SYS_DEFAULT_NAMESPACE_ID);
        assertNotNull(result2);

        List<RuleData> ruleDatas = Lists.newArrayList(mock(RuleData.class));
        listener.updateCache(ConfigGroupEnum.RULE, ruleDatas, SYS_DEFAULT_NAMESPACE_ID);
        ConfigData<?> result3 = listener.fetchConfig(ConfigGroupEnum.RULE, SYS_DEFAULT_NAMESPACE_ID);
        assertNotNull(result3);

        List<SelectorData> selectorDatas = Lists.newArrayList(mock(SelectorData.class));
        listener.updateCache(ConfigGroupEnum.SELECTOR, selectorDatas, SYS_DEFAULT_NAMESPACE_ID);
        ConfigData<?> result4 = listener.fetchConfig(ConfigGroupEnum.SELECTOR, SYS_DEFAULT_NAMESPACE_ID);
        assertNotNull(result4);

        List<MetaData> metaDatas = Lists.newArrayList(mock(MetaData.class));
        listener.updateCache(ConfigGroupEnum.META_DATA, metaDatas, SYS_DEFAULT_NAMESPACE_ID);
        ConfigData<?> result5 = listener.fetchConfig(ConfigGroupEnum.META_DATA, SYS_DEFAULT_NAMESPACE_ID);
        assertNotNull(result5);
    }

    @Test
    public void testOnAppAuthChanged() {
        List<AppAuthData> empty = Lists.newArrayList();
        DataEventTypeEnum eventType = mock(DataEventTypeEnum.class);
        listener.onAppAuthChanged(empty, eventType);
        assertFalse(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.APP_AUTH.name())));
        List<AppAuthData> appAuthDatas = Lists.newArrayList(mock(AppAuthData.class));
        listener.onAppAuthChanged(appAuthDatas, eventType);
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.APP_AUTH.name())));
    }

    @Test
    public void testOnMetaDataChanged() {
        List<MetaData> empty = Lists.newArrayList();
        DataEventTypeEnum eventType = mock(DataEventTypeEnum.class);
        listener.onMetaDataChanged(empty, eventType);
        assertFalse(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.META_DATA.name())));
        List<MetaData> metaDatas = Lists.newArrayList(mock(MetaData.class));
        listener.onMetaDataChanged(metaDatas, eventType);
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.META_DATA.name())));
    }

    @Test
    public void testOnPluginChanged() {
        List<PluginData> empty = Lists.newArrayList();
        DataEventTypeEnum eventType = mock(DataEventTypeEnum.class);
        listener.onPluginChanged(empty, eventType);
        assertFalse(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.PLUGIN.name())));
        List<PluginData> pluginDatas = Lists.newArrayList(mock(PluginData.class));
        PluginData pluginData = new PluginData();
        pluginData.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        pluginDatas.set(0, pluginData);
        listener.onPluginChanged(pluginDatas, eventType);
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.PLUGIN.name())));
    }

    @Test
    public void testOnRuleChanged() {
        List<RuleData> empty = Lists.newArrayList();
        DataEventTypeEnum eventType = mock(DataEventTypeEnum.class);
        listener.onRuleChanged(empty, eventType);
        assertFalse(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.RULE.name())));
        List<RuleData> ruleDatas = Lists.newArrayList(mock(RuleData.class));
        listener.onRuleChanged(ruleDatas, eventType);
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.RULE.name())));
    }

    @Test
    public void testOnSelectorChanged() {
        List<SelectorData> empty = Lists.newArrayList();
        DataEventTypeEnum eventType = mock(DataEventTypeEnum.class);
        listener.onSelectorChanged(empty, eventType);
        assertFalse(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.SELECTOR.name())));
        List<SelectorData> selectorDatas = Lists.newArrayList(mock(SelectorData.class));
        listener.onSelectorChanged(selectorDatas, eventType);
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.SELECTOR.name())));
    }

    @Test
    public void testAfterPropertiesSet() {
        listener.afterPropertiesSet();
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.APP_AUTH.name())));
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.PLUGIN.name())));
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.RULE.name())));
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.SELECTOR.name())));
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.META_DATA.name())));
    }

    @Test
    public void testUpdateCache() {
        List<AppAuthData> appAuthDatas = Lists.newArrayList(mock(AppAuthData.class));
        listener.updateCache(ConfigGroupEnum.APP_AUTH, appAuthDatas, SYS_DEFAULT_NAMESPACE_ID);
        assertTrue(listener.getCache().containsKey(HttpLongPollingDataChangedListener.buildCacheKey(SYS_DEFAULT_NAMESPACE_ID, ConfigGroupEnum.APP_AUTH.name())));
    }

    static class MockAbstractDataChangedListener extends AbstractDataChangedListener {

        private final Map<String, List<?>> callbackBatches = new HashMap<>();

        @Override
        protected void afterInitialize() {
            // NOP
        }

        public ConcurrentMap<String, ConfigDataCache> getCache() {
            return CACHE;
        }

        @Override
        protected void afterAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
            recordCallback(ConfigGroupEnum.APP_AUTH, changed, namespaceId);
        }

        @Override
        protected void afterMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
            recordCallback(ConfigGroupEnum.META_DATA, changed, namespaceId);
        }

        @Override
        protected void afterPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
            super.afterPluginChanged(changed, eventType, namespaceId);
            recordCallback(ConfigGroupEnum.PLUGIN, changed, namespaceId);
        }

        @Override
        protected void afterRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
            recordCallback(ConfigGroupEnum.RULE, changed, namespaceId);
        }

        @Override
        protected void afterSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
            recordCallback(ConfigGroupEnum.SELECTOR, changed, namespaceId);
        }

        @Override
        protected void afterProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
            recordCallback(ConfigGroupEnum.PROXY_SELECTOR, changed, namespaceId);
        }

        @Override
        protected void afterDiscoveryUpstreamDataChanged(final List<DiscoverySyncData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
            recordCallback(ConfigGroupEnum.DISCOVER_UPSTREAM, changed, namespaceId);
        }

        private void recordCallback(final ConfigGroupEnum group, final List<?> changed, final String namespaceId) {
            callbackBatches.put(group.name() + "_" + namespaceId, changed);
        }
    }
}
