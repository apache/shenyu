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
import java.util.List;
import java.util.concurrent.ConcurrentMap;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

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

        List<AppAuthData> appAuthDatas = Lists.newArrayList(mock(AppAuthData.class));
        when(appAuthService.listAll()).thenReturn(appAuthDatas);
        List<PluginData> pluginDatas = Lists.newArrayList(mock(PluginData.class));
        when(namespacePluginService.listAll(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(pluginDatas);
        List<RuleData> ruleDatas = Lists.newArrayList(mock(RuleData.class));
        when(ruleService.listAll()).thenReturn(ruleDatas);
        List<SelectorData> selectorDatas = Lists.newArrayList(mock(SelectorData.class));
        when(selectorService.listAll()).thenReturn(selectorDatas);
        List<MetaData> metaDatas = Lists.newArrayList(mock(MetaData.class));
        when(metaDataService.listAll()).thenReturn(metaDatas);
        List<ProxySelectorData> proxySelectorDatas = Lists.newArrayList(mock(ProxySelectorData.class));
        when(proxySelectorService.listAll()).thenReturn(proxySelectorDatas);
        List<DiscoverySyncData> discoverySyncDatas = Lists.newArrayList(mock(DiscoverySyncData.class));
        when(discoveryUpstreamService.listAll()).thenReturn(discoverySyncDatas);
        List<NamespaceVO> list = new ArrayList<>();
        NamespaceVO namespaceVO = new NamespaceVO();
        namespaceVO.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        list.add(namespaceVO);
        when(namespaceService.listAll()).thenReturn(list);

        // clear first
        listener.getCache().clear();
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

        @Override
        protected void afterInitialize() {
            // NOP
        }

        public ConcurrentMap<String, ConfigDataCache> getCache() {
            return CACHE;
        }
    }
}
