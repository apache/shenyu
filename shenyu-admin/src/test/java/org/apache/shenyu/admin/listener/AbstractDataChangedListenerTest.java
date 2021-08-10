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

import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.assertj.core.util.Lists;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.powermock.reflect.Whitebox;

import java.util.List;
import java.util.concurrent.ConcurrentMap;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The TestCase for AbstractDataChangedListener.
 */
public final class AbstractDataChangedListenerTest {

    private MockAbstractDataChangedListener listener;

    private AppAuthService appAuthService;

    private PluginService pluginService;

    private RuleService ruleService;

    private SelectorService selectorService;

    private MetaDataService metaDataService;

    @Before
    public void setUp() {
        listener = new MockAbstractDataChangedListener();
        appAuthService = mock(AppAuthService.class);
        pluginService = mock(PluginService.class);
        ruleService = mock(RuleService.class);
        selectorService = mock(SelectorService.class);
        metaDataService = mock(MetaDataService.class);

        Whitebox.setInternalState(listener, "appAuthService", appAuthService);
        Whitebox.setInternalState(listener, "pluginService", pluginService);
        Whitebox.setInternalState(listener, "ruleService", ruleService);
        Whitebox.setInternalState(listener, "selectorService", selectorService);
        Whitebox.setInternalState(listener, "metaDataService", metaDataService);

        List<AppAuthData> appAuthDatas = Lists.newArrayList(mock(AppAuthData.class));
        when(appAuthService.listAll()).thenReturn(appAuthDatas);
        List<PluginData> pluginDatas = Lists.newArrayList(mock(PluginData.class));
        when(pluginService.listAll()).thenReturn(pluginDatas);
        List<RuleData> ruleDatas = Lists.newArrayList(mock(RuleData.class));
        when(ruleService.listAll()).thenReturn(ruleDatas);
        List<SelectorData> selectorDatas = Lists.newArrayList(mock(SelectorData.class));
        when(selectorService.listAll()).thenReturn(selectorDatas);
        List<MetaData> metaDatas = Lists.newArrayList(mock(MetaData.class));
        when(metaDataService.listAll()).thenReturn(metaDatas);
    }

    @After
    public void cleanUp() {
        listener.getCache().clear();
    }

    @Test
    public void testFetchConfig() {
        List<AppAuthData> appAuthDatas = Lists.newArrayList(mock(AppAuthData.class));
        listener.updateCache(ConfigGroupEnum.APP_AUTH, appAuthDatas);
        ConfigData<?> result1 = listener.fetchConfig(ConfigGroupEnum.APP_AUTH);
        assertNotNull(result1);

        List<PluginData> pluginDatas = Lists.newArrayList(mock(PluginData.class));
        listener.updateCache(ConfigGroupEnum.PLUGIN, pluginDatas);
        ConfigData<?> result2 = listener.fetchConfig(ConfigGroupEnum.PLUGIN);
        assertNotNull(result2);

        List<RuleData> ruleDatas = Lists.newArrayList(mock(RuleData.class));
        listener.updateCache(ConfigGroupEnum.RULE, ruleDatas);
        ConfigData<?> result3 = listener.fetchConfig(ConfigGroupEnum.RULE);
        assertNotNull(result3);

        List<SelectorData> selectorDatas = Lists.newArrayList(mock(SelectorData.class));
        listener.updateCache(ConfigGroupEnum.SELECTOR, selectorDatas);
        ConfigData<?> result4 = listener.fetchConfig(ConfigGroupEnum.SELECTOR);
        assertNotNull(result4);

        List<MetaData> metaDatas = Lists.newArrayList(mock(MetaData.class));
        listener.updateCache(ConfigGroupEnum.META_DATA, metaDatas);
        ConfigData<?> result5 = listener.fetchConfig(ConfigGroupEnum.META_DATA);
        assertNotNull(result5);
    }

    @Test
    public void testOnAppAuthChanged() {
        List<AppAuthData> empty = Lists.newArrayList();
        DataEventTypeEnum eventType = mock(DataEventTypeEnum.class);
        listener.onAppAuthChanged(empty, eventType);
        assertFalse(listener.getCache().containsKey(ConfigGroupEnum.APP_AUTH.name()));
        List<AppAuthData> appAuthDatas = Lists.newArrayList(mock(AppAuthData.class));
        listener.onAppAuthChanged(appAuthDatas, eventType);
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.APP_AUTH.name()));
    }

    @Test
    public void testOnMetaDataChanged() {
        List<MetaData> empty = Lists.newArrayList();
        DataEventTypeEnum eventType = mock(DataEventTypeEnum.class);
        listener.onMetaDataChanged(empty, eventType);
        assertFalse(listener.getCache().containsKey(ConfigGroupEnum.META_DATA.name()));
        List<MetaData> metaDatas = Lists.newArrayList(mock(MetaData.class));
        listener.onMetaDataChanged(metaDatas, eventType);
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.META_DATA.name()));
    }

    @Test
    public void testOnPluginChanged() {
        List<PluginData> empty = Lists.newArrayList();
        DataEventTypeEnum eventType = mock(DataEventTypeEnum.class);
        listener.onPluginChanged(empty, eventType);
        assertFalse(listener.getCache().containsKey(ConfigGroupEnum.PLUGIN.name()));
        List<PluginData> pluginDatas = Lists.newArrayList(mock(PluginData.class));
        listener.onPluginChanged(pluginDatas, eventType);
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.PLUGIN.name()));
    }

    @Test
    public void testOnRuleChanged() {
        List<RuleData> empty = Lists.newArrayList();
        DataEventTypeEnum eventType = mock(DataEventTypeEnum.class);
        listener.onRuleChanged(empty, eventType);
        assertFalse(listener.getCache().containsKey(ConfigGroupEnum.RULE.name()));
        List<RuleData> ruleDatas = Lists.newArrayList(mock(RuleData.class));
        listener.onRuleChanged(ruleDatas, eventType);
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.RULE.name()));
    }

    @Test
    public void testOnSelectorChanged() {
        List<SelectorData> empty = Lists.newArrayList();
        DataEventTypeEnum eventType = mock(DataEventTypeEnum.class);
        listener.onSelectorChanged(empty, eventType);
        assertFalse(listener.getCache().containsKey(ConfigGroupEnum.SELECTOR.name()));
        List<SelectorData> selectorDatas = Lists.newArrayList(mock(SelectorData.class));
        listener.onSelectorChanged(selectorDatas, eventType);
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.SELECTOR.name()));
    }

    @Test
    public void testAfterPropertiesSet() {
        listener.afterPropertiesSet();
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.APP_AUTH.name()));
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.PLUGIN.name()));
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.RULE.name()));
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.SELECTOR.name()));
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.META_DATA.name()));
    }

    @Test
    public void testUpdateCache() {
        List<AppAuthData> appAuthDatas = Lists.newArrayList(mock(AppAuthData.class));
        listener.updateCache(ConfigGroupEnum.APP_AUTH, appAuthDatas);
        assertTrue(listener.getCache().containsKey(ConfigGroupEnum.APP_AUTH.name()));
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
