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

package org.apache.shenyu.plugin.sync.data.websocket.handler;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

public final class WebsocketDataHandlerTest {

    private PluginDataSubscriber pluginDataSubscriber;

    private WebsocketDataHandler websocketDataHandler;

    @BeforeEach
    public void testWebsocketDataHandler() {
        pluginDataSubscriber = mock(PluginDataSubscriber.class);
        List<AuthDataSubscriber> authDataSubscribers = new LinkedList<>();
        List<MetaDataSubscriber> metaDataSubscribers = new LinkedList<>();
        List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new LinkedList<>();
        List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers = new LinkedList<>();
        List<AiProxyApiKeyDataSubscriber> aiProxyApiKeyDataSubscribers = new LinkedList<>();
        websocketDataHandler = new WebsocketDataHandler(
                pluginDataSubscriber,
                metaDataSubscribers,
                authDataSubscribers,
                proxySelectorDataSubscribers,
                discoveryUpstreamDataSubscribers,
                aiProxyApiKeyDataSubscribers);
    }

    @Test
    public void testPluginRefreshExecutor() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.REFRESH.name());
        Mockito.verify(pluginDataSubscriber).refreshPluginDataAll();
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        Mockito.verify(pluginDataSubscriber).onPluginRefresh(pluginDataList);
    }

    @Test
    public void testPluginMyselfExecutor() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.MYSELF.name());
        Mockito.verify(pluginDataSubscriber).refreshPluginDataAll();
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        Mockito.verify(pluginDataSubscriber).onPluginRefresh(pluginDataList);
    }

    @Test
    public void testPluginUpdateExecutor() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.UPDATE.name());
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        pluginDataList.forEach(verify(pluginDataSubscriber)::onSubscribe);
    }

    @Test
    public void testPluginCreateExecutor() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.CREATE.name());
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        pluginDataList.forEach(verify(pluginDataSubscriber)::onSubscribe);
    }

    @Test
    public void testDuplicatedEventIsAppliedOnlyOnce() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.REFRESH.name());
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.REFRESH.name());
        Mockito.verify(pluginDataSubscriber, Mockito.times(1)).refreshPluginDataAll();
        Mockito.verify(pluginDataSubscriber, Mockito.times(1)).onPluginRefresh(Mockito.anyList());
    }

    @Test
    public void testChangedPayloadIsAppliedAgain() {
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, getJson("plugin_test"), DataEventTypeEnum.REFRESH.name());
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, getJson("plugin_test_2"), DataEventTypeEnum.REFRESH.name());
        Mockito.verify(pluginDataSubscriber, Mockito.times(2)).refreshPluginDataAll();
    }

    @Test
    public void testSamePayloadWithAnotherEventTypeIsApplied() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.REFRESH.name());
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.UPDATE.name());
        Mockito.verify(pluginDataSubscriber).refreshPluginDataAll();
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        pluginDataList.forEach(verify(pluginDataSubscriber)::onSubscribe);
    }

    @Test
    public void testSnapshotInvalidatesDeduplication() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.REFRESH.name());
        websocketDataHandler.snapshot(ConfigGroupEnum.PLUGIN, "[]", "namespace-a", "namespace-a");
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.REFRESH.name());
        Mockito.verify(pluginDataSubscriber, Mockito.times(2)).refreshPluginDataAll();
        Mockito.verify(pluginDataSubscriber).refreshPluginDataNamespace("namespace-a");
    }

    @Test
    public void testDeduplicationIsPerGroup() {
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, "[]", DataEventTypeEnum.REFRESH.name());
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, "[]", DataEventTypeEnum.REFRESH.name());
        websocketDataHandler.executor(ConfigGroupEnum.SELECTOR, "[]", DataEventTypeEnum.REFRESH.name());
        Mockito.verify(pluginDataSubscriber, Mockito.times(1)).refreshPluginDataAll();
        Mockito.verify(pluginDataSubscriber, Mockito.times(1)).refreshSelectorDataAll();
    }

    @Test
    public void testPluginDeleteExecutor() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.DELETE.name());
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        pluginDataList.forEach(verify(pluginDataSubscriber)::unSubscribe);
    }

    @Test
    public void testEmptySnapshotClearsOnlyItsGroup() {
        websocketDataHandler.snapshot(ConfigGroupEnum.RULE, "[]", "namespace-a", "namespace-a");
        verify(pluginDataSubscriber).refreshRuleDataNamespace("namespace-a");
        Mockito.verifyNoMoreInteractions(pluginDataSubscriber);
    }

    @Test
    public void testWrongNamespaceCannotClearCache() {
        org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class,
                () -> websocketDataHandler.snapshot(ConfigGroupEnum.PLUGIN, "[]", "namespace-b", "namespace-a"));
        Mockito.verifyNoInteractions(pluginDataSubscriber);
    }

    @Test
    public void testNullSnapshotCannotClearCache() {
        org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class,
                () -> websocketDataHandler.snapshot(ConfigGroupEnum.PLUGIN, "null", "namespace-a", "namespace-a"));
        Mockito.verifyNoInteractions(pluginDataSubscriber);
    }

    @Test
    public void testSnapshotReplacesStalePluginBeforeSubscribing() {
        websocketDataHandler.snapshot(ConfigGroupEnum.PLUGIN, getJson(), "namespace-a", "namespace-a");
        org.mockito.InOrder order = Mockito.inOrder(pluginDataSubscriber);
        order.verify(pluginDataSubscriber).refreshPluginDataNamespace("namespace-a");
        order.verify(pluginDataSubscriber).onSubscribe(Mockito.any(PluginData.class));
    }

    @Test
    public void testEmptySnapshotsRemoveStaleGatewayCache() {
        BaseDataCache cache = BaseDataCache.getInstance();
        PluginDataSubscriber subscriber = new CommonPluginDataSubscriber(Collections.emptyList(),
                new ShenyuConfig.SelectorMatchCache(), new ShenyuConfig.RuleMatchCache());
        WebsocketDataHandler handler = new WebsocketDataHandler(subscriber, Collections.emptyList(), Collections.emptyList(),
                Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
        cache.cachePluginData(PluginData.builder().name("snapshot-plugin").namespaceId("namespace-a").build());
        cache.cacheSelectData(SelectorData.builder().id("snapshot-selector").pluginName("snapshot-plugin").namespaceId("namespace-a").sort(1).build());
        cache.cacheRuleData(RuleData.builder().id("snapshot-rule").selectorId("snapshot-selector").namespaceId("namespace-a").sort(1).build());
        try {
            handler.snapshot(ConfigGroupEnum.RULE, "[]", "namespace-a", "namespace-a");
            org.junit.jupiter.api.Assertions.assertNull(cache.obtainRuleData("snapshot-selector"));
            org.junit.jupiter.api.Assertions.assertNotNull(cache.obtainSelectorData("snapshot-plugin"));
            handler.snapshot(ConfigGroupEnum.SELECTOR, "[]", "namespace-a", "namespace-a");
            org.junit.jupiter.api.Assertions.assertNull(cache.obtainSelectorData("snapshot-plugin"));
            org.junit.jupiter.api.Assertions.assertNotNull(cache.obtainPluginData("snapshot-plugin"));
            handler.snapshot(ConfigGroupEnum.PLUGIN, "[]", "namespace-a", "namespace-a");
            org.junit.jupiter.api.Assertions.assertNull(cache.obtainPluginData("snapshot-plugin"));
        } finally {
            cache.cleanRuleData();
            cache.cleanSelectorData();
            cache.cleanPluginData();
        }
    }

    @Test
    public void testUnsupportedNamespaceSnapshotsDoNotTouchOtherGroups() {
        MetaDataSubscriber metadata = mock(MetaDataSubscriber.class);
        AuthDataSubscriber auth = mock(AuthDataSubscriber.class);
        ProxySelectorDataSubscriber proxy = mock(ProxySelectorDataSubscriber.class);
        DiscoveryUpstreamDataSubscriber discovery = mock(DiscoveryUpstreamDataSubscriber.class);
        AiProxyApiKeyDataSubscriber apiKey = mock(AiProxyApiKeyDataSubscriber.class);
        WebsocketDataHandler handler = new WebsocketDataHandler(pluginDataSubscriber, List.of(metadata), List.of(auth),
                List.of(proxy), List.of(discovery), List.of(apiKey));
        for (ConfigGroupEnum group : List.of(ConfigGroupEnum.META_DATA, ConfigGroupEnum.APP_AUTH,
                ConfigGroupEnum.PROXY_SELECTOR, ConfigGroupEnum.DISCOVER_UPSTREAM, ConfigGroupEnum.AI_PROXY_API_KEY)) {
            org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class,
                    () -> handler.snapshot(group, "[]", "namespace-a", "namespace-a"));
        }
        Mockito.verifyNoInteractions(metadata, auth, proxy, discovery, apiKey);
    }

    @Test
    public void testEmptyNamespaceSnapshotPreservesOtherNamespaces() {
        BaseDataCache cache = BaseDataCache.getInstance();
        PluginDataSubscriber subscriber = new CommonPluginDataSubscriber(Collections.emptyList(),
                new ShenyuConfig.SelectorMatchCache(), new ShenyuConfig.RuleMatchCache());
        WebsocketDataHandler handler = new WebsocketDataHandler(subscriber, Collections.emptyList(), Collections.emptyList(),
                Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
        PluginData other = PluginData.builder().name("other-plugin").namespaceId("namespace-b").build();
        SelectorData selector = SelectorData.builder().id("other-selector").pluginName("other-plugin")
                .namespaceId("namespace-b").sort(1).build();
        RuleData rule = RuleData.builder().id("other-rule").selectorId("other-selector")
                .namespaceId("namespace-b").sort(1).build();
        cache.cachePluginData(other);
        cache.cacheSelectData(selector);
        cache.cacheRuleData(rule);
        try {
            for (ConfigGroupEnum group : List.of(ConfigGroupEnum.PLUGIN, ConfigGroupEnum.SELECTOR, ConfigGroupEnum.RULE)) {
                handler.snapshot(group, "[]", "namespace-a", "namespace-a");
            }
            org.junit.jupiter.api.Assertions.assertEquals(other, cache.obtainPluginData("other-plugin"));
            org.junit.jupiter.api.Assertions.assertEquals(List.of(selector), cache.obtainSelectorData("other-plugin"));
            org.junit.jupiter.api.Assertions.assertEquals(List.of(rule), cache.obtainRuleData("other-selector"));
            org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class,
                    () -> handler.snapshot(ConfigGroupEnum.PLUGIN, getJson(), "namespace-b", "namespace-b"));
            org.junit.jupiter.api.Assertions.assertEquals(other, cache.obtainPluginData("other-plugin"));
        } finally {
            cache.cleanPluginData();
            cache.cleanSelectorData();
            cache.cleanRuleData();
        }
    }

    @Test
    public void testNonEmptySnapshotRemovesMissingRulesAndPreservesOtherNamespace() {
        BaseDataCache cache = BaseDataCache.getInstance();
        PluginDataSubscriber subscriber = new CommonPluginDataSubscriber(Collections.emptyList(),
                new ShenyuConfig.SelectorMatchCache(), new ShenyuConfig.RuleMatchCache());
        WebsocketDataHandler handler = new WebsocketDataHandler(subscriber, Collections.emptyList(), Collections.emptyList(),
                Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
        RuleData stale = RuleData.builder().id("stale-rule").selectorId("selector-a")
                .namespaceId("namespace-a").sort(1).build();
        RuleData replacement = RuleData.builder().id("replacement-rule").selectorId("selector-a")
                .namespaceId("namespace-a").sort(2).build();
        RuleData other = RuleData.builder().id("other-rule").selectorId("selector-b")
                .namespaceId("namespace-b").sort(1).build();
        cache.cacheRuleData(stale);
        cache.cacheRuleData(other);
        try {
            handler.snapshot(ConfigGroupEnum.RULE, GsonUtils.getInstance().toJson(List.of(replacement)),
                    "namespace-a", "namespace-a");
            org.junit.jupiter.api.Assertions.assertEquals(List.of(replacement), cache.obtainRuleData("selector-a"));
            org.junit.jupiter.api.Assertions.assertEquals(List.of(other), cache.obtainRuleData("selector-b"));
        } finally {
            cache.cleanRuleData();
        }
    }

    private String getJson() {
        return getJson("plugin_test");
    }

    private String getJson(final String pluginName) {
        PluginData pluginData = new PluginData();
        pluginData.setId("1397952341475799040");
        pluginData.setName(pluginName);
        pluginData.setConfig("config_test");
        pluginData.setNamespaceId("namespace-a");
        pluginData.setEnabled(true);
        pluginData.setRole("1");
        LinkedList<PluginData> list = new LinkedList<>();
        list.add(pluginData);
        return GsonUtils.getGson().toJson(list);
    }
}
