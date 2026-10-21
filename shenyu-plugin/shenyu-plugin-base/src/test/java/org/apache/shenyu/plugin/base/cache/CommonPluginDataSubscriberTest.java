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

package org.apache.shenyu.plugin.base.cache;

import com.google.common.collect.Lists;
import org.apache.shenyu.common.config.ShenyuConfig.RuleMatchCache;
import org.apache.shenyu.common.config.ShenyuConfig.SelectorMatchCache;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginHandlerEventEnum;
import org.apache.shenyu.common.utils.InitialSyncApplication;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ConfigurableApplicationContext;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test cases for CommonPluginDataSubscriber.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class CommonPluginDataSubscriberTest {

    private final String mockName1 = "MOCK_NAME_1";

    private final String mockName2 = "MOCK_NAME_2";

    private final String mockPluginName1 = "MOCK_PLUGIN_NAME_1";

    private final String mockPluginName2 = "MOCK_PLUGIN_NAME_2";

    private final String mockSelectorId1 = "MOCK_SELECTOR_ID_1";

    private final String mockSelectorId2 = "MOCK_SELECTOR_ID_2";

    private CommonPluginDataSubscriber commonPluginDataSubscriber;

    @Mock
    private ApplicationEventPublisher eventPublisher;
    
    private BaseDataCache baseDataCache;

    @Mock
    private PluginDataHandler handler;

    @BeforeEach
    public void setup() {
        this.mockShenyuTrieConfig();
        ArrayList<PluginDataHandler> pluginDataHandlerList = Lists.newArrayList();
        commonPluginDataSubscriber = new CommonPluginDataSubscriber(pluginDataHandlerList, eventPublisher, new SelectorMatchCache(), new RuleMatchCache());
        baseDataCache = BaseDataCache.getInstance();
        clearCaches();
        when(handler.pluginNamed()).thenReturn("divide");
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(handler));
    }

    @Test
    public void testOnSubscribe() {
        baseDataCache.cleanPluginData();

        PluginData pluginData = PluginData.builder().name(mockName1).build();
        commonPluginDataSubscriber.onSubscribe(pluginData);
        assertNotNull(baseDataCache.obtainPluginData(pluginData.getName()));
        assertEquals(pluginData, baseDataCache.obtainPluginData(pluginData.getName()));
    }


    @Test
    public void testPluginGroupRefreshPreservesSelectorAndRuleData() {
        final PluginData pluginData = PluginData.builder().name(mockPluginName1).build();
        final SelectorData selectorData = SelectorData.builder().id(mockSelectorId1).pluginName(mockPluginName1).build();
        final RuleData ruleData = RuleData.builder().id(mockName1).selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        baseDataCache.cacheSelectData(selectorData);
        baseDataCache.cacheRuleData(ruleData);

        commonPluginDataSubscriber.refreshPluginDataAll();
        commonPluginDataSubscriber.onSubscribe(pluginData);

        assertEquals(pluginData, baseDataCache.obtainPluginData(mockPluginName1));
        assertEquals(List.of(selectorData), baseDataCache.obtainSelectorData(mockPluginName1));
        assertEquals(List.of(ruleData), baseDataCache.obtainRuleData(mockSelectorId1));
    }
    @Test
    public void testReplaceExtendPluginDataHandlerWithoutOverridingBuiltInHandler() {
        PluginDataHandler firstExtendHandler = mock(PluginDataHandler.class);
        PluginDataHandler replacementExtendHandler = mock(PluginDataHandler.class);
        PluginDataHandler builtInHandler = mock(PluginDataHandler.class);
        PluginDataHandler conflictingExtendHandler = mock(PluginDataHandler.class);
        when(firstExtendHandler.pluginNamed()).thenReturn(mockPluginName1);
        when(replacementExtendHandler.pluginNamed()).thenReturn(mockPluginName1);
        when(builtInHandler.pluginNamed()).thenReturn(mockPluginName2);
        when(conflictingExtendHandler.pluginNamed()).thenReturn(mockPluginName2);
        CommonPluginDataSubscriber subscriber = new CommonPluginDataSubscriber(
                Lists.newArrayList(builtInHandler), eventPublisher, new SelectorMatchCache(), new RuleMatchCache());

        subscriber.putExtendPluginDataHandler(Lists.newArrayList(firstExtendHandler));
        subscriber.putExtendPluginDataHandler(Lists.newArrayList(replacementExtendHandler, conflictingExtendHandler));
        subscriber.onSelectorSubscribe(SelectorData.builder().pluginName(mockPluginName1).build());
        subscriber.onSelectorSubscribe(SelectorData.builder().pluginName(mockPluginName2).build());

        verify(replacementExtendHandler).handlerSelector(org.mockito.ArgumentMatchers.any());
        verify(firstExtendHandler, never()).handlerSelector(org.mockito.ArgumentMatchers.any());
        verify(builtInHandler).handlerSelector(org.mockito.ArgumentMatchers.any());
        verify(conflictingExtendHandler, never()).handlerSelector(org.mockito.ArgumentMatchers.any());
    }

    @Test
    public void testReloadedExtensionReceivesConfigurationUpdates() {
        PluginDataHandler replacement = mock(PluginDataHandler.class);
        when(replacement.pluginNamed()).thenReturn("divide");
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(replacement));
        PluginData pluginData = plugin("divide", 1);
        SelectorData selectorData = selector("selector", 1);
        RuleData ruleData = rule("rule", 1);

        commonPluginDataSubscriber.onSubscribe(pluginData);
        commonPluginDataSubscriber.onSelectorSubscribe(selectorData);
        commonPluginDataSubscriber.onRuleSubscribe(ruleData);

        verify(replacement).handlerPlugin(pluginData);
        verify(replacement).handlerSelector(selectorData);
        verify(replacement).handlerRule(ruleData);
        verify(handler, never()).handlerPlugin(any());
        verify(handler, never()).handlerSelector(any());
        verify(handler, never()).handlerRule(any());
    }

    @Test
    public void testReloadedExtensionReceivesConfigurationRemovals() {
        PluginDataHandler replacement = mock(PluginDataHandler.class);
        when(replacement.pluginNamed()).thenReturn("divide");
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(replacement));
        PluginData pluginData = plugin("divide", 1);
        SelectorData selectorData = selector("selector", 1);
        RuleData ruleData = rule("rule", 1);
        baseDataCache.cachePluginData(pluginData);
        baseDataCache.cacheSelectData(selectorData);
        baseDataCache.cacheRuleData(ruleData);

        commonPluginDataSubscriber.unRuleSubscribe(ruleData);
        commonPluginDataSubscriber.unSelectorSubscribe(selectorData);
        commonPluginDataSubscriber.unSubscribe(pluginData);

        verify(replacement).removePlugin(pluginData);
        verify(replacement).removeSelector(selectorData);
        verify(replacement).removeRule(ruleData);
        verify(handler, never()).removePlugin(any());
        verify(handler, never()).removeSelector(any());
        verify(handler, never()).removeRule(any());
    }

    @Test
    public void testExtensionCannotReplaceBuiltInHandler() {
        commonPluginDataSubscriber = new CommonPluginDataSubscriber(List.of(handler), eventPublisher, new SelectorMatchCache(), new RuleMatchCache());
        PluginDataHandler replacement = mock(PluginDataHandler.class);
        when(replacement.pluginNamed()).thenReturn("divide");
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(replacement));
        commonPluginDataSubscriber.removeExtendPluginDataHandlers(Set.of("divide"));
        PluginData pluginData = plugin("divide", 1);

        commonPluginDataSubscriber.onSubscribe(pluginData);

        verify(handler).handlerPlugin(pluginData);
        verify(replacement, never()).handlerPlugin(any());
    }

    @Test
    public void testRemovedExtensionCanBeRegisteredAgain() {
        commonPluginDataSubscriber.removeExtendPluginDataHandlers(Set.of("divide"));
        commonPluginDataSubscriber.onSubscribe(plugin("divide", 1));
        verify(handler, never()).handlerPlugin(any());

        PluginDataHandler replacement = mock(PluginDataHandler.class);
        when(replacement.pluginNamed()).thenReturn("divide");
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(replacement));
        PluginData pluginData = plugin("divide", 2);
        commonPluginDataSubscriber.onSubscribe(pluginData);

        verify(replacement).handlerPlugin(pluginData);
        verify(handler, never()).handlerPlugin(any());
    }

    @Test
    public void testRemovalKeepsUnrelatedExtensionHandler() {
        PluginDataHandler unrelated = mock(PluginDataHandler.class);
        when(unrelated.pluginNamed()).thenReturn("unrelated");
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(unrelated));

        commonPluginDataSubscriber.removeExtendPluginDataHandlers(Set.of("divide", "unknown"));
        PluginData pluginData = plugin("unrelated", 1);
        commonPluginDataSubscriber.onSubscribe(pluginData);

        verify(unrelated).handlerPlugin(pluginData);
    }

    @Test
    public void testReloadedExtensionReceivesRefreshCallbacks() {
        PluginDataHandler replacement = mock(PluginDataHandler.class);
        when(replacement.pluginNamed()).thenReturn("divide");
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(replacement));
        PluginData pluginData = plugin("divide", 1);
        SelectorData selectorData = selector("selector", 1);
        RuleData ruleData = rule("rule", 1);

        commonPluginDataSubscriber.onPluginRefresh(List.of(pluginData));
        commonPluginDataSubscriber.onSelectorRefresh(List.of(selectorData));
        commonPluginDataSubscriber.onRuleRefresh(List.of(ruleData));

        verify(replacement).handlerPlugin(pluginData);
        verify(replacement).handlerSelector(selectorData);
        verify(replacement).handlerRule(ruleData);
        verify(handler, never()).handlerPlugin(any());
        verify(handler, never()).handlerSelector(any());
        verify(handler, never()).handlerRule(any());
    }

    @Test
    void testPluginSnapshotKeepsExistingEntryUntilReplacementIsApplied() {
        final PluginData oldPlugin = PluginData.builder().id("divide-id").name("divide").config("old")
                .namespaceId("namespace-a").build();
        final PluginData replacement = PluginData.builder().id("divide-id").name("divide").config("new")
                .namespaceId("namespace-a").build();
        final PluginData stale = PluginData.builder().id("removed-id").name("removed").namespaceId("namespace-a").build();
        final PluginData otherNamespace = PluginData.builder().id("other-id").name("other").namespaceId("namespace-b").build();
        baseDataCache.cachePluginData(oldPlugin);
        baseDataCache.cachePluginData(stale);
        baseDataCache.cachePluginData(otherNamespace);
        doAnswer(invocation -> {
            assertEquals(oldPlugin, baseDataCache.obtainPluginData("divide"));
            return null;
        }).when(handler).handlerPlugin(replacement);

        commonPluginDataSubscriber.applyPluginDataSnapshot("namespace-a", List.of(replacement));

        assertEquals(replacement, baseDataCache.obtainPluginData("divide"));
        assertNull(baseDataCache.obtainPluginData("removed"));
        assertEquals(otherNamespace, baseDataCache.obtainPluginData("other"));
    }

    @Test
    void testSelectorSnapshotReplacesRowsAndRemovesOnlyStaleNamespaceRows() {
        final SelectorData oldSelector = SelectorData.builder().id("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        final SelectorData replacement = SelectorData.builder().id("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(2).build();
        final SelectorData stale = SelectorData.builder().id("stale-selector").pluginName("divide")
                .namespaceId("namespace-a").sort(3).build();
        final SelectorData otherNamespace = SelectorData.builder().id("other-selector").pluginName("divide")
                .namespaceId("namespace-b").sort(4).build();
        baseDataCache.cacheSelectData(oldSelector);
        baseDataCache.cacheSelectData(stale);
        baseDataCache.cacheSelectData(otherNamespace);

        commonPluginDataSubscriber.applySelectorDataSnapshot("namespace-a", List.of(replacement));

        assertEquals(List.of(replacement, otherNamespace), baseDataCache.obtainSelectorData("divide"));
    }

    @Test
    void testRuleSnapshotReplacesRowsAndRemovesOnlyStaleNamespaceRows() {
        final RuleData oldRule = RuleData.builder().id("rule-a").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        final RuleData replacement = RuleData.builder().id("rule-a").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(2).build();
        final RuleData stale = RuleData.builder().id("stale-rule").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(3).build();
        final RuleData otherNamespace = RuleData.builder().id("other-rule").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-b").sort(4).build();
        baseDataCache.cacheRuleData(oldRule);
        baseDataCache.cacheRuleData(stale);
        baseDataCache.cacheRuleData(otherNamespace);

        commonPluginDataSubscriber.applyRuleDataSnapshot("namespace-a", List.of(replacement));

        assertEquals(List.of(replacement, otherNamespace), baseDataCache.obtainRuleData("selector-a"));
    }

    @Test
    public void testSelectorSnapshotRemovesPluginBucketAbsentFromPayload() {
        // a complete namespace snapshot replaces every plugin of the namespace, including a plugin
        // whose whole bucket is absent from the payload because its last row was deleted
        final PluginDataHandler grpcHandler = mock(PluginDataHandler.class);
        when(grpcHandler.pluginNamed()).thenReturn("grpc");
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(grpcHandler));
        final SelectorData divideRow = SelectorData.builder().id("s-divide").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        final SelectorData grpcRow = SelectorData.builder().id("s-grpc").pluginName("grpc")
                .namespaceId("ns1").sort(1).build();
        baseDataCache.cacheSelectData(divideRow);
        baseDataCache.cacheSelectData(grpcRow);

        commonPluginDataSubscriber.applySelectorDataSnapshot("ns1", List.of(divideRow));

        assertEquals(List.of(divideRow), baseDataCache.obtainSelectorData("divide"));
        assertNull(baseDataCache.obtainSelectorData("grpc"));
        verify(grpcHandler).removeSelector(grpcRow);
    }

    @Test
    public void testRuleSnapshotRemovesPluginBucketAbsentFromPayload() {
        final PluginDataHandler grpcHandler = mock(PluginDataHandler.class);
        when(grpcHandler.pluginNamed()).thenReturn("grpc");
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(grpcHandler));
        final RuleData divideRule = RuleData.builder().id("r-divide").selectorId("selector-1").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        final RuleData grpcRule = RuleData.builder().id("r-grpc").selectorId("selector-2").pluginName("grpc")
                .namespaceId("ns1").sort(1).build();
        baseDataCache.cacheRuleData(divideRule);
        baseDataCache.cacheRuleData(grpcRule);

        commonPluginDataSubscriber.applyRuleDataSnapshot("ns1", List.of(divideRule));

        assertEquals(List.of(divideRule), baseDataCache.obtainRuleData("selector-1"));
        assertNull(baseDataCache.obtainRuleData("selector-2"));
        verify(grpcHandler).removeRule(grpcRule);
    }

    @Test
    void testInitialSyncPropagatesHandlerFailureWithoutChangingLegacyBehavior() {
        PluginDataHandler handler = mock(PluginDataHandler.class);
        org.mockito.Mockito.when(handler.pluginNamed()).thenReturn(mockName1);
        PluginData data = PluginData.builder().name(mockName1).build();
        doThrow(new IllegalStateException("handler failed")).when(handler).handlerPlugin(data);
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(handler));
        assertThrows(IllegalStateException.class, () -> InitialSyncApplication.run(() -> commonPluginDataSubscriber.onSubscribe(data)));
        assertFalse(InitialSyncApplication.isActive());
        assertDoesNotThrow(() -> commonPluginDataSubscriber.onSubscribe(data));
    }

    @Test
    void testInitialSyncTracksDeferredRefreshApplication() {
        PluginData data = PluginData.builder().name("divide").build();
        CompletableFuture<Void> deferred = new CompletableFuture<>();
        doAnswer(invocation -> {
            InitialSyncApplication.register(deferred);
            return null;
        }).when(handler).handlerPlugin(data);
        CompletableFuture<Void> application = InitialSyncApplication.run(() -> commonPluginDataSubscriber.onPluginRefresh(List.of(data)));
        assertFalse(application.isDone());
        assertFalse(InitialSyncApplication.isActive());
        deferred.completeExceptionally(new IllegalStateException("deferred refresh failed"));
        assertTrue(application.isCompletedExceptionally());
    }

    @Test
    void testInitialSyncPropagatesRefreshFailure() {
        PluginData data = PluginData.builder().name("divide").build();
        doThrow(new IllegalStateException("refresh failed")).when(handler).handlerPlugin(data);
        assertThrows(IllegalStateException.class, () -> InitialSyncApplication.run(() -> commonPluginDataSubscriber.onPluginRefresh(List.of(data))));
        assertFalse(InitialSyncApplication.isActive());
    }

    @Test
    public void testUnSubscribe() {
        baseDataCache.cleanPluginData();
        PluginData pluginData = PluginData.builder().name(mockName1).build();
        baseDataCache.cachePluginData(pluginData);
        assertNotNull(baseDataCache.obtainPluginData(pluginData.getName()));
        
        commonPluginDataSubscriber.unSubscribe(pluginData);
        assertNull(baseDataCache.obtainPluginData(pluginData.getName()));
    }

    @Test
    public void testRefreshPluginDataAll() {
        baseDataCache.cleanPluginData();
        PluginData firstCachedPluginData = PluginData.builder().name(mockName1).build();
        PluginData secondCachedPluginData = PluginData.builder().name(mockName2).build();
        baseDataCache.cachePluginData(firstCachedPluginData);
        baseDataCache.cachePluginData(secondCachedPluginData);
        assertNotNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        assertNotNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));

        commonPluginDataSubscriber.refreshPluginDataAll();
        assertNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        assertNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));
    }

    @Test
    public void testRefreshPluginDataSelf() {
        baseDataCache.cleanPluginData();
        PluginData firstCachedPluginData = PluginData.builder().name(mockName1).build();
        PluginData secondCachedPluginData = PluginData.builder().name(mockName2).build();
        baseDataCache.cachePluginData(firstCachedPluginData);
        baseDataCache.cachePluginData(secondCachedPluginData);
        assertNotNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        assertNotNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));

        commonPluginDataSubscriber.refreshPluginDataSelf(Lists.newArrayList(firstCachedPluginData));
        assertNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        assertNotNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));
    }

    @Test
    public void testOnSelectorSubscribe() {
        baseDataCache.cleanSelectorData();

        SelectorData selectorData = SelectorData.builder().id("1").enabled(true).pluginName(mockPluginName1).sort(1).build();
        commonPluginDataSubscriber.onSelectorSubscribe(selectorData);
        List<SelectorData> obtainSelectorData = baseDataCache.obtainSelectorData(selectorData.getPluginName());
        assertEquals(Lists.newArrayList(selectorData), obtainSelectorData);
    }

    @Test
    public void testUnSelectorSubscribe() {
        final String path = "/selector";
        final String emptyRulePath = "/empty-rule";
        final String unrelatedRulePath = "/unrelated-rule";
        final MatchDataCache matchDataCache = MatchDataCache.getInstance();
        baseDataCache.cleanSelectorData();
        matchDataCache.cleanSelectorData();
        matchDataCache.cleanRuleDataData();

        final SelectorData selectorData = SelectorData.builder().id(mockSelectorId1).enabled(true).pluginName(mockPluginName1).build();
        final RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        final RuleData emptyRuleData = RuleData.builder().pluginName(mockPluginName1).build();
        final RuleData unrelatedRuleData = RuleData.builder().id("2").selectorId(mockSelectorId2).pluginName(mockPluginName1).build();
        baseDataCache.cacheSelectData(selectorData);
        matchDataCache.cacheSelectorData(path, selectorData, 100, 100);
        matchDataCache.cacheRuleData(path, ruleData, 100, 100);
        matchDataCache.cacheRuleData(emptyRulePath, emptyRuleData, 100, 100);
        matchDataCache.cacheRuleData(unrelatedRulePath, unrelatedRuleData, 100, 100);

        try {
            assertNotNull(baseDataCache.obtainSelectorData(selectorData.getPluginName()));
            assertEquals(selectorData, matchDataCache.obtainSelectorData(mockPluginName1, path));
            assertEquals(ruleData, matchDataCache.obtainRuleData(mockPluginName1, path));

            commonPluginDataSubscriber.unSelectorSubscribe(selectorData);

            assertNull(baseDataCache.obtainSelectorData(selectorData.getPluginName()));
            assertNull(matchDataCache.obtainSelectorData(mockPluginName1, path));
            assertNull(matchDataCache.obtainRuleData(mockPluginName1, path));
            assertNull(matchDataCache.obtainRuleData(mockPluginName1, emptyRulePath));
            assertEquals(unrelatedRuleData, matchDataCache.obtainRuleData(mockPluginName1, unrelatedRulePath));
        } finally {
            matchDataCache.cleanSelectorData();
            matchDataCache.cleanRuleDataData();
        }
    }

    @Test
    public void testUnSelectorSubscribeWithMissingPluginName() {
        final String path = "/dangling";
        final MatchDataCache matchDataCache = MatchDataCache.getInstance();
        baseDataCache.cleanSelectorData();
        matchDataCache.cleanSelectorData();
        matchDataCache.cleanRuleDataData();

        // the gateway cached the selector under its real plugin name before the plugin row vanished
        final SelectorData cachedSelector = SelectorData.builder().id(mockSelectorId1).enabled(true).pluginName(mockPluginName1).build();
        final SelectorData otherPluginSelector = SelectorData.builder().id(mockSelectorId2).enabled(true).pluginName(mockPluginName2).build();
        final RuleData cachedRule = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        baseDataCache.cacheSelectData(cachedSelector);
        baseDataCache.cacheSelectData(otherPluginSelector);
        matchDataCache.cacheSelectorData(path, cachedSelector, 100, 100);
        matchDataCache.cacheRuleData(path, cachedRule, 100, 100);

        // the admin cannot resolve a plugin name for the dangling selector, so the delete event carries none
        final SelectorData deletion = SelectorData.builder().id(mockSelectorId1).enabled(true).build();
        commonPluginDataSubscriber.unSelectorSubscribe(deletion);

        assertNull(baseDataCache.obtainSelectorData(mockPluginName1));
        assertNull(matchDataCache.obtainSelectorData(mockPluginName1, path));
        assertNull(matchDataCache.obtainRuleData(mockPluginName1, path));
        // an unrelated plugin keeps its selector
        assertEquals(Lists.newArrayList(otherPluginSelector), baseDataCache.obtainSelectorData(mockPluginName2));
    }

    @Test
    public void testUnRuleSubscribeWithMissingPluginName() {
        final MatchDataCache matchDataCache = MatchDataCache.getInstance();
        baseDataCache.cleanRuleData();
        matchDataCache.cleanRuleDataData();

        final RuleData cachedRule = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        baseDataCache.cacheRuleData(cachedRule);
        matchDataCache.cacheRuleData("/rule", cachedRule, 100, 100);

        // the admin cannot resolve a plugin name for a rule of a deleted plugin, so the delete event carries none
        final RuleData deletion = RuleData.builder().id("1").selectorId(mockSelectorId1).build();
        commonPluginDataSubscriber.unRuleSubscribe(deletion);

        assertNull(baseDataCache.obtainRuleData(mockSelectorId1));
        assertNull(matchDataCache.obtainRuleData(mockPluginName1, "/rule"));
    }

    @Test
    public void testRefreshSelectorDataAll() {
        baseDataCache.cleanSelectorData();
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").enabled(true).pluginName(mockPluginName1).build();
        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").enabled(true).pluginName(mockPluginName2).build();
        baseDataCache.cacheSelectData(firstCachedSelectorData);
        baseDataCache.cacheSelectData(secondCachedSelectorData);
        assertNotNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        assertNotNull(baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));

        commonPluginDataSubscriber.refreshSelectorDataAll();
        assertNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        assertNull(baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));
    }

    @Test
    public void testRefreshSelectorDataSelf() {
        baseDataCache.cleanSelectorData();
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").pluginName(mockPluginName2).build();
        baseDataCache.cacheSelectData(firstCachedSelectorData);
        baseDataCache.cacheSelectData(secondCachedSelectorData);
        assertNotNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        assertNotNull(baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));

        commonPluginDataSubscriber.refreshSelectorDataSelf(Lists.newArrayList(firstCachedSelectorData));
        assertNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        assertEquals(Lists.newArrayList(secondCachedSelectorData), baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));
    }

    @Test
    public void testOnRuleSubscribe() {
        baseDataCache.cleanRuleData();

        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).enabled(true).pluginName(mockPluginName1).sort(1).build();
        commonPluginDataSubscriber.onRuleSubscribe(ruleData);
        assertNotNull(baseDataCache.obtainRuleData(ruleData.getSelectorId()));
        assertEquals(Lists.newArrayList(ruleData), baseDataCache.obtainRuleData(ruleData.getSelectorId()));
    }

    @Test
    public void testUnRuleSubscribe() {
        baseDataCache.cleanRuleData();
        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).sort(1).build();
        baseDataCache.cacheRuleData(ruleData);
        assertNotNull(baseDataCache.obtainRuleData(ruleData.getSelectorId()));

        commonPluginDataSubscriber.unRuleSubscribe(ruleData);
        assertNull(baseDataCache.obtainRuleData(ruleData.getSelectorId()));
    }

    @Test
    public void testRefreshRuleDataAll() {
        baseDataCache.cleanRuleData();
        RuleData firstCachedRuleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        RuleData secondCachedRuleData = RuleData.builder().id("2").selectorId(mockSelectorId2).pluginName(mockPluginName2).build();
        baseDataCache.cacheRuleData(firstCachedRuleData);
        baseDataCache.cacheRuleData(secondCachedRuleData);
        assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));

        commonPluginDataSubscriber.refreshRuleDataAll();
        assertNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        assertNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
    }

    @Test
    public void testRefreshRuleDataSelf() {
        baseDataCache.cleanRuleData();
        RuleData firstCachedRuleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        RuleData secondCachedRuleData = RuleData.builder().id("2").selectorId(mockSelectorId2).pluginName(mockPluginName2).build();
        baseDataCache.cacheRuleData(firstCachedRuleData);
        baseDataCache.cacheRuleData(secondCachedRuleData);
        assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));

        commonPluginDataSubscriber.refreshRuleDataSelf(Lists.newArrayList(firstCachedRuleData));
        assertNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        assertEquals(Lists.newArrayList(secondCachedRuleData), baseDataCache.obtainRuleData(secondCachedRuleData.getSelectorId()));
    }

    @Test
    public void testRefreshSelectorDataScopedRebuildsOnlyPayloadPlugins() {
        final SelectorData pluginARow = SelectorData.builder().id("s-a").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        final SelectorData pluginBRow = SelectorData.builder().id("s-b").pluginName("pluginB")
                .namespaceId("namespace-a").sort(1).build();
        baseDataCache.cacheSelectData(pluginARow);
        baseDataCache.cacheSelectData(pluginBRow);
        final SelectorData pluginAUpdated = SelectorData.builder().id("s-a").pluginName("divide")
                .namespaceId("namespace-a").sort(2).build();

        commonPluginDataSubscriber.refreshSelectorDataScoped(List.of(pluginAUpdated));

        assertEquals(List.of(pluginAUpdated), baseDataCache.obtainSelectorData("divide"));
        assertEquals(List.of(pluginBRow), baseDataCache.obtainSelectorData("pluginB"));
    }

    @Test
    public void testRefreshSelectorDataScopedDoesNotTouchOtherNamespaceOrPlugin() {
        final SelectorData ns1Divide = SelectorData.builder().id("s-ns1").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        final SelectorData ns2Divide = SelectorData.builder().id("s-ns2").pluginName("divide")
                .namespaceId("ns2").sort(1).build();
        final SelectorData ns1Other = SelectorData.builder().id("s-other").pluginName("pluginB")
                .namespaceId("ns1").sort(1).build();
        final SelectorData orphan = SelectorData.builder().id("s-orphan").pluginName("divide")
                .namespaceId("ns1").sort(2).build();
        baseDataCache.cacheSelectData(ns1Divide);
        baseDataCache.cacheSelectData(ns2Divide);
        baseDataCache.cacheSelectData(ns1Other);
        baseDataCache.cacheSelectData(orphan);

        final SelectorData ns1Replacement = SelectorData.builder().id("s-ns1").pluginName("divide")
                .namespaceId("ns1").sort(3).build();
        commonPluginDataSubscriber.refreshSelectorDataScoped(List.of(ns1Replacement));

        // a stale filter missing the namespace condition would also delete ns2Divide
        assertEquals(List.of(ns2Divide, ns1Replacement), baseDataCache.obtainSelectorData("divide"));
        assertEquals(List.of(ns1Other), baseDataCache.obtainSelectorData("pluginB"));
        verify(handler).removeSelector(orphan);
    }

    @Test
    public void testRefreshSelectorDataScopedDoesNotDeleteCrossNamespacePluginRows() {
        final SelectorData ns1Divide = SelectorData.builder().id("s-ns1-divide").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        final SelectorData ns2Divide = SelectorData.builder().id("s-ns2-divide").pluginName("divide")
                .namespaceId("ns2").sort(2).build();
        final SelectorData ns1Other = SelectorData.builder().id("s-ns1-other").pluginName("pluginB")
                .namespaceId("ns1").sort(2).build();
        final SelectorData ns2Other = SelectorData.builder().id("s-ns2-other").pluginName("pluginB")
                .namespaceId("ns2").sort(1).build();
        baseDataCache.cacheSelectData(ns2Divide);
        baseDataCache.cacheSelectData(ns1Other);

        commonPluginDataSubscriber.refreshSelectorDataScoped(List.of(ns1Divide, ns2Other));

        assertEquals(List.of(ns1Divide, ns2Divide), baseDataCache.obtainSelectorData("divide"));
        assertEquals(List.of(ns2Other, ns1Other), baseDataCache.obtainSelectorData("pluginB"));
    }

    @Test
    public void testRefreshSelectorDataScopedNormalizesNullNamespace() {
        final SelectorData cached = SelectorData.builder().id("s-cached").pluginName("divide")
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID).sort(1).build();
        final SelectorData incoming = SelectorData.builder().id("s-incoming").pluginName("divide")
                .namespaceId(null).sort(2).build();
        baseDataCache.cacheSelectData(cached);

        commonPluginDataSubscriber.refreshSelectorDataScoped(List.of(incoming));

        assertEquals(Constants.SYS_DEFAULT_NAMESPACE_ID, incoming.getNamespaceId());
        assertEquals(List.of(incoming), baseDataCache.obtainSelectorData("divide"));
        verify(handler).removeSelector(cached);
    }

    @Test
    public void testRefreshRuleDataScopedNormalizesNullNamespace() {
        final RuleData cached = RuleData.builder().id("r-cached").selectorId("selector-1").pluginName("divide")
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID).sort(1).build();
        final RuleData incoming = RuleData.builder().id("r-incoming").selectorId("selector-1").pluginName("divide")
                .namespaceId(null).sort(2).build();
        baseDataCache.cacheRuleData(cached);

        commonPluginDataSubscriber.refreshRuleDataScoped(List.of(incoming));

        assertEquals(Constants.SYS_DEFAULT_NAMESPACE_ID, incoming.getNamespaceId());
        assertEquals(List.of(incoming), baseDataCache.obtainRuleData("selector-1"));
        verify(handler).removeRule(cached);
    }

    @Test
    public void testRefreshSelectorDataScopedHandlerFailurePropagatesAndCacheConverges() {
        final SelectorData staleRow = SelectorData.builder().id("s-stale").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        baseDataCache.cacheSelectData(staleRow);
        final SelectorData failing = SelectorData.builder().id("s-failure").pluginName("divide")
                .namespaceId("ns1").sort(2).build();
        final SelectorData succeeding = SelectorData.builder().id("s-next").pluginName("divide")
                .namespaceId("ns1").sort(3).build();
        doThrow(new IllegalStateException("handler failed")).when(handler).handlerSelector(failing);

        assertThrows(IllegalStateException.class, () -> commonPluginDataSubscriber.refreshSelectorDataScoped(List.of(failing, succeeding)));

        // the cache already converged before the callbacks ran, so the failure does not roll it back,
        // and the failing row aborts the remaining callbacks like the legacy refresh did
        assertEquals(List.of(failing, succeeding), baseDataCache.obtainSelectorData("divide"));
        verify(handler).removeSelector(staleRow);
        verify(handler, never()).handlerSelector(succeeding);
    }

    @Test
    public void testRefreshRuleDataScopedRebuildsOnlyPayloadPlugins() {
        final RuleData divideRule = RuleData.builder().id("r-1").selectorId("selector-1").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        final RuleData otherPluginRule = RuleData.builder().id("r-2").selectorId("selector-2").pluginName("pluginB")
                .namespaceId("namespace-a").sort(1).build();
        baseDataCache.cacheRuleData(divideRule);
        baseDataCache.cacheRuleData(otherPluginRule);
        final RuleData divideUpdated = RuleData.builder().id("r-1").selectorId("selector-1").pluginName("divide")
                .namespaceId("namespace-a").sort(2).build();

        commonPluginDataSubscriber.refreshRuleDataScoped(List.of(divideUpdated));

        assertEquals(List.of(divideUpdated), baseDataCache.obtainRuleData("selector-1"));
        assertEquals(List.of(otherPluginRule), baseDataCache.obtainRuleData("selector-2"));
    }

    @Test
    public void testRefreshRuleDataScopedDoesNotTouchOtherNamespaceOrPlugin() {
        final RuleData ns1Divide = RuleData.builder().id("r-ns1").selectorId("selector-1").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        final RuleData ns2Divide = RuleData.builder().id("r-ns2").selectorId("selector-1").pluginName("divide")
                .namespaceId("ns2").sort(1).build();
        final RuleData ns1Other = RuleData.builder().id("r-other").selectorId("selector-2").pluginName("pluginB")
                .namespaceId("ns1").sort(1).build();
        final RuleData orphan = RuleData.builder().id("r-orphan").selectorId("selector-3").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        baseDataCache.cacheRuleData(ns1Divide);
        baseDataCache.cacheRuleData(ns2Divide);
        baseDataCache.cacheRuleData(ns1Other);
        baseDataCache.cacheRuleData(orphan);

        final RuleData ns1Replacement = RuleData.builder().id("r-ns1").selectorId("selector-1").pluginName("divide")
                .namespaceId("ns1").sort(2).build();
        commonPluginDataSubscriber.refreshRuleDataScoped(List.of(ns1Replacement));

        // a stale filter missing the namespace condition would also delete ns2Divide
        assertEquals(List.of(ns2Divide, ns1Replacement), baseDataCache.obtainRuleData("selector-1"));
        assertEquals(List.of(ns1Other), baseDataCache.obtainRuleData("selector-2"));
        assertNull(baseDataCache.obtainRuleData("selector-3"));
        verify(handler).removeRule(orphan);
    }

    @Test
    public void testRefreshRuleDataScopedDoesNotDeleteCrossNamespacePluginRows() {
        final RuleData ns1Divide = RuleData.builder().id("r-ns1-divide").selectorId("selector-divide")
                .pluginName("divide").namespaceId("ns1").sort(1).build();
        final RuleData ns2Divide = RuleData.builder().id("r-ns2-divide").selectorId("selector-divide")
                .pluginName("divide").namespaceId("ns2").sort(2).build();
        final RuleData ns1Other = RuleData.builder().id("r-ns1-other").selectorId("selector-other")
                .pluginName("pluginB").namespaceId("ns1").sort(2).build();
        final RuleData ns2Other = RuleData.builder().id("r-ns2-other").selectorId("selector-other")
                .pluginName("pluginB").namespaceId("ns2").sort(1).build();
        baseDataCache.cacheRuleData(ns2Divide);
        baseDataCache.cacheRuleData(ns1Other);

        commonPluginDataSubscriber.refreshRuleDataScoped(List.of(ns1Divide, ns2Other));

        assertEquals(List.of(ns1Divide, ns2Divide), baseDataCache.obtainRuleData("selector-divide"));
        assertEquals(List.of(ns2Other, ns1Other), baseDataCache.obtainRuleData("selector-other"));
    }

    @Test
    public void testRefreshRuleDataScopedHandlerFailurePropagatesAndCacheConverges() {
        final RuleData staleRow = RuleData.builder().id("r-stale").selectorId("selector-failure").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        baseDataCache.cacheRuleData(staleRow);
        final RuleData failing = RuleData.builder().id("r-failure").selectorId("selector-failure").pluginName("divide")
                .namespaceId("ns1").sort(2).build();
        final RuleData succeeding = RuleData.builder().id("r-next").selectorId("selector-failure").pluginName("divide")
                .namespaceId("ns1").sort(3).build();
        doThrow(new IllegalStateException("handler failed")).when(handler).handlerRule(failing);

        assertThrows(IllegalStateException.class, () -> commonPluginDataSubscriber.refreshRuleDataScoped(List.of(failing, succeeding)));

        assertEquals(List.of(failing, succeeding), baseDataCache.obtainRuleData("selector-failure"));
        verify(handler).removeRule(staleRow);
        verify(handler, never()).handlerRule(succeeding);
    }

    @Test
    public void testRefreshRuleDataScopedRemovesMovedRuleMembership() {
        final RuleData original = RuleData.builder().id("r-1").selectorId("selector-1").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        baseDataCache.cacheRuleData(original);
        final RuleData moved = RuleData.builder().id("r-1").selectorId("selector-2").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();

        commonPluginDataSubscriber.refreshRuleDataScoped(List.of(moved));

        assertNull(baseDataCache.obtainRuleData("selector-1"));
        assertEquals(List.of(moved), baseDataCache.obtainRuleData("selector-2"));
        verify(handler).removeRule(original);
    }

    @Test
    public void testRefreshRuleDataScopedInstallsMovedRuleStateAfterStaleRemoval() {
        // handlers like gRPC key rule state by rule id alone, so the stale removal of the old
        // membership must not delete the state the moved row just installed
        final Map<String, String> ruleHandles = new HashMap<>();
        doAnswer(invocation -> {
            RuleData data = invocation.getArgument(0);
            ruleHandles.put(data.getId(), data.getHandle());
            return null;
        }).when(handler).handlerRule(any(RuleData.class));
        doAnswer(invocation -> {
            RuleData data = invocation.getArgument(0);
            ruleHandles.remove(data.getId());
            return null;
        }).when(handler).removeRule(any(RuleData.class));
        final RuleData original = RuleData.builder().id("r-1").selectorId("selector-1").pluginName("divide")
                .namespaceId("namespace-a").handle("old").sort(1).build();
        baseDataCache.cacheRuleData(original);
        final RuleData moved = RuleData.builder().id("r-1").selectorId("selector-2").pluginName("divide")
                .namespaceId("namespace-a").handle("new").sort(1).build();

        commonPluginDataSubscriber.refreshRuleDataScoped(List.of(moved));

        assertEquals("new", ruleHandles.get("r-1"));
        final InOrder inOrder = inOrder(handler);
        inOrder.verify(handler).removeRule(original);
        inOrder.verify(handler).handlerRule(moved);
    }

    @Test
    public void testScopedRefreshKeepsFailedSelectorRemovalRetryable() {
        final SelectorData staleRow = SelectorData.builder().id("s-old").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        baseDataCache.cacheSelectData(staleRow);
        final SelectorData replacement = SelectorData.builder().id("s-new").pluginName("divide")
                .namespaceId("ns1").sort(2).build();
        doThrow(new IllegalStateException("removal failed")).doNothing().when(handler).removeSelector(staleRow);

        assertThrows(IllegalStateException.class, () -> commonPluginDataSubscriber.refreshSelectorDataScoped(List.of(replacement)));
        // the failed removal keeps the stale row cached, so the retry rediscovers it as stale
        assertEquals(List.of(staleRow, replacement), baseDataCache.obtainSelectorData("divide"));

        commonPluginDataSubscriber.refreshSelectorDataScoped(List.of(replacement));

        assertEquals(List.of(replacement), baseDataCache.obtainSelectorData("divide"));
        verify(handler, times(2)).removeSelector(staleRow);
    }

    @Test
    public void testScopedRefreshKeepsFailedRuleRemovalRetryable() {
        final RuleData staleRow = RuleData.builder().id("r-old").selectorId("selector-1").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        baseDataCache.cacheRuleData(staleRow);
        final RuleData replacement = RuleData.builder().id("r-new").selectorId("selector-1").pluginName("divide")
                .namespaceId("ns1").sort(2).build();
        doThrow(new IllegalStateException("removal failed")).doNothing().when(handler).removeRule(staleRow);

        assertThrows(IllegalStateException.class, () -> commonPluginDataSubscriber.refreshRuleDataScoped(List.of(replacement)));
        assertEquals(List.of(staleRow, replacement), baseDataCache.obtainRuleData("selector-1"));

        commonPluginDataSubscriber.refreshRuleDataScoped(List.of(replacement));

        assertEquals(List.of(replacement), baseDataCache.obtainRuleData("selector-1"));
        verify(handler, times(2)).removeRule(staleRow);
    }

    @Test
    public void testSnapshotKeepsFailedRuleRemovalRetryable() {
        final RuleData staleRow = RuleData.builder().id("r-old").selectorId("selector-1").pluginName("divide")
                .namespaceId("ns1").sort(1).build();
        baseDataCache.cacheRuleData(staleRow);
        final RuleData replacement = RuleData.builder().id("r-new").selectorId("selector-1").pluginName("divide")
                .namespaceId("ns1").sort(2).build();
        doThrow(new IllegalStateException("removal failed")).doNothing().when(handler).removeRule(staleRow);

        // a snapshot isolates per-row failures, but must still keep the failed removal discoverable
        assertDoesNotThrow(() -> commonPluginDataSubscriber.applyRuleDataSnapshot("ns1", List.of(replacement)));
        assertEquals(List.of(staleRow, replacement), baseDataCache.obtainRuleData("selector-1"));

        commonPluginDataSubscriber.applyRuleDataSnapshot("ns1", List.of(replacement));

        assertEquals(List.of(replacement), baseDataCache.obtainRuleData("selector-1"));
        verify(handler, times(2)).removeRule(staleRow);
    }

    @Test
    public void testRefreshScopedWithEmptyPayloadClearsAll() {
        baseDataCache.cacheSelectData(SelectorData.builder().id("s-1").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build());
        baseDataCache.cacheRuleData(RuleData.builder().id("r-1").selectorId("selector-1").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build());

        commonPluginDataSubscriber.refreshSelectorDataScoped(List.of());
        commonPluginDataSubscriber.refreshRuleDataScoped(List.of());

        assertNull(baseDataCache.obtainSelectorData("divide"));
        assertNull(baseDataCache.obtainRuleData("selector-1"));
    }

    private void mockShenyuTrieConfig() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @AfterEach
    public void clearCaches() {
        baseDataCache.cleanPluginData();
        baseDataCache.cleanSelectorData();
        baseDataCache.cleanRuleData();
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    @Test
    public void pluginRefreshPublishesTheCompleteBatch() {
        final PluginData a = plugin("jwt", 1);
        final PluginData old = plugin("divide", 2);
        final PluginData updated = plugin("divide", 3);
        final PluginData added = plugin("rewrite", 4);
        baseDataCache.cachePluginData(a);
        baseDataCache.cachePluginData(old);
        final var previous = baseDataCache.getPluginMap();
        doAnswer(invocation -> {
            assertSame(previous, baseDataCache.getPluginMap());
            assertSame(old, baseDataCache.obtainPluginData("divide"));
            assertNull(baseDataCache.obtainPluginData("rewrite"));
            verifyNoInteractions(eventPublisher);
            return null;
        }).when(handler).handlerPlugin(updated);
        doAnswer(invocation -> {
            assertSame(updated, baseDataCache.obtainPluginData("divide"));
            assertSame(added, baseDataCache.obtainPluginData("rewrite"));
            return null;
        }).when(eventPublisher).publishEvent(any(PluginHandlerEvent.class));
        commonPluginDataSubscriber.onPluginRefresh(List.of(updated, added));
        assertNotSame(previous, baseDataCache.getPluginMap());
        assertEquals(3, baseDataCache.getPluginMap().size());
        assertSame(a, baseDataCache.obtainPluginData("jwt"));
        assertSame(old, previous.get("divide"));
        verify(eventPublisher).publishEvent(org.mockito.ArgumentMatchers.argThat((PluginHandlerEvent event) ->
                event.getSource() == updated && event.getPluginStateEnums() == PluginHandlerEventEnum.ENABLED));
        verify(eventPublisher).publishEvent(org.mockito.ArgumentMatchers.argThat((PluginHandlerEvent event) ->
                event.getSource() == updated && event.getPluginStateEnums() == PluginHandlerEventEnum.SORTED));
    }

    @Test
    public void selectorHandlerSeesThePublishedBatch() {
        final SelectorData first = selector("a", 1);
        final SelectorData second = selector("b", 2);
        doAnswer(invocation -> {
            assertEquals(List.of(first, second), baseDataCache.obtainSelectorData("divide"));
            return null;
        }).when(handler).handlerSelector(first);
        commonPluginDataSubscriber.onSelectorRefresh(List.of(first, second));
        verify(handler).handlerSelector(second);
    }

    @Test
    public void ruleHandlerSeesThePublishedBatch() {
        final RuleData first = rule("a", 1);
        final RuleData second = rule("b", 2);
        doAnswer(invocation -> {
            assertEquals(List.of(first, second), baseDataCache.obtainRuleData("selector"));
            return null;
        }).when(handler).handlerRule(first);
        commonPluginDataSubscriber.onRuleRefresh(List.of(first, second));
        verify(handler).handlerRule(second);
    }

    @Test
    public void emptyBatchesRetainAllMaps() {
        baseDataCache.cachePluginData(plugin("divide", 1));
        baseDataCache.cacheSelectData(selector("a", 1));
        baseDataCache.cacheRuleData(rule("a", 1));
        final var plugins = baseDataCache.getPluginMap();
        final var selectors = baseDataCache.getSelectorMap();
        final var rules = baseDataCache.getRuleMap();
        commonPluginDataSubscriber.onPluginRefresh(List.of());
        commonPluginDataSubscriber.onSelectorRefresh(List.of());
        commonPluginDataSubscriber.onRuleRefresh(List.of());
        assertSame(plugins, baseDataCache.getPluginMap());
        assertSame(selectors, baseDataCache.getSelectorMap());
        assertSame(rules, baseDataCache.getRuleMap());
        verifyNoInteractions(eventPublisher);
    }

    @Test
    public void selectorHandlerFailureDoesNotRollBackPublishedBatch() {
        final SelectorData old = selector("b", 1);
        final SelectorData updated = selector("b", 2);
        final SelectorData added = selector("c", 3);
        baseDataCache.cacheSelectData(old);
        doThrow(new IllegalStateException("handler failed")).when(handler).handlerSelector(updated);
        assertThrows(IllegalStateException.class, () -> commonPluginDataSubscriber.onSelectorRefresh(List.of(updated, added)));
        assertEquals(List.of(updated, added), baseDataCache.obtainSelectorData("divide"));
    }

    @Test
    public void ruleHandlerFailureDoesNotRollBackPublishedBatch() {
        final RuleData old = rule("b", 1);
        final RuleData updated = rule("b", 2);
        final RuleData added = rule("c", 3);
        baseDataCache.cacheRuleData(old);
        doThrow(new IllegalStateException("handler failed")).when(handler).handlerRule(updated);
        assertThrows(IllegalStateException.class, () -> commonPluginDataSubscriber.onRuleRefresh(List.of(updated, added)));
        assertEquals(List.of(updated, added), baseDataCache.obtainRuleData("selector"));
    }

    @Test
    public void refreshInvalidatesMatchingAndNegativeCacheEntries() {
        final SelectorMatchCache selectorConfig = new SelectorMatchCache();
        final RuleMatchCache ruleConfig = new RuleMatchCache();
        selectorConfig.getCache().setEnabled(true);
        ruleConfig.getCache().setEnabled(true);
        commonPluginDataSubscriber = new CommonPluginDataSubscriber(List.of(handler), eventPublisher, selectorConfig, ruleConfig);
        final MatchDataCache matches = MatchDataCache.getInstance();
        matches.cacheSelectorData("/selector", selector("selector", 1), 100, 100);
        matches.cacheSelectorData("/empty", SelectorData.builder().pluginName("divide").build(), 100, 100);
        matches.cacheRuleData("/selector", rule("a", 1), 100, 100);
        matches.cacheRuleData("/empty", RuleData.builder().pluginName("divide").build(), 100, 100);
        commonPluginDataSubscriber.onSelectorRefresh(List.of(selector("selector", 2)));
        assertNull(matches.obtainSelectorData("divide", "/selector"));
        assertNull(matches.obtainSelectorData("divide", "/empty"));
        assertNull(matches.obtainRuleData("divide", "/selector"));
        assertNull(matches.obtainRuleData("divide", "/empty"));

        matches.cacheRuleData("/rule", rule("a", 1), 100, 100);
        matches.cacheRuleData("/empty", RuleData.builder().pluginName("divide").build(), 100, 100);
        commonPluginDataSubscriber.onRuleRefresh(List.of(rule("a", 2)));
        assertNull(matches.obtainRuleData("divide", "/rule"));
        assertNull(matches.obtainRuleData("divide", "/empty"));

        matches.cacheSelectorData("/selector", selector("a", 1), 100, 100);
        matches.cacheRuleData("/rule", rule("a", 1), 100, 100);
        commonPluginDataSubscriber.onPluginRefresh(List.of(plugin("divide", 1)));
        assertNull(matches.obtainSelectorData("divide", "/selector"));
        assertNull(matches.obtainRuleData("divide", "/rule"));
    }

    @Test
    public void disabledPluginEventIsPublishedAfterTheBatch() {
        final PluginData disabled = PluginData.builder().name("divide").enabled(false).sort(1).build();
        doAnswer(invocation -> {
            assertSame(disabled, baseDataCache.obtainPluginData("divide"));
            return null;
        }).when(eventPublisher).publishEvent(any(PluginHandlerEvent.class));
        commonPluginDataSubscriber.onPluginRefresh(List.of(disabled));
        verify(eventPublisher).publishEvent(org.mockito.ArgumentMatchers.argThat((PluginHandlerEvent event) ->
                event.getSource() == disabled && event.getPluginStateEnums() == PluginHandlerEventEnum.DISABLED));
    }

    private PluginData plugin(final String name, final int sort) {
        return PluginData.builder().name(name).sort(sort).enabled(true).build();
    }

    private SelectorData selector(final String id, final int sort) {
        return SelectorData.builder().id(id).pluginName("divide").sort(sort).build();
    }

    private RuleData rule(final String id, final int sort) {
        return RuleData.builder().id(id).pluginName("divide").selectorId("selector").sort(sort).build();
    }

    @Test
    public void selectorSnapshotBatchesMergesAndPreservesSortOrder() {
        final SelectorData existing = SelectorData.builder().id("existing").pluginName("divide")
                .namespaceId("namespace-a").sort(2).build();
        baseDataCache.cacheSelectData(existing);
        final SelectorData first = SelectorData.builder().id("s-3").pluginName("divide")
                .namespaceId("namespace-a").sort(3).build();
        final SelectorData second = SelectorData.builder().id("s-1").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        final SelectorData replacement = SelectorData.builder().id("existing").pluginName("divide")
                .namespaceId("namespace-a").sort(4).build();

        commonPluginDataSubscriber.applySelectorDataSnapshot("namespace-a", List.of(first, second, replacement));

        // one merge+sort per plugin bucket: sorted by the sort key, replacement supersedes the existing row
        assertEquals(List.of(second, first, replacement), baseDataCache.obtainSelectorData("divide"));
        verify(handler).handlerSelector(first);
        verify(handler).handlerSelector(second);
        verify(handler).handlerSelector(replacement);
    }

    @Test
    public void ruleSnapshotBatchesMergesAndPreservesSortOrder() {
        final RuleData existing = RuleData.builder().id("existing").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(2).build();
        baseDataCache.cacheRuleData(existing);
        final RuleData first = RuleData.builder().id("r-3").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(3).build();
        final RuleData second = RuleData.builder().id("r-1").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        final RuleData replacement = RuleData.builder().id("existing").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(4).build();

        commonPluginDataSubscriber.applyRuleDataSnapshot("namespace-a", List.of(first, second, replacement));

        assertEquals(List.of(second, first, replacement), baseDataCache.obtainRuleData("selector-a"));
        verify(handler).handlerRule(first);
        verify(handler).handlerRule(second);
        verify(handler).handlerRule(replacement);
    }

    @Test
    public void selectorSnapshotInvokesRemoveSelectorForEachStaleRow() {
        final SelectorData staleA = SelectorData.builder().id("stale-a").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        final SelectorData staleB = SelectorData.builder().id("stale-b").pluginName("divide")
                .namespaceId("namespace-a").sort(2).build();
        final SelectorData otherNamespace = SelectorData.builder().id("other").pluginName("divide")
                .namespaceId("namespace-b").sort(3).build();
        baseDataCache.cacheSelectData(staleA);
        baseDataCache.cacheSelectData(staleB);
        baseDataCache.cacheSelectData(otherNamespace);

        commonPluginDataSubscriber.applySelectorDataSnapshot("namespace-a", List.of());

        assertEquals(List.of(otherNamespace), baseDataCache.obtainSelectorData("divide"));
        verify(handler).removeSelector(staleA);
        verify(handler).removeSelector(staleB);
    }

    @Test
    public void ruleSnapshotInvokesRemoveRuleForEachStaleRow() {
        final RuleData staleA = RuleData.builder().id("stale-a").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        final RuleData staleB = RuleData.builder().id("stale-b").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(2).build();
        final RuleData otherNamespace = RuleData.builder().id("other").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-b").sort(3).build();
        baseDataCache.cacheRuleData(staleA);
        baseDataCache.cacheRuleData(staleB);
        baseDataCache.cacheRuleData(otherNamespace);

        commonPluginDataSubscriber.applyRuleDataSnapshot("namespace-a", List.of());

        assertEquals(List.of(otherNamespace), baseDataCache.obtainRuleData("selector-a"));
        verify(handler).removeRule(staleA);
        verify(handler).removeRule(staleB);
    }

    @Test
    public void snapshotCoalescesMatchCacheInvalidationPerAffectedPlugin() {
        final SelectorMatchCache selectorConfig = new SelectorMatchCache();
        final RuleMatchCache ruleConfig = new RuleMatchCache();
        selectorConfig.getCache().setEnabled(true);
        ruleConfig.getCache().setEnabled(true);
        commonPluginDataSubscriber = new CommonPluginDataSubscriber(List.of(handler), eventPublisher, selectorConfig, ruleConfig);
        final MatchDataCache matches = MatchDataCache.getInstance();
        // Preload positive and negative cache entries for two plugins.
        matches.cacheSelectorData("/divide", selector("s-1", 1), 100, 100);
        matches.cacheSelectorData("/divide-empty", SelectorData.builder().pluginName("divide").build(), 100, 100);
        matches.cacheRuleData("/divide", rule("r-1", 1), 100, 100);
        final SelectorData otherPlugin = SelectorData.builder().id("s-other").pluginName("rewrite")
                .namespaceId("namespace-a").sort(1).build();
        matches.cacheSelectorData("/rewrite", otherPlugin, 100, 100);

        // Snapshot only touches the "divide" plugin; the "rewrite" cache must remain.
        final SelectorData incoming = SelectorData.builder().id("s-2").pluginName("divide")
                .namespaceId("namespace-a").sort(2).build();
        commonPluginDataSubscriber.applySelectorDataSnapshot("namespace-a", List.of(incoming));

        assertNull(matches.obtainSelectorData("divide", "/divide"));
        assertNull(matches.obtainSelectorData("divide", "/divide-empty"));
        assertNull(matches.obtainRuleData("divide", "/divide"));
        assertNotNull(matches.obtainSelectorData("rewrite", "/rewrite"));
    }

    @Test
    public void selectorSnapshotHandlerFailurePropagatesDuringInitialSync() {
        final SelectorData incoming = SelectorData.builder().id("s-1").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        doThrow(new IllegalStateException("handler failed")).when(handler).handlerSelector(incoming);
        assertThrows(IllegalStateException.class, () -> InitialSyncApplication.run(
                () -> commonPluginDataSubscriber.applySelectorDataSnapshot("namespace-a", List.of(incoming))));
        // cache write already published before the handler ran, mirroring the pre-batch behavior
        assertEquals(List.of(incoming), baseDataCache.obtainSelectorData("divide"));
    }

    @Test
    public void selectorSnapshotHandlerFailureIsLoggedOutsideInitialSync() {
        final SelectorData first = SelectorData.builder().id("s-1").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        final SelectorData second = SelectorData.builder().id("s-2").pluginName("divide")
                .namespaceId("namespace-a").sort(2).build();
        doThrow(new IllegalStateException("handler failed")).when(handler).handlerSelector(first);
        assertDoesNotThrow(() -> commonPluginDataSubscriber.applySelectorDataSnapshot("namespace-a", List.of(first, second)));
        verify(handler).handlerSelector(second);
        assertEquals(List.of(first, second), baseDataCache.obtainSelectorData("divide"));
    }

    @Test
    public void danglingSelectorSnapshotSweepsMatchCacheAcrossBuckets() {
        final SelectorMatchCache selectorConfig = new SelectorMatchCache();
        final RuleMatchCache ruleConfig = new RuleMatchCache();
        selectorConfig.getCache().setEnabled(true);
        ruleConfig.getCache().setEnabled(true);
        commonPluginDataSubscriber = new CommonPluginDataSubscriber(List.of(handler), eventPublisher, selectorConfig, ruleConfig);
        final MatchDataCache matches = MatchDataCache.getInstance();

        // a selector cached without a plugin name (dangling), plus match-cache entries under a real plugin bucket
        final SelectorData dangling = SelectorData.builder().id("s-dangle").pluginName("").namespaceId("namespace-a").sort(1).build();
        baseDataCache.cacheSelectData(dangling);
        matches.cacheSelectorData("/dangle", SelectorData.builder().id("s-dangle").pluginName("divide").build(), 100, 100);
        matches.cacheRuleData("/dangle", RuleData.builder().id("r-dangle").selectorId("s-dangle").pluginName("divide").build(), 100, 100);

        // the snapshot drops the dangling selector; its blank name must still sweep every match-cache bucket by id
        commonPluginDataSubscriber.applySelectorDataSnapshot("namespace-a", List.of());

        assertNull(matches.obtainSelectorData("divide", "/dangle"));
        assertNull(matches.obtainRuleData("divide", "/dangle"));
    }

    @Test
    public void danglingRuleSnapshotSweepsMatchCacheAcrossBuckets() {
        final SelectorMatchCache selectorConfig = new SelectorMatchCache();
        final RuleMatchCache ruleConfig = new RuleMatchCache();
        selectorConfig.getCache().setEnabled(true);
        ruleConfig.getCache().setEnabled(true);
        commonPluginDataSubscriber = new CommonPluginDataSubscriber(List.of(handler), eventPublisher, selectorConfig, ruleConfig);
        final MatchDataCache matches = MatchDataCache.getInstance();

        // a rule keyed by selectorId whose plugin name is blank, cached under a real plugin bucket
        final RuleData dangling = RuleData.builder().id("r-dangle").selectorId("selector-a").pluginName("")
                .namespaceId("namespace-a").sort(1).build();
        baseDataCache.cacheRuleData(dangling);
        matches.cacheRuleData("/dangle", RuleData.builder().id("r-dangle").selectorId("selector-a").pluginName("divide").build(), 100, 100);

        commonPluginDataSubscriber.applyRuleDataSnapshot("namespace-a", List.of());

        assertNull(matches.obtainRuleData("divide", "/dangle"));
    }

    @Test
    public void malformedSelectorRowIsSkippedWithoutAbortingSnapshot() {
        final SelectorData valid = SelectorData.builder().id("s-ok").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        // no plugin name: would produce a null grouping key and NPE inside the batch cache write
        final SelectorData malformed = SelectorData.builder().id("s-bad").namespaceId("namespace-a").sort(2).build();

        assertDoesNotThrow(() -> commonPluginDataSubscriber.applySelectorDataSnapshot("namespace-a", List.of(malformed, valid)));

        assertEquals(List.of(valid), baseDataCache.obtainSelectorData("divide"));
        verify(handler).handlerSelector(valid);
    }

    @Test
    public void malformedRuleRowIsSkippedWithoutAbortingSnapshot() {
        final RuleData valid = RuleData.builder().id("r-ok").selectorId("selector-a").pluginName("divide")
                .namespaceId("namespace-a").sort(1).build();
        // no selector id: would produce a null grouping key and NPE inside the batch cache write
        final RuleData malformed = RuleData.builder().id("r-bad").pluginName("divide").namespaceId("namespace-a").sort(2).build();

        assertDoesNotThrow(() -> commonPluginDataSubscriber.applyRuleDataSnapshot("namespace-a", List.of(malformed, valid)));

        assertEquals(List.of(valid), baseDataCache.obtainRuleData("selector-a"));
        verify(handler).handlerRule(valid);
    }
}
