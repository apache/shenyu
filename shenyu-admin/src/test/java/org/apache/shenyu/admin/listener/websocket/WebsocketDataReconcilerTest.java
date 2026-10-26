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

package org.apache.shenyu.admin.listener.websocket;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.service.AiProxyApiKeyService;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.NamespacePluginService;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.ObjectProvider;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Tests for {@link WebsocketDataReconciler}.
 */
@ExtendWith(MockitoExtension.class)
public final class WebsocketDataReconcilerTest {

    private static final String NAMESPACE_1 = "namespace-1";

    private static final String NAMESPACE_2 = "namespace-2";

    @Mock
    private AppAuthService appAuthService;

    @Mock
    private NamespacePluginService namespacePluginService;

    @Mock
    private SelectorService selectorService;

    @Mock
    private RuleService ruleService;

    @Mock
    private MetaDataService metaDataService;

    @Mock
    private ProxySelectorService proxySelectorService;

    @Mock
    private DiscoveryUpstreamService discoveryUpstreamService;

    @Mock
    private AiProxyApiKeyService aiProxyApiKeyService;

    @Mock
    private ObjectProvider<ClusterSelectMasterService> masterServiceProvider;

    @Mock
    private ClusterSelectMasterService clusterSelectMasterService;

    private WebsocketSyncProperties properties;

    private ClusterProperties clusterProperties;

    private WebsocketDataReconciler reconciler;

    @BeforeEach
    public void setUp() {
        properties = new WebsocketSyncProperties();
        clusterProperties = new ClusterProperties();
        reconciler = new WebsocketDataReconciler(properties, clusterProperties, masterServiceProvider,
                appAuthService, namespacePluginService, selectorService, ruleService,
                metaDataService, proxySelectorService, discoveryUpstreamService, aiProxyApiKeyService);
    }

    @Test
    public void testReconciliationDefaultsToDisabled() {
        org.junit.jupiter.api.Assertions.assertFalse(properties.getReconciliation().isEnabled());
    }

    @Test
    public void testUnsupportedGroupsAreNotPolled() {
        stubAllGroups();
        try (ReconciledCollector ignored = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            reconciler.reconcileSafely();
            verifyNoInteractions(appAuthService, metaDataService, proxySelectorService,
                    discoveryUpstreamService, aiProxyApiKeyService);
        }
    }

    @Test
    public void testLifecycleMethodsDoNotThrow() {
        properties.getReconciliation().setEnabled(false);
        assertDoesNotThrow(() -> reconciler.afterPropertiesSet());
        reconciler.destroy();
        // a fresh reconciler starts and stops cleanly with scheduling enabled
        WebsocketDataReconciler started = new WebsocketDataReconciler(properties, clusterProperties,
                masterServiceProvider, appAuthService, namespacePluginService, selectorService, ruleService,
                metaDataService, proxySelectorService, discoveryUpstreamService, aiProxyApiKeyService);
        properties.getReconciliation().setEnabled(true);
        assertDoesNotThrow(started::afterPropertiesSet);
        started.destroy();
    }

    @Test
    public void testNoActiveSessionsSkipsAllLoads() {
        try (ReconciledCollector ignored = new ReconciledCollector(Collections.emptySet())) {
            reconciler.reconcileSafely();
            verifyNoInteractions(appAuthService, namespacePluginService, selectorService, ruleService,
                    metaDataService, proxySelectorService, discoveryUpstreamService, aiProxyApiKeyService);
        }
    }

    @Test
    public void testChangedGroupsArePushedOncePerCycle() {
        stubAllGroups();
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            reconciler.reconcileSafely();
            mocked.verifySends(NAMESPACE_1, 3);
            // unchanged state: the second cycle pushes nothing
            reconciler.reconcileSafely();
            mocked.verifySends(NAMESPACE_1, 3);
            verify(namespacePluginService, times(2)).listAll(NAMESPACE_1);
            verify(selectorService, times(2)).listAllByNamespaceId(NAMESPACE_1);
            verify(ruleService, times(2)).listAllByNamespaceId(NAMESPACE_1);
        }
    }

    @Test
    public void testEmptyGroupIsPushedForClearing() {
        stubAllGroups();
        when(ruleService.listAllByNamespaceId(NAMESPACE_1)).thenReturn(Collections.emptyList());
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            reconciler.reconcileSafely();
            List<String> messages = mocked.capturedMessages(NAMESPACE_1);
            assertEquals(3, messages.size());
            String ruleMessage = messages.stream()
                    .filter(m -> m.contains("\"groupType\":\"RULE\""))
                    .findFirst()
                    .orElse("");
            assertTrue(ruleMessage.contains("\"eventType\":\"REFRESH\""), ruleMessage);
            assertTrue(ruleMessage.contains("\"data\":[]"), ruleMessage);
            assertTrue(ruleMessage.contains("\"fullSnapshot\":true"), ruleMessage);
            assertTrue(ruleMessage.contains("\"namespaceId\":\"namespace-1\""), ruleMessage);
            assertEquals(DigestUtils.md5Hex(""), reconciler.getDigestCursor().get(NAMESPACE_1 + ":RULE"));
            reconciler.reconcileSafely();
            mocked.verifySends(NAMESPACE_1, 3);
        }
    }

    @Test
    public void testFailedLoadIsRetriedNextCycle() {
        stubAllGroups();
        when(ruleService.listAllByNamespaceId(NAMESPACE_1))
                .thenThrow(new RuntimeException("db down"))
                .thenReturn(Collections.singletonList(new RuleData().setId("rule-1")));
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            // the failed group is not pushed and does not fail the whole cycle
            reconciler.reconcileSafely();
            mocked.verifySends(NAMESPACE_1, 2);
            // the cursor was not advanced, the next cycle retries the group
            reconciler.reconcileSafely();
            mocked.verifySends(NAMESPACE_1, 3);
            assertEquals(1, mocked.capturedMessages(NAMESPACE_1).stream()
                    .filter(m -> m.contains("\"groupType\":\"RULE\""))
                    .count());
        }
    }

    @Test
    public void testOnlyChangedNamespaceIsRepulsed() {
        stubAllGroups();
        when(ruleService.listAllByNamespaceId(NAMESPACE_1))
                .thenReturn(Collections.singletonList(new RuleData().setId("rule-v1")))
                .thenReturn(Collections.singletonList(new RuleData().setId("rule-v2")));
        when(ruleService.listAllByNamespaceId(NAMESPACE_2))
                .thenReturn(Collections.singletonList(new RuleData().setId("rule-ns2")));
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1, NAMESPACE_2))) {
            reconciler.reconcileSafely();
            mocked.verifySends(NAMESPACE_1, 3);
            mocked.verifySends(NAMESPACE_2, 3);
            // only the changed group of namespace-1 is pushed in the second cycle
            reconciler.reconcileSafely();
            mocked.verifySends(NAMESPACE_1, 4);
            mocked.verifySends(NAMESPACE_2, 3);
        }
    }

    @Test
    public void testClusterNonMasterSkipsCycle() {
        clusterProperties.setEnabled(true);
        when(masterServiceProvider.getIfAvailable()).thenReturn(clusterSelectMasterService);
        when(clusterSelectMasterService.isMaster()).thenReturn(false);
        stubAllGroups();
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            reconciler.reconcileSafely();
            mocked.verifyNoSends();
            verifyNoInteractions(appAuthService, namespacePluginService, selectorService, ruleService,
                    metaDataService, proxySelectorService, discoveryUpstreamService, aiProxyApiKeyService);
        }
    }

    @Test
    public void testClusterMasterReconciles() {
        clusterProperties.setEnabled(true);
        when(masterServiceProvider.getIfAvailable()).thenReturn(clusterSelectMasterService);
        when(clusterSelectMasterService.isMaster()).thenReturn(true);
        stubAllGroups();
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            reconciler.reconcileSafely();
            mocked.verifySends(NAMESPACE_1, 3);
        }
    }

    @Test
    public void testReorderingDoesNotPushAgain() {
        stubAllGroups();
        RuleData first = new RuleData().setId("first");
        RuleData second = new RuleData().setId("second");
        when(ruleService.listAllByNamespaceId(NAMESPACE_1))
                .thenReturn(List.of(first, second)).thenReturn(List.of(second, first));
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            reconciler.reconcileSafely();
            reconciler.reconcileSafely();
            mocked.verifySends(NAMESPACE_1, 3);
            assertTrue(mocked.capturedMessages(NAMESPACE_1).stream()
                    .allMatch(message -> message.contains("\"fullSnapshot\":true")
                            && message.contains("\"namespaceId\":\"namespace-1\"")));
        }
    }

    @Test
    public void testCanonicalSnapshotsMatchPreviousDigestAndWireFormat() {
        String namespaceId = "namespace-中文-<>&\"\\\n😀";
        String handle = "{\"value\":\"中文😀 <>&=' \\\" \\\\ \\n\",\"empty\":null}";
        PluginData plugin = PluginData.builder().id("plugin-1").name("divide").config(handle).namespaceId(namespaceId).build();
        SelectorData selector = SelectorData.builder().id("selector-1").handle(handle).namespaceId(namespaceId).build();
        RuleData first = new RuleData().setId("rule-1").setHandle(handle);
        RuleData second = new RuleData().setId("rule-2").setHandle("");
        List<PluginData> plugins = List.of(plugin);
        List<SelectorData> selectors = List.of(selector);
        List<RuleData> rules = Arrays.asList(second, first, null, first);
        when(namespacePluginService.listAll(namespaceId)).thenReturn(plugins);
        when(selectorService.listAllByNamespaceId(namespaceId)).thenReturn(selectors);
        when(ruleService.listAllByNamespaceId(namespaceId)).thenReturn(rules);
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(namespaceId))) {
            reconciler.reconcileSafely();
            List<String> messages = mocked.capturedMessages(namespaceId);
            assertEquals(3, messages.size());
            assertPreviousEncoding(namespaceId, ConfigGroupEnum.PLUGIN, plugins, messages);
            assertPreviousEncoding(namespaceId, ConfigGroupEnum.SELECTOR, selectors, messages);
            assertPreviousEncoding(namespaceId, ConfigGroupEnum.RULE, rules, messages);
        }
    }

    @Test
    public void testFieldChangesAdditionsAndDeletionsAreDetectedWithoutEvents() {
        stubAllGroups();
        RuleData first = new RuleData().setId("rule-1").setHandle("old");
        RuleData changed = new RuleData().setId("rule-1").setHandle("new");
        RuleData added = new RuleData().setId("rule-2");
        when(ruleService.listAllByNamespaceId(NAMESPACE_1)).thenReturn(List.of(first), List.of(changed),
                List.of(added, changed), List.of(added), Collections.emptyList(), Collections.emptyList());
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            for (int cycle = 0; cycle < 6; cycle++) {
                reconciler.reconcileSafely();
            }
            List<String> rules = mocked.capturedMessages(NAMESPACE_1).stream()
                    .filter(message -> message.contains("\"groupType\":\"RULE\""))
                    .collect(Collectors.toList());
            assertEquals(5, rules.size());
            assertTrue(rules.get(1).contains("\"handle\":\"new\""));
            assertEquals(2, JsonParser.parseString(rules.get(2)).getAsJsonObject().getAsJsonArray("data").size());
            assertFalse(rules.get(3).contains("rule-1"));
            assertTrue(rules.get(4).contains("\"data\":[]"));
            mocked.verifySends(NAMESPACE_1, 7);
        }
    }

    @Test
    public void testFailedSerializationKeepsPreviousCursorAndRetries() {
        stubAllGroups();
        RuleData changed = new RuleData().setId("changed");
        when(ruleService.listAllByNamespaceId(NAMESPACE_1)).thenReturn(List.of(new RuleData()), List.of(changed));
        Gson gson = GsonUtils.getGson();
        GsonUtils serializer = spy(GsonUtils.getInstance());
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            reconciler.reconcileSafely();
            String previousDigest = reconciler.getDigestCursor().get(NAMESPACE_1 + ":RULE");
            doThrow(new IllegalArgumentException("serialization failed")).doCallRealMethod().when(serializer).toJson(changed);
            try (MockedStatic<GsonUtils> mockedGson = mockStatic(GsonUtils.class)) {
                mockedGson.when(GsonUtils::getInstance).thenReturn(serializer);
                mockedGson.when(GsonUtils::getGson).thenReturn(gson);
                reconciler.reconcileSafely();
                assertEquals(previousDigest, reconciler.getDigestCursor().get(NAMESPACE_1 + ":RULE"));
                mocked.verifySends(NAMESPACE_1, 3);
                reconciler.reconcileSafely();
                mocked.verifySends(NAMESPACE_1, 4);
                assertEquals(DigestUtils.md5Hex(serializer.toJson(changed)), reconciler.getDigestCursor().get(NAMESPACE_1 + ":RULE"));
            }
        }
    }

    @Test
    public void testFailedPublicationDoesNotCreateCursor() {
        stubAllGroups();
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            mocked.failNextRuleSend(NAMESPACE_1);
            reconciler.reconcileSafely();
            assertFalse(reconciler.getDigestCursor().containsKey(NAMESPACE_1 + ":RULE"));
            assertEquals(2, reconciler.getDigestCursor().size());
            reconciler.reconcileSafely();
            assertTrue(reconciler.getDigestCursor().containsKey(NAMESPACE_1 + ":RULE"));
            mocked.verifySends(NAMESPACE_1, 4);
        }
    }

    @Test
    public void testFailedMessageWritingKeepsPreviousCursorAndRetries() {
        stubAllGroups();
        RuleData changed = new RuleData().setId("changed");
        when(ruleService.listAllByNamespaceId(NAMESPACE_1)).thenReturn(List.of(new RuleData()), List.of(changed));
        Gson gson = GsonUtils.getGson();
        GsonUtils serializer = GsonUtils.getInstance();
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            reconciler.reconcileSafely();
            String previousDigest = reconciler.getDigestCursor().get(NAMESPACE_1 + ":RULE");
            try (MockedStatic<GsonUtils> mockedGson = mockStatic(GsonUtils.class)) {
                mockedGson.when(GsonUtils::getInstance).thenReturn(serializer);
                mockedGson.when(GsonUtils::getGson).thenThrow(new IllegalStateException("writer failed")).thenReturn(gson);
                reconciler.reconcileSafely();
                assertEquals(previousDigest, reconciler.getDigestCursor().get(NAMESPACE_1 + ":RULE"));
                mocked.verifySends(NAMESPACE_1, 3);
                reconciler.reconcileSafely();
                mocked.verifySends(NAMESPACE_1, 4);
                assertEquals(DigestUtils.md5Hex(serializer.toJson(changed)), reconciler.getDigestCursor().get(NAMESPACE_1 + ":RULE"));
            }
        }
    }

    @Test
    public void testFailedPublicationKeepsPreviousCursorAndRetriesEmptySnapshot() {
        stubAllGroups();
        when(ruleService.listAllByNamespaceId(NAMESPACE_1)).thenReturn(List.of(new RuleData()), Collections.emptyList());
        try (ReconciledCollector mocked = new ReconciledCollector(Set.of(NAMESPACE_1))) {
            reconciler.reconcileSafely();
            String previousDigest = reconciler.getDigestCursor().get(NAMESPACE_1 + ":RULE");
            mocked.failNextRuleSend(NAMESPACE_1);
            reconciler.reconcileSafely();
            assertEquals(previousDigest, reconciler.getDigestCursor().get(NAMESPACE_1 + ":RULE"));
            reconciler.reconcileSafely();
            assertEquals(DigestUtils.md5Hex(""), reconciler.getDigestCursor().get(NAMESPACE_1 + ":RULE"));
            reconciler.reconcileSafely();
            mocked.verifySends(NAMESPACE_1, 5);
        }
    }

    private void assertPreviousEncoding(final String namespaceId, final ConfigGroupEnum group, final List<?> data, final List<String> messages) {
        List<String> rows = data.stream().map(GsonUtils.getInstance()::toJson).sorted().collect(Collectors.toList());
        assertEquals(DigestUtils.md5Hex(String.join("\n", rows)), reconciler.getDigestCursor().get(namespaceId + ":" + group.name()));
        List<JsonElement> trees = rows.stream().map(JsonParser::parseString).collect(Collectors.toList());
        WebsocketData<?> expected = new WebsocketData<>(group.name(), DataEventTypeEnum.REFRESH.name(), trees);
        expected.setNamespaceId(namespaceId);
        expected.setFullSnapshot(true);
        assertTrue(messages.contains(GsonUtils.getInstance().toJson(expected)), group.name());
    }

    private void stubAllGroups() {
        lenient().when(appAuthService.listAllByNamespaceId(anyString()))
                .thenReturn(Collections.singletonList(new AppAuthData()));
        lenient().when(namespacePluginService.listAll(anyString()))
                .thenReturn(Collections.singletonList(new PluginData()));
        lenient().when(selectorService.listAllByNamespaceId(anyString()))
                .thenReturn(Collections.singletonList(new SelectorData()));
        lenient().when(ruleService.listAllByNamespaceId(anyString()))
                .thenReturn(Collections.singletonList(new RuleData()));
        lenient().when(metaDataService.listAllByNamespaceId(anyString()))
                .thenReturn(Collections.singletonList(new MetaData()));
        lenient().when(proxySelectorService.listAllByNamespaceId(anyString()))
                .thenReturn(Collections.singletonList(new ProxySelectorData()));
        lenient().when(discoveryUpstreamService.listAllByNamespaceId(anyString()))
                .thenReturn(Collections.singletonList(new DiscoverySyncData()));
        lenient().when(aiProxyApiKeyService.listAllByNamespaceId(anyString()))
                .thenReturn(Collections.singletonList(new ProxyApiKeyData()));
    }

    /**
     * Wrapper around the static {@link WebsocketCollector} mock that records and
     * verifies the refresh messages pushed by the reconciler.
     */
    private static final class ReconciledCollector implements AutoCloseable {

        private final MockedStatic<WebsocketCollector> mocked;

        private final ArgumentCaptor<String> messageCaptor;

        private ReconciledCollector(final Set<String> activeNamespaces) {
            this.mocked = mockStatic(WebsocketCollector.class);
            this.messageCaptor = ArgumentCaptor.forClass(String.class);
            mocked.when(WebsocketCollector::getActiveNamespaceIds).thenReturn(activeNamespaces);
        }

        private void verifySends(final String namespaceId, final long expected) {
            mocked.verify(() -> WebsocketCollector.send(eq(namespaceId), anyString(),
                    eq(DataEventTypeEnum.REFRESH)), times((int) expected));
        }

        private void verifyNoSends() {
            mocked.verify(() -> WebsocketCollector.send(anyString(), anyString(), any()), never());
        }

        private void failNextRuleSend(final String namespaceId) {
            mocked.when(() -> WebsocketCollector.send(eq(namespaceId), contains("\"groupType\":\"RULE\""), eq(DataEventTypeEnum.REFRESH)))
                    .thenThrow(new IllegalStateException("publication failed")).thenAnswer(invocation -> null);
        }

        private List<String> capturedMessages(final String namespaceId) {
            mocked.verify(() -> WebsocketCollector.send(eq(namespaceId), messageCaptor.capture(),
                    eq(DataEventTypeEnum.REFRESH)), atLeast(0));
            return messageCaptor.getAllValues();
        }

        @Override
        public void close() {
            mocked.close();
        }
    }
}
