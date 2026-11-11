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

import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class AbstractPathDataChangedListenerTest {

    private static final String NAMESPACE_A = "namespace-a";

    private static final String NAMESPACE_B = "namespace-b";

    @Test
    public void testOnDiscoveryUpstreamChangedIgnoresDeleteWithEmptyPluginName() {
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setNamespaceId("default");
        discoverySyncData.setSelectorId("selector-id");
        discoverySyncData.setPluginName("");
        TestPathDataChangedListener listener = new TestPathDataChangedListener();

        listener.onDiscoveryUpstreamChanged(Collections.singletonList(discoverySyncData), DataEventTypeEnum.DELETE);

        assertNull(listener.deletedPath);
    }

    @Test
    public void testOnDiscoveryUpstreamChangedIgnoresUpdateWithEmptyPluginName() {
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setNamespaceId("default");
        discoverySyncData.setSelectorId("selector-id");
        discoverySyncData.setPluginName("");
        TestPathDataChangedListener listener = new TestPathDataChangedListener();

        listener.onDiscoveryUpstreamChanged(Collections.singletonList(discoverySyncData), DataEventTypeEnum.UPDATE);

        assertNull(listener.updatedPath);
    }

    @Test
    public void testOnDiscoveryUpstreamChangedIgnoresDeleteWithMissingSelectorId() {
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setNamespaceId("default");
        discoverySyncData.setPluginName("divide");
        TestPathDataChangedListener listener = new TestPathDataChangedListener();

        listener.onDiscoveryUpstreamChanged(Collections.singletonList(discoverySyncData), DataEventTypeEnum.DELETE);

        assertNull(listener.deletedPath);
    }

    @Test
    public void testOnDiscoveryUpstreamChangedIgnoresUpdateWithBlankSelectorId() {
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setNamespaceId("default");
        discoverySyncData.setSelectorId(" ");
        discoverySyncData.setPluginName("divide");
        TestPathDataChangedListener listener = new TestPathDataChangedListener();

        listener.onDiscoveryUpstreamChanged(Collections.singletonList(discoverySyncData), DataEventTypeEnum.UPDATE);

        assertNull(listener.updatedPath);
    }

    @Test
    public void testOnDiscoveryUpstreamChangedDeletesValidPath() {
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setNamespaceId("default");
        discoverySyncData.setSelectorId("selector-id");
        discoverySyncData.setPluginName("divide");
        TestPathDataChangedListener listener = new TestPathDataChangedListener();

        listener.onDiscoveryUpstreamChanged(Collections.singletonList(discoverySyncData), DataEventTypeEnum.DELETE);

        assertEquals("/default/shenyu/discoveryUpstream/divide/selector-id", listener.deletedPath);
    }

    @Test
    public void testOnAppAuthChangedEmptyNamespaceRefreshClearsOnlyTargetNamespace() {
        TestPathDataChangedListener listener = new TestPathDataChangedListener();
        String appAuthA = DefaultPathConstants.buildAppAuthPath(NAMESPACE_A, "app-a");
        String appAuthB = DefaultPathConstants.buildAppAuthPath(NAMESPACE_B, "app-b");
        listener.putConfig(appAuthA, appAuthData(NAMESPACE_A, "app-a"));
        listener.putConfig(appAuthB, appAuthData(NAMESPACE_B, "app-b"));

        listener.onAppAuthChanged(Collections.emptyList(), DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertFalse(listener.has(appAuthA));
        assertTrue(listener.has(appAuthB));
    }

    @Test
    public void testOnPluginChangedEmptyNamespaceRefreshClearsOnlyTargetNamespace() {
        TestPathDataChangedListener listener = new TestPathDataChangedListener();
        String pluginA = DefaultPathConstants.buildPluginPath(NAMESPACE_A, "divide");
        String pluginB = DefaultPathConstants.buildPluginPath(NAMESPACE_B, "divide");
        listener.putConfig(pluginA, PluginData.builder().namespaceId(NAMESPACE_A).name("divide").build());
        listener.putConfig(pluginB, PluginData.builder().namespaceId(NAMESPACE_B).name("divide").build());

        listener.onPluginChanged(Collections.emptyList(), DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertFalse(listener.has(pluginA));
        assertTrue(listener.has(pluginB));
    }

    @Test
    public void testOnPluginChangedNamespaceRefreshRemovesRemovedPluginSubtrees() {
        TestPathDataChangedListener listener = new TestPathDataChangedListener();
        String retained = DefaultPathConstants.buildPluginPath(NAMESPACE_A, "divide");
        String removed = DefaultPathConstants.buildPluginPath(NAMESPACE_A, "http");
        String removedSelector = DefaultPathConstants.buildSelectorRealPath(NAMESPACE_A, "http", "selector-a");
        String removedRule = DefaultPathConstants.buildRulePath(NAMESPACE_A, "http", "selector-a", "rule-a");
        listener.putConfig(retained, PluginData.builder().namespaceId(NAMESPACE_A).name("divide").build());
        listener.putConfig(removed, PluginData.builder().namespaceId(NAMESPACE_A).name("http").build());
        listener.putConfig(removedSelector, selectorData(NAMESPACE_A, "selector-a"));
        listener.putConfig(removedRule, ruleData(NAMESPACE_A, "selector-a", "rule-a"));

        listener.onPluginChanged(Collections.singletonList(PluginData.builder().namespaceId(NAMESPACE_A).name("divide").build()),
                DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertTrue(listener.has(retained));
        assertFalse(listener.has(removed));
        assertFalse(listener.has(removedSelector));
        assertFalse(listener.has(removedRule));
    }

    @Test
    public void testOnSelectorChangedEmptyNamespaceRefreshClearsOnlyTargetNamespace() {
        TestPathDataChangedListener listener = new TestPathDataChangedListener();
        String selectorA = DefaultPathConstants.buildSelectorRealPath(NAMESPACE_A, "divide", "selector-a");
        String selectorB = DefaultPathConstants.buildSelectorRealPath(NAMESPACE_B, "divide", "selector-b");
        listener.putConfig(selectorA, selectorData(NAMESPACE_A, "selector-a"));
        listener.putConfig(selectorB, selectorData(NAMESPACE_B, "selector-b"));

        listener.onSelectorChanged(Collections.emptyList(), DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertFalse(listener.has(selectorA));
        assertFalse(listener.has(DefaultPathConstants.buildSelectorParentPath(NAMESPACE_A, "divide")));
        assertTrue(listener.has(selectorB));
    }

    @Test
    public void testOnRuleChangedEmptyNamespaceRefreshClearsOnlyTargetNamespace() {
        TestPathDataChangedListener listener = new TestPathDataChangedListener();
        String ruleA = DefaultPathConstants.buildRulePath(NAMESPACE_A, "divide", "selector-a", "rule-a");
        String ruleB = DefaultPathConstants.buildRulePath(NAMESPACE_B, "divide", "selector-b", "rule-b");
        listener.putConfig(ruleA, ruleData(NAMESPACE_A, "selector-a", "rule-a"));
        listener.putConfig(ruleB, ruleData(NAMESPACE_B, "selector-b", "rule-b"));

        listener.onRuleChanged(Collections.emptyList(), DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertFalse(listener.has(ruleA));
        assertFalse(listener.has(DefaultPathConstants.buildRuleParentPath(NAMESPACE_A, "divide")));
        assertTrue(listener.has(ruleB));
    }

    @Test
    public void testOnSelectorChangedNamespaceRefreshUpsertsBeforeDeletingStaleData() {
        TestPathDataChangedListener listener = new TestPathDataChangedListener();
        String retained = DefaultPathConstants.buildSelectorRealPath(NAMESPACE_A, "divide", "retained");
        String stale = DefaultPathConstants.buildSelectorRealPath(NAMESPACE_A, "divide", "stale");
        listener.putConfig(retained, selectorData(NAMESPACE_A, "retained"));
        listener.putConfig(stale, selectorData(NAMESPACE_A, "stale"));

        listener.onSelectorChanged(Collections.singletonList(selectorData(NAMESPACE_A, "retained")),
                DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertTrue(listener.has(retained));
        assertFalse(listener.has(stale));
        assertTrue(listener.indexOfOperation("create:" + retained) < listener.indexOfOperation("delete:" + stale));
        assertFalse(listener.hasRecursiveDelete(DefaultPathConstants.buildSelectorParentPath(NAMESPACE_A, "divide")));
    }

    @Test
    public void testOnSelectorChangedPayloadRefreshKeepsOtherPluginsAndUpsertsFirst() {
        TestPathDataChangedListener listener = new TestPathDataChangedListener();
        String retained = DefaultPathConstants.buildSelectorRealPath(NAMESPACE_A, "divide", "retained");
        String stale = DefaultPathConstants.buildSelectorRealPath(NAMESPACE_A, "divide", "stale");
        String otherPlugin = DefaultPathConstants.buildSelectorRealPath(NAMESPACE_A, "http", "other");
        listener.putConfig(retained, selectorData(NAMESPACE_A, "retained"));
        listener.putConfig(stale, selectorData(NAMESPACE_A, "stale"));
        listener.putConfig(otherPlugin, selectorData(NAMESPACE_A, "other"));

        listener.onSelectorChanged(Collections.singletonList(selectorData(NAMESPACE_A, "retained")),
                DataEventTypeEnum.REFRESH);

        assertTrue(listener.has(retained));
        assertFalse(listener.has(stale));
        assertTrue(listener.has(otherPlugin));
        assertTrue(listener.indexOfOperation("create:" + retained) < listener.indexOfOperation("delete:" + stale));
    }

    private static AppAuthData appAuthData(final String namespaceId, final String appKey) {
        return AppAuthData.builder().namespaceId(namespaceId).appKey(appKey).build();
    }

    private static SelectorData selectorData(final String namespaceId, final String selectorId) {
        return SelectorData.builder().namespaceId(namespaceId).pluginName("divide").id(selectorId).build();
    }

    private static RuleData ruleData(final String namespaceId, final String selectorId, final String ruleId) {
        return RuleData.builder().namespaceId(namespaceId).pluginName("divide").selectorId(selectorId).id(ruleId).build();
    }

    private static final class TestPathDataChangedListener extends AbstractPathDataChangedListener {

        private final Map<String, Object> configs = new HashMap<>();

        private final List<String> operations = new ArrayList<>();

        private String deletedPath;

        private String updatedPath;

        @Override
        public void createOrUpdate(final String pluginPath, final Object data) {
            updatedPath = pluginPath;
            configs.put(pluginPath, data);
            operations.add("create:" + pluginPath);
        }

        @Override
        public void deleteNode(final String pluginPath) {
            deletedPath = pluginPath;
            operations.add("delete:" + pluginPath);
            removePath(pluginPath);
        }

        @Override
        public void deletePathRecursive(final String selectorParentPath) {
            operations.add("recursive:" + selectorParentPath);
            removePath(selectorParentPath);
        }

        @Override
        public List<String> getChildren(final String path) {
            List<String> children = new ArrayList<>();
            String prefix = path + DefaultPathConstants.PATH_SEPARATOR;
            configs.keySet().stream().filter(key -> key.startsWith(prefix)).forEach(key -> {
                String child = key.substring(prefix.length());
                int separatorIndex = child.indexOf(DefaultPathConstants.PATH_SEPARATOR);
                String name = separatorIndex < 0 ? child : child.substring(0, separatorIndex);
                if (!children.contains(name)) {
                    children.add(name);
                }
            });
            return children;
        }

        private void removePath(final String path) {
            String prefix = path + DefaultPathConstants.PATH_SEPARATOR;
            configs.keySet().removeIf(key -> key.equals(path) || key.startsWith(prefix));
        }

        private void putConfig(final String path, final Object data) {
            configs.put(path, data);
        }

        private boolean has(final String path) {
            return configs.containsKey(path);
        }

        private int indexOfOperation(final String operation) {
            return operations.indexOf(operation);
        }

        private boolean hasRecursiveDelete(final String path) {
            return operations.contains("recursive:" + path);
        }
    }
}
