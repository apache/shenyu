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

import org.apache.shenyu.common.constant.DefaultNodeConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class AbstractNodeDataChangedListenerTest {

    private static final String NAMESPACE_ID = "namespace";

    private static final String FIRST_PLUGIN = "firstPlugin";

    private static final String SECOND_PLUGIN = "secondPlugin";

    private static final String RETAINED_ID = "retained";

    private static final String ADDED_ID = "added";

    private static final String STALE_ID = "stale";

    private static final String NAMESPACE_A = "namespace-a";

    private static final String NAMESPACE_B = "namespace-b";

    @Test
    public void testOnAppAuthChangedEmptyNamespaceRefreshClearsOnlyTargetNamespace() {
        TestNodeDataChangedListener listener = new TestNodeDataChangedListener();
        listener.putConfig(authListKey(NAMESPACE_A), Collections.singletonList("app-a"));
        listener.putConfig(authDataKey(NAMESPACE_A, "app-a"), appAuthData(NAMESPACE_A, "app-a"));
        listener.putConfig(authListKey(NAMESPACE_B), Collections.singletonList("app-b"));
        listener.putConfig(authDataKey(NAMESPACE_B, "app-b"), appAuthData(NAMESPACE_B, "app-b"));

        listener.onAppAuthChanged(Collections.emptyList(), DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertEquals(Collections.emptyList(), listener.config(authListKey(NAMESPACE_A)));
        assertNull(listener.config(authDataKey(NAMESPACE_A, "app-a")));
        assertTrue(listener.wasDeleted(authDataKey(NAMESPACE_A, "app-a")));
        assertEquals(Collections.singletonList("app-b"), listener.config(authListKey(NAMESPACE_B)));
        assertEquals(appAuthData(NAMESPACE_B, "app-b"), listener.config(authDataKey(NAMESPACE_B, "app-b")));
    }

    @Test
    public void testOnPluginChangedEmptyNamespaceRefreshClearsOnlyTargetNamespace() {
        TestNodeDataChangedListener listener = new TestNodeDataChangedListener();
        listener.putConfig(pluginListKey(NAMESPACE_A), Collections.singletonList(FIRST_PLUGIN));
        listener.putConfig(pluginDataKey(NAMESPACE_A, FIRST_PLUGIN), pluginData(NAMESPACE_A, FIRST_PLUGIN));
        listener.putConfig(pluginListKey(NAMESPACE_B), Collections.singletonList(SECOND_PLUGIN));
        listener.putConfig(pluginDataKey(NAMESPACE_B, SECOND_PLUGIN), pluginData(NAMESPACE_B, SECOND_PLUGIN));

        listener.onPluginChanged(Collections.emptyList(), DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertEquals(Collections.emptyList(), listener.config(pluginListKey(NAMESPACE_A)));
        assertNull(listener.config(pluginDataKey(NAMESPACE_A, FIRST_PLUGIN)));
        assertTrue(listener.wasDeleted(pluginDataKey(NAMESPACE_A, FIRST_PLUGIN)));
        assertEquals(Collections.singletonList(SECOND_PLUGIN), listener.config(pluginListKey(NAMESPACE_B)));
        assertEquals(pluginData(NAMESPACE_B, SECOND_PLUGIN), listener.config(pluginDataKey(NAMESPACE_B, SECOND_PLUGIN)));
    }

    @Test
    public void testOnPluginChangedNamespaceRefreshRemovesSelectorsAndRulesOfRemovedPlugins() {
        TestNodeDataChangedListener listener = new TestNodeDataChangedListener();
        listener.putConfig(pluginListKey(NAMESPACE_A), Arrays.asList(FIRST_PLUGIN, SECOND_PLUGIN));
        listener.putConfig(selectorListKey(NAMESPACE_A, FIRST_PLUGIN), Collections.singletonList(RETAINED_ID));
        listener.putConfig(selectorDataKey(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID),
                selectorData(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID));
        listener.putConfig(selectorListKey(NAMESPACE_A, SECOND_PLUGIN), Collections.singletonList(STALE_ID));
        listener.putConfig(selectorDataKey(NAMESPACE_A, SECOND_PLUGIN, STALE_ID),
                selectorData(NAMESPACE_A, SECOND_PLUGIN, STALE_ID));
        listener.putConfig(ruleListKey(NAMESPACE_A, SECOND_PLUGIN, STALE_ID), Collections.singletonList(ADDED_ID));
        listener.putConfig(ruleDataKey(NAMESPACE_A, SECOND_PLUGIN, STALE_ID, ADDED_ID),
                ruleData(NAMESPACE_A, SECOND_PLUGIN, STALE_ID, ADDED_ID));

        listener.onPluginChanged(Collections.singletonList(pluginData(NAMESPACE_A, FIRST_PLUGIN)),
                DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertEquals(Collections.singletonList(FIRST_PLUGIN), listener.config(pluginListKey(NAMESPACE_A)));
        assertEquals(Collections.singletonList(RETAINED_ID), listener.config(selectorListKey(NAMESPACE_A, FIRST_PLUGIN)));
        assertEquals(selectorData(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID),
                listener.config(selectorDataKey(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID)));
        assertTrue(listener.wasDeleted(selectorDataKey(NAMESPACE_A, SECOND_PLUGIN, STALE_ID)));
        assertNull(listener.config(selectorListKey(NAMESPACE_A, SECOND_PLUGIN)));
        assertTrue(listener.wasDeleted(ruleDataKey(NAMESPACE_A, SECOND_PLUGIN, STALE_ID, ADDED_ID)));
        assertNull(listener.config(ruleListKey(NAMESPACE_A, SECOND_PLUGIN, STALE_ID)));
    }

    @Test
    public void testOnSelectorChangedEmptyNamespaceRefreshClearsOnlyTargetNamespace() {
        TestNodeDataChangedListener listener = new TestNodeDataChangedListener();
        listener.putConfig(pluginListKey(NAMESPACE_A), Collections.singletonList(FIRST_PLUGIN));
        listener.putConfig(selectorListKey(NAMESPACE_A, FIRST_PLUGIN), Collections.singletonList(STALE_ID));
        listener.putConfig(selectorDataKey(NAMESPACE_A, FIRST_PLUGIN, STALE_ID), selectorData(NAMESPACE_A, FIRST_PLUGIN, STALE_ID));
        listener.putConfig(pluginListKey(NAMESPACE_B), Collections.singletonList(SECOND_PLUGIN));
        listener.putConfig(selectorListKey(NAMESPACE_B, SECOND_PLUGIN), Collections.singletonList(RETAINED_ID));
        listener.putConfig(selectorDataKey(NAMESPACE_B, SECOND_PLUGIN, RETAINED_ID), selectorData(NAMESPACE_B, SECOND_PLUGIN, RETAINED_ID));

        listener.onSelectorChanged(Collections.emptyList(), DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertEquals(Collections.emptyList(), listener.config(selectorListKey(NAMESPACE_A, FIRST_PLUGIN)));
        assertNull(listener.config(selectorDataKey(NAMESPACE_A, FIRST_PLUGIN, STALE_ID)));
        assertTrue(listener.wasDeleted(selectorDataKey(NAMESPACE_A, FIRST_PLUGIN, STALE_ID)));
        assertEquals(Collections.singletonList(RETAINED_ID), listener.config(selectorListKey(NAMESPACE_B, SECOND_PLUGIN)));
        assertEquals(selectorData(NAMESPACE_B, SECOND_PLUGIN, RETAINED_ID),
                listener.config(selectorDataKey(NAMESPACE_B, SECOND_PLUGIN, RETAINED_ID)));
    }

    @Test
    public void testOnSelectorChangedNamespaceRefreshRemovesGroupsAbsentFromPayload() {
        TestNodeDataChangedListener listener = new TestNodeDataChangedListener();
        listener.putConfig(pluginListKey(NAMESPACE_A), Arrays.asList(FIRST_PLUGIN, SECOND_PLUGIN));
        listener.putConfig(selectorListKey(NAMESPACE_A, FIRST_PLUGIN), Arrays.asList(RETAINED_ID, STALE_ID));
        listener.putConfig(selectorListKey(NAMESPACE_A, SECOND_PLUGIN), Collections.singletonList(STALE_ID));
        listener.putConfig(selectorDataKey(NAMESPACE_A, FIRST_PLUGIN, STALE_ID), selectorData(NAMESPACE_A, FIRST_PLUGIN, STALE_ID));
        listener.putConfig(selectorDataKey(NAMESPACE_A, SECOND_PLUGIN, STALE_ID), selectorData(NAMESPACE_A, SECOND_PLUGIN, STALE_ID));

        listener.onSelectorChanged(Collections.singletonList(selectorData(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID)),
                DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertEquals(Collections.singletonList(RETAINED_ID), listener.config(selectorListKey(NAMESPACE_A, FIRST_PLUGIN)));
        assertNull(listener.config(selectorDataKey(NAMESPACE_A, FIRST_PLUGIN, STALE_ID)));
        assertEquals(Collections.emptyList(), listener.config(selectorListKey(NAMESPACE_A, SECOND_PLUGIN)));
        assertNull(listener.config(selectorDataKey(NAMESPACE_A, SECOND_PLUGIN, STALE_ID)));
    }

    @Test
    public void testOnRuleChangedEmptyNamespaceRefreshClearsOnlyTargetNamespace() {
        TestNodeDataChangedListener listener = new TestNodeDataChangedListener();
        listener.putConfig(pluginListKey(NAMESPACE_A), Collections.singletonList(FIRST_PLUGIN));
        listener.putConfig(selectorListKey(NAMESPACE_A, FIRST_PLUGIN), Collections.singletonList(RETAINED_ID));
        listener.putConfig(ruleListKey(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID), Collections.singletonList(STALE_ID));
        listener.putConfig(ruleDataKey(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID, STALE_ID),
                ruleData(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID, STALE_ID));
        listener.putConfig(pluginListKey(NAMESPACE_B), Collections.singletonList(SECOND_PLUGIN));
        listener.putConfig(selectorListKey(NAMESPACE_B, SECOND_PLUGIN), Collections.singletonList(RETAINED_ID));
        listener.putConfig(ruleListKey(NAMESPACE_B, SECOND_PLUGIN, RETAINED_ID), Collections.singletonList(ADDED_ID));
        listener.putConfig(ruleDataKey(NAMESPACE_B, SECOND_PLUGIN, RETAINED_ID, ADDED_ID),
                ruleData(NAMESPACE_B, SECOND_PLUGIN, RETAINED_ID, ADDED_ID));

        listener.onRuleChanged(Collections.emptyList(), DataEventTypeEnum.REFRESH, NAMESPACE_A);

        assertEquals(Collections.emptyList(), listener.config(ruleListKey(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID)));
        assertNull(listener.config(ruleDataKey(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID, STALE_ID)));
        assertTrue(listener.wasDeleted(ruleDataKey(NAMESPACE_A, FIRST_PLUGIN, RETAINED_ID, STALE_ID)));
        assertEquals(Collections.singletonList(ADDED_ID), listener.config(ruleListKey(NAMESPACE_B, SECOND_PLUGIN, RETAINED_ID)));
        assertEquals(ruleData(NAMESPACE_B, SECOND_PLUGIN, RETAINED_ID, ADDED_ID),
                listener.config(ruleDataKey(NAMESPACE_B, SECOND_PLUGIN, RETAINED_ID, ADDED_ID)));
    }

    @Test
    public void testOnSelectorChangedRefreshReplacesPluginSelectorIds() {
        TestNodeDataChangedListener listener = new TestNodeDataChangedListener();
        listener.putConfig(selectorListKey(FIRST_PLUGIN), Arrays.asList(RETAINED_ID, STALE_ID));
        listener.putConfig(selectorListKey(SECOND_PLUGIN), Arrays.asList(RETAINED_ID, STALE_ID));
        listener.putConfig(selectorDataKey(FIRST_PLUGIN, STALE_ID), new SelectorData());
        listener.putConfig(selectorDataKey(SECOND_PLUGIN, STALE_ID), new SelectorData());

        listener.onSelectorChanged(Arrays.asList(
                selectorData(FIRST_PLUGIN, RETAINED_ID),
                selectorData(SECOND_PLUGIN, RETAINED_ID),
                selectorData(FIRST_PLUGIN, ADDED_ID)), DataEventTypeEnum.REFRESH);

        assertEquals(Arrays.asList(RETAINED_ID, ADDED_ID), listener.config(selectorListKey(FIRST_PLUGIN)));
        assertEquals(Arrays.asList(RETAINED_ID), listener.config(selectorListKey(SECOND_PLUGIN)));
        assertNull(listener.config(selectorDataKey(FIRST_PLUGIN, STALE_ID)));
        assertNull(listener.config(selectorDataKey(SECOND_PLUGIN, STALE_ID)));
        assertTrue(listener.wasDeleted(selectorDataKey(FIRST_PLUGIN, STALE_ID)));
        assertTrue(listener.wasDeleted(selectorDataKey(SECOND_PLUGIN, STALE_ID)));
    }

    @Test
    public void testOnPluginChangedRefreshWithEqualCardinalityRemovesStaleEntries() {
        TestNodeDataChangedListener listener = new TestNodeDataChangedListener();
        final String configKeyPrefix = NAMESPACE_ID + DefaultNodeConstants.JOIN_POINT + "plugin" + DefaultNodeConstants.JOIN_POINT;
        listener.putConfig(configKeyPrefix + DefaultNodeConstants.LIST_STR, Arrays.asList("A", "B", "C", "D"));
        listener.putConfig(configKeyPrefix + "A", pluginData("A"));
        listener.putConfig(configKeyPrefix + "B", pluginData("B"));

        listener.onPluginChanged(Arrays.asList(
                pluginData("C"), pluginData("D"), pluginData("E"), pluginData("F")), DataEventTypeEnum.REFRESH);

        assertNull(listener.config(configKeyPrefix + "A"));
        assertNull(listener.config(configKeyPrefix + "B"));
        assertTrue(listener.wasDeleted(configKeyPrefix + "A"));
        assertTrue(listener.wasDeleted(configKeyPrefix + "B"));
    }

    @Test
    public void testOnSelectorChangedWithNullNamespaceUsesDefaultNamespace() {
        TestNodeDataChangedListener listener = new TestNodeDataChangedListener();
        SelectorData selectorData = SelectorData.builder()
                .pluginName(FIRST_PLUGIN)
                .id(ADDED_ID)
                .build();

        listener.onSelectorChanged(Collections.singletonList(selectorData), DataEventTypeEnum.UPDATE);

        final String configKeyPrefix = SYS_DEFAULT_NAMESPACE_ID + DefaultNodeConstants.JOIN_POINT + "selector" + DefaultNodeConstants.JOIN_POINT;
        assertEquals(Collections.singletonList(ADDED_ID),
                listener.config(configKeyPrefix + FIRST_PLUGIN + DefaultNodeConstants.POINT_LIST));
        assertEquals(selectorData,
                listener.config(configKeyPrefix + FIRST_PLUGIN + DefaultNodeConstants.JOIN_POINT + ADDED_ID));
    }

    private static PluginData pluginData(final String name) {
        return pluginData(NAMESPACE_ID, name);
    }

    private static PluginData pluginData(final String namespaceId, final String name) {
        return PluginData.builder()
                .namespaceId(namespaceId)
                .name(name)
                .build();
    }

    private static AppAuthData appAuthData(final String namespaceId, final String appKey) {
        return AppAuthData.builder()
                .namespaceId(namespaceId)
                .appKey(appKey)
                .build();
    }

    private static SelectorData selectorData(final String pluginName, final String selectorId) {
        return selectorData(NAMESPACE_ID, pluginName, selectorId);
    }

    private static SelectorData selectorData(final String namespaceId, final String pluginName, final String selectorId) {
        return SelectorData.builder()
                .namespaceId(namespaceId)
                .pluginName(pluginName)
                .id(selectorId)
                .build();
    }

    private static RuleData ruleData(final String namespaceId, final String pluginName,
                                     final String selectorId, final String ruleId) {
        return RuleData.builder()
                .namespaceId(namespaceId)
                .pluginName(pluginName)
                .selectorId(selectorId)
                .id(ruleId)
                .build();
    }

    private static String selectorListKey(final String pluginName) {
        return selectorListKey(NAMESPACE_ID, pluginName);
    }

    private static String selectorListKey(final String namespaceId, final String pluginName) {
        return namespaceId + ".selector." + pluginName + DefaultNodeConstants.POINT_LIST;
    }

    private static String selectorDataKey(final String pluginName, final String selectorId) {
        return selectorDataKey(NAMESPACE_ID, pluginName, selectorId);
    }

    private static String selectorDataKey(final String namespaceId, final String pluginName, final String selectorId) {
        return namespaceId + ".selector." + pluginName + DefaultNodeConstants.JOIN_POINT + selectorId;
    }

    private static String authListKey(final String namespaceId) {
        return namespaceId + ".auth.list";
    }

    private static String authDataKey(final String namespaceId, final String appKey) {
        return namespaceId + ".auth." + appKey;
    }

    private static String pluginListKey(final String namespaceId) {
        return namespaceId + ".plugin.list";
    }

    private static String pluginDataKey(final String namespaceId, final String pluginName) {
        return namespaceId + ".plugin." + pluginName;
    }

    private static String ruleListKey(final String namespaceId, final String pluginName, final String selectorId) {
        return namespaceId + ".rule." + pluginName + "." + selectorId + DefaultNodeConstants.POINT_LIST;
    }

    private static String ruleDataKey(final String namespaceId, final String pluginName,
                                      final String selectorId, final String ruleId) {
        return namespaceId + ".rule." + pluginName + "." + selectorId + DefaultNodeConstants.JOIN_POINT + ruleId;
    }

    private static final class TestNodeDataChangedListener extends AbstractNodeDataChangedListener {

        private final Map<String, Object> configs = new HashMap<>();

        private final Set<String> deletedDataIds = new HashSet<>();

        private TestNodeDataChangedListener() {
            super(new ChangeData("plugin", "selector", "rule", "auth", "meta", "proxy.selector", "discovery"));
        }

        @Override
        public void doPublishConfig(final String dataId, final Object data) {
            configs.put(dataId, data);
        }

        @Override
        protected void doDelConfig(final String dataId) {
            configs.remove(dataId);
            deletedDataIds.add(dataId);
        }

        @Override
        public String getConfig(final String dataId) {
            return Optional.ofNullable(configs.get(dataId))
                    .map(GsonUtils.getInstance()::toJson)
                    .orElse(null);
        }

        private void putConfig(final String dataId, final Object data) {
            configs.put(dataId, data);
        }

        private Object config(final String dataId) {
            return configs.get(dataId);
        }

        private boolean wasDeleted(final String dataId) {
            return deletedDataIds.contains(dataId);
        }
    }
}
