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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

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

    private static PluginData pluginData(final String name) {
        return PluginData.builder()
                .namespaceId(NAMESPACE_ID)
                .name(name)
                .build();
    }

    private static SelectorData selectorData(final String pluginName, final String selectorId) {
        return SelectorData.builder()
                .namespaceId(NAMESPACE_ID)
                .pluginName(pluginName)
                .id(selectorId)
                .build();
    }

    private static String selectorListKey(final String pluginName) {
        return NAMESPACE_ID + ".selector." + pluginName + DefaultNodeConstants.POINT_LIST;
    }

    private static String selectorDataKey(final String pluginName, final String selectorId) {
        return NAMESPACE_ID + ".selector." + pluginName + DefaultNodeConstants.JOIN_POINT + selectorId;
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
