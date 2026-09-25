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

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public final class AbstractPathDataChangedListenerTest {

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

        assertNull(listener.deletedPath);
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

    private static final class TestPathDataChangedListener extends AbstractPathDataChangedListener {

        private String deletedPath;

        @Override
        public void createOrUpdate(final String pluginPath, final Object data) {
        }

        @Override
        public void deleteNode(final String pluginPath) {
            deletedPath = pluginPath;
        }

        @Override
        public void deletePathRecursive(final String selectorParentPath) {
        }
    }
}
