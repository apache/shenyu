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

package org.apache.shenyu.plugin.tcp.handler;

import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.cache.CommonProxySelectorDataSubscriber;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.core.AbstractNodeDataSyncService;
import org.apache.shenyu.sync.data.core.AbstractPathDataSyncService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.Collections;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Verifies real TCP listener removal through path and node synchronization.
 */
class ProxySelectorIdentityDeletionTest {

    private final TcpBootstrapFactory factory = TcpBootstrapFactory.getSingleton();

    @BeforeEach
    void setUp() {
        factory.clearCache();
    }

    @AfterEach
    void tearDown() {
        factory.clearCache();
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testIdBasedDeletionReleasesListenerPort(final boolean node) throws IOException {
        CommonProxySelectorDataSubscriber subscriber = new CommonProxySelectorDataSubscriber(Collections.singletonList(new TcpProxySelectorDataHandler()));
        ProxySelectorData data = new ProxySelectorData();
        data.setPluginName("tcp");
        data.setId("selector-id");
        data.setName("listener-name");
        try (ServerSocket socket = new ServerSocket(0)) {
            data.setForwardPort(socket.getLocalPort());
        }
        final String json = GsonUtils.getInstance().toJson(data);
        final Runnable delete;
        if (node) {
            NodeSyncService service = new NodeSyncService(subscriber);
            service.put(json);
            delete = service::delete;
        } else {
            AbstractPathDataSyncService service = new AbstractPathDataSyncService(null, Collections.emptyList(), Collections.emptyList(),
                    Collections.singletonList(subscriber), Collections.emptyList()) {
                @Override
                public void close() {
                }
            };
            String path = DefaultPathConstants.buildProxySelectorPath("namespace", "tcp", data.getId());
            String parent = "/namespace" + DefaultPathConstants.PROXY_SELECTOR;
            service.event("/namespace", path, json, parent, AbstractPathDataSyncService.EventType.PUT);
            delete = () -> service.event("/namespace", path, null, parent, AbstractPathDataSyncService.EventType.DELETE);
        }

        assertNotNull(factory.getCache(data.getName()));
        delete.run();

        assertFalse(factory.inCache(data.getName()));
        assertFalse(UpstreamProvider.getSingleton().inCache(data.getName()));
        try (ServerSocket released = new ServerSocket(data.getForwardPort())) {
            assertEquals(data.getForwardPort().intValue(), released.getLocalPort());
        }
    }

    private static final class NodeSyncService extends AbstractNodeDataSyncService {

        private NodeSyncService(final ProxySelectorDataSubscriber subscriber) {
            super(null, null, Collections.emptyList(), Collections.emptyList(), Collections.singletonList(subscriber), Collections.emptyList(), null);
        }

        private void put(final String json) {
            cacheProxySelectorData(json);
        }

        private void delete() {
            unCacheProxySelectorData("namespace.proxy.selector.tcp.selector-id");
        }

        @Override
        protected String getServiceConfig(final String key, final Consumer<String> updateHandler, final Consumer<String> deleteHandler) {
            return null;
        }

        @Override
        protected void doRemoveListener(final String removeKey) {
        }
    }
}
