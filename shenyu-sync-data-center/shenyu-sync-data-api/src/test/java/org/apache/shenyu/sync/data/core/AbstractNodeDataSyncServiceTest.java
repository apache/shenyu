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

package org.apache.shenyu.sync.data.core;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

@RunWith(MockitoJUnitRunner.class)
public class AbstractNodeDataSyncServiceTest {

    @Mock
    private AbstractNodeDataSyncService.ChangeData changeData;

    @Mock
    private PluginDataSubscriber pluginDataSubscriber;

    private List<MetaDataSubscriber> metaDataSubscribers;

    private List<AuthDataSubscriber> authDataSubscribers;

    private List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers;

    @Mock
    private List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers;

    @Mock
    private ShenyuConfig shenyuConfig;

    @Mock
    private AuthDataSubscriber authDataSubscriber;

    @Mock
    private MetaDataSubscriber metaDataSubscriber;

    @Mock
    private ProxySelectorDataSubscriber proxySelectorDataSubscriber;

    private AbstractNodeDataSyncService nodeDataSyncService;

    @Before
    public void setUp() {

        MockitoAnnotations.openMocks(this);

        authDataSubscribers = new ArrayList<>();
        authDataSubscribers.add(authDataSubscriber);
        metaDataSubscribers = new ArrayList<>();
        metaDataSubscribers.add(metaDataSubscriber);
        proxySelectorDataSubscribers = new ArrayList<>();
        proxySelectorDataSubscribers.add(proxySelectorDataSubscriber);

        nodeDataSyncService = new AbstractNodeDataSyncServiceImpl(
                changeData,
                pluginDataSubscriber,
                metaDataSubscribers,
                authDataSubscribers,
                proxySelectorDataSubscribers,
                discoveryUpstreamDataSubscribers,
                shenyuConfig
        );
    }

    @Test
    public void testCachePluginData() {

        String jsonData = "{\"name\":\"testPlugin\"}";

        nodeDataSyncService.cachePluginData(jsonData);

        ArgumentCaptor<PluginData> captor = ArgumentCaptor.forClass(PluginData.class);
        verify(pluginDataSubscriber).onSubscribe(captor.capture());

        assertEquals("testPlugin", captor.getValue().getName());
    }

    @Test
    public void testUnCachePluginData() {

        nodeDataSyncService.unCachePluginData("namespace.plugin.testPlugin");

        ArgumentCaptor<PluginData> captor = ArgumentCaptor.forClass(PluginData.class);
        verify(pluginDataSubscriber).unSubscribe(captor.capture());
        assertEquals("testPlugin", captor.getValue().getName());
    }

    @Test
    public void testCacheAuthData() {

        String jsonData = "{\"appKey\":\"testApp\"}";

        nodeDataSyncService.cacheAuthData(jsonData);

        verify(authDataSubscribers.get(0)).onSubscribe(any());
    }

    @Test
    public void testUnCacheAuthData() {
        nodeDataSyncService.unCacheAuthData("namespace.auth.testApp");

        ArgumentCaptor<AppAuthData> captor = ArgumentCaptor.forClass(AppAuthData.class);
        verify(authDataSubscriber).unSubscribe(captor.capture());
        assertEquals("testApp", captor.getValue().getAppKey());
    }

    @Test
    public void testUnCacheMetaData() {
        nodeDataSyncService.unCacheMetaData("namespace.meta.metaId");

        ArgumentCaptor<MetaData> captor = ArgumentCaptor.forClass(MetaData.class);
        verify(metaDataSubscriber).unSubscribe(captor.capture());
        assertEquals("metaId", captor.getValue().getId());
    }

    @Test
    public void testUnCacheProxySelectorData() {
        nodeDataSyncService.unCacheProxySelectorData("namespace.proxy.selector.tcp.selectorName");

        ArgumentCaptor<ProxySelectorData> captor = ArgumentCaptor.forClass(ProxySelectorData.class);
        verify(proxySelectorDataSubscriber).unSubscribe(captor.capture());
        assertEquals("tcp", captor.getValue().getPluginName());
        assertEquals("selectorName", captor.getValue().getName());
    }

    @Test
    public void testUnCacheDataWithInvalidKey() {
        assertDoesNotThrow(() -> nodeDataSyncService.unCachePluginData("namespace"));
        assertDoesNotThrow(() -> nodeDataSyncService.unCacheAuthData("namespace"));
        assertDoesNotThrow(() -> nodeDataSyncService.unCacheMetaData("namespace"));
        assertDoesNotThrow(() -> nodeDataSyncService.unCacheProxySelectorData("namespace"));

        verifyNoInteractions(pluginDataSubscriber, authDataSubscriber, metaDataSubscriber, proxySelectorDataSubscriber);
    }

    // Mock implementation
    static class AbstractNodeDataSyncServiceImpl extends AbstractNodeDataSyncService {

        AbstractNodeDataSyncServiceImpl(final ChangeData changeData,
                                               final PluginDataSubscriber pluginDataSubscriber,
                                               final List<MetaDataSubscriber> metaDataSubscribers,
                                               final List<AuthDataSubscriber> authDataSubscribers,
                                               final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                               final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers,
                                               final ShenyuConfig shenyuConfig
        ) {

            super(changeData, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                    proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers, shenyuConfig);
        }

        @Override
        protected String getServiceConfig(
                final String key,
                final Consumer<String> updateHandler,
                final Consumer<String> deleteHandler
        ) {

            // Mocked response for testing
            return "";
        }

        @Override
        protected void doRemoveListener(final String removeKey) {

            // No implementation needed for testing
        }
    }

}
