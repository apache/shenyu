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

import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

@RunWith(MockitoJUnitRunner.class)
public class AbstractPathDataSyncServiceTest {

    @Mock
    private PluginDataSubscriber pluginDataSubscriber;

    @Mock
    private MetaDataSubscriber metaDataSubscriber;

    @Mock
    private AuthDataSubscriber authDataSubscriber;

    @Mock
    private ProxySelectorDataSubscriber proxySelectorDataSubscriber;

    @Mock
    private DiscoveryUpstreamDataSubscriber discoveryUpstreamDataSubscriber;

    private AbstractPathDataSyncService pathDataSyncService;

    @Before
    public void setUp() {

        MockitoAnnotations.openMocks(this);

        List<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        authDataSubscribers.add(authDataSubscriber);

        pathDataSyncService = new AbstractPathDataSyncServiceImpl(
                pluginDataSubscriber,
                List.of(metaDataSubscriber),
                authDataSubscribers,
                List.of(proxySelectorDataSubscriber),
                List.of(discoveryUpstreamDataSubscriber)
        );
    }

    @Test
    public void testUnCacheAuthData() {

        String jsonData = "{\"appKey\":\"testApp\"}";

        pathDataSyncService.cacheAuthData(GsonUtils.getInstance().fromJson(jsonData, AppAuthData.class));
        pathDataSyncService.unCacheAuthData("/namespace/auths/testApp");

        verify(authDataSubscriber).unSubscribe(any());
    }

    // Mock implementation
    static class AbstractPathDataSyncServiceImpl extends AbstractPathDataSyncService {

        AbstractPathDataSyncServiceImpl(final PluginDataSubscriber pluginDataSubscriber,
                                               final List<MetaDataSubscriber> metaDataSubscribers,
                                               final List<AuthDataSubscriber> authDataSubscribers,
                                               final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                               final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {

            super(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                    proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers);
        }

        @Override
        public void event(
                final String namespaceId,
                final String updatePath,
                final String updateData,
                final String registerPath,
                final EventType eventType
        ) {

            super.event(namespaceId, updatePath, updateData, registerPath, eventType);
        }

        @Override
        public void close() throws Exception {
            // for test.
        }
    }

}
