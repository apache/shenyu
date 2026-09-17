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

package org.apache.shenyu.sync.data.apollo;

import com.ctrip.framework.apollo.enums.ConfigSourceType;
import com.ctrip.framework.apollo.internals.ConfigRepository;
import com.ctrip.framework.apollo.internals.DefaultConfig;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Integration test case for {@link ApolloDataService} that flows through apollo-client's real
 * change listener filtering (interested keys and prefixes are matched in {@link com.ctrip.framework.apollo.internals.AbstractConfig}).
 */
@ExtendWith(MockitoExtension.class)
class ApolloDataServiceIntegrationTest {

    private static final String NAMESPACE = "shenyu";

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

    @Test
    void testIncrementalChangeEventFlowsThroughApolloPrefixFiltering() throws InterruptedException {
        final DefaultConfig defaultConfig = createDefaultConfig();

        final CountDownLatch latch = new CountDownLatch(1);
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(pluginDataSubscriber).onSubscribe(any());

        new ApolloDataService(defaultConfig,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                createShenyuConfig());

        final Properties changedProperties = new Properties();
        changedProperties.setProperty(NAMESPACE + ".plugin.divide", "{\"id\":\"1\",\"name\":\"divide\"}");
        defaultConfig.onRepositoryChange(NAMESPACE, changedProperties);

        assertTrue(latch.await(5, TimeUnit.SECONDS),
                "incremental change event should reach the subscriber through apollo prefix filtering");
    }

    @Test
    void testDeletedEventFlowsThroughApolloPrefixFiltering() throws InterruptedException {
        final DefaultConfig defaultConfig = createDefaultConfig();
        final Properties initialProperties = new Properties();
        initialProperties.setProperty(NAMESPACE + ".plugin.divide", "{\"id\":\"1\",\"name\":\"divide\"}");
        defaultConfig.onRepositoryChange(NAMESPACE, initialProperties);

        final CountDownLatch latch = new CountDownLatch(1);
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(pluginDataSubscriber).unSubscribe(any());

        new ApolloDataService(defaultConfig,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                createShenyuConfig());

        defaultConfig.onRepositoryChange(NAMESPACE, new Properties());

        assertTrue(latch.await(5, TimeUnit.SECONDS),
                "deleted event should reach the subscriber through apollo prefix filtering");
        ArgumentCaptor<PluginData> captor = ArgumentCaptor.forClass(PluginData.class);
        verify(pluginDataSubscriber).unSubscribe(captor.capture());
        assertEquals("divide", captor.getValue().getName());
    }

    private DefaultConfig createDefaultConfig() {
        final ConfigRepository configRepository = mock(ConfigRepository.class);
        when(configRepository.getConfig()).thenReturn(new Properties());
        when(configRepository.getSourceType()).thenReturn(ConfigSourceType.LOCAL);
        return new DefaultConfig(NAMESPACE, configRepository);
    }

    private ShenyuConfig createShenyuConfig() {
        final ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.setNamespace(NAMESPACE);
        return shenyuConfig;
    }
}
