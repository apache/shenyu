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

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.ConfigChangeListener;
import com.ctrip.framework.apollo.enums.PropertyChangeType;
import com.ctrip.framework.apollo.model.ConfigChange;
import com.ctrip.framework.apollo.model.ConfigChangeEvent;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.ApolloPathConstants;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ApolloDataService}.
 */
@ExtendWith(MockitoExtension.class)
class ApolloDataServiceTest {

    @Mock
    private Config configService;

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

    private ShenyuConfig shenyuConfig;

    private ConfigChangeListener capturedListener;

    @BeforeEach
    void setUp() {
        shenyuConfig = new ShenyuConfig();
        shenyuConfig.setNamespace("shenyu");

        lenient().when(configService.getProperty(anyString(), any())).thenReturn("[]");
    }

    @Test
    void testConstructorAndClose() {
        final ApolloDataService apolloDataService = new ApolloDataService(
                configService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigChangeListener.class);
        verify(configService).addChangeListener(listenerCaptor.capture(), anySet(), eq(ApolloPathConstants.pathKeySet()));

        capturedListener = listenerCaptor.getValue();
        assertNotNull(capturedListener);

        apolloDataService.close();
        verify(configService).removeChangeListener(capturedListener);
    }

    @Test
    void testCloseWithNullListener() {
        lenient().when(configService.getProperty(anyString(), any())).thenReturn(null);

        ApolloDataService apolloDataService = new ApolloDataService(
                configService,
                null,
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList(),
                shenyuConfig
        );

        apolloDataService.close();
        verify(configService, times(1)).removeChangeListener(any());
    }

    @Test
    void testPluginDataChange() {
        ApolloDataService apolloDataService = createApolloDataService();

        ConfigChangeEvent event = mockConfigChangeEvent(
                ApolloPathConstants.PLUGIN_DATA_ID + "/test-plugin",
                "{\"id\":\"1\",\"name\":\"test\"}",
                PropertyChangeType.MODIFIED
        );

        capturedListener.onChange(event);
        verify(pluginDataSubscriber, times(1)).onSubscribe(any());
    }

    @Test
    void testPluginDataAdd() {
        ApolloDataService apolloDataService = createApolloDataService();

        ConfigChangeEvent event = mockConfigChangeEvent(
                ApolloPathConstants.PLUGIN_DATA_ID + "/new-plugin",
                "{\"id\":\"2\",\"name\":\"new\"}",
                PropertyChangeType.ADDED
        );

        capturedListener.onChange(event);
        verify(pluginDataSubscriber, times(1)).onSubscribe(any());
    }

    @Test
    void testSelectorDataChange() {
        ApolloDataService apolloDataService = createApolloDataService();

        ConfigChangeEvent event = mockConfigChangeEvent(
                ApolloPathConstants.SELECTOR_DATA_ID + "/test-selector",
                "{\"id\":\"1\",\"name\":\"test\"}",
                PropertyChangeType.MODIFIED
        );

        capturedListener.onChange(event);
        verify(pluginDataSubscriber, times(1)).onSelectorSubscribe(any());
    }

    @Test
    void testRuleDataChange() {
        ApolloDataService apolloDataService = createApolloDataService();

        ConfigChangeEvent event = mockConfigChangeEvent(
                ApolloPathConstants.RULE_DATA_ID + "/test-rule",
                "{\"id\":\"1\",\"name\":\"test\"}",
                PropertyChangeType.MODIFIED
        );

        capturedListener.onChange(event);
        verify(pluginDataSubscriber, times(1)).onRuleSubscribe(any());
    }

    @Test
    void testAuthDataChange() {
        ApolloDataService apolloDataService = createApolloDataService();

        ConfigChangeEvent event = mockConfigChangeEvent(
                ApolloPathConstants.AUTH_DATA_ID + "/test-auth",
                "{\"appKey\":\"test\"}",
                PropertyChangeType.MODIFIED
        );

        capturedListener.onChange(event);
        verify(authDataSubscriber, times(1)).onSubscribe(any());
    }

    @Test
    void testMetaDataChange() {
        ApolloDataService apolloDataService = createApolloDataService();

        ConfigChangeEvent event = mockConfigChangeEvent(
                ApolloPathConstants.META_DATA_ID + "/test-meta",
                "{\"id\":\"1\",\"path\":\"/test\"}",
                PropertyChangeType.MODIFIED
        );

        capturedListener.onChange(event);
        verify(metaDataSubscriber, times(1)).onSubscribe(any());
    }

    @Test
    void testProxySelectorDataChange() {
        ApolloDataService apolloDataService = createApolloDataService();

        ConfigChangeEvent event = mockConfigChangeEvent(
                ApolloPathConstants.PROXY_SELECTOR_DATA_ID + "/test-proxy",
                "{\"id\":\"1\",\"name\":\"test\"}",
                PropertyChangeType.MODIFIED
        );

        capturedListener.onChange(event);
        verify(proxySelectorDataSubscriber, times(1)).onSubscribe(any());
    }

    @Test
    void testDiscoveryDataChange() {
        ApolloDataService apolloDataService = createApolloDataService();

        ConfigChangeEvent event = mockConfigChangeEvent(
                ApolloPathConstants.DISCOVERY_DATA_ID + "/test-discovery",
                "{\"name\":\"test\"}",
                PropertyChangeType.MODIFIED
        );

        capturedListener.onChange(event);
        verify(discoveryUpstreamDataSubscriber, times(1)).onSubscribe(any());
    }

    @Test
    void testNullConfigChange() {
        final ApolloDataService apolloDataService = createApolloDataService();

        ConfigChangeEvent event = mock(ConfigChangeEvent.class);
        Set<String> keys = new HashSet<>();
        keys.add("test-key");
        when(event.changedKeys()).thenReturn(keys);
        when(event.getChange("test-key")).thenReturn(null);

        capturedListener.onChange(event);
        // Should handle null config change gracefully
        assertNotNull(apolloDataService);
    }

    @Test
    void testExceptionInChangeHandler() {
        final ApolloDataService apolloDataService = createApolloDataService();

        ConfigChangeEvent event = mock(ConfigChangeEvent.class);
        Set<String> keys = new HashSet<>();
        keys.add(ApolloPathConstants.PLUGIN_DATA_ID + "/test");
        when(event.changedKeys()).thenReturn(keys);
        when(event.getChange(anyString())).thenThrow(new RuntimeException("Test exception"));

        capturedListener.onChange(event);
        // Should handle exception gracefully
        assertNotNull(apolloDataService);
    }

    private ApolloDataService createApolloDataService() {
        ApolloDataService service = new ApolloDataService(
                configService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigChangeListener.class);
        verify(configService).addChangeListener(listenerCaptor.capture(), anySet(), eq(ApolloPathConstants.pathKeySet()));
        capturedListener = listenerCaptor.getValue();

        return service;
    }

    private ConfigChangeEvent mockConfigChangeEvent(final String key, final String newValue, final PropertyChangeType changeType) {
        ConfigChangeEvent event = mock(ConfigChangeEvent.class);
        ConfigChange configChange = mock(ConfigChange.class);

        Set<String> keys = new HashSet<>();
        keys.add(key);

        when(event.changedKeys()).thenReturn(keys);
        when(event.getChange(key)).thenReturn(configChange);
        when(configChange.getNewValue()).thenReturn(newValue);
        when(configChange.getChangeType()).thenReturn(changeType);

        return event;
    }
}

