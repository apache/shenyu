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

package org.apache.shenyu.sync.data.polaris;

import com.tencent.polaris.configuration.api.core.ChangeType;
import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileChangeEvent;
import com.tencent.polaris.configuration.api.core.ConfigFileChangeListener;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.polaris.config.PolarisConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * add test case for {@link PolarisSyncDataService}.
 */
@ExtendWith(MockitoExtension.class)
class PolarisSyncDataServiceTest {

    @Mock
    private ConfigFileService configFileService;

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

    @Mock
    private ConfigFile configFile;

    private PolarisConfig polarisConfig;

    private ShenyuConfig shenyuConfig;

    @BeforeEach
    void setup() {
        polarisConfig = new PolarisConfig();
        polarisConfig.setNamespace("test-namespace");
        polarisConfig.setFileGroup("test-group");
        shenyuConfig = new ShenyuConfig();

        lenient().when(configFileService.getConfigFile(anyString(), anyString(), anyString())).thenReturn(configFile);
        lenient().when(configFile.hasContent()).thenReturn(true);
        lenient().when(configFile.getContent()).thenReturn("[]");
    }

    @Test
    void testConstructor() {
        PolarisSyncDataService service = new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        assertNotNull(service);
        verify(configFileService, atLeastOnce()).getConfigFile(anyString(), anyString(), anyString());
    }

    @Test
    void testConfigFileWithNoContent() {
        when(configFile.hasContent()).thenReturn(false);

        PolarisSyncDataService service = new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        assertNotNull(service);
    }

    @Test
    void testListenerUpdateEvent() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigFileChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigFileChangeListener.class);
        verify(configFile, atLeastOnce()).addChangeListener(listenerCaptor.capture());

        ConfigFileChangeEvent event = mock(ConfigFileChangeEvent.class);
        when(event.getChangeType()).thenReturn(ChangeType.MODIFIED);
        when(event.getOldValue()).thenReturn("old");
        when(event.getNewValue()).thenReturn("new");

        listenerCaptor.getValue().onChange(event);
    }

    @Test
    void testListenerDeleteEvent() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigFileChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigFileChangeListener.class);
        verify(configFile, atLeastOnce()).addChangeListener(listenerCaptor.capture());

        ConfigFileChangeEvent event = mock(ConfigFileChangeEvent.class);
        when(event.getChangeType()).thenReturn(ChangeType.DELETED);

        listenerCaptor.getValue().onChange(event);
    }

    @Test
    void testListenerBlankNewValue() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigFileChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigFileChangeListener.class);
        verify(configFile, atLeastOnce()).addChangeListener(listenerCaptor.capture());

        ConfigFileChangeEvent event = mock(ConfigFileChangeEvent.class);
        when(event.getChangeType()).thenReturn(ChangeType.MODIFIED);
        when(event.getNewValue()).thenReturn("");

        listenerCaptor.getValue().onChange(event);
    }

    @Test
    void testListenerSameOldAndNewValue() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigFileChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigFileChangeListener.class);
        verify(configFile, atLeastOnce()).addChangeListener(listenerCaptor.capture());

        ConfigFileChangeEvent event = mock(ConfigFileChangeEvent.class);
        when(event.getChangeType()).thenReturn(ChangeType.MODIFIED);
        when(event.getOldValue()).thenReturn("same");
        when(event.getNewValue()).thenReturn("same");

        listenerCaptor.getValue().onChange(event);
    }

    @Test
    void testListenerException() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigFileChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigFileChangeListener.class);
        verify(configFile, atLeastOnce()).addChangeListener(listenerCaptor.capture());

        ConfigFileChangeEvent event = mock(ConfigFileChangeEvent.class);
        when(event.getChangeType()).thenThrow(new RuntimeException("Test exception"));

        listenerCaptor.getValue().onChange(event);
    }

    @Test
    void testGetServiceConfigException() {
        when(configFileService.getConfigFile(anyString(), anyString(), anyString()))
                .thenThrow(new RuntimeException("Config error"));

        assertThrows(ShenyuException.class, () -> new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        ));
    }

    @Test
    void testClose() {
        PolarisSyncDataService service = new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        service.close();
        verify(configFile, atLeastOnce()).removeChangeListener(any(ConfigFileChangeListener.class));
    }

    @Test
    void testCloseWithNullListener() {
        when(configFile.hasContent()).thenReturn(false);

        PolarisSyncDataService service = new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                null,
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList(),
                shenyuConfig
        );

        service.close();
        verify(configFile, atLeastOnce()).removeChangeListener(any(ConfigFileChangeListener.class));
    }

    @Test
    void testListenerWithDeleteHandler() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigFileChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigFileChangeListener.class);
        verify(configFile, atLeastOnce()).addChangeListener(listenerCaptor.capture());

        ConfigFileChangeEvent event = mock(ConfigFileChangeEvent.class);
        when(event.getChangeType()).thenReturn(ChangeType.DELETED);

        listenerCaptor.getValue().onChange(event);
    }

    @Test
    void testListenerWithValidUpdate() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigFileChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigFileChangeListener.class);
        verify(configFile, atLeastOnce()).addChangeListener(listenerCaptor.capture());

        ConfigFileChangeEvent event = mock(ConfigFileChangeEvent.class);
        when(event.getChangeType()).thenReturn(ChangeType.MODIFIED);
        when(event.getOldValue()).thenReturn("[]");
        when(event.getNewValue()).thenReturn("[{\"name\":\"test\"}]");

        listenerCaptor.getValue().onChange(event);
    }

    @Test
    void testGetServiceConfigReturnsContent() {
        String expectedContent = "[]";
        when(configFile.hasContent()).thenReturn(true);
        when(configFile.getContent()).thenReturn(expectedContent);

        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        verify(configFile, atLeastOnce()).getContent();
    }

    @Test
    void testGetServiceConfigReturnsNull() {
        when(configFile.hasContent()).thenReturn(false);

        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        verify(configFile, atLeastOnce()).hasContent();
    }

    @Test
    void testMultipleConfigFiles() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        verify(configFileService, atLeastOnce()).getConfigFile(anyString(), anyString(), anyString());
    }

    @Test
    void testListenerAddedToAllConfigFiles() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        verify(configFile, atLeastOnce()).addChangeListener(any(ConfigFileChangeListener.class));
    }

    @Test
    void testCloseRemovesAllListeners() {
        PolarisSyncDataService service = new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        service.close();

        verify(configFileService, atLeastOnce()).getConfigFile(anyString(), anyString(), anyString());
        verify(configFile, atLeastOnce()).removeChangeListener(any(ConfigFileChangeListener.class));
    }

    @Test
    void testListenerWithNullOldValue() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigFileChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigFileChangeListener.class);
        verify(configFile, atLeastOnce()).addChangeListener(listenerCaptor.capture());

        ConfigFileChangeEvent event = mock(ConfigFileChangeEvent.class);
        when(event.getChangeType()).thenReturn(ChangeType.MODIFIED);
        when(event.getOldValue()).thenReturn(null);
        when(event.getNewValue()).thenReturn("{\"data\":\"new\"}");

        listenerCaptor.getValue().onChange(event);
    }

    @Test
    void testListenerWithAddedChangeType() {
        new PolarisSyncDataService(
                polarisConfig,
                configFileService,
                pluginDataSubscriber,
                Collections.singletonList(metaDataSubscriber),
                Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber),
                Collections.singletonList(discoveryUpstreamDataSubscriber),
                shenyuConfig
        );

        ArgumentCaptor<ConfigFileChangeListener> listenerCaptor = ArgumentCaptor.forClass(ConfigFileChangeListener.class);
        verify(configFile, atLeastOnce()).addChangeListener(listenerCaptor.capture());

        ConfigFileChangeEvent event = mock(ConfigFileChangeEvent.class);
        when(event.getChangeType()).thenReturn(ChangeType.ADDED);
        when(event.getNewValue()).thenReturn("{\"data\":\"added\"}");
        when(event.getOldValue()).thenReturn(null);

        listenerCaptor.getValue().onChange(event);
    }
}
