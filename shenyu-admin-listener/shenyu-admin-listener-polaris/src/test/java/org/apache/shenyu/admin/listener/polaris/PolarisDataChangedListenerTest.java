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

package org.apache.shenyu.admin.listener.polaris;

import com.google.common.collect.ImmutableList;
import com.tencent.polaris.api.exception.PolarisException;
import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFilePublishService;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.client.internal.DefaultConfigFileMetadata;
import org.apache.shenyu.admin.config.properties.PolarisProperties;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link PolarisDataChangedListener}.
 */
@ExtendWith(MockitoExtension.class)
public class PolarisDataChangedListenerTest {

    private static final String MOCK_APP_KEY = "MOCK_APP_KEY";

    private static final String MOCK_APP_SECRET = "MOCK_APP_SECRET";

    private static final String MOCK_ID = "MOCK_ID";

    private static final String MOCK_PATH = "MOCK_PATH";

    private static final String MOCK_APP_NAME = "MOCK_APP_NAME";

    private static final String MOCK_NAME = "MOCK_NAME";

    private static final String MOCK_CONFIG = "MOCK_CONFIG";

    private static final String MOCK_PLUGIN_NAME = "MOCK_PLUGIN_NAME";

    private static final String MOCK_SELECTOR_ID = "MOCK_SELECTOR_ID";

    @Mock
    private ConfigFile configFile;

    @Mock
    private PolarisProperties polarisProperties;

    @Mock
    private ConfigFileService polarisConfigFileService;

    @Mock
    private ConfigFilePublishService polarisConfigFilePublishService;

    @InjectMocks
    private PolarisDataChangedListener polarisDataChangedListener;

    @Test
    public void testOnAppAuthChanged() throws PolarisException {
        when(polarisProperties.getNamespace()).thenReturn(PolarisPathConstants.NAMESPACE);
        when(polarisProperties.getFileGroup()).thenReturn(PolarisPathConstants.FILE_GROUP);
        when(configFile.hasContent()).thenReturn(false);
        when(configFile.getContent()).thenReturn(null);
        when(polarisConfigFileService.getConfigFile(any())).thenReturn(configFile);
        when(polarisConfigFileService.getConfigFile(any(), any(), any())).thenReturn(configFile);
        AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_KEY).appSecret(MOCK_APP_SECRET).build();
        polarisDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.DELETE);
        polarisDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.REFRESH);
        polarisDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.MYSELF);
        polarisDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.CREATE);
        verify(polarisConfigFilePublishService, times(8)).createConfigFile(any(DefaultConfigFileMetadata.class), any(String.class));
        verify(polarisConfigFilePublishService, times(8)).releaseConfigFile(any(DefaultConfigFileMetadata.class));
    }

    @Test
    public void testOnPluginChanged() throws PolarisException {
        when(polarisProperties.getNamespace()).thenReturn(PolarisPathConstants.NAMESPACE);
        when(polarisProperties.getFileGroup()).thenReturn(PolarisPathConstants.FILE_GROUP);
        when(configFile.hasContent()).thenReturn(false);
        when(configFile.getContent()).thenReturn(null);
        when(polarisConfigFileService.getConfigFile(any())).thenReturn(configFile);
        when(polarisConfigFileService.getConfigFile(any(), any(), any())).thenReturn(configFile);
        PluginData pluginData = PluginData.builder().id(MOCK_ID).name(MOCK_NAME).config(MOCK_CONFIG).build();
        polarisDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.DELETE);
        polarisDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.REFRESH);
        polarisDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.MYSELF);
        polarisDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.CREATE);
        verify(polarisConfigFilePublishService, times(8)).createConfigFile(any(DefaultConfigFileMetadata.class), any(String.class));
        verify(polarisConfigFilePublishService, times(8)).releaseConfigFile(any(DefaultConfigFileMetadata.class));
    }

    @Test
    public void testOnSelectorChanged() throws PolarisException {
        when(polarisProperties.getNamespace()).thenReturn(PolarisPathConstants.NAMESPACE);
        when(polarisProperties.getFileGroup()).thenReturn(PolarisPathConstants.FILE_GROUP);
        when(configFile.hasContent()).thenReturn(false);
        when(configFile.getContent()).thenReturn(null);
        when(polarisConfigFileService.getConfigFile(any())).thenReturn(configFile);
        when(polarisConfigFileService.getConfigFile(any(), any(), any())).thenReturn(configFile);
        SelectorData selectorData = SelectorData.builder().id(MOCK_ID).name(MOCK_NAME).pluginName(MOCK_PLUGIN_NAME).build();
        polarisDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.DELETE);
        polarisDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.REFRESH);
        polarisDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.MYSELF);
        polarisDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.CREATE);
        verify(polarisConfigFilePublishService, times(8)).createConfigFile(any(DefaultConfigFileMetadata.class), any(String.class));
        verify(polarisConfigFilePublishService, times(8)).releaseConfigFile(any(DefaultConfigFileMetadata.class));
    }

    @Test
    public void testOnMetaDataChanged() throws PolarisException {
        when(polarisProperties.getNamespace()).thenReturn(PolarisPathConstants.NAMESPACE);
        when(polarisProperties.getFileGroup()).thenReturn(PolarisPathConstants.FILE_GROUP);
        when(configFile.hasContent()).thenReturn(false);
        when(configFile.getContent()).thenReturn(null);
        when(polarisConfigFileService.getConfigFile(any())).thenReturn(configFile);
        when(polarisConfigFileService.getConfigFile(any(), any(), any())).thenReturn(configFile);
        MetaData metaData = MetaData.builder().id(MOCK_ID).path(MOCK_PATH).appName(MOCK_APP_NAME).build();
        polarisDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.DELETE);
        polarisDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.REFRESH);
        polarisDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.MYSELF);
        polarisDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.CREATE);
        verify(polarisConfigFilePublishService, times(8)).createConfigFile(any(DefaultConfigFileMetadata.class), any(String.class));
        verify(polarisConfigFilePublishService, times(8)).releaseConfigFile(any(DefaultConfigFileMetadata.class));
    }

    @Test
    public void testOnRuleChanged() throws PolarisException {
        when(polarisProperties.getNamespace()).thenReturn(PolarisPathConstants.NAMESPACE);
        when(polarisProperties.getFileGroup()).thenReturn(PolarisPathConstants.FILE_GROUP);
        when(configFile.hasContent()).thenReturn(false);
        when(configFile.getContent()).thenReturn(null);
        when(polarisConfigFileService.getConfigFile(any())).thenReturn(configFile);
        RuleData ruleData = RuleData.builder().id(MOCK_ID).name(MOCK_NAME).pluginName(MOCK_PLUGIN_NAME).selectorId(MOCK_SELECTOR_ID).build();
        when(polarisConfigFileService.getConfigFile(any(), any(), any())).thenReturn(configFile);
        polarisDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.DELETE);
        polarisDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.REFRESH);
        polarisDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.MYSELF);
        polarisDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.CREATE);
        verify(polarisConfigFilePublishService, times(8)).createConfigFile(any(DefaultConfigFileMetadata.class), any(String.class));
        verify(polarisConfigFilePublishService, times(8)).releaseConfigFile(any(DefaultConfigFileMetadata.class));
    }
}
