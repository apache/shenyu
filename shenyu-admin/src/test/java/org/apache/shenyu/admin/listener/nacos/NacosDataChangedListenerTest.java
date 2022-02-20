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

package org.apache.shenyu.admin.listener.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.exception.NacosException;
import com.google.common.collect.ImmutableList;
import org.apache.shenyu.common.constant.NacosPathConstants;
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
 * The testCase for {@link NacosDataChangedListener}.
 */
@ExtendWith(MockitoExtension.class)
public class NacosDataChangedListenerTest {

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
    private ConfigService configService;

    @InjectMocks
    private NacosDataChangedListener nacosDataChangedListener;

    @Test
    public void testOnAppAuthChanged() throws NacosException {
        String config = "{\"divide\":{\"appKey\":\"appKey\",\"appSecret\":\"appSecret\",\"open\":true}}";
        AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_KEY).appSecret(MOCK_APP_SECRET).build();

        when(configService.getConfig(NacosPathConstants.AUTH_DATA_ID, NacosPathConstants.GROUP,
                NacosPathConstants.DEFAULT_TIME_OUT)).thenReturn(config);
        nacosDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.DELETE);
        nacosDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.REFRESH);
        nacosDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.MYSELF);
        nacosDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.CREATE);
        verify(configService, times(4)).publishConfig(any(String.class), any(String.class), any(String.class));

        when(configService.getConfig(NacosPathConstants.AUTH_DATA_ID, NacosPathConstants.GROUP,
                NacosPathConstants.DEFAULT_TIME_OUT)).thenReturn(NacosPathConstants.EMPTY_CONFIG_DEFAULT_VALUE);
        nacosDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.DELETE);
        nacosDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.REFRESH);
        nacosDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.MYSELF);
        nacosDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.CREATE);
        verify(configService, times(8)).publishConfig(any(String.class), any(String.class), any(String.class));
    }

    @Test
    public void testOnPluginChanged() throws NacosException {
        String config = "{\"divide\":{\"id\":\"id\",\"name\":\"name\",\"enabled\":true}}";
        PluginData pluginData = PluginData.builder().id(MOCK_ID).name(MOCK_NAME).config(MOCK_CONFIG).build();

        when(configService.getConfig(NacosPathConstants.PLUGIN_DATA_ID, NacosPathConstants.GROUP,
                NacosPathConstants.DEFAULT_TIME_OUT)).thenReturn(config);
        nacosDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.DELETE);
        nacosDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.REFRESH);
        nacosDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.MYSELF);
        nacosDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.CREATE);
        verify(configService, times(4)).publishConfig(any(String.class), any(String.class), any(String.class));

        when(configService.getConfig(NacosPathConstants.PLUGIN_DATA_ID, NacosPathConstants.GROUP,
                NacosPathConstants.DEFAULT_TIME_OUT)).thenReturn(NacosPathConstants.EMPTY_CONFIG_DEFAULT_VALUE);
        nacosDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.DELETE);
        nacosDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.REFRESH);
        nacosDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.MYSELF);
        nacosDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.CREATE);
        verify(configService, times(8)).publishConfig(any(String.class), any(String.class), any(String.class));
    }

    @Test
    public void testOnSelectorChanged() throws NacosException {
        String config = "{\"divide\":[{\"id\":\"id\",\"name\":\"name\",\"enabled\":true}]}";
        SelectorData selectorData = SelectorData.builder().id(MOCK_ID).name(MOCK_NAME).pluginName(MOCK_PLUGIN_NAME).build();

        when(configService.getConfig(NacosPathConstants.SELECTOR_DATA_ID, NacosPathConstants.GROUP,
                NacosPathConstants.DEFAULT_TIME_OUT)).thenReturn(config);
        nacosDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.DELETE);
        nacosDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.REFRESH);
        nacosDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.MYSELF);
        nacosDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.CREATE);
        verify(configService, times(4)).publishConfig(any(String.class), any(String.class), any(String.class));

        when(configService.getConfig(NacosPathConstants.SELECTOR_DATA_ID, NacosPathConstants.GROUP,
                NacosPathConstants.DEFAULT_TIME_OUT)).thenReturn(NacosPathConstants.EMPTY_CONFIG_DEFAULT_VALUE);
        nacosDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.DELETE);
        nacosDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.REFRESH);
        nacosDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.MYSELF);
        nacosDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.CREATE);
        verify(configService, times(8)).publishConfig(any(String.class), any(String.class), any(String.class));
    }

    @Test
    public void testOnMetaDataChanged() throws NacosException {
        String config = "{\"divide\":{\"id\":\"id\",\"appName\":\"appName\",\"enabled\":true}}";
        MetaData metaData = MetaData.builder().id(MOCK_ID).path(MOCK_PATH).appName(MOCK_APP_NAME).build();

        when(configService.getConfig(NacosPathConstants.META_DATA_ID, NacosPathConstants.GROUP,
                NacosPathConstants.DEFAULT_TIME_OUT)).thenReturn(config);
        nacosDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.DELETE);
        nacosDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.REFRESH);
        nacosDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.MYSELF);
        nacosDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.CREATE);
        verify(configService, times(4)).publishConfig(any(String.class), any(String.class), any(String.class));

        when(configService.getConfig(NacosPathConstants.META_DATA_ID, NacosPathConstants.GROUP,
                NacosPathConstants.DEFAULT_TIME_OUT)).thenReturn(NacosPathConstants.EMPTY_CONFIG_DEFAULT_VALUE);
        nacosDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.DELETE);
        nacosDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.REFRESH);
        nacosDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.MYSELF);
        nacosDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.CREATE);
        verify(configService, times(8)).publishConfig(any(String.class), any(String.class), any(String.class));
    }

    @Test
    public void testOnRuleChanged() throws NacosException {
        String config = "{\"divide\":[{\"id\":\"id\",\"appName\":\"appName\",\"enabled\":true}]}";
        RuleData ruleData = RuleData.builder()
                .id(MOCK_ID)
                .name(MOCK_NAME)
                .pluginName(MOCK_PLUGIN_NAME)
                .selectorId(MOCK_SELECTOR_ID)
                .build();

        when(configService.getConfig(NacosPathConstants.RULE_DATA_ID, NacosPathConstants.GROUP,
                NacosPathConstants.DEFAULT_TIME_OUT)).thenReturn(config);
        nacosDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.DELETE);
        nacosDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.REFRESH);
        nacosDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.MYSELF);
        nacosDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.CREATE);
        verify(configService, times(4)).publishConfig(any(String.class), any(String.class), any(String.class));

        when(configService.getConfig(NacosPathConstants.RULE_DATA_ID, NacosPathConstants.GROUP,
                NacosPathConstants.DEFAULT_TIME_OUT)).thenReturn(NacosPathConstants.EMPTY_CONFIG_DEFAULT_VALUE);
        nacosDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.DELETE);
        nacosDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.REFRESH);
        nacosDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.MYSELF);
        nacosDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.CREATE);
        verify(configService, times(8)).publishConfig(any(String.class), any(String.class), any(String.class));
    }
}
