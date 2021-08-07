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
import com.google.common.collect.ImmutableList;
import lombok.SneakyThrows;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The testCase for {@link NacosDataChangedListener}.
 */
@RunWith(MockitoJUnitRunner.class)
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
    @SneakyThrows
    public void testOnAppAuthChanged() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(MOCK_APP_KEY);
        appAuthData.setAppSecret(MOCK_APP_SECRET);
        String config = "{\"divide\":{\"appKey\":\"appKey\",\"appSecret\":\"appSecret\",\"open\":true}}";
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
    @SneakyThrows
    public void testOnPluginChanged() {
        PluginData pluginData = new PluginData();
        pluginData.setId(MOCK_ID);
        pluginData.setName(MOCK_NAME);
        pluginData.setConfig(MOCK_CONFIG);
        String config = "{\"divide\":{\"id\":\"id\",\"name\":\"name\",\"enabled\":true}}";
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
    @SneakyThrows
    public void testOnSelectorChanged() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId(MOCK_ID);
        selectorData.setName(MOCK_NAME);
        selectorData.setPluginName(MOCK_PLUGIN_NAME);
        String config = "{\"divide\":[{\"id\":\"id\",\"name\":\"name\",\"enabled\":true}]}";
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
    @SneakyThrows
    public void testOnMetaDataChanged() {
        MetaData metaData = new MetaData();
        metaData.setId(MOCK_ID);
        metaData.setPath(MOCK_PATH);
        metaData.setAppName(MOCK_APP_NAME);
        String config = "{\"divide\":{\"id\":\"id\",\"appName\":\"appName\",\"enabled\":true}}";
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
    @SneakyThrows
    public void testOnRuleChanged() {
        RuleData ruleData = new RuleData();
        ruleData.setId(MOCK_ID);
        ruleData.setName(MOCK_NAME);
        ruleData.setPluginName(MOCK_PLUGIN_NAME);
        ruleData.setSelectorId(MOCK_SELECTOR_ID);
        String config = "{\"divide\":[{\"id\":\"id\",\"appName\":\"appName\",\"enabled\":true}]}";
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
