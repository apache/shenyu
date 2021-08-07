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

package org.apache.shenyu.admin.listener.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.kv.model.GetValue;
import com.google.common.collect.ImmutableList;
import lombok.SneakyThrows;
import org.apache.shenyu.common.constant.ConsulConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The testCase for {@link ConsulDataChangedListener}.
 */
@RunWith(MockitoJUnitRunner.class)
public class ConsulDataChangedListenerTest {

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
    private ConsulClient consulClient;

    @InjectMocks
    private ConsulDataChangedListener consulDataChangedListener;

    @Test
    @SneakyThrows
    public void testOnAppAuthChanged() {
        final AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(MOCK_APP_KEY);
        appAuthData.setAppSecret(MOCK_APP_SECRET);
        Response<GetValue> response = mock(Response.class);
        GetValue getValueModel = mock(GetValue.class);
        String config = "{\"divide\":{\"appKey\":\"appKey\",\"appSecret\":\"appSecret\",\"open\":true}}";
        when(consulClient.getKVValue(anyString())).thenReturn(response);
        when(response.getValue()).thenReturn(getValueModel);
        when(getValueModel.getDecodedValue()).thenReturn(config);

        consulDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.DELETE);
        consulDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.REFRESH);
        consulDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.MYSELF);
        consulDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.CREATE);
        verify(consulClient, times(4)).setKVValue(any(String.class), any(String.class));

        getValueModel.setValue(ConsulConstants.EMPTY_CONFIG_DEFAULT_VALUE);
        consulDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.DELETE);
        consulDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.REFRESH);
        consulDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.MYSELF);
        consulDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.CREATE);
        verify(consulClient, times(8)).setKVValue(any(String.class), any(String.class));
    }

    @Test
    @SneakyThrows
    public void testOnPluginChanged() {
        final PluginData pluginData = new PluginData();
        pluginData.setId(MOCK_ID);
        pluginData.setName(MOCK_NAME);
        pluginData.setConfig(MOCK_CONFIG);
        Response<GetValue> response = mock(Response.class);
        GetValue getValueModel = mock(GetValue.class);
        String config = "{\"divide\":{\"id\":\"id\",\"name\":\"name\",\"enabled\":true}}";
        when(consulClient.getKVValue(anyString())).thenReturn(response);
        when(response.getValue()).thenReturn(getValueModel);
        when(getValueModel.getDecodedValue()).thenReturn(config);

        consulDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.DELETE);
        consulDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.REFRESH);
        consulDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.MYSELF);
        consulDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.CREATE);
        verify(consulClient, times(4)).setKVValue(any(String.class), any(String.class));

        getValueModel.setValue(ConsulConstants.EMPTY_CONFIG_DEFAULT_VALUE);
        consulDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.DELETE);
        consulDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.REFRESH);
        consulDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.MYSELF);
        consulDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.CREATE);
        verify(consulClient, times(8)).setKVValue(any(String.class), any(String.class));
    }

    @Test
    @SneakyThrows
    public void testOnSelectorChanged() {
        final SelectorData selectorData = new SelectorData();
        selectorData.setId(MOCK_ID);
        selectorData.setName(MOCK_NAME);
        selectorData.setPluginName(MOCK_PLUGIN_NAME);

        Response<GetValue> response = mock(Response.class);
        GetValue getValueModel = mock(GetValue.class);
        String config = "{\"divide\":[{\"id\":\"id\",\"name\":\"name\",\"enabled\":true}]}";
        when(consulClient.getKVValue(anyString())).thenReturn(response);
        when(response.getValue()).thenReturn(getValueModel);
        when(getValueModel.getDecodedValue()).thenReturn(config);

        consulDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.DELETE);
        consulDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.REFRESH);
        consulDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.MYSELF);
        consulDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.CREATE);
        verify(consulClient, times(4)).setKVValue(any(String.class), any(String.class));

        getValueModel.setValue(ConsulConstants.EMPTY_CONFIG_DEFAULT_VALUE);
        consulDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.DELETE);
        consulDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.REFRESH);
        consulDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.MYSELF);
        consulDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.CREATE);
        verify(consulClient, times(8)).setKVValue(any(String.class), any(String.class));
    }

    @Test
    @SneakyThrows
    public void testOnMetaDataChanged() {
        final MetaData metaData = new MetaData();
        metaData.setId(MOCK_ID);
        metaData.setPath(MOCK_PATH);
        metaData.setAppName(MOCK_APP_NAME);
        Response<GetValue> response = mock(Response.class);
        GetValue getValueModel = mock(GetValue.class);
        String config = "{\"divide\":{\"id\":\"id\",\"appName\":\"appName\",\"enabled\":true}}";
        when(consulClient.getKVValue(anyString())).thenReturn(response);
        when(response.getValue()).thenReturn(getValueModel);
        when(getValueModel.getDecodedValue()).thenReturn(config);

        consulDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.DELETE);
        consulDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.REFRESH);
        consulDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.MYSELF);
        consulDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.CREATE);
        verify(consulClient, times(4)).setKVValue(any(String.class), any(String.class));

        getValueModel.setValue(ConsulConstants.EMPTY_CONFIG_DEFAULT_VALUE);
        consulDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.DELETE);
        consulDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.REFRESH);
        consulDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.MYSELF);
        consulDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.CREATE);
        verify(consulClient, times(8)).setKVValue(any(String.class), any(String.class));
    }

    @Test
    @SneakyThrows
    public void testOnRuleChanged() {
        final RuleData ruleData = new RuleData();
        ruleData.setId(MOCK_ID);
        ruleData.setName(MOCK_NAME);
        ruleData.setPluginName(MOCK_PLUGIN_NAME);
        ruleData.setSelectorId(MOCK_SELECTOR_ID);
        Response<GetValue> response = mock(Response.class);
        GetValue getValueModel = mock(GetValue.class);
        String config = "{\"divide\":[{\"id\":\"id\",\"appName\":\"appName\",\"enabled\":true}]}";
        when(consulClient.getKVValue(anyString())).thenReturn(response);
        when(response.getValue()).thenReturn(getValueModel);
        when(getValueModel.getDecodedValue()).thenReturn(config);

        consulDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.DELETE);
        consulDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.REFRESH);
        consulDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.MYSELF);
        consulDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.CREATE);
        verify(consulClient, times(4)).setKVValue(any(String.class), any(String.class));

        getValueModel.setValue(ConsulConstants.EMPTY_CONFIG_DEFAULT_VALUE);
        consulDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.DELETE);
        consulDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.REFRESH);
        consulDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.MYSELF);
        consulDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.CREATE);
        verify(consulClient, times(8)).setKVValue(any(String.class), any(String.class));
    }
}
