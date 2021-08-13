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
    public void testOnAppAuthChanged() {
        String config = "{\"divide\":{\"appKey\":\"appKey\",\"appSecret\":\"appSecret\",\"open\":true}}";
        final AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_KEY).appSecret(MOCK_APP_SECRET).build();

        Response<GetValue> response = mock(Response.class);
        GetValue getValueModel = mock(GetValue.class);
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
    public void testOnPluginChanged() {
        String config = "{\"divide\":{\"id\":\"id\",\"name\":\"name\",\"enabled\":true}}";
        final PluginData pluginData = PluginData.builder().id(MOCK_ID).name(MOCK_NAME).config(MOCK_CONFIG).build();
        Response<GetValue> response = mock(Response.class);
        GetValue getValueModel = mock(GetValue.class);
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
    public void testOnSelectorChanged() {
        String config = "{\"divide\":[{\"id\":\"id\",\"name\":\"name\",\"enabled\":true}]}";
        final SelectorData selectorData = SelectorData.builder().id(MOCK_ID).name(MOCK_NAME).pluginName(MOCK_PLUGIN_NAME).build();

        Response<GetValue> response = mock(Response.class);
        GetValue getValueModel = mock(GetValue.class);
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
    public void testOnMetaDataChanged() {
        String config = "{\"divide\":{\"id\":\"id\",\"appName\":\"appName\",\"enabled\":true}}";
        final MetaData metaData = MetaData.builder().id(MOCK_ID).path(MOCK_PATH).appName(MOCK_APP_NAME).build();

        Response<GetValue> response = mock(Response.class);
        GetValue getValueModel = mock(GetValue.class);
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
    public void testOnRuleChanged() {
        String config = "{\"divide\":[{\"id\":\"id\",\"appName\":\"appName\",\"enabled\":true}]}";
        final RuleData ruleData = RuleData.builder()
                .id(MOCK_ID)
                .name(MOCK_NAME)
                .pluginName(MOCK_PLUGIN_NAME)
                .selectorId(MOCK_SELECTOR_ID)
                .build();

        Response<GetValue> response = mock(Response.class);
        GetValue getValueModel = mock(GetValue.class);
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
