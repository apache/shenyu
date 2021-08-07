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

package org.apache.shenyu.admin.listener.etcd;

import com.google.common.collect.ImmutableList;
import lombok.SneakyThrows;
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

/**
 * The testCase for {@link EtcdDataDataChangedListener}.
 */
@RunWith(MockitoJUnitRunner.class)
public class EtcdDataDataChangedListenerTest {

    private static final String MOCK_APP_KEY = "MOCK_APP_KEY";

    private static final String MOCK_APP_SECRET = "MOCK_APP_SECRET";

    private static final String MOCK_ID = "MOCK_ID";

    private static final String MOCK_PATH = "MOCK_PATH";

    private static final String MOCK_APP_NAME = "MOCK_APP_NAME";

    private static final String MOCK_NAME = "MOCK_NAME";

    private static final String MOCK_CONFIG = "MOCK_CONFIG";

    private static final String MOCK_PLUGIN_NAME = "MOCK_PLUGIN_NAME";

    private static final String MOCK_SELECTOR_ID = "MOCK_SELECTOR_ID";

    @InjectMocks
    private EtcdDataDataChangedListener etcdDataDataChangedListener;

    @Mock
    private EtcdClient etcdClient;

    /**
     * test case onAppAuthChanged event.
     */
    @Test
    public void testOnAppAuthChanged() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(MOCK_APP_KEY);
        appAuthData.setAppSecret(MOCK_APP_SECRET);
        etcdDataDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.CREATE);
        etcdDataDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.UPDATE);
        etcdDataDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.DELETE);
        verify(etcdClient, times(2)).put(any(String.class), any(String.class));
        verify(etcdClient, times(1)).delete(any(String.class));
    }

    /**
     * test case onMetaDataChanged event.
     */
    @SneakyThrows
    @Test
    public void testOnMetaDataChanged() {
        MetaData metaData = new MetaData();
        metaData.setId(MOCK_ID);
        metaData.setPath(MOCK_PATH);
        metaData.setAppName(MOCK_APP_NAME);
        etcdDataDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.CREATE);
        etcdDataDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.UPDATE);
        etcdDataDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.DELETE);
        verify(etcdClient, times(2)).put(any(String.class), any(String.class));
        verify(etcdClient, times(1)).delete(any(String.class));
    }

    /**
     * test case onPluginChanged event.
     */
    @Test
    public void testOnPluginChanged() {
        PluginData pluginData = new PluginData();
        pluginData.setId(MOCK_ID);
        pluginData.setName(MOCK_NAME);
        pluginData.setConfig(MOCK_CONFIG);
        etcdDataDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.CREATE);
        etcdDataDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.UPDATE);
        etcdDataDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.DELETE);
        verify(etcdClient, times(2)).put(any(String.class), any(String.class));
        verify(etcdClient, times(3)).deleteEtcdPathRecursive(any(String.class));
    }

    /**
     * test case onSelectorChanged event.
     */
    @Test
    public void testOnSelectorChanged() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId(MOCK_ID);
        selectorData.setName(MOCK_NAME);
        selectorData.setPluginName(MOCK_PLUGIN_NAME);
        etcdDataDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.CREATE);
        etcdDataDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.UPDATE);
        etcdDataDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.REFRESH);
        etcdDataDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.DELETE);
        verify(etcdClient, times(3)).put(any(String.class), any(String.class));
        verify(etcdClient, times(1)).delete(any(String.class));
        verify(etcdClient, times(1)).deleteEtcdPathRecursive(any(String.class));
    }

    /**
     * test case onRuleChanged event.
     */
    @Test
    public void testOnRuleChanged() {
        RuleData ruleData = new RuleData();
        ruleData.setId(MOCK_ID);
        ruleData.setName(MOCK_NAME);
        ruleData.setPluginName(MOCK_PLUGIN_NAME);
        ruleData.setSelectorId(MOCK_SELECTOR_ID);
        etcdDataDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.CREATE);
        etcdDataDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.UPDATE);
        etcdDataDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.REFRESH);
        etcdDataDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.DELETE);
        verify(etcdClient, times(3)).put(any(String.class), any(String.class));
        verify(etcdClient, times(1)).delete(any(String.class));
        verify(etcdClient, times(1)).deleteEtcdPathRecursive(any(String.class));
    }
}
