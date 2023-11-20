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

/**
 * The testCase for {@link EtcdDataDataChangedListener}.
 */
@ExtendWith(MockitoExtension.class)
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
        AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_KEY).appSecret(MOCK_APP_SECRET).build();
        etcdDataDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.CREATE);
        etcdDataDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.UPDATE);
        etcdDataDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.DELETE);
        verify(etcdClient, times(2)).put(any(String.class), any(String.class));
        verify(etcdClient, times(1)).delete(any(String.class));
    }

    /**
     * test case onMetaDataChanged event.
     */
    @Test
    public void testOnMetaDataChanged() {
        MetaData metaData = MetaData.builder().id(MOCK_ID).path(MOCK_PATH).appName(MOCK_APP_NAME).build();
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
        PluginData pluginData = PluginData.builder().id(MOCK_ID).name(MOCK_NAME).config(MOCK_CONFIG).build();
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
        SelectorData selectorData = SelectorData.builder().id(MOCK_ID).name(MOCK_NAME).pluginName(MOCK_PLUGIN_NAME).build();
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
        RuleData ruleData = RuleData.builder()
                .id(MOCK_ID)
                .name(MOCK_NAME)
                .pluginName(MOCK_PLUGIN_NAME)
                .selectorId(MOCK_SELECTOR_ID)
                .build();
        etcdDataDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.CREATE);
        etcdDataDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.UPDATE);
        etcdDataDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.REFRESH);
        etcdDataDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.DELETE);
        verify(etcdClient, times(3)).put(any(String.class), any(String.class));
        verify(etcdClient, times(1)).delete(any(String.class));
        verify(etcdClient, times(1)).deleteEtcdPathRecursive(any(String.class));
    }
}
