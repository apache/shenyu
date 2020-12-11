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

package org.dromara.soul.admin.listener.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.exception.NacosException;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * The TestCase for NacosDataChangedListener.
 *
 * @author cottom
 */
@RunWith(MockitoJUnitRunner.class)
public final class NacosDataChangedListenerTest {
    private static final String GROUP = "DEFAULT_GROUP";

    private static final String PLUGIN_DATA_ID = "soul.plugin.json";

    private static final String SELECTOR_DATA_ID = "soul.selector.json";

    private static final String RULE_DATA_ID = "soul.rule.json";

    private static final String AUTH_DATA_ID = "soul.auth.json";

    private static final String META_DATA_ID = "soul.meta.json";

    private ConfigService configService;

    private NacosDataChangedListener nacosDataChangedListener;

    @Before
    public void setUp() {
        configService = new NacosMockConfigService();
        nacosDataChangedListener = new NacosDataChangedListener(configService);
    }

    @Test
    public void testOnAppAuthChanged() throws NacosException {
        String mockAppKey = "MOCK_APP_KEY";
        String mockAppKey2 = "MOCK_APP_KEY2";
        String mockAppSecret = "MOCK_APP_SECRET";
        AppAuthData appAuthData =
                AppAuthData.builder().appKey(mockAppKey).appSecret(mockAppSecret).enabled(true).build();
        AppAuthData appAuthData2 =
                AppAuthData.builder().appKey(mockAppKey2).appSecret(mockAppSecret).enabled(true).build();
        nacosDataChangedListener.onAppAuthChanged(
                ImmutableList.of(appAuthData, appAuthData2), DataEventTypeEnum.CREATE);
        Assert.assertEquals(
                configService.getConfig(AUTH_DATA_ID, GROUP, 100),
                GsonUtils.getInstance()
                        .toJson(ImmutableMap.of(mockAppKey2, appAuthData2, mockAppKey, appAuthData)));
        nacosDataChangedListener.onAppAuthChanged(
                ImmutableList.of(appAuthData), DataEventTypeEnum.DELETE);
        Assert.assertEquals(
                configService.getConfig(AUTH_DATA_ID, GROUP, 100),
                GsonUtils.getInstance().toJson(ImmutableMap.of(mockAppKey2, appAuthData2)));
        nacosDataChangedListener.onAppAuthChanged(
                ImmutableList.of(appAuthData), DataEventTypeEnum.REFRESH);
        Assert.assertEquals(
                configService.getConfig(AUTH_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(ImmutableMap.of(mockAppKey, appAuthData)));
    }

    @Test
    public void testOnPluginChanged() throws NacosException {
        String pluginName1 = "PLUGIN_NAME_1";
        String pluginName2 = "PLUGIN_NAME_2";
        PluginData pluginData1 =
                PluginData.builder().name(pluginName1).id("plugin_1").config("config_1").build();
        PluginData pluginData2 =
                PluginData.builder().name(pluginName2).id("plugin_2").config("config_2").build();

        nacosDataChangedListener.onPluginChanged(
                ImmutableList.of(pluginData1, pluginData2), DataEventTypeEnum.CREATE);
        Assert.assertEquals(
                configService.getConfig(PLUGIN_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(ImmutableMap.of(pluginName2, pluginData2, pluginName1, pluginData1)));
        nacosDataChangedListener.onPluginChanged(
                ImmutableList.of(pluginData1), DataEventTypeEnum.DELETE);
        Assert.assertEquals(
                configService.getConfig(PLUGIN_DATA_ID, GROUP, 1),
                GsonUtils.getInstance().toJson(ImmutableMap.of(pluginName2, pluginData2)));
        nacosDataChangedListener.onPluginChanged(
                ImmutableList.of(pluginData1), DataEventTypeEnum.REFRESH);
        Assert.assertEquals(
                configService.getConfig(PLUGIN_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(ImmutableMap.of(pluginName1, pluginData1)));
    }

    @Test
    public void testOnSelectorChanged() throws NacosException {
        String selectorDataPluginName1 = "SELECTOR_DATA_1";
        String selectorDataPluginName2 = "SELECTOR_DATA_2";
        SelectorData selectorData1 =
                SelectorData.builder()
                        .pluginName(selectorDataPluginName1)
                        .id("select_1")
                        .name("SELECT_DATA_NAME_1")
                        .build();
        SelectorData selectorData2 =
                SelectorData.builder()
                        .pluginName(selectorDataPluginName2)
                        .id("select_2")
                        .name("SELECT_DATA_NAME_2")
                        .build();

        nacosDataChangedListener.onSelectorChanged(
                ImmutableList.of(selectorData1, selectorData2), DataEventTypeEnum.CREATE);
        Assert.assertEquals(
                configService.getConfig(SELECTOR_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(
                                ImmutableMap.of(
                                        selectorDataPluginName2,
                                        ImmutableList.of(selectorData2),
                                        selectorDataPluginName1,
                                        ImmutableList.of(selectorData1))));
        nacosDataChangedListener.onSelectorChanged(
                ImmutableList.of(selectorData1), DataEventTypeEnum.DELETE);
        Assert.assertEquals(
                configService.getConfig(SELECTOR_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(
                                ImmutableMap.of(
                                        selectorDataPluginName2,
                                        ImmutableList.of(selectorData2),
                                        selectorDataPluginName1,
                                        ImmutableList.of())));
        nacosDataChangedListener.onSelectorChanged(
                ImmutableList.of(selectorData1), DataEventTypeEnum.REFRESH);
        Assert.assertEquals(
                configService.getConfig(SELECTOR_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(ImmutableMap.of(selectorDataPluginName1, ImmutableList.of())));
    }

    @Test
    public void testOnMetaDataChanged() throws NacosException {
        String metadataPath1 = "METADATA_PATH_1";
        String metadataPath2 = "METADATA_PATH_2";
        MetaData metaData1 = MetaData.builder().path(metadataPath1).id("meta_1").build();
        MetaData metaData2 = MetaData.builder().path(metadataPath2).id("meta_2").build();

        nacosDataChangedListener.onMetaDataChanged(
                ImmutableList.of(metaData1, metaData2), DataEventTypeEnum.CREATE);
        Assert.assertEquals(
                configService.getConfig(META_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(ImmutableMap.of(metadataPath1, metaData1, metadataPath2, metaData2)));
        nacosDataChangedListener.onMetaDataChanged(
                ImmutableList.of(metaData1), DataEventTypeEnum.DELETE);
        Assert.assertEquals(
                configService.getConfig(META_DATA_ID, GROUP, 1),
                GsonUtils.getInstance().toJson(ImmutableMap.of(metadataPath2, metaData2)));
        nacosDataChangedListener.onMetaDataChanged(
                ImmutableList.of(metaData1), DataEventTypeEnum.REFRESH);
        Assert.assertEquals(
                configService.getConfig(META_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(ImmutableMap.of(metadataPath1, metaData1)));
    }

    @Test
    public void testOnRuleChanged() throws NacosException {
        String ruleDataId1 = "RULE_DATA_1";
        String ruleDataId2 = "RULE_DATA_2";
        String selectorId1 = "ID_1";
        String selectorId2 = "ID_2";
        RuleData ruleData1 = RuleData.builder().selectorId(selectorId1).id(ruleDataId1).build();
        RuleData ruleData2 = RuleData.builder().selectorId(selectorId2).id(ruleDataId2).build();

        nacosDataChangedListener.onRuleChanged(
                ImmutableList.of(ruleData1, ruleData2), DataEventTypeEnum.CREATE);
        Assert.assertEquals(
                configService.getConfig(RULE_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(
                                ImmutableMap.of(
                                        selectorId2,
                                        ImmutableList.of(ruleData2),
                                        selectorId1,
                                        ImmutableList.of(ruleData1))));
        nacosDataChangedListener.onRuleChanged(ImmutableList.of(ruleData1), DataEventTypeEnum.DELETE);
        Assert.assertEquals(
                configService.getConfig(RULE_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(
                                ImmutableMap.of(
                                        selectorId2, ImmutableList.of(ruleData2), selectorId1, ImmutableList.of())));
        nacosDataChangedListener.onRuleChanged(ImmutableList.of(ruleData1), DataEventTypeEnum.REFRESH);
        Assert.assertEquals(
                configService.getConfig(RULE_DATA_ID, GROUP, 1),
                GsonUtils.getInstance().toJson(ImmutableMap.of(selectorId1, ImmutableList.of())));
    }
}
