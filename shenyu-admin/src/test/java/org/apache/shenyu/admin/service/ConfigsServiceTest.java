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

package org.apache.shenyu.admin.service;

import com.google.common.collect.Lists;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.impl.ConfigsServiceImpl;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.common.constant.ExportImportConstants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Test cases for ConfigsService.
 */
@ExtendWith(MockitoExtension.class)
public final class ConfigsServiceTest {

    private ConfigsServiceImpl configsService;

    @Mock
    private SelectorService selectorService;

    @Mock
    private PluginService pluginService;

    @Mock
    private NamespacePluginService namespacePluginService;

    @Mock
    private PluginHandleService pluginHandleService;

    @Mock
    private RuleService ruleService;

    @Mock
    private MetaDataService metaDataService;

    @Mock
    private AppAuthService appAuthService;

    @Mock
    private ShenyuDictService shenyuDictService;

    @Mock
    private ProxySelectorService proxySelectorService;

    @Mock
    private DiscoveryService discoveryService;

    @Mock
    private DiscoveryUpstreamService discoveryUpstreamService;

    @BeforeEach
    public void setUp() {
        configsService = new ConfigsServiceImpl(appAuthService, pluginService, namespacePluginService, pluginHandleService, selectorService, ruleService,
                metaDataService, shenyuDictService, proxySelectorService, discoveryService, discoveryUpstreamService, Collections.emptyList());
    }

    @Test
    public void testConfigsExport() {
        ShenyuAdminResult result = this.configsService.configsExport();
        assertThat(result.getCode(), is(CommonErrorCode.SUCCESSFUL));
    }

    @Test
    public void testConfigsImport() {

        when(this.appAuthService.importData(any())).thenReturn(
                ConfigImportResult.success(1));

        when(this.metaDataService.importData(any())).thenReturn(
                ConfigImportResult.success(1));

        when(this.pluginService.importData(any(), any())).thenReturn(
                ConfigImportResult.success(1));

        when(this.selectorService.importData(any())).thenReturn(
                ConfigImportResult.success(1));

        when(this.ruleService.importData(any())).thenReturn(
                ConfigImportResult.success(1));

        when(this.shenyuDictService.importData(any())).thenReturn(
                ConfigImportResult.success(1));

        when(this.discoveryService.importData(any())).thenReturn(
                ConfigImportResult.success(1));

        when(this.discoveryUpstreamService.importData(any())).thenReturn(
                ConfigImportResult.success(1));

        when(this.proxySelectorService.importData(any())).thenReturn(
                ConfigImportResult.success(1));

        ShenyuAdminResult result = this.configsService.configsImport(buildImportData());
        assertThat(result.getCode(), is(CommonErrorCode.SUCCESSFUL));
        Map<String, Object> resultData = (Map<String, Object>) result.getData();

        assertThat(resultData.get(ExportImportConstants.AUTH_IMPORT_SUCCESS_COUNT), is(1));
        assertThat(resultData.get(ExportImportConstants.META_IMPORT_SUCCESS_COUNT), is(1));
        assertThat(resultData.get(ExportImportConstants.DICT_IMPORT_SUCCESS_COUNT), is(1));
        assertThat(resultData.get(ExportImportConstants.PLUGIN_IMPORT_SUCCESS_COUNT), is(1));
        assertThat(resultData.get(ExportImportConstants.SELECTOR_IMPORT_SUCCESS_COUNT), is(1));
        assertThat(resultData.get(ExportImportConstants.RULE_IMPORT_SUCCESS_COUNT), is(1));
        assertThat(resultData.get(ExportImportConstants.PROXY_SELECTOR_IMPORT_SUCCESS_COUNT), is(1));
        assertThat(resultData.get(ExportImportConstants.DISCOVERY_UPSTREAM_IMPORT_SUCCESS_COUNT), is(1));
        assertThat(resultData.get(ExportImportConstants.DISCOVERY_IMPORT_SUCCESS_COUNT), is(1));
    }

    private byte[] buildImportData() {
        List<ZipUtil.ZipItem> zipItemList = Lists.newArrayList();

        zipItemList.add(buildAuthData());
        zipItemList.add(buildMetadata());
        zipItemList.add(buildPluginData());
        zipItemList.add(buildSelectorData());
        zipItemList.add(buildRuleData());
        zipItemList.add(buildDictData());
        zipItemList.add(buildDiscoveryData());
        zipItemList.add(buildDiscoveryUpstreamData());
        zipItemList.add(buildProxySelectorData());

        return ZipUtil.zip(zipItemList);
    }

    private ZipUtil.ZipItem buildAuthData() {
        return new ZipUtil.ZipItem(ExportImportConstants.AUTH_JSON, "[\n"
                + "  {\n"
                + "    \"id\": \"1766646853203050496\",\n"
                + "    \"appKey\": \"2A405EA90F5D4D5084A9B366D4D17CAB\",\n"
                + "    \"appSecret\": \"94C4328DA39C4CBDBCD5A78209BD34B9\",\n"
                + "    \"userId\": \"1\",\n"
                + "    \"phone\": \"17610178109\",\n"
                + "    \"extInfo\": \"1213231\",\n"
                + "    \"open\": true,\n"
                + "    \"enabled\": true,\n"
                + "    \"authParamList\": [{ \"appName\": \"app\", \"appParam\": \"123\" }],\n"
                + "    \"authPathList\": [\n"
                + "      {\n"
                + "        \"id\": \"1766646853219827712\",\n"
                + "        \"appName\": \"app\",\n"
                + "        \"path\": \"/auth/info\",\n"
                + "        \"enabled\": true\n"
                + "      }\n"
                + "    ],\n"
                + "    \"dateUpdated\": \"2024-03-10 02:06:47\"\n"
                + "  }\n"
                + "]\n");
    }

    private ZipUtil.ZipItem buildMetadata() {
        return new ZipUtil.ZipItem(ExportImportConstants.META_JSON, "[\n"
                + "  {\n"
                + "    \"appName\": \"app\",\n"
                + "    \"path\": \"/auth/info\",\n"
                + "    \"pathDesc\": \"test\",\n"
                + "    \"rpcType\": \"http\",\n"
                + "    \"serviceName\": \"user-center-server\",\n"
                + "    \"methodName\": \"test\",\n"
                + "    \"parameterTypes\": \"String\",\n"
                + "    \"rpcExt\": \"String\",\n"
                + "    \"id\": \"1766646853299519488\",\n"
                + "    \"dateCreated\": \"2024-03-10 10:06:47\",\n"
                + "    \"dateUpdated\": \"2024-03-10 10:06:47\",\n"
                + "    \"enabled\": true\n"
                + "  }\n"
                + "]\n");
    }

    private ZipUtil.ZipItem buildPluginData() {
        return new ZipUtil.ZipItem(ExportImportConstants.PLUGIN_JSON, "[\n"
                + "  {\n"
                + "    \"id\": \"1\",\n"
                + "    \"role\": \"Authentication\",\n"
                + "    \"name\": \"sign\",\n"
                + "    \"config\": \"\",\n"
                + "    \"sort\": 20,\n"
                + "    \"enabled\": false,\n"
                + "    \"dateCreated\": \"2022-05-25 18:02:53\",\n"
                + "    \"dateUpdated\": \"2024-02-29 12:59:52\",\n"
                + "    \"file\": \"\",\n"
                + "    \"pluginHandleList\": [\n"
                + "      {\n"
                + "        \"id\": \"1529402613204172891\",\n"
                + "        \"pluginId\": \"1\",\n"
                + "        \"field\": \"signRequestBody\",\n"
                + "        \"label\": \"signRequestBody\",\n"
                + "        \"dataType\": 3,\n"
                + "        \"type\": 2,\n"
                + "        \"sort\": 9,\n"
                + "        \"extObj\": \"{\\\"required\\\":\\\"0\\\",\\\"defaultValue\\\":\\\"false\\\",\\\"placeholder\\\":\\\"signRequestBody\\\",\\\"rule\\\":\\\"\\\"}\",\n"
                + "        \"dateCreated\": \"2022-06-29 10:08:02\",\n"
                + "        \"dateUpdated\": \"2022-06-29 10:08:02\",\n"
                + "        \"dictOptions\": null\n"
                + "      }\n"
                + "    ]\n"
                + "  }\n"
                + "]");
    }

    private ZipUtil.ZipItem buildSelectorData() {
        return new ZipUtil.ZipItem(ExportImportConstants.SELECTOR_JSON, "[\n"
                + "  {\n"
                + "    \"id\": \"1763015953925517312\",\n"
                + "    \"pluginId\": \"1\",\n"
                + "    \"name\": \"dgpc\",\n"
                + "    \"matchMode\": 0,\n"
                + "    \"type\": 1,\n"
                + "    \"typeName\": \"custom\",\n"
                + "    \"sort\": 1,\n"
                + "    \"enabled\": true,\n"
                + "    \"matchModeName\": \"and\",\n"
                + "    \"loged\": true,\n"
                + "    \"continued\": true,\n"
                + "    \"matchRestful\": false,\n"
                + "    \"handle\": \"\",\n"
                + "    \"selectorConditions\": [\n"
                + "      {\n"
                + "        \"id\": \"1764068020806303744\",\n"
                + "        \"selectorId\": \"1763015953925517312\",\n"
                + "        \"paramType\": \"uri\",\n"
                + "        \"paramTypeName\": \"uri\",\n"
                + "        \"operator\": \"match\",\n"
                + "        \"operatorName\": \"match\",\n"
                + "        \"paramName\": \"/\",\n"
                + "        \"paramValue\": \"/dgpc/**\",\n"
                + "        \"dateCreated\": \"2024-03-06 09:12:31\",\n"
                + "        \"dateUpdated\": \"2024-03-06 17:12:31\"\n"
                + "      }\n"
                + "    ],\n"
                + "    \"dateCreated\": \"2024-03-06 09:12:31\",\n"
                + "    \"dateUpdated\": \"2024-03-06 17:12:31\",\n"
                + "    \"discoveryHandler\": null,\n"
                + "    \"discoveryVO\": null,\n"
                + "    \"discoveryUpstreams\": null,\n"
                + "    \"selectorRules\": null\n"
                + "  }\n"
                + "]");
    }

    private ZipUtil.ZipItem buildRuleData() {
        return new ZipUtil.ZipItem(ExportImportConstants.RULE_JSON, "[\n"
                + "  {\n"
                + "    \"id\": \"1763016014227025920\",\n"
                + "    \"selectorId\": \"1763015953925517312\",\n"
                + "    \"matchMode\": 0,\n"
                + "    \"matchModeName\": \"and\",\n"
                + "    \"name\": \"/dgpc/**\",\n"
                + "    \"enabled\": true,\n"
                + "    \"loged\": true,\n"
                + "    \"sort\": 1,\n"
                + "    \"handle\": \"{\\\"signRequestBody\\\":\\\"false\\\"}\",\n"
                + "    \"matchRestful\": false,\n"
                + "    \"ruleConditions\": [\n"
                + "      {\n"
                + "        \"id\": \"1763016014369632256\",\n"
                + "        \"ruleId\": \"1763016014227025920\",\n"
                + "        \"paramType\": \"uri\",\n"
                + "        \"paramTypeName\": \"uri\",\n"
                + "        \"operator\": \"match\",\n"
                + "        \"operatorName\": \"match\",\n"
                + "        \"paramName\": \"/\",\n"
                + "        \"paramValue\": \"/dgpc1/**\",\n"
                + "        \"dateCreated\": \"2024-03-06 09:12:31\",\n"
                + "        \"dateUpdated\": \"2024-03-06 17:12:31\"\n"
                + "      }\n"
                + "    ],\n"
                + "    \"dateCreated\": \"2024-03-06 09:12:31\",\n"
                + "    \"dateUpdated\": \"2024-03-06 17:12:31\"\n"
                + "  }\n"
                + "]");
    }

    private ZipUtil.ZipItem buildDictData() {
        return new ZipUtil.ZipItem(ExportImportConstants.DICT_JSON, "[\n"
                + "  {\n"
                + "   \"id\": \"1529402613195784282\",\n"
                + "   \"type\": \"addPrefixed\",\n"
                + "   \"dictCode\": \"ADD_PREFIXED\",\n"
                + "   \"dictName\": \"open\",\n"
                + "   \"dictValue\": \"true\",\n"
                + "   \"desc\": \"\",\n"
                + "   \"sort\": 0,\n"
                + "   \"enabled\": true,\n"
                + "   \"dateCreated\": \"2022-09-27 12:00:00\",\n"
                + "   \"dateUpdated\": \"2022-09-27 12:00:00\"\n"
                + " }\n"
                + "]");
    }

    private ZipUtil.ZipItem buildDiscoveryData() {
        return new ZipUtil.ZipItem(ExportImportConstants.DISCOVERY_JSON, "[\n"
                + " {\n"
                + "   \"id\": \"1767463923129982976\",\n"
                + "   \"name\": \"logic\",\n"
                + "   \"type\": \"local\",\n"
                + "   \"level\": \"0\",\n"
                + "   \"serverList\": \"\",\n"
                + "   \"pluginName\": \"tcp\",\n"
                + "   \"props\": \"{}\",\n"
                + "   \"discoveryHandler\": {\n"
                + "     \"id\": \"1767211655786549248\",\n"
                + "     \"discoveryId\": \"1767463923129982976\",\n"
                + "     \"handler\": \"{}\",\n"
                + "     \"listenerNode\": \"\",\n"
                + "     \"props\": \"{\\\"bossGroupThreadCount\\\":\\\"1\\\","
                + "\\\"workerGroupThreadCount\\\":\\\"12\\\",\\\"loadBalance\\\":\\\"random\\\","
                + "\\\"clientPendingAcquireMaxCount\\\":\\\"5\\\","
                + "\\\"clientPendingAcquireTimeout\\\":\\\"5\\\","
                + "\\\"clientMaxConnections\\\":\\\"20\\\","
                + "\\\"clientMaxIdleTimeMs\\\":\\\"30000\\\","
                + "\\\"clientMaxLifeTimeMs\\\":\\\"60000\\\"}\"\n"
                + "   },\n"
                + "   \"discoveryRel\": {\n"
                + "     \"id\": \"1767211655790743552\",\n"
                + "     \"pluginName\": \"tcp\",\n"
                + "     \"discoveryHandlerId\": \"1767211655786549248\",\n"
                + "     \"selectorId\": \"\",\n"
                + "     \"proxySelectorId\": \"1767211655773966336\"\n"
                + "   }\n"
                + " }\n"
                + "]\n");
    }

    private ZipUtil.ZipItem buildDiscoveryUpstreamData() {
        return new ZipUtil.ZipItem(ExportImportConstants.DISCOVERY_UPSTREAM_JSON, "[\n"
                + " {\n"
                + "   \"id\": \"1767463923272589312\",\n"
                + "   \"discoveryHandlerId\": \"1767211655786549248\",\n"
                + "   \"protocol\": \"http://\",\n"
                + "   \"url\": \"localhost:\",\n"
                + "   \"status\": 0,\n"
                + "   \"weight\": 50,\n"
                + "   \"props\": \"{\\\"warmupTime\\\":10}\",\n"
                + "   \"startupTime\": \"1710231211856\"\n"
                + " }\n"
                + "]\n");
    }

    private ZipUtil.ZipItem buildProxySelectorData() {
        return new ZipUtil.ZipItem(ExportImportConstants.PROXY_SELECTOR_JSON, " [\n"
                + " {\n"
                + "   \"id\": \"1767211655773966336\",\n"
                + "   \"name\": \"logic\",\n"
                + "   \"pluginName\": \"tcp\",\n"
                + "   \"type\": \"tcp\",\n"
                + "   \"forwardPort\": 8989,\n"
                + "   \"props\": {\n"
                + "     \"clientMaxLifeTimeMs\": \"60000\",\n"
                + "     \"bossGroupThreadCount\": \"1\",\n"
                + "     \"clientPendingAcquireMaxCount\": \"5\",\n"
                + "     \"workerGroupThreadCount\": \"12\",\n"
                + "     \"clientMaxConnections\": \"20\",\n"
                + "     \"clientPendingAcquireTimeout\": \"5\",\n"
                + "     \"clientMaxIdleTimeMs\": \"30000\",\n"
                + "     \"loadBalance\": \"random\"\n"
                + "   }\n"
                + " }\n"
                + "]");
    }

}
