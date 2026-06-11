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

package org.apache.shenyu.admin.listener.websocket;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;

/**
 * Data Change WebSocketListener Test.
 */
@ExtendWith(MockitoExtension.class)
public final class WebsocketDataChangedListenerTest {

    private WebsocketDataChangedListener websocketDataChangedListener;

    private final List<PluginData> pluginDataList = new ArrayList<>();

    private final List<SelectorData> selectorDataList = new ArrayList<>();

    private final List<RuleData> ruleDataList = new ArrayList<>();

    private final List<AppAuthData> appAuthDataList = new ArrayList<>();

    private final List<MetaData> metaDataList = new ArrayList<>();

    @BeforeEach
    public void before() {
        websocketDataChangedListener = new WebsocketDataChangedListener();
        initSelectorDataList();
        initPluginDataList();
        initRuleDataList();
        initAppAuthDataList();
        initMetaDataList();
    }

    /**
     * test PluginData.
     */
    @Test
    public void testOnPluginChanged() {
        String message = "{\"groupType\":\"PLUGIN\",\"eventType\":\"UPDATE\",\"data\":[{\"config\":\"{\\\\\\\"model\\\\\\\":\\\\\\\"black\\\\\\\"}\","
                + "\"role\":\"1\",\"id\":\"2\",\"name\":\"waf\",\"enabled\":true,\"namespaceId\":\"649330b6-c2d7-4edc-be8e-8a54df9eb385\"}]}";
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.send(anyString(), anyString(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onPluginChanged(pluginDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.send(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                argThat(actualMsg -> jsonEquals(message, actualMsg)),
                eq(DataEventTypeEnum.UPDATE)
            ));
        }
    }

    /**
     * test PluginData with empty list — no send.
     */
    @Test
    public void testOnPluginChangedEmptyList() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onPluginChanged(Collections.emptyList(), DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.send(anyString(), anyString(), any()), never());
        }
    }

    /**
     * test SelectorData.
     */
    @Test
    public void testOnSelectorChanged() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.send(anyString(), anyString(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onSelectorChanged(selectorDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.send(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                anyString(),
                eq(DataEventTypeEnum.UPDATE)
            ));
        }
    }

    /**
     * test SelectorData with empty list — no send.
     */
    @Test
    public void testOnSelectorChangedEmptyList() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onSelectorChanged(Collections.emptyList(), DataEventTypeEnum.DELETE);
            mockedStatic.verify(() -> WebsocketCollector.send(anyString(), anyString(), any()), never());
        }
    }

    /**
     * test RuleData.
     */
    @Test
    public void testOnRuleChanged() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.send(anyString(), anyString(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onRuleChanged(ruleDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.send(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                anyString(),
                eq(DataEventTypeEnum.UPDATE)
            ));
        }
    }

    /**
     * test RuleData with empty list — no send.
     */
    @Test
    public void testOnRuleChangedEmptyList() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onRuleChanged(Collections.emptyList(), DataEventTypeEnum.DELETE);
            mockedStatic.verify(() -> WebsocketCollector.send(anyString(), anyString(), any()), never());
        }
    }

    /**
     * test AppAuthData.
     */
    @Test
    public void testOnAppAuthChanged() {
        String message = "{\"groupType\":\"APP_AUTH\",\"eventType\":\"UPDATE\",\"data\":[{\"appKey\":"
                + "\"D9FD95F496C9495DB5604778A13C3D08\",\"appSecret\":\"02D25048AA1E466F8920E68B08E668DE\","
                + "\"enabled\":true,\"paramDataList\":[{\"appName\":\"axiba\",\"appParam\":\"123\"}]"
                + ",\"pathDataList\":[{\"appName\":\"alibaba\",\"path\":\"/1\",\"enabled\":true}],\"namespaceId\":\"649330b6-c2d7-4edc-be8e-8a54df9eb385\"}]}";
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.send(Constants.SYS_DEFAULT_NAMESPACE_ID, message, DataEventTypeEnum.UPDATE))
                .thenAnswer((Answer<Void>) invocation -> null);
            websocketDataChangedListener.onAppAuthChanged(appAuthDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.send(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                argThat(actualMsg -> jsonEquals(message, actualMsg)),
                eq(DataEventTypeEnum.UPDATE)
            ));
        }
    }

    /**
     * test AppAuthData with empty list — no send.
     */
    @Test
    public void testOnAppAuthChangedEmptyList() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onAppAuthChanged(Collections.emptyList(), DataEventTypeEnum.DELETE);
            mockedStatic.verify(() -> WebsocketCollector.send(anyString(), anyString(), any()), never());
        }
    }

    /**
     * test MetaData.
     */
    @Test
    public void testOnMetaDataChanged() {
        String message = "{\"groupType\":\"META_DATA\",\"eventType\":\"CREATE\",\"data\":[{\"appName\":\"axiba\","
                + "\"path\":\"/test/execute\",\"rpcType\":\"http\",\"serviceName\":\"execute\",\"methodName\":"
                + "\"execute\",\"parameterTypes\":\"int\",\"rpcExt\":\"{}\",\"enabled\":true,\"namespaceId\":\"649330b6-c2d7-4edc-be8e-8a54df9eb385\"}]}";
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.send(anyString(), anyString(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onMetaDataChanged(metaDataList, DataEventTypeEnum.CREATE);
            mockedStatic.verify(() -> WebsocketCollector.send(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                argThat(actualMsg -> jsonEquals(message, actualMsg)),
                eq(DataEventTypeEnum.CREATE)
            ));
        }
    }

    /**
     * test MetaData with empty list — no send.
     */
    @Test
    public void testOnMetaDataChangedEmptyList() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onMetaDataChanged(Collections.emptyList(), DataEventTypeEnum.DELETE);
            mockedStatic.verify(() -> WebsocketCollector.send(anyString(), anyString(), any()), never());
        }
    }

    /**
     * test ProxySelectorData.
     */
    @Test
    public void testOnProxySelectorChanged() {
        ProxySelectorData data = new ProxySelectorData();
        data.setId("ps-1");
        data.setName("proxySelector");
        data.setPluginName("tcp");
        data.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        List<ProxySelectorData> list = Collections.singletonList(data);

        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.send(anyString(), anyString(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onProxySelectorChanged(list, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.send(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                anyString(),
                eq(DataEventTypeEnum.UPDATE)
            ));
        }
    }

    /**
     * test ProxySelectorData with empty list — no send.
     */
    @Test
    public void testOnProxySelectorChangedEmptyList() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onProxySelectorChanged(Collections.emptyList(), DataEventTypeEnum.DELETE);
            mockedStatic.verify(() -> WebsocketCollector.send(anyString(), anyString(), any()), never());
        }
    }

    /**
     * test ProxyApiKeyData.
     */
    @Test
    public void testOnAiProxyApiKeyChanged() {
        ProxyApiKeyData data = ProxyApiKeyData.builder()
                .realApiKey("real-key")
                .proxyApiKey("proxy-key")
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID)
                .build();
        List<ProxyApiKeyData> list = Collections.singletonList(data);

        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.send(anyString(), anyString(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onAiProxyApiKeyChanged(list, DataEventTypeEnum.CREATE);
            mockedStatic.verify(() -> WebsocketCollector.send(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                anyString(),
                eq(DataEventTypeEnum.CREATE)
            ));
        }
    }

    /**
     * test ProxyApiKeyData with empty list — no send.
     */
    @Test
    public void testOnAiProxyApiKeyChangedEmptyList() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onAiProxyApiKeyChanged(Collections.emptyList(), DataEventTypeEnum.DELETE);
            mockedStatic.verify(() -> WebsocketCollector.send(anyString(), anyString(), any()), never());
        }
    }

    /**
     * test DiscoverySyncData.
     */
    @Test
    public void testOnDiscoveryUpstreamChanged() {
        DiscoverySyncData data = new DiscoverySyncData();
        data.setSelectorId("sel-1");
        data.setPluginName("divide");
        data.setSelectorName("test");
        data.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        List<DiscoverySyncData> list = Collections.singletonList(data);

        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.send(anyString(), anyString(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onDiscoveryUpstreamChanged(list, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.send(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                anyString(),
                eq(DataEventTypeEnum.UPDATE)
            ));
        }
    }

    /**
     * test DiscoverySyncData with empty list — no send.
     */
    @Test
    public void testOnDiscoveryUpstreamChangedEmptyList() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onDiscoveryUpstreamChanged(Collections.emptyList(), DataEventTypeEnum.DELETE);
            mockedStatic.verify(() -> WebsocketCollector.send(anyString(), anyString(), any()), never());
        }
    }

    /**
     * test namespaceId fallback to default when null.
     */
    @Test
    public void testOnPluginChangedNullNamespaceUsesDefault() {
        PluginData data = new PluginData();
        data.setId("3");
        data.setName("test-plugin");
        data.setNamespaceId(null);
        List<PluginData> list = Collections.singletonList(data);

        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.send(anyString(), anyString(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onPluginChanged(list, DataEventTypeEnum.DELETE);
            mockedStatic.verify(() -> WebsocketCollector.send(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                anyString(),
                eq(DataEventTypeEnum.DELETE)
            ));
        }
    }

    private boolean jsonEquals(final String expected, final String actual) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            JsonNode expectedJson = mapper.readTree(expected);
            JsonNode actualJson = mapper.readTree(actual);
            return expectedJson.equals(actualJson);
        } catch (Exception e) {
            return false;
        }
    }

    private void initMetaDataList() {
        MetaData metaData = new MetaData();
        metaData.setAppName("axiba");
        metaData.setEnabled(true);
        metaData.setMethodName("execute");
        metaData.setParameterTypes("int");
        metaData.setPath("/test/execute");
        metaData.setRpcExt("{}");
        metaData.setRpcType("http");
        metaData.setServiceName("execute");
        metaData.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        metaDataList.add(metaData);
    }

    private void initAppAuthDataList() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("D9FD95F496C9495DB5604778A13C3D08");
        appAuthData.setAppSecret("02D25048AA1E466F8920E68B08E668DE");
        appAuthData.setEnabled(true);
        appAuthData.setParamDataList(buildAuthParamDataList("axiba", "123"));
        appAuthData.setPathDataList(buildAuthPathDataList("alibaba", "/1"));
        appAuthData.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        appAuthDataList.add(appAuthData);
    }

    private void initPluginDataList() {
        PluginData pluginData = new PluginData();
        pluginData.setConfig("{\\\"model\\\":\\\"black\\\"}");
        pluginData.setEnabled(true);
        pluginData.setId("2");
        pluginData.setName("waf");
        pluginData.setRole("1");
        pluginData.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        pluginDataList.add(pluginData);
    }

    private void initRuleDataList() {
        RuleData ruleData = new RuleData();
        ruleData.setEnabled(true);
        ruleData.setHandle("{\\\"permission\\\":\\\"reject\\\",\\\"statusCode\\\":\\\"503\\\"}");
        ruleData.setId("1336350040008105984");
        ruleData.setLoged(true);
        ruleData.setMatchMode(1);
        ruleData.setName("test");
        ruleData.setPluginName("waf");
        ruleData.setSelectorId("1336349806465064960");
        ruleData.setSort(1);
        ruleData.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        ruleData.setConditionDataList(buildConditionDataList("=", "test", "header", "a"));
        ruleDataList.add(ruleData);
    }

    private void initSelectorDataList() {
        SelectorData selectorData = new SelectorData();
        selectorData.setContinued(true);
        selectorData.setEnabled(true);
        selectorData.setHandle("[{\\\"upstreamHost\\\":\\\"localhost\\\",\\\"protocol\\\":\\\"http://\\\",\\\"upstreamUrl\\\":\\\"127.0.0.1:8187\\\","
                + "\\\"weight\\\":\\\"51\\\"},{\\\"upstreamHost\\\":\\\"localhost\\\",\\\"protocol\\\":\\\"http://\\\",\\\"upstreamUrl\\\":\\\"127.0.0.1:8188\\\",\\\"weight\\\":\\\"49\\\"}]");
        selectorData.setId("1336329408516136960");
        selectorData.setLogged(true);
        selectorData.setMatchMode(0);
        selectorData.setName("/http");
        selectorData.setPluginId("5");
        selectorData.setPluginName("divide");
        selectorData.setSort(1);
        selectorData.setType(1);
        selectorData.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        selectorData.setConditionList(buildConditionDataList("match", "/", "uri", "/http/**"));
        selectorDataList.add(selectorData);
    }

    private List<ConditionData> buildConditionDataList(final String operator, final String paramName,
                                                       final String paramType, final String paramValue) {
        ConditionData conditionData = new ConditionData();
        conditionData.setOperator(operator);
        conditionData.setParamName(paramName);
        conditionData.setParamType(paramType);
        conditionData.setParamValue(paramValue);
        List<ConditionData> conditionList = new ArrayList<>();
        conditionList.add(conditionData);
        return conditionList;
    }

    private List<AuthParamData> buildAuthParamDataList(final String appName, final String appParam) {
        AuthParamData authParamData = new AuthParamData();
        authParamData.setAppName(appName);
        authParamData.setAppParam(appParam);
        List<AuthParamData> authParamDataList = new ArrayList<>();
        authParamDataList.add(authParamData);
        return authParamDataList;
    }

    private List<AuthPathData> buildAuthPathDataList(final String appName, final String path) {
        AuthPathData authPathData = new AuthPathData();
        authPathData.setAppName(appName);
        authPathData.setEnabled(true);
        authPathData.setPath(path);
        List<AuthPathData> authPathDataList = new ArrayList<>();
        authPathDataList.add(authPathData);
        return authPathDataList;
    }
}
