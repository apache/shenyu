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

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.mockStatic;

/**
 * Data Change WebSocketListener Test.
 */
@RunWith(MockitoJUnitRunner.class)
public final class WebsocketDataChangedListenerTest {

    private WebsocketDataChangedListener websocketDataChangedListener;

    private final List<PluginData> pluginDataList = new ArrayList<>();

    private final List<SelectorData> selectorDataList = new ArrayList<>();

    private final List<RuleData> ruleDataList = new ArrayList<>();

    private final List<AppAuthData> appAuthDataList = new ArrayList<>();

    private final List<MetaData> metaDataList = new ArrayList<>();

    @Before
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
        String message = "{\"groupType\":\"PLUGIN\",\"eventType\":\"UPDATE\",\"data\":[{\"id\":\"2\",\"name\":\"waf\","
                + "\"config\":\"{\\\\\\\"model\\\\\\\":\\\\\\\"black\\\\\\\"}\",\"role\":\"1\",\"enabled\":true}]}";
        MockedStatic.Verification verification = () -> WebsocketCollector.send(message, DataEventTypeEnum.UPDATE);
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(verification).thenAnswer((Answer<Void>) invocation -> null);
            websocketDataChangedListener.onPluginChanged(pluginDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(verification);
        }
    }

    /**
     * test SelectorData.
     */
    @Test
    public void testOnSelectorChanged() {
        String message = "{\"groupType\":\"SELECTOR\",\"eventType\":\"UPDATE\",\"data\":"
                + "[{\"id\":\"1336329408516136960\",\"pluginId\":\"5\",\"pluginName\":\"divide\",\"name\":"
                + "\"/http\",\"matchMode\":0,\"type\":1,\"sort\":1,\"enabled\":true,\"logged\":true,"
                + "\"continued\":true,\"handle\":\"[{\\\\\\\"upstreamHost\\\\\\\":\\\\\\\"localhost\\\\\\\","
                + "\\\\\\\"protocol\\\\\\\":\\\\\\\"http://\\\\\\\",\\\\\\\"upstreamUrl\\\\\\\":"
                + "\\\\\\\"127.0.0.1:8187\\\\\\\",\\\\\\\"weight\\\\\\\":\\\\\\\"51\\\\\\\"},"
                + "{\\\\\\\"upstreamHost\\\\\\\":\\\\\\\"localhost\\\\\\\",\\\\\\\"protocol\\\\\\\":"
                + "\\\\\\\"http://\\\\\\\",\\\\\\\"upstreamUrl\\\\\\\":\\\\\\\"127.0.0.1:8188\\\\\\\","
                + "\\\\\\\"weight\\\\\\\":\\\\\\\"49\\\\\\\"}]\",\"conditionList\":[{\"paramType\":\"uri\","
                + "\"operator\":\"match\",\"paramName\":\"/\",\"paramValue\":\"/http/**\"}]}]}";
        MockedStatic.Verification verification = () -> WebsocketCollector.send(message, DataEventTypeEnum.UPDATE);
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(verification).thenAnswer((Answer<Void>) invocation -> null);
            websocketDataChangedListener.onSelectorChanged(selectorDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(verification);
        }
    }

    /**
     * test RuleData.
     */
    @Test
    public void testOnRuleChanged() {
        String message = "{\"groupType\":\"RULE\",\"eventType\":\"UPDATE\",\"data\":[{\"id\":\"1336350040008105984\","
                + "\"name\":\"test\",\"pluginName\":\"waf\",\"selectorId\":\"1336349806465064960\","
                + "\"matchMode\":1,\"sort\":1,\"enabled\":true,\"loged\":true,\"handle\":"
                + "\"{\\\\\\\"permission\\\\\\\":\\\\\\\"reject\\\\\\\",\\\\\\\"statusCode\\\\\\\":"
                + "\\\\\\\"503\\\\\\\"}\",\"conditionDataList\":[{\"paramType\":\"header\",\"operator\":"
                + "\"\\u003d\",\"paramName\":\"test\",\"paramValue\":\"a\"}]}]}";
        MockedStatic.Verification verification = () -> WebsocketCollector.send(message, DataEventTypeEnum.UPDATE);
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(verification).thenAnswer((Answer<Void>) invocation -> null);
            websocketDataChangedListener.onRuleChanged(ruleDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(verification);
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
                + ",\"pathDataList\":[{\"appName\":\"alibaba\",\"path\":\"/1\",\"enabled\":true}]}]}";
        MockedStatic.Verification verification = () -> WebsocketCollector.send(message, DataEventTypeEnum.UPDATE);
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(verification).thenAnswer((Answer<Void>) invocation -> null);
            websocketDataChangedListener.onAppAuthChanged(appAuthDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(verification);
        }
    }

    /**
     * test MetaData.
     */
    @Test
    public void testOnMetaDataChanged() {
        String message = "{\"groupType\":\"META_DATA\",\"eventType\":\"CREATE\",\"data\":[{\"appName\":\"axiba\","
                + "\"path\":\"/test/execute\",\"rpcType\":\"http\",\"serviceName\":\"execute\",\"methodName\":"
                + "\"execute\",\"parameterTypes\":\"int\",\"rpcExt\":\"{}\",\"enabled\":true}]}";
        MockedStatic.Verification verification = () -> WebsocketCollector.send(message, DataEventTypeEnum.CREATE);
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(verification).thenAnswer((Answer<Void>) invocation -> null);
            websocketDataChangedListener.onMetaDataChanged(metaDataList, DataEventTypeEnum.CREATE);
            mockedStatic.verify(verification);
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
        metaDataList.add(metaData);
    }

    private void initAppAuthDataList() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("D9FD95F496C9495DB5604778A13C3D08");
        appAuthData.setAppSecret("02D25048AA1E466F8920E68B08E668DE");
        appAuthData.setEnabled(true);
        appAuthData.setParamDataList(buildAuthParamDataList("axiba", "123"));
        appAuthData.setPathDataList(buildAuthPathDataList("alibaba", "/1"));
        appAuthDataList.add(appAuthData);
    }

    private void initPluginDataList() {
        PluginData pluginData = new PluginData();
        pluginData.setConfig("{\\\"model\\\":\\\"black\\\"}");
        pluginData.setEnabled(true);
        pluginData.setId("2");
        pluginData.setName("waf");
        pluginData.setRole("1");
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
