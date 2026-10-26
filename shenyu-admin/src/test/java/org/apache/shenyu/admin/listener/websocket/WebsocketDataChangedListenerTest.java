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
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.config.WebSocketSyncConfiguration;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
                + "\"role\":\"1\",\"id\":\"2\",\"name\":\"waf\",\"enabled\":true,\"namespaceId\":\"649330b6-c2d7-4edc-be8e-8a54df9eb385\"}],\"fullSnapshot\":false}";
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onPluginChanged(pluginDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                argThat(actualData -> dataEquals(message, actualData)),
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
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()), never());
        }
    }

    /**
     * test SelectorData.
     */
    @Test
    public void testOnSelectorChanged() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onSelectorChanged(selectorDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                any(),
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
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()), never());
        }
    }

    /**
     * test RuleData.
     */
    @Test
    public void testOnRuleChanged() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onRuleChanged(ruleDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                any(),
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
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()), never());
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
                + ",\"pathDataList\":[{\"appName\":\"alibaba\",\"path\":\"/1\",\"enabled\":true}],\"namespaceId\":\"649330b6-c2d7-4edc-be8e-8a54df9eb385\"}],\"fullSnapshot\":false}";
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onAppAuthChanged(appAuthDataList, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                argThat(actualData -> dataEquals(message, actualData)),
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
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()), never());
        }
    }

    /**
     * test empty namespace snapshots.
     */
    @Test
    public void testEmptyNamespaceSnapshots() {
        String namespaceId = "namespace-id";
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onPluginChanged(
                    Collections.emptyList(), DataEventTypeEnum.MYSELF, namespaceId);
            websocketDataChangedListener.onSelectorChanged(
                    Collections.emptyList(), DataEventTypeEnum.MYSELF, namespaceId);
            websocketDataChangedListener.onRuleChanged(
                    Collections.emptyList(), DataEventTypeEnum.MYSELF, namespaceId);
            websocketDataChangedListener.onAppAuthChanged(
                    Collections.emptyList(), DataEventTypeEnum.REFRESH, namespaceId);

            verifyEmptySnapshot(mockedStatic, namespaceId, "PLUGIN", DataEventTypeEnum.MYSELF);
            verifyEmptySnapshot(mockedStatic, namespaceId, "SELECTOR", DataEventTypeEnum.MYSELF);
            verifyEmptySnapshot(mockedStatic, namespaceId, "RULE", DataEventTypeEnum.MYSELF);
            verifyEmptySnapshot(mockedStatic, namespaceId, "APP_AUTH", DataEventTypeEnum.REFRESH);
        }
    }

    /**
     * test MetaData.
     */
    @Test
    public void testOnMetaDataChanged() {
        String message = "{\"groupType\":\"META_DATA\",\"eventType\":\"CREATE\",\"data\":[{\"appName\":\"axiba\","
                + "\"path\":\"/test/execute\",\"rpcType\":\"http\",\"serviceName\":\"execute\",\"methodName\":"
                + "\"execute\",\"parameterTypes\":\"int\",\"rpcExt\":\"{}\",\"enabled\":true,\"namespaceId\":\"649330b6-c2d7-4edc-be8e-8a54df9eb385\"}],\"fullSnapshot\":false}";
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onMetaDataChanged(metaDataList, DataEventTypeEnum.CREATE);
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                argThat(actualData -> dataEquals(message, actualData)),
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
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()), never());
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
            mockedStatic.when(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onProxySelectorChanged(list, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                any(),
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
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()), never());
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
            mockedStatic.when(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onAiProxyApiKeyChanged(list, DataEventTypeEnum.CREATE);
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                any(),
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
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()), never());
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
            mockedStatic.when(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onDiscoveryUpstreamChanged(list, DataEventTypeEnum.UPDATE);
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                any(),
                eq(DataEventTypeEnum.UPDATE)
            ));
        }
    }

    @ParameterizedTest
    @EnumSource(value = DataEventTypeEnum.class, names = {"UPDATE", "CREATE", "DELETE", "MYSELF", "REFRESH"})
    public void testCompactDiscoveryPreservesRuntimeData(final DataEventTypeEnum eventType) {
        DiscoverySyncData data = discoveryData();
        final String original = GsonUtils.getInstance().toJson(data);
        final String legacy = discoveryMessage(new WebsocketDataChangedListener(), data, eventType);
        WebsocketSyncProperties properties = new WebsocketSyncProperties();
        properties.setCompactDiscoveryPayload(true);
        WebsocketDataChangedListener configured = (WebsocketDataChangedListener) new WebSocketSyncConfiguration().websocketDataChangedListener(properties);
        String compact = discoveryMessage(configured, data, eventType);
        JsonObject compactFrame = JsonParser.parseString(compact).getAsJsonObject();
        JsonObject compactData = compactFrame.getAsJsonArray("data").get(0).getAsJsonObject();
        JsonObject compactUpstream = compactData.getAsJsonArray("upstreamDataList").get(0).getAsJsonObject();
        assertEquals(eventType.name(), compactFrame.get("eventType").getAsString());
        assertEquals("DISCOVER_UPSTREAM", compactFrame.get("groupType").getAsString());
        assertFalse(compactData.has("discoveryHandlerId"));
        for (String field : List.of("id", "dateUpdated", "discoveryHandlerId", "namespaceId")) {
            assertFalse(compactUpstream.has(field), field);
        }
        DiscoverySyncData decoded = GsonUtils.getInstance().fromJson(compactData.toString(), DiscoverySyncData.class);
        assertEquals(data.getSelectorId(), decoded.getSelectorId());
        assertEquals(data.getSelectorName(), decoded.getSelectorName());
        assertEquals(data.getPluginName(), decoded.getPluginName());
        assertEquals(data.getNamespaceId(), decoded.getNamespaceId());
        DiscoveryUpstreamData expected = data.getUpstreamDataList().get(0);
        DiscoveryUpstreamData actual = decoded.getUpstreamDataList().get(0);
        assertEquals(expected.getDateCreated(), actual.getDateCreated());
        assertEquals(expected.getProtocol(), actual.getProtocol());
        assertEquals(expected.getUrl(), actual.getUrl());
        assertEquals(expected.getStatus(), actual.getStatus());
        assertEquals(expected.getWeight(), actual.getWeight());
        assertEquals(expected.getProps(), actual.getProps());
        assertEquals(original, GsonUtils.getInstance().toJson(data));
        assertTrue(legacy.contains("dateUpdated"));
        assertTrue(compact.length() < legacy.length());
    }

    @Test
    public void testCompactDiscoveryRetainsEmptyUpstreamList() {
        DiscoverySyncData data = discoveryData();
        data.setUpstreamDataList(Collections.emptyList());
        String message = discoveryMessage(new WebsocketDataChangedListener(true), data, DataEventTypeEnum.UPDATE);
        JsonObject decoded = JsonParser.parseString(message).getAsJsonObject().getAsJsonArray("data").get(0).getAsJsonObject();
        assertTrue(decoded.getAsJsonArray("upstreamDataList").isEmpty());
    }

    @Test
    public void testCompactDiscoverySeparatesNamespacesAndSkipsNullRows() {
        DiscoverySyncData first = discoveryData();
        DiscoverySyncData second = discoveryData();
        second.setNamespaceId("other-namespace");
        second.setSelectorId("other-selector");
        List<DiscoverySyncData> data = new ArrayList<>();
        data.add(first);
        data.add(null);
        data.add(second);
        try (MockedStatic<WebsocketCollector> collector = mockStatic(WebsocketCollector.class)) {
            new WebsocketDataChangedListener(true).onDiscoveryUpstreamChanged(data, DataEventTypeEnum.UPDATE);
            for (DiscoverySyncData item : List.of(first, second)) {
                collector.verify(() -> WebsocketCollector.sendDiscovery(eq(item.getNamespaceId()), any(), argThat(message -> {
                    JsonObject frame = JsonParser.parseString(message.get()).getAsJsonObject();
                    return frame.getAsJsonArray("data").size() == 1
                            && item.getSelectorId().equals(frame.getAsJsonArray("data").get(0).getAsJsonObject().get("selectorId").getAsString());
                }), eq(DataEventTypeEnum.UPDATE)));
            }
        }
    }

    @Test
    public void testCompactDiscoveryPreservesMissingListAndDefaultNamespace() {
        DiscoverySyncData data = discoveryData();
        data.setNamespaceId(null);
        data.setUpstreamDataList(null);
        try (MockedStatic<WebsocketCollector> collector = mockStatic(WebsocketCollector.class)) {
            new WebsocketDataChangedListener(true).onDiscoveryUpstreamChanged(Collections.singletonList(data), DataEventTypeEnum.DELETE);
            collector.verify(() -> WebsocketCollector.sendDiscovery(eq(Constants.SYS_DEFAULT_NAMESPACE_ID), any(), argThat(message -> {
                JsonObject item = JsonParser.parseString(message.get()).getAsJsonObject().getAsJsonArray("data").get(0).getAsJsonObject();
                return !item.has("upstreamDataList") && "selector-id".equals(item.get("selectorId").getAsString());
            }), eq(DataEventTypeEnum.DELETE)));
        }
    }

    @Test
    public void testCompactDiscoveryPreservesNullUpstreamEntries() {
        DiscoverySyncData data = discoveryData();
        List<DiscoveryUpstreamData> upstreams = new ArrayList<>(data.getUpstreamDataList());
        upstreams.add(null);
        data.setUpstreamDataList(upstreams);
        String message = discoveryMessage(new WebsocketDataChangedListener(true), data, DataEventTypeEnum.UPDATE);
        JsonObject item = JsonParser.parseString(message).getAsJsonObject().getAsJsonArray("data").get(0).getAsJsonObject();
        assertEquals(2, item.getAsJsonArray("upstreamDataList").size());
        assertTrue(item.getAsJsonArray("upstreamDataList").get(1).isJsonNull());
    }

    private String discoveryMessage(final WebsocketDataChangedListener listener, final DiscoverySyncData data,
                                    final DataEventTypeEnum eventType) {
        List<String> messages = new ArrayList<>();
        try (MockedStatic<WebsocketCollector> collector = mockStatic(WebsocketCollector.class)) {
            collector.when(() -> WebsocketCollector.send(eq(data.getNamespaceId()), anyString(), eq(eventType)))
                    .thenAnswer(invocation -> {
                        messages.add(invocation.getArgument(1));
                        return null;
                    });
            collector.when(() -> WebsocketCollector.sendDiscovery(eq(data.getNamespaceId()), any(), any(), eq(eventType)))
                    .thenAnswer(invocation -> {
                        messages.add(invocation.<java.util.function.Supplier<String>>getArgument(2).get());
                        return null;
                    });
            // the non-compact path routes through sendConfiguration, which serializes the
            // message itself; capture what it would have handed to send()
            collector.when(() -> WebsocketCollector.sendConfiguration(eq(data.getNamespaceId()), any(), eq(eventType)))
                    .thenAnswer(invocation -> {
                        messages.add(GsonUtils.getInstance().toJson(invocation.getArgument(1)));
                        return null;
                    });
            listener.onDiscoveryUpstreamChanged(Collections.singletonList(data), eventType);
        }
        assertEquals(1, messages.size());
        return messages.get(0);
    }

    private DiscoverySyncData discoveryData() {
        DiscoveryUpstreamData upstream = new DiscoveryUpstreamData();
        upstream.setId("row-id");
        upstream.setDateCreated(new Timestamp(1700000000000L));
        upstream.setDateUpdated(new Timestamp(1700000001000L));
        upstream.setDiscoveryHandlerId("handler-id");
        upstream.setProtocol("http://");
        upstream.setUrl("127.0.0.1:8080");
        upstream.setStatus(0);
        upstream.setWeight(100);
        upstream.setProps("{\"warmup\":600000,\"healthCheckEnabled\":false}");
        upstream.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        DiscoverySyncData data = new DiscoverySyncData();
        data.setSelectorId("selector-id");
        data.setSelectorName("tcp-route");
        data.setPluginName("divide");
        data.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        data.setDiscoveryHandlerId("handler-id");
        data.setUpstreamDataList(Collections.singletonList(upstream));
        return data;
    }

    /**
     * test DiscoverySyncData with empty list — no send.
     */
    @Test
    public void testOnDiscoveryUpstreamChangedEmptyList() {
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            websocketDataChangedListener.onDiscoveryUpstreamChanged(Collections.emptyList(), DataEventTypeEnum.DELETE);
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()), never());
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
            mockedStatic.when(() -> WebsocketCollector.sendConfiguration(anyString(), any(), any()))
                .thenAnswer(invocation -> null);
            websocketDataChangedListener.onPluginChanged(list, DataEventTypeEnum.DELETE);
            mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(
                eq(Constants.SYS_DEFAULT_NAMESPACE_ID),
                any(),
                eq(DataEventTypeEnum.DELETE)
            ));
        }
    }

    private boolean dataEquals(final String expected, final WebsocketData<?> actual) {
        return jsonEquals(expected, GsonUtils.getInstance().toJson(actual));
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

    private void verifyEmptySnapshot(final MockedStatic<WebsocketCollector> mockedStatic, final String namespaceId,
                                     final String groupType, final DataEventTypeEnum eventType) {
        String message = String.format(
                "{\"groupType\":\"%s\",\"eventType\":\"%s\",\"data\":[],\"fullSnapshot\":false}", groupType, eventType.name());
        mockedStatic.verify(() -> WebsocketCollector.sendConfiguration(
                eq(namespaceId), argThat(actualData -> dataEquals(message, actualData)), eq(eventType)));
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
        ruleData.setLogged(true);
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
