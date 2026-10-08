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
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;

/**
 * Mixed-namespace delivery for websocket data-change batches.
 */
public final class WebsocketDataChangedListenerNamespaceTest {

    private static final String NAMESPACE_A = "namespace-a";

    private static final String NAMESPACE_B = "namespace-b";

    private final WebsocketDataChangedListener listener = new WebsocketDataChangedListener();

    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    public void testMixedNamespacePluginBatchIsSplit() throws Exception {
        Map<String, JsonNode> sent = capture(() -> listener.onPluginChanged(
                Arrays.asList(plugin("plugin-a", NAMESPACE_A), plugin("plugin-b", NAMESPACE_B)),
                DataEventTypeEnum.UPDATE));
        assertSplit(sent, "PLUGIN", "name", "plugin-a", "plugin-b");
    }

    @Test
    public void testMixedNamespaceDeliveryIsIndependentOfOrder() throws Exception {
        Map<String, JsonNode> sent = capture(() -> listener.onPluginChanged(
                Arrays.asList(plugin("plugin-b", NAMESPACE_B), plugin("plugin-a", NAMESPACE_A)),
                DataEventTypeEnum.UPDATE));
        assertSplit(sent, "PLUGIN", "name", "plugin-a", "plugin-b");
    }

    @Test
    public void testSingleNamespacePluginBatchStaysOneMessage() throws Exception {
        Map<String, JsonNode> sent = capture(() -> listener.onPluginChanged(
                Arrays.asList(plugin("plugin-a", NAMESPACE_A), plugin("plugin-c", NAMESPACE_A)),
                DataEventTypeEnum.DELETE));
        assertEquals(1, sent.size());
        assertEquals(2, sent.get(NAMESPACE_A).get("data").size());
        assertEquals("DELETE", sent.get(NAMESPACE_A).get("eventType").asText());
    }

    @Test
    public void testNullPluginRecordIsSkipped() throws Exception {
        Map<String, JsonNode> sent = capture(() -> listener.onPluginChanged(
                Arrays.asList(null, plugin("plugin-a", NAMESPACE_A)),
                DataEventTypeEnum.UPDATE));
        assertEquals(1, sent.size());
        assertEquals(1, sent.get(NAMESPACE_A).get("data").size());
        assertEquals("plugin-a", sent.get(NAMESPACE_A).get("data").get(0).get("name").asText());
    }

    @Test
    public void testNullOnlyBatchSendsNothing() throws Exception {
        Map<String, JsonNode> sent = capture(() -> listener.onPluginChanged(
                Collections.singletonList(null), DataEventTypeEnum.UPDATE));
        assertTrue(sent.isEmpty());
    }

    @Test
    public void testNullNamespaceFallsBackWithoutMixing() throws Exception {
        PluginData missing = plugin("plugin-default", null);
        Map<String, JsonNode> sent = capture(() -> listener.onPluginChanged(
                Arrays.asList(missing, plugin("plugin-b", NAMESPACE_B)),
                DataEventTypeEnum.UPDATE));
        assertEquals(2, sent.size());
        assertTrue(sent.containsKey(Constants.SYS_DEFAULT_NAMESPACE_ID));
        assertTrue(sent.containsKey(NAMESPACE_B));
        assertFalse(sent.get(Constants.SYS_DEFAULT_NAMESPACE_ID).toString().contains(NAMESPACE_B));
        assertEquals("plugin-default", sent.get(Constants.SYS_DEFAULT_NAMESPACE_ID).get("data").get(0).get("name").asText());
    }

    @Test
    public void testMixedNamespaceSelectorBatchIsSplit() throws Exception {
        SelectorData first = new SelectorData();
        first.setId("selector-a");
        first.setName("selector-a");
        first.setNamespaceId(NAMESPACE_A);
        SelectorData second = new SelectorData();
        second.setId("selector-b");
        second.setName("selector-b");
        second.setNamespaceId(NAMESPACE_B);
        Map<String, JsonNode> sent = capture(() -> listener.onSelectorChanged(
                Arrays.asList(first, second), DataEventTypeEnum.UPDATE));
        assertSplit(sent, "SELECTOR", "name", "selector-a", "selector-b");
    }

    @Test
    public void testMixedNamespaceRuleBatchIsSplit() throws Exception {
        RuleData first = new RuleData();
        first.setId("rule-a");
        first.setName("rule-a");
        first.setNamespaceId(NAMESPACE_A);
        RuleData second = new RuleData();
        second.setId("rule-b");
        second.setName("rule-b");
        second.setNamespaceId(NAMESPACE_B);
        Map<String, JsonNode> sent = capture(() -> listener.onRuleChanged(
                Arrays.asList(first, second), DataEventTypeEnum.UPDATE));
        assertSplit(sent, "RULE", "name", "rule-a", "rule-b");
    }

    @Test
    public void testMixedNamespaceAppAuthBatchIsSplit() throws Exception {
        AppAuthData first = new AppAuthData();
        first.setAppKey("auth-a");
        first.setNamespaceId(NAMESPACE_A);
        AppAuthData second = new AppAuthData();
        second.setAppKey("auth-b");
        second.setNamespaceId(NAMESPACE_B);
        Map<String, JsonNode> sent = capture(() -> listener.onAppAuthChanged(
                Arrays.asList(first, second), DataEventTypeEnum.UPDATE));
        assertSplit(sent, "APP_AUTH", "appKey", "auth-a", "auth-b");
    }

    @Test
    public void testMixedNamespaceMetaDataBatchIsSplit() throws Exception {
        MetaData first = new MetaData();
        first.setId("meta-a");
        first.setPath("/a");
        first.setNamespaceId(NAMESPACE_A);
        MetaData second = new MetaData();
        second.setId("meta-b");
        second.setPath("/b");
        second.setNamespaceId(NAMESPACE_B);
        Map<String, JsonNode> sent = capture(() -> listener.onMetaDataChanged(
                Arrays.asList(first, second), DataEventTypeEnum.UPDATE));
        assertSplit(sent, "META_DATA", "path", "/a", "/b");
    }

    @Test
    public void testMixedNamespaceProxySelectorBatchIsSplit() throws Exception {
        ProxySelectorData first = new ProxySelectorData();
        first.setId("proxy-a");
        first.setName("proxy-a");
        first.setNamespaceId(NAMESPACE_A);
        ProxySelectorData second = new ProxySelectorData();
        second.setId("proxy-b");
        second.setName("proxy-b");
        second.setNamespaceId(NAMESPACE_B);
        Map<String, JsonNode> sent = capture(() -> listener.onProxySelectorChanged(
                Arrays.asList(first, second), DataEventTypeEnum.UPDATE));
        assertSplit(sent, "PROXY_SELECTOR", "name", "proxy-a", "proxy-b");
    }

    @Test
    public void testMixedNamespaceApiKeyBatchIsSplit() throws Exception {
        ProxyApiKeyData first = new ProxyApiKeyData();
        first.setProxyApiKey("key-a");
        first.setNamespaceId(NAMESPACE_A);
        ProxyApiKeyData second = new ProxyApiKeyData();
        second.setProxyApiKey("key-b");
        second.setNamespaceId(NAMESPACE_B);
        Map<String, JsonNode> sent = capture(() -> listener.onAiProxyApiKeyChanged(
                Arrays.asList(first, second), DataEventTypeEnum.UPDATE));
        assertSplit(sent, "AI_PROXY_API_KEY", "proxyApiKey", "key-a", "key-b");
    }

    @Test
    public void testMixedNamespaceDiscoveryBatchIsSplit() throws Exception {
        DiscoverySyncData first = new DiscoverySyncData();
        first.setSelectorId("disc-a");
        first.setNamespaceId(NAMESPACE_A);
        DiscoverySyncData second = new DiscoverySyncData();
        second.setSelectorId("disc-b");
        second.setNamespaceId(NAMESPACE_B);
        Map<String, JsonNode> sent = capture(() -> listener.onDiscoveryUpstreamChanged(
                Arrays.asList(first, second), DataEventTypeEnum.UPDATE));
        assertSplit(sent, "DISCOVER_UPSTREAM", "selectorId", "disc-a", "disc-b");
    }

    private Map<String, JsonNode> capture(final Runnable action) throws Exception {
        Map<String, String> raw = new LinkedHashMap<>();
        try (MockedStatic<WebsocketCollector> mockedStatic = mockStatic(WebsocketCollector.class)) {
            mockedStatic.when(() -> WebsocketCollector.send(anyString(), anyString(), any()))
                    .thenAnswer(invocation -> {
                        raw.put(invocation.getArgument(0), invocation.getArgument(1));
                        return null;
                    });
            action.run();
        }
        Map<String, JsonNode> parsed = new LinkedHashMap<>();
        for (Map.Entry<String, String> entry : raw.entrySet()) {
            parsed.put(entry.getKey(), mapper.readTree(entry.getValue()));
        }
        return parsed;
    }

    private void assertSplit(final Map<String, JsonNode> sent, final String group,
                             final String field, final String valueA, final String valueB) {
        assertEquals(2, sent.size());
        JsonNode payloadA = sent.get(NAMESPACE_A);
        JsonNode payloadB = sent.get(NAMESPACE_B);
        assertEquals(group, payloadA.get("groupType").asText());
        assertEquals(group, payloadB.get("groupType").asText());
        assertEquals("UPDATE", payloadA.get("eventType").asText());
        assertEquals(1, payloadA.get("data").size());
        assertEquals(1, payloadB.get("data").size());
        assertEquals(NAMESPACE_A, payloadA.get("data").get(0).get("namespaceId").asText());
        assertEquals(NAMESPACE_B, payloadB.get("data").get(0).get("namespaceId").asText());
        assertEquals(valueA, payloadA.get("data").get(0).get(field).asText());
        assertEquals(valueB, payloadB.get("data").get(0).get(field).asText());
        assertFalse(payloadA.toString().contains(NAMESPACE_B));
        assertFalse(payloadB.toString().contains(NAMESPACE_A));
    }

    private PluginData plugin(final String name, final String namespaceId) {
        PluginData data = new PluginData();
        data.setId(name);
        data.setName(name);
        data.setEnabled(true);
        data.setNamespaceId(namespaceId);
        return data;
    }
}
