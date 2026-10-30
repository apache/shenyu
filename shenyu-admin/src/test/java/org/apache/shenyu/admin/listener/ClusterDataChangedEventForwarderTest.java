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

package org.apache.shenyu.admin.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterDataChangedEventPayload;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.mock.http.MockHttpInputMessage;
import org.springframework.mock.http.MockHttpOutputMessage;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ClusterDataChangedEventForwarder}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ClusterDataChangedEventForwarderTest {

    private static final String OPERATOR_TOKEN = "operator-token";

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private ClusterProperties clusterProperties;

    @Mock
    private ClusterSelectMasterService clusterSelectMasterService;

    private ClusterDataChangedEventForwarder forwarder;

    @BeforeEach
    public void setUp() {
        when(clusterProperties.getEventSecret()).thenReturn("node-secret");
        forwarder = new ClusterDataChangedEventForwarder(restTemplate, clusterProperties,
                clusterSelectMasterService, new ObjectMapper());
        when(clusterProperties.getSchema()).thenReturn("http");
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader(Constants.X_ACCESS_TOKEN, OPERATOR_TOKEN);
        RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(request));
    }

    @AfterEach
    public void tearDown() {
        RequestContextHolder.resetRequestAttributes();
    }

    /**
     * Forward posts the serialized event, authenticated with the dedicated node credential,
     * to the master url built from the master dto, preserving an empty refresh's namespace.
     *
     * @throws IOException if the payload cannot be serialized or deserialized
     */
    @Test
    public void forwardPostsPayloadToMasterUrlTest() throws IOException {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        master.setContextPath("/admin");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok("ok"));

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.REFRESH,
                Collections.emptyList(), "namespace-a");
        boolean forwarded = forwarder.forward(event);

        assertTrue(forwarded);
        ArgumentCaptor<HttpEntity<ClusterDataChangedEventPayload>> captor = ArgumentCaptor.forClass(HttpEntity.class);
        verify(restTemplate).postForEntity(eq("http://10.0.0.2:9095/admin/cluster/data-change-event"),
                captor.capture(), eq(String.class));
        assertEquals("node-secret", captor.getValue().getHeaders().getFirst(
                org.apache.shenyu.admin.shiro.bean.ClusterEventAuthFilter.HEADER));
        org.junit.jupiter.api.Assertions.assertNull(captor.getValue().getHeaders().getFirst(Constants.X_ACCESS_TOKEN));
        ClusterDataChangedEventPayload payload = captor.getValue().getBody();
        assertEquals(ConfigGroupEnum.RULE.name(), payload.getGroupKey());
        assertEquals(DataEventTypeEnum.REFRESH.name(), payload.getEventType());
        assertNull(payload.getSource());
        assertEquals(2, payload.getPayloadVersion());
        assertTrue(payload.getStructuredSource().isArray());
        assertEquals(0, payload.getStructuredSource().size());
        assertEquals("namespace-a", payload.getNamespaceId());
        MappingJackson2HttpMessageConverter converter = new MappingJackson2HttpMessageConverter();
        MockHttpOutputMessage output = new MockHttpOutputMessage();
        converter.write(payload, MediaType.APPLICATION_JSON, output);
        ClusterDataChangedEventPayload deserialized = (ClusterDataChangedEventPayload) converter.read(
                ClusterDataChangedEventPayload.class, new MockHttpInputMessage(output.getBodyAsBytes()));
        assertEquals("namespace-a", deserialized.getNamespaceId());
    }

    /**
     * Forward without a master available returns false without posting.
     */
    @Test
    public void forwardWithoutMasterReturnsFalseTest() {
        when(clusterSelectMasterService.getMaster()).thenReturn(null);

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(new PluginData()));
        boolean forwarded = forwarder.forward(event);

        assertFalse(forwarded);
        verify(restTemplate, never()).postForEntity(any(String.class), any(Object.class), eq(String.class));
    }

    /**
     * An event published off a request thread uses the configured node credential.
     */
    @Test
    public void forwardWithoutRequestContextUsesNodeCredentialTest() {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        RequestContextHolder.resetRequestAttributes();
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok("ok"));

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(new PluginData()));
        boolean forwarded = forwarder.forward(event);

        assertTrue(forwarded);
        verify(restTemplate).postForEntity(any(String.class), any(Object.class), eq(String.class));
    }

    /**
     * Forward rejected by the master (non-2xx) returns false.
     */
    @Test
    public void forwardRejectedByMasterReturnsFalseTest() {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenReturn(ResponseEntity.status(HttpStatus.CONFLICT).body("not master"));

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.UPDATE,
                Collections.singletonList(new org.apache.shenyu.common.dto.RuleData()));
        boolean forwarded = forwarder.forward(event);

        assertFalse(forwarded);
    }

    /**
     * Forward failure (connection error) returns false instead of throwing.
     */
    @Test
    public void forwardConnectionFailureReturnsFalseTest() {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenThrow(new RestClientException("connection refused"));

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.META_DATA, DataEventTypeEnum.UPDATE,
                Collections.singletonList(new org.apache.shenyu.common.dto.MetaData()));
        boolean forwarded = forwarder.forward(event);

        assertFalse(forwarded);
    }

    /**
     * The source list is sent as a structured array, without a serialized string envelope.
     */
    @Test
    public void forwardSerializesSourceAsJsonTest() {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok("ok"));

        PluginData pluginData = new PluginData();
        pluginData.setName("mockPlugin");
        List<PluginData> source = Collections.singletonList(pluginData);
        forwarder.forward(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE, source));

        ArgumentCaptor<HttpEntity<ClusterDataChangedEventPayload>> captor = ArgumentCaptor.forClass(HttpEntity.class);
        verify(restTemplate).postForEntity(any(String.class), captor.capture(), eq(String.class));
        assertTrue(Objects.nonNull(captor.getValue().getBody()));
        ClusterDataChangedEventPayload payload = captor.getValue().getBody();
        assertEquals(2, payload.getPayloadVersion());
        assertNull(payload.getSource());
        assertTrue(payload.getStructuredSource().isArray());
        assertEquals("mockPlugin", payload.getStructuredSource().get(0).get("name").asText());
    }

    /**
     * A structured event carries the same record content as the legacy string form.
     */
    @Test
    public void structuredAndLegacyEnvelopesCarryTheSameRecordsTest() throws Exception {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok("ok"));

        RuleData ruleData = new RuleData();
        ruleData.setName("structured-rule");
        ruleData.setHandle("{\"loadBalance\":\"roundRobin\"}");
        forwarder.forward(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.UPDATE,
                Collections.singletonList(ruleData)));
        ArgumentCaptor<HttpEntity<ClusterDataChangedEventPayload>> first = ArgumentCaptor.forClass(HttpEntity.class);
        verify(restTemplate).postForEntity(any(String.class), first.capture(), eq(String.class));

        ClusterDataChangedEventPayload structuredPayload = first.getValue().getBody();
        List<?> structured = new ObjectMapper().convertValue(
                structuredPayload.getStructuredSource(),
                new ObjectMapper().getTypeFactory()
                        .constructCollectionType(List.class, RuleData.class));
        List<?> legacy = GsonUtils.getInstance().fromList(
                GsonUtils.getInstance().toJson(Collections.singletonList(ruleData)), RuleData.class);
        assertEquals(legacy.get(0).getClass(), structured.get(0).getClass());
        assertEquals(((RuleData) legacy.get(0)).getName(), ((RuleData) structured.get(0)).getName());
        assertEquals(((RuleData) legacy.get(0)).getHandle(), ((RuleData) structured.get(0)).getHandle());
    }

    /**
     * A master that predates the structured envelope rejects it with 400; the event is
     * retried once with the legacy string envelope and the decision is remembered.
     */
    @Test
    public void forwardRetriesWithLegacyEnvelopeWhenStructuredIsRejectedTest() {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenThrow(org.springframework.web.client.HttpClientErrorException.create(
                        HttpStatus.BAD_REQUEST, "unsupported", org.springframework.http.HttpHeaders.EMPTY,
                        new byte[0], null))
                .thenReturn(ResponseEntity.ok("ok"));

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(new PluginData()));
        assertTrue(forwarder.forward(event));

        ArgumentCaptor<HttpEntity<ClusterDataChangedEventPayload>> captor = ArgumentCaptor.forClass(HttpEntity.class);
        verify(restTemplate, org.mockito.Mockito.times(2))
                .postForEntity(any(String.class), captor.capture(), eq(String.class));
        List<HttpEntity<ClusterDataChangedEventPayload>> attempts = captor.getAllValues();
        assertEquals(2, attempts.get(0).getBody().getPayloadVersion());
        assertEquals(1, attempts.get(1).getBody().getPayloadVersion());
        assertTrue(Objects.nonNull(attempts.get(1).getBody().getSource()));

        forwarder.forward(event);
        ArgumentCaptor<HttpEntity<ClusterDataChangedEventPayload>> legacyAttempt =
                ArgumentCaptor.forClass(HttpEntity.class);
        verify(restTemplate, org.mockito.Mockito.times(3))
                .postForEntity(any(String.class), legacyAttempt.capture(), eq(String.class));
        assertEquals(1, legacyAttempt.getAllValues().get(2).getBody().getPayloadVersion());
        assertTrue(Objects.nonNull(legacyAttempt.getAllValues().get(2).getBody().getSource()));
    }

    /**
     * A transport failure is not a format rejection and is not retried.
     */
    @Test
    public void forwardDoesNotRetryAfterTransportFailureTest() {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenThrow(new RestClientException("connection refused"));

        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(new PluginData()));
        assertFalse(forwarder.forward(event));
        verify(restTemplate, org.mockito.Mockito.times(1))
                .postForEntity(any(String.class), any(Object.class), eq(String.class));
    }

    /**
     * The structured source describes exactly the same records as the legacy Gson
     * string for every config group, so switching envelopes does not change content.
     */
    @Test
    public void structuredSourceMatchesLegacyContentForEveryGroupTest() throws Exception {
        ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost("10.0.0.2");
        master.setMasterPort("9095");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
        when(restTemplate.postForEntity(any(String.class), any(Object.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok("ok"));

        List<ConfigGroupEnum> groups = new java.util.ArrayList<>();
        List<List<?>> samples = new java.util.ArrayList<>();
        groups.add(ConfigGroupEnum.PLUGIN);
        samples.add(Collections.singletonList(samplePlugin()));
        groups.add(ConfigGroupEnum.RULE);
        samples.add(Collections.singletonList(sampleRule()));
        groups.add(ConfigGroupEnum.SELECTOR);
        samples.add(Collections.singletonList(sampleSelector()));
        groups.add(ConfigGroupEnum.APP_AUTH);
        samples.add(Collections.singletonList(sampleAppAuth()));
        groups.add(ConfigGroupEnum.META_DATA);
        samples.add(Collections.singletonList(sampleMetaData()));
        groups.add(ConfigGroupEnum.PROXY_SELECTOR);
        samples.add(Collections.singletonList(sampleProxySelector()));
        groups.add(ConfigGroupEnum.AI_PROXY_API_KEY);
        samples.add(Collections.singletonList(sampleProxyApiKey()));
        groups.add(ConfigGroupEnum.DISCOVER_UPSTREAM);
        samples.add(Collections.singletonList(sampleDiscoverySyncData()));

        for (int i = 0; i < groups.size(); i++) {
            forwarder.forward(new DataChangedEvent(groups.get(i), DataEventTypeEnum.UPDATE, samples.get(i)));
        }

        ArgumentCaptor<HttpEntity<ClusterDataChangedEventPayload>> captor = ArgumentCaptor.forClass(HttpEntity.class);
        verify(restTemplate, org.mockito.Mockito.times(groups.size()))
                .postForEntity(any(String.class), captor.capture(), eq(String.class));
        ObjectMapper mapper = new ObjectMapper();
        for (int i = 0; i < groups.size(); i++) {
            com.fasterxml.jackson.databind.JsonNode legacy = mapper.readTree(
                    GsonUtils.getInstance().toJson(samples.get(i)));
            assertEquals(legacy, captor.getAllValues().get(i).getBody().getStructuredSource(), groups.get(i).name());
        }
    }

    private PluginData samplePlugin() {
        PluginData pluginData = new PluginData();
        pluginData.setId("plugin-id");
        pluginData.setName("divide");
        pluginData.setConfig("{\"model\":\"black\"}");
        pluginData.setRole("admin");
        pluginData.setEnabled(true);
        pluginData.setSort(1);
        pluginData.setPluginJar("divide.jar");
        pluginData.setNamespaceId("namespace-a");
        return pluginData;
    }

    private RuleData sampleRule() {
        ConditionData condition = new ConditionData();
        condition.setParamType("uri");
        condition.setOperator("=");
        condition.setParamName("/http/**");
        condition.setParamValue("/http/**");
        return RuleData.builder()
                .id("rule-id")
                .name("rule")
                .pluginName("divide")
                .selectorId("selector-id")
                .matchMode(0)
                .sort(1)
                .enabled(true)
                .logged(false)
                .handle("{\"loadBalance\":\"roundRobin\"}")
                .conditionDataList(Collections.singletonList(condition))
                .matchRestful(false)
                .namespaceId("namespace-a")
                .build();
    }

    private SelectorData sampleSelector() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector-id");
        selectorData.setName("selector");
        selectorData.setPluginId("plugin-id");
        selectorData.setPluginName("divide");
        selectorData.setMatchMode(0);
        selectorData.setType(1);
        selectorData.setSort(1);
        selectorData.setEnabled(true);
        selectorData.setLogged(true);
        selectorData.setContinued(true);
        selectorData.setHandle("[]");
        selectorData.setNamespaceId("namespace-a");
        return selectorData;
    }

    private AppAuthData sampleAppAuth() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("app-key");
        appAuthData.setAppSecret("app-secret");
        appAuthData.setEnabled(true);
        appAuthData.setNamespaceId("namespace-a");
        return appAuthData;
    }

    private MetaData sampleMetaData() {
        MetaData metaData = new MetaData();
        metaData.setId("meta-id");
        metaData.setAppName("app");
        metaData.setContextPath("/app");
        metaData.setPath("/app/execute");
        metaData.setRpcType("http");
        metaData.setServiceName("execute");
        metaData.setMethodName("execute");
        metaData.setParameterTypes("int");
        metaData.setRpcExt("{}");
        metaData.setEnabled(true);
        metaData.setNamespaceId("namespace-a");
        return metaData;
    }

    private ProxySelectorData sampleProxySelector() {
        ProxySelectorData proxySelectorData = new ProxySelectorData();
        proxySelectorData.setId("proxy-id");
        proxySelectorData.setName("proxy");
        proxySelectorData.setPluginName("tcp");
        proxySelectorData.setType("tcp");
        proxySelectorData.setForwardPort(8080);
        proxySelectorData.setNamespaceId("namespace-a");
        return proxySelectorData;
    }

    private ProxyApiKeyData sampleProxyApiKey() {
        return ProxyApiKeyData.builder()
                .proxyApiKey("proxy-key")
                .realApiKey("real-key")
                .enabled(true)
                .namespaceId("namespace-a")
                .build();
    }

    private DiscoverySyncData sampleDiscoverySyncData() {
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setSelectorId("selector-id");
        discoverySyncData.setPluginName("divide");
        discoverySyncData.setSelectorName("selector");
        discoverySyncData.setNamespaceId("namespace-a");
        discoverySyncData.setDiscoveryHandlerId("handler-id");
        return discoverySyncData;
    }
}
