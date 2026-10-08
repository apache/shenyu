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

package org.apache.shenyu.k8s.parser;

import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1IngressTLS;
import io.kubernetes.client.openapi.models.V1IngressTLSBuilder;
import io.kubernetes.client.openapi.models.V1Secret;
import io.kubernetes.client.openapi.models.V1SecretBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1ServiceBuilder;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.WebSocketUpstream;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for {@link WebSocketParser}.
 */
public class WebSocketParserTest {

    private static final String NAMESPACE = "ws-namespace";

    private static final String SERVICE_NAME = "ws-service";

    private Indexer<V1Service> serviceIndexer;

    private Indexer<V1Endpoints> endpointsIndexer;

    private Lister<V1Service> serviceLister;

    private Lister<V1Endpoints> endpointsLister;

    @BeforeEach
    @SuppressWarnings("unchecked")
    public void setUp() {
        serviceIndexer = mock(Indexer.class);
        endpointsIndexer = mock(Indexer.class);
        serviceLister = new Lister<>(serviceIndexer);
        endpointsLister = new Lister<>(endpointsIndexer);
    }

    @Test
    public void testParseIngressRuleToWebSocketSelectorAndRule() {
        mockEndpoints("10.0.0.1");

        V1Ingress ingress = createIngress(createRule("www.example.com", "/ws", "Prefix", SERVICE_NAME, 8001),
                Collections.emptyMap(), null);

        ShenyuMemoryConfig config = new WebSocketParser(serviceLister, endpointsLister).parse(ingress, null);

        List<IngressConfiguration> routeConfigList = config.getRouteConfigList();
        assertNotNull(routeConfigList);
        assertEquals(1, routeConfigList.size());

        SelectorData selectorData = routeConfigList.get(0).getSelectorData();
        assertNotNull(selectorData);
        assertEquals(PluginEnum.WEB_SOCKET.getName(), selectorData.getPluginName());
        assertEquals(String.valueOf(PluginEnum.WEB_SOCKET.getCode()), selectorData.getPluginId());
        assertEquals("/ws", selectorData.getName());
        assertEquals(2, selectorData.getConditionList().size());
        assertEquals("www.example.com", selectorData.getConditionList().get(0).getParamValue());
        assertEquals(ParamTypeEnum.DOMAIN.getName(), selectorData.getConditionList().get(0).getParamType());
        assertEquals(OperatorEnum.EQ.getAlias(), selectorData.getConditionList().get(0).getOperator());
        assertEquals("/ws", selectorData.getConditionList().get(1).getParamValue());
        assertEquals(ParamTypeEnum.URI.getName(), selectorData.getConditionList().get(1).getParamType());
        assertEquals(OperatorEnum.STARTS_WITH.getAlias(), selectorData.getConditionList().get(1).getOperator());

        List<WebSocketUpstream> upstreams = GsonUtils.getInstance().fromList(selectorData.getHandle(), WebSocketUpstream.class);
        assertEquals(1, upstreams.size());
        assertEquals("10.0.0.1:8001", upstreams.get(0).getUpstreamUrl());
        assertEquals("ws://", upstreams.get(0).getProtocol());
        assertEquals(100, upstreams.get(0).getWeight());

        List<RuleData> ruleDataList = routeConfigList.get(0).getRuleDataList();
        assertEquals(1, ruleDataList.size());
        assertEquals("/ws", ruleDataList.get(0).getName());
        assertEquals(PluginEnum.WEB_SOCKET.getName(), ruleDataList.get(0).getPluginName());
        assertThat(ruleDataList.get(0).getHandle(), containsString("\"timeout\":3000"));
    }

    @Test
    public void testPathTypeToOperatorMapping() {
        mockEndpoints("10.0.0.1");

        assertEquals(OperatorEnum.EQ.getAlias(), parseFirstPathConditionOperator("Exact"));
        assertEquals(OperatorEnum.STARTS_WITH.getAlias(), parseFirstPathConditionOperator("Prefix"));
        assertEquals(OperatorEnum.MATCH.getAlias(), parseFirstPathConditionOperator("ImplementationSpecific"));
        assertEquals(OperatorEnum.MATCH.getAlias(), parseFirstPathConditionOperator("Unknown"));
    }

    @Test
    public void testParseIngressRuleWithoutHost() {
        mockEndpoints("10.0.0.1");

        V1Ingress ingress = createIngress(createRule(null, "/ws", "Prefix", SERVICE_NAME, 8001),
                Collections.emptyMap(), null);

        ShenyuMemoryConfig config = new WebSocketParser(serviceLister, endpointsLister).parse(ingress, null);

        List<ConditionData> conditionList = config.getRouteConfigList().get(0).getSelectorData().getConditionList();
        assertEquals(1, conditionList.size());
        assertEquals(ParamTypeEnum.URI.getName(), conditionList.get(0).getParamType());
    }

    @Test
    public void testParseIngressRuleSkipsPathWithoutPath() {
        V1IngressRule rule = new V1IngressRuleBuilder()
                .withNewHttp().withPaths(new V1HTTPIngressPathBuilder().withNewBackend().withNewService()
                        .withName(SERVICE_NAME).withNewPort().withNumber(8001).endPort().endService().endBackend().build())
                .endHttp().build();
        V1Ingress ingress = createIngress(rule, Collections.emptyMap(), null);

        ShenyuMemoryConfig config = new WebSocketParser(serviceLister, endpointsLister).parse(ingress, null);

        List<IngressConfiguration> routeConfigList = config.getRouteConfigList();
        assertNotNull(routeConfigList);
        assertEquals(0, routeConfigList.size());
    }

    @Test
    public void testParseGlobalDefaultBackendWithServiceProtocolAnnotation() {
        mockEndpoints("10.0.0.1", "10.0.0.2");
        Map<String, String> serviceAnnotations = new HashMap<>();
        serviceAnnotations.put(IngressConstants.UPSTREAMS_PROTOCOL_ANNOTATION_KEY, "ws://,wss://");
        mockService(serviceAnnotations);

        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("ws-ingress").withNamespace(NAMESPACE).withAnnotations(Collections.emptyMap()).endMetadata()
                .withNewSpec()
                .withNewDefaultBackend().withNewService().withName(SERVICE_NAME).withNewPort().withNumber(8001).endPort().endService().endDefaultBackend()
                .endSpec()
                .build();

        ShenyuMemoryConfig config = new WebSocketParser(serviceLister, endpointsLister).parse(ingress, null);

        assertNotNull(config.getGlobalDefaultBackend());
        SelectorData selectorData = config.getGlobalDefaultBackend().getRight().getSelectorData();
        assertEquals(PluginEnum.WEB_SOCKET.getName(), selectorData.getPluginName());
        assertEquals("default-selector", selectorData.getName());

        List<WebSocketUpstream> upstreams = GsonUtils.getInstance().fromList(selectorData.getHandle(), WebSocketUpstream.class);
        assertEquals(2, upstreams.size());
        assertEquals("10.0.0.1:8001", upstreams.get(0).getUpstreamUrl());
        assertEquals("ws://", upstreams.get(0).getProtocol());
        assertEquals("10.0.0.2:8001", upstreams.get(1).getUpstreamUrl());
        assertEquals("wss://", upstreams.get(1).getProtocol());
    }

    @Test
    public void testParseIngressRuleFallsBackToEmptyUpstreamWhenEndpointsMissing() {
        V1Ingress ingress = createIngress(createRule(null, "/ws", "Prefix", SERVICE_NAME, 8001),
                Collections.emptyMap(), null);

        ShenyuMemoryConfig config = new WebSocketParser(serviceLister, endpointsLister).parse(ingress, null);

        assertEquals("[]", config.getRouteConfigList().get(0).getSelectorData().getHandle());
    }

    @Test
    public void testParseIngressRuleUsesDefaultUpstreamFromDefaultBackend() {
        mockEndpoints("10.0.0.1");
        mockService(Collections.emptyMap());

        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("ws-ingress").withNamespace(NAMESPACE).withAnnotations(Collections.emptyMap()).endMetadata()
                .withNewSpec()
                .withNewDefaultBackend().withNewService().withName(SERVICE_NAME).withNewPort().withNumber(8001).endPort().endService().endDefaultBackend()
                .withRules(createRule(null, "/ws", "Prefix", "unknown-service", 8002))
                .endSpec()
                .build();

        ShenyuMemoryConfig config = new WebSocketParser(serviceLister, endpointsLister).parse(ingress, null);

        List<WebSocketUpstream> upstreams = GsonUtils.getInstance()
                .fromList(config.getRouteConfigList().get(0).getSelectorData().getHandle(), WebSocketUpstream.class);
        assertEquals(1, upstreams.size());
        assertEquals("10.0.0.1:8001", upstreams.get(0).getUpstreamUrl());
    }

    @Test
    public void testParseTlsConfigurations() throws Exception {
        mockEndpoints("10.0.0.1");
        CoreV1Api coreV1Api = mock(CoreV1Api.class);
        Map<String, byte[]> secretData = new HashMap<>();
        secretData.put("tls.crt", "crt".getBytes());
        secretData.put("tls.key", "key".getBytes());
        V1Secret secret = new V1SecretBuilder()
                .withNewMetadata().withName("ws-tls-secret").withNamespace(NAMESPACE).endMetadata()
                .withData(secretData)
                .build();
        when(coreV1Api.readNamespacedSecret(eq("ws-tls-secret"), eq(NAMESPACE), anyString())).thenReturn(secret);

        V1Ingress ingress = createIngress(createRule(null, "/ws", "Prefix", SERVICE_NAME, 8001),
                Collections.emptyMap(),
                new V1IngressTLSBuilder().withHosts(Arrays.asList("www.example.com")).withSecretName("ws-tls-secret").build());

        ShenyuMemoryConfig config = new WebSocketParser(serviceLister, endpointsLister).parse(ingress, coreV1Api);

        assertNotNull(config.getTlsConfigList());
        assertEquals(1, config.getTlsConfigList().size());
        assertEquals("www.example.com", config.getTlsConfigList().get(0).getDomain());
    }

    @Test
    public void testParseWithoutSpecReturnsEmptyConfig() {
        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("ws-ingress").withNamespace(NAMESPACE).endMetadata()
                .build();

        ShenyuMemoryConfig config = new WebSocketParser(serviceLister, endpointsLister).parse(ingress, null);

        assertNull(config.getRouteConfigList());
        assertNull(config.getGlobalDefaultBackend());
    }

    private String parseFirstPathConditionOperator(final String pathType) {
        V1Ingress ingress = createIngress(createRule(null, "/ws", pathType, SERVICE_NAME, 8001),
                Collections.emptyMap(), null);
        ShenyuMemoryConfig config = new WebSocketParser(serviceLister, endpointsLister).parse(ingress, null);
        return config.getRouteConfigList().get(0).getSelectorData().getConditionList().get(0).getOperator();
    }

    private void mockEndpoints(final String... ips) {
        V1EndpointAddress[] addresses = Arrays.stream(ips).map(ip -> new V1EndpointAddress().ip(ip)).toArray(V1EndpointAddress[]::new);
        V1Endpoints endpoints = new V1EndpointsBuilder()
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(addresses).build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(endpoints);
    }

    private void mockService(final Map<String, String> annotations) {
        when(serviceIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(new V1ServiceBuilder()
                .withNewMetadata().withName(SERVICE_NAME).withNamespace(NAMESPACE).withAnnotations(annotations).endMetadata()
                .withKind("Service").build());
    }

    private V1IngressRule createRule(final String host, final String path, final String pathType,
                                     final String serviceName, final int port) {
        V1HTTPIngressPathBuilder ingressPathBuilder = new V1HTTPIngressPathBuilder()
                .withPath(path)
                .withPathType(pathType)
                .withNewBackend()
                    .withNewService().withName(serviceName).withNewPort().withNumber(port).endPort().endService()
                .endBackend();
        return new V1IngressRuleBuilder().withHost(host).withNewHttp().withPaths(ingressPathBuilder.build()).endHttp().build();
    }

    private V1Ingress createIngress(final V1IngressRule rule, final Map<String, String> annotations, final V1IngressTLS tls) {
        List<V1IngressTLS> tlsList = Objects.isNull(tls) ? Collections.emptyList() : Collections.singletonList(tls);
        return new V1IngressBuilder()
                .withNewMetadata().withName("ws-ingress").withNamespace(NAMESPACE).withAnnotations(annotations).endMetadata()
                .withNewSpec().withRules(rule).withTls(tlsList).endSpec()
                .withKind("Ingress")
                .build();
    }
}
