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
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1ServiceBuilder;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for {@link GrpcParser}.
 */
public class GrpcParserTest {

    private static final String NAMESPACE = "grpc-namespace";

    private static final String SERVICE_NAME = "grpc-service";

    private static final String METADATA_SERVICE_NAME = "grpc-metadata-service";

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
    public void testParseIngressRuleWithMetadataLabels() {
        mockEndpoints("10.0.0.1");
        Map<String, String> metadataAnnotations = new HashMap<>();
        metadataAnnotations.put(IngressConstants.PLUGIN_GRPC_APP_NAME, "grpc-app");
        metadataAnnotations.put(IngressConstants.PLUGIN_GRPC_PATH, "/grpc/hello");
        metadataAnnotations.put(IngressConstants.PLUGIN_GRPC_RPC_TYPE, "grpc");
        metadataAnnotations.put(IngressConstants.PLUGIN_GRPC_SERVICE_NAME, "hello.HelloService");
        metadataAnnotations.put(IngressConstants.PLUGIN_GRPC_METHOD_NAME, "hello");
        metadataAnnotations.put(IngressConstants.PLUGIN_GRPC_PARAMS_TYPE, "hello.HelloRequest");
        metadataAnnotations.put(IngressConstants.PLUGIN_GRPC_RPC_EXPAND, "{\"timeout\":5000}");
        mockService(METADATA_SERVICE_NAME, metadataAnnotations);

        Map<String, String> labels = new HashMap<>();
        labels.put("shenyu.apache.org/metadata-labels-1", METADATA_SERVICE_NAME);
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.LOADBALANCER_ANNOTATION_KEY, "hash");

        V1Ingress ingress = createIngress(annotations, labels, createRule("www.example.com", "/grpc", "Prefix"));

        ShenyuMemoryConfig config = new GrpcParser(serviceLister, endpointsLister).parse(ingress, null);

        List<IngressConfiguration> routeConfigList = config.getRouteConfigList();
        assertNotNull(routeConfigList);
        assertEquals(1, routeConfigList.size());

        SelectorData selectorData = routeConfigList.get(0).getSelectorData();
        assertEquals(PluginEnum.GRPC.getName(), selectorData.getPluginName());
        assertEquals(String.valueOf(PluginEnum.GRPC.getCode()), selectorData.getPluginId());
        assertEquals("/grpc", selectorData.getName());
        assertEquals(2, selectorData.getConditionList().size());
        assertEquals("www.example.com", selectorData.getConditionList().get(0).getParamValue());
        assertEquals(OperatorEnum.EQ.getAlias(), selectorData.getConditionList().get(0).getOperator());
        assertEquals(OperatorEnum.STARTS_WITH.getAlias(), selectorData.getConditionList().get(1).getOperator());

        List<GrpcUpstream> upstreams = GsonUtils.getInstance().fromList(selectorData.getHandle(), GrpcUpstream.class);
        assertEquals(1, upstreams.size());
        assertEquals("10.0.0.1:50051", upstreams.get(0).getUpstreamUrl());
        assertEquals(100, upstreams.get(0).getWeight());

        List<RuleData> ruleDataList = routeConfigList.get(0).getRuleDataList();
        assertEquals(1, ruleDataList.size());
        assertEquals("/grpc/hello", ruleDataList.get(0).getName());
        assertEquals(PluginEnum.GRPC.getName(), ruleDataList.get(0).getPluginName());
        assertEquals(1, ruleDataList.get(0).getConditionDataList().size());
        assertEquals(OperatorEnum.EQ.getAlias(), ruleDataList.get(0).getConditionDataList().get(0).getOperator());
        assertEquals("/grpc/hello", ruleDataList.get(0).getConditionDataList().get(0).getParamValue());

        List<MetaData> metaDataList = routeConfigList.get(0).getMetaDataList();
        assertEquals(1, metaDataList.size());
        assertEquals("grpc-app", metaDataList.get(0).getAppName());
        assertEquals("/grpc/hello", metaDataList.get(0).getPath());
        assertEquals("grpc", metaDataList.get(0).getRpcType());
        assertEquals("hello.HelloService", metaDataList.get(0).getServiceName());
        assertEquals("hello", metaDataList.get(0).getMethodName());
        assertEquals("hello.HelloRequest", metaDataList.get(0).getParameterTypes());
        assertTrue(metaDataList.get(0).getEnabled());
    }

    @Test
    public void testParseGlobalDefaultBackendDefaults() {
        mockEndpoints("10.0.0.1");

        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("grpc-ingress").withNamespace(NAMESPACE)
                .withAnnotations(new HashMap<>()).withLabels(new HashMap<>()).endMetadata()
                .withNewSpec()
                .withNewDefaultBackend().withNewService().withName(SERVICE_NAME).withNewPort().withNumber(50051).endPort().endService().endDefaultBackend()
                .endSpec()
                .withKind("Ingress")
                .build();

        ShenyuMemoryConfig config = new GrpcParser(serviceLister, endpointsLister).parse(ingress, null);

        assertNotNull(config.getGlobalDefaultBackend());
        IngressConfiguration defaultRouteConfig = config.getGlobalDefaultBackend().getRight();
        assertEquals("grpc-selector", defaultRouteConfig.getSelectorData().getName());
        assertEquals(PluginEnum.GRPC.getName(), defaultRouteConfig.getSelectorData().getPluginName());
        assertEquals("grpc-rule", defaultRouteConfig.getRuleDataList().get(0).getName());
        assertEquals("/**", defaultRouteConfig.getRuleDataList().get(0).getConditionDataList().get(0).getParamValue());
        assertEquals(OperatorEnum.PATH_PATTERN.getAlias(), defaultRouteConfig.getRuleDataList().get(0).getConditionDataList().get(0).getOperator());

        List<GrpcUpstream> upstreams = GsonUtils.getInstance()
                .fromList(defaultRouteConfig.getSelectorData().getHandle(), GrpcUpstream.class);
        assertEquals(1, upstreams.size());
        assertEquals("10.0.0.1:50051", upstreams.get(0).getUpstreamUrl());
        assertEquals(50, upstreams.get(0).getWeight());

        MetaData metaData = defaultRouteConfig.getMetaDataList().get(0);
        assertEquals("grpc", metaData.getAppName());
        assertEquals("/grpc/helloService/hello", metaData.getPath());
        assertEquals("grpc", metaData.getRpcType());
        assertEquals("hello.HelloService", metaData.getServiceName());
        assertEquals("/grpc", metaData.getContextPath());
        assertTrue(metaData.getEnabled());
    }

    @Test
    public void testParseIngressRuleWithoutMetadataLabels() {
        mockEndpoints("10.0.0.1");

        V1Ingress ingress = createIngress(new HashMap<>(), new HashMap<>(), createRule(null, "/grpc", "Prefix"));

        ShenyuMemoryConfig config = new GrpcParser(serviceLister, endpointsLister).parse(ingress, null);

        IngressConfiguration routeConfig = config.getRouteConfigList().get(0);
        assertEquals(0, routeConfig.getRuleDataList().size());
        assertEquals(0, routeConfig.getMetaDataList().size());
        List<GrpcUpstream> upstreams = GsonUtils.getInstance().fromList(routeConfig.getSelectorData().getHandle(), GrpcUpstream.class);
        assertEquals("10.0.0.1:50051", upstreams.get(0).getUpstreamUrl());
    }

    @Test
    public void testParseIngressRuleWithoutEndpointsSubsets() {
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(new V1EndpointsBuilder()
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata()
                .build());

        V1Ingress ingress = createIngress(new HashMap<>(), new HashMap<>(), createRule(null, "/grpc", "Prefix"));

        ShenyuMemoryConfig config = new GrpcParser(serviceLister, endpointsLister).parse(ingress, null);

        assertEquals("[]", config.getRouteConfigList().get(0).getSelectorData().getHandle());
    }

    @Test
    public void testParsePathTypeToOperatorMapping() {
        mockEndpoints("10.0.0.1");

        assertEquals(OperatorEnum.EQ.getAlias(), parsePathOperator("Exact"));
        assertEquals(OperatorEnum.STARTS_WITH.getAlias(), parsePathOperator("Prefix"));
        assertEquals(OperatorEnum.MATCH.getAlias(), parsePathOperator("ImplementationSpecific"));
        assertEquals(OperatorEnum.MATCH.getAlias(), parsePathOperator("Unknown"));
    }

    @Test
    public void testParseIngressWithoutSpec() {
        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("grpc-ingress").withNamespace(NAMESPACE).endMetadata()
                .build();

        ShenyuMemoryConfig config = new GrpcParser(serviceLister, endpointsLister).parse(ingress, null);

        assertNull(config.getRouteConfigList());
        assertNull(config.getGlobalDefaultBackend());
    }

    private String parsePathOperator(final String pathType) {
        V1Ingress ingress = createIngress(new HashMap<>(), new HashMap<>(), createRule(null, "/grpc", pathType));
        ShenyuMemoryConfig config = new GrpcParser(serviceLister, endpointsLister).parse(ingress, null);
        return config.getRouteConfigList().get(0).getSelectorData().getConditionList().get(0).getOperator();
    }

    private void mockEndpoints(final String... ips) {
        V1EndpointAddress[] addresses = new V1EndpointAddress[ips.length];
        for (int i = 0; i < ips.length; i++) {
            addresses[i] = new V1EndpointAddress().ip(ips[i]);
        }
        V1Endpoints endpoints = new V1EndpointsBuilder()
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(addresses).build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(endpoints);
    }

    private void mockService(final String serviceName, final Map<String, String> annotations) {
        when(serviceIndexer.getByKey(NAMESPACE + "/" + serviceName)).thenReturn(new V1ServiceBuilder()
                .withNewMetadata().withName(serviceName).withNamespace(NAMESPACE).withAnnotations(annotations).endMetadata()
                .withKind("Service").build());
    }

    private V1Ingress createIngress(final Map<String, String> annotations, final Map<String, String> labels, final V1IngressRule rule) {
        return new V1IngressBuilder()
                .withNewMetadata().withName("grpc-ingress").withNamespace(NAMESPACE)
                .withAnnotations(annotations).withLabels(labels).endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .withKind("Ingress")
                .build();
    }

    private V1IngressRule createRule(final String host, final String path, final String pathType) {
        return new V1IngressRuleBuilder().withHost(host).withNewHttp().withPaths(new V1HTTPIngressPathBuilder()
                        .withPath(path)
                        .withPathType(pathType)
                        .withNewBackend()
                            .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(50051).endPort().endService()
                        .endBackend()
                        .build())
                .endHttp().build();
    }
}
