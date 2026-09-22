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

import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Indexer;
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
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for {@link IngressParser}.
 */
public class IngressParserTest {

    private static final String NAMESPACE = "dispatch-namespace";

    private static final String SERVICE_NAME = "dispatch-service";

    private IngressParser ingressParser;

    @BeforeEach
    @SuppressWarnings("unchecked")
    public void setUp() {
        SharedIndexInformer<V1Service> serviceInformer = mock(SharedIndexInformer.class);
        when(serviceInformer.getIndexer()).thenReturn(mock(Indexer.class));

        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        SharedIndexInformer<V1Endpoints> endpointsInformer = mock(SharedIndexInformer.class);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);

        V1Endpoints endpoints = new V1EndpointsBuilder()
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("10.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(endpoints);

        ingressParser = new IngressParser(serviceInformer, endpointsInformer);
    }

    @Test
    public void testDispatchToDivideWhenNoPluginAnnotationEnabled() {
        List<ShenyuMemoryConfig> configs = parse(new HashMap<>(), "Exact");

        assertEquals(1, configs.size());
        assertEquals(Arrays.asList(PluginEnum.DIVIDE.getName()), pluginNames(configs));
    }

    @Test
    public void testDispatchAddsContextPathConfigForPrefixPath() {
        List<ShenyuMemoryConfig> configs = parse(new HashMap<>(), "Prefix");

        assertEquals(2, configs.size());
        List<String> pluginNames = pluginNames(configs);
        assertTrue(pluginNames.contains(PluginEnum.CONTEXT_PATH.getName()));
        assertTrue(pluginNames.contains(PluginEnum.DIVIDE.getName()));
    }

    @Test
    public void testDispatchToDubboWhenEnabled() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_DUBBO_ENABLED, "true");

        List<ShenyuMemoryConfig> configs = parse(annotations, "Exact");

        assertEquals(1, configs.size());
        assertEquals(Arrays.asList(PluginEnum.DUBBO.getName()), pluginNames(configs));
    }

    @Test
    public void testDispatchToWebSocketWhenEnabled() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_WEB_SOCKET_ENABLED, "true");

        List<ShenyuMemoryConfig> configs = parse(annotations, "Exact");

        assertEquals(1, configs.size());
        assertEquals(Arrays.asList(PluginEnum.WEB_SOCKET.getName()), pluginNames(configs));
    }

    @Test
    public void testDispatchToGrpcWhenEnabled() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_GRPC_ENABLED, "true");

        List<ShenyuMemoryConfig> configs = parse(annotations, "Exact");

        assertEquals(1, configs.size());
        assertEquals(Arrays.asList(PluginEnum.GRPC.getName()), pluginNames(configs));
    }

    @Test
    public void testDispatchToSofaWhenEnabled() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_SOFA_ENABLED, "true");

        List<ShenyuMemoryConfig> configs = parse(annotations, "Exact");

        assertEquals(1, configs.size());
        assertEquals(Arrays.asList(PluginEnum.SOFA.getName()), pluginNames(configs));
    }

    @Test
    public void testDispatchToDivideWhenAllPluginAnnotationsDisabled() {
        Map<String, String> annotations = new HashMap<>();
        for (String key : Arrays.asList(IngressConstants.PLUGIN_DUBBO_ENABLED, IngressConstants.PLUGIN_WEB_SOCKET_ENABLED,
                IngressConstants.PLUGIN_BRPC_ENABLED, IngressConstants.PLUGIN_GRPC_ENABLED, IngressConstants.PLUGIN_SOFA_ENABLED)) {
            annotations.put(key, "false");
        }

        List<ShenyuMemoryConfig> configs = parse(annotations, "Exact");

        assertEquals(1, configs.size());
        assertEquals(Arrays.asList(PluginEnum.DIVIDE.getName()), pluginNames(configs));
    }

    private List<ShenyuMemoryConfig> parse(final Map<String, String> annotations, final String pathType) {
        Map<String, String> allAnnotations = new HashMap<>();
        allAnnotations.put(IngressConstants.K8S_INGRESS_CLASS_ANNOTATION_KEY, IngressConstants.SHENYU_INGRESS_CLASS);
        allAnnotations.putAll(annotations);

        V1IngressRule rule = new V1IngressRuleBuilder().withNewHttp().withPaths(new V1HTTPIngressPathBuilder()
                        .withPath("/test")
                        .withPathType(pathType)
                        .withNewBackend()
                            .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(9090).endPort().endService()
                        .endBackend()
                        .build())
                .endHttp().build();
        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("dispatch-ingress").withNamespace(NAMESPACE)
                .withAnnotations(allAnnotations).withLabels(new HashMap<>()).endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .withKind("Ingress")
                .build();

        return ingressParser.parse(ingress, mock(CoreV1Api.class));
    }

    private List<String> pluginNames(final List<ShenyuMemoryConfig> configs) {
        return configs.stream()
                .map(ShenyuMemoryConfig::getRouteConfigList)
                .filter(Objects::nonNull)
                .flatMap(List::stream)
                .map(IngressConfiguration::getSelectorData)
                .filter(Objects::nonNull)
                .map(SelectorData::getPluginName)
                .collect(Collectors.toList());
    }
}
