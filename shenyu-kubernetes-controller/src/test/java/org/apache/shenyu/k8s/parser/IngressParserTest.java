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
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Tests for {@link IngressParser}.
 */
public final class IngressParserTest {

    private SharedIndexInformer<V1Service> serviceInformer;

    private SharedIndexInformer<V1Endpoints> endpointsInformer;

    private Indexer<V1Endpoints> endpointsIndexer;

    private IngressParser ingressParser;

    @BeforeEach
    @SuppressWarnings("unchecked")
    public void setUp() {
        serviceInformer = mock(SharedIndexInformer.class);
        endpointsInformer = mock(SharedIndexInformer.class);
        when(serviceInformer.getIndexer()).thenReturn(mock(Indexer.class));
        endpointsIndexer = mock(Indexer.class);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);
        ingressParser = new IngressParser(serviceInformer, endpointsInformer);
    }

    @Test
    public void parseShouldSkipContextPathConfigWhenSpecificPluginHasNoContextPathAnnotations() {
        mockEndpoints("demo-ns", "demo-service");

        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata()
                .withName("demo-ingress")
                .withNamespace("demo-ns")
                .addToAnnotations(IngressConstants.K8S_INGRESS_CLASS_ANNOTATION_KEY, IngressConstants.SHENYU_INGRESS_CLASS)
                .addToAnnotations(IngressConstants.PLUGIN_WEB_SOCKET_ENABLED, Boolean.TRUE.toString())
                .endMetadata()
                .withNewSpec()
                .withRules(new V1IngressRuleBuilder()
                        .withNewHttp()
                        .withPaths(new V1HTTPIngressPathBuilder()
                                .withPath("/ws")
                                .withPathType("Prefix")
                                .withNewBackend()
                                .withNewService()
                                .withName("demo-service")
                                .withNewPort()
                                .withNumber(8080)
                                .endPort()
                                .endService()
                                .endBackend()
                                .build())
                        .endHttp()
                        .build())
                .endSpec()
                .build();

        List<ShenyuMemoryConfig> configs = ingressParser.parse(ingress, mock(CoreV1Api.class));

        Assertions.assertEquals(1, configs.size());
        Assertions.assertEquals(PluginEnum.WEB_SOCKET.getName(),
                configs.get(0).getRouteConfigList().get(0).getSelectorData().getPluginName());
    }

    @Test
    public void parseShouldKeepExplicitContextPathConfig() {
        mockEndpoints("demo-ns", "demo-service");

        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata()
                .withName("demo-ingress")
                .withNamespace("demo-ns")
                .addToAnnotations(IngressConstants.K8S_INGRESS_CLASS_ANNOTATION_KEY, IngressConstants.SHENYU_INGRESS_CLASS)
                .addToAnnotations(IngressConstants.PLUGIN_CONTEXT_PATH_PATH, "/gateway")
                .addToAnnotations(IngressConstants.PLUGIN_CONTEXT_PATH_ADD_PREFIX, "/backend")
                .endMetadata()
                .withNewSpec()
                .withRules(new V1IngressRuleBuilder()
                        .withNewHttp()
                        .withPaths(new V1HTTPIngressPathBuilder()
                                .withPath("/gateway")
                                .withPathType("Prefix")
                                .withNewBackend()
                                .withNewService()
                                .withName("demo-service")
                                .withNewPort()
                                .withNumber(8080)
                                .endPort()
                                .endService()
                                .endBackend()
                                .build())
                        .endHttp()
                        .build())
                .endSpec()
                .build();

        List<ShenyuMemoryConfig> configs = ingressParser.parse(ingress, mock(CoreV1Api.class));

        Assertions.assertEquals(2, configs.size());
        Assertions.assertEquals(PluginEnum.CONTEXT_PATH.getName(),
                configs.get(0).getRouteConfigList().get(0).getSelectorData().getPluginName());
        Assertions.assertEquals(PluginEnum.DIVIDE.getName(),
                configs.get(1).getRouteConfigList().get(0).getSelectorData().getPluginName());
    }

    private void mockEndpoints(final String namespace, final String serviceName) {
        V1Endpoints endpoints = new V1EndpointsBuilder()
                .withNewMetadata()
                .withNamespace(namespace)
                .withName(serviceName)
                .endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder()
                        .withAddresses(new V1EndpointAddress().ip("127.0.0.1"))
                        .build())
                .build();
        when(endpointsIndexer.getByKey(namespace + "/" + serviceName)).thenReturn(endpoints);
    }
}
