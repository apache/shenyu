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
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.openapi.models.V1HTTPIngressPath;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link IngressParser}.
 */
public final class IngressParserTest {

    private static final String NAMESPACE = "default";

    private static final String SERVICE_NAME = "rpc-service";

    /**
     * Test parsing a Dubbo ingress does not create a context path configuration.
     */
    @Test
    public void testParseDubboIngressWithoutContextPathConfiguration() {
        assertRpcIngressConfiguration(IngressConstants.PLUGIN_DUBBO_ENABLED, PluginEnum.DUBBO);
    }

    /**
     * Test parsing a Sofa ingress does not create a context path configuration.
     */
    @Test
    public void testParseSofaIngressWithoutContextPathConfiguration() {
        assertRpcIngressConfiguration(IngressConstants.PLUGIN_SOFA_ENABLED, PluginEnum.SOFA);
    }

    private void assertRpcIngressConfiguration(final String enabledAnnotation, final PluginEnum expectedPlugin) {
        SharedIndexInformer<V1Service> serviceInformer = mock(SharedIndexInformer.class);
        Indexer<V1Service> serviceIndexer = mock(Indexer.class);
        when(serviceInformer.getIndexer()).thenReturn(serviceIndexer);

        SharedIndexInformer<V1Endpoints> endpointsInformer = mock(SharedIndexInformer.class);
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);
        V1Endpoints endpoints = new V1EndpointsBuilder()
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata()
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(endpoints);

        Map<String, String> annotations = new HashMap<>();
        annotations.put(enabledAnnotation, Boolean.TRUE.toString());
        annotations.put(IngressConstants.UPSTREAMS_PROTOCOL_ANNOTATION_KEY, "dubbo://");
        V1HTTPIngressPath path = new V1HTTPIngressPathBuilder()
                .withPath("/rpc")
                .withPathType("Prefix")
                .withNewBackend()
                    .withNewService()
                        .withName(SERVICE_NAME)
                        .withNewPort().withNumber(20880).endPort()
                    .endService()
                .endBackend()
                .build();
        V1IngressRule rule = new V1IngressRuleBuilder().withNewHttp().withPaths(path).endHttp().build();
        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata()
                    .withName("rpc-ingress")
                    .withNamespace(NAMESPACE)
                    .withAnnotations(annotations)
                    .withLabels(new HashMap<>())
                .endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .build();

        List<ShenyuMemoryConfig> configs = new IngressParser(serviceInformer, endpointsInformer).parse(ingress, null);

        assertEquals(1, configs.size());
        assertEquals(expectedPlugin.getName(), configs.get(0).getRouteConfigList().get(0).getSelectorData().getPluginName());
    }
}
