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
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1ServiceBuilder;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for SofaParser null-safety.
 */
public class SofaParserTest {

    private static final String NAMESPACE = "testNamespace";

    private static final String SERVICE_NAME = "testService";

    private Lister<V1Service> newServiceLister() {
        final SharedIndexInformer<V1Service> informer = mock(SharedIndexInformer.class);
        final Indexer<V1Service> indexer = mock(Indexer.class);
        when(informer.getIndexer()).thenReturn(indexer);
        return new Lister<>(indexer);
    }

    private Lister<V1Endpoints> newEndpointsLister() {
        final SharedIndexInformer<V1Endpoints> informer = mock(SharedIndexInformer.class);
        final Indexer<V1Endpoints> indexer = mock(Indexer.class);
        when(informer.getIndexer()).thenReturn(indexer);
        return new Lister<>(indexer);
    }

    @Test
    public void testParseWithNullLabels() {
        final Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");

        final V1IngressRule rule = new V1IngressRuleBuilder()
                .withNewHttp()
                .withPaths(new V1HTTPIngressPathBuilder()
                        .withPath("/sofa/findById")
                        .withNewBackend()
                        .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(20888).endPort().endService()
                        .endBackend().build())
                .endHttp().build();

        final V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("testIngress").withNamespace(NAMESPACE)
                .withAnnotations(annotations).withLabels(null).endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .build();

        final SofaParser parser = new SofaParser(newServiceLister(), newEndpointsLister());
        final ShenyuMemoryConfig result = parser.parse(ingress, null);
        Assertions.assertNotNull(result);
    }

    @Test
    public void testParseWithMissingService() {
        final Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");

        final Map<String, String> labels = new HashMap<>();
        labels.put("shenyu.apache.org/metadata-labels-1", "nonExistentService");

        final V1IngressRule rule = new V1IngressRuleBuilder()
                .withNewHttp()
                .withPaths(new V1HTTPIngressPathBuilder()
                        .withPath("/sofa/findById")
                        .withNewBackend()
                        .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(20888).endPort().endService()
                        .endBackend().build())
                .endHttp().build();

        final V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("testIngress").withNamespace(NAMESPACE)
                .withAnnotations(annotations).withLabels(labels).endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .build();

        final SofaParser parser = new SofaParser(newServiceLister(), newEndpointsLister());
        final ShenyuMemoryConfig result = parser.parse(ingress, null);
        Assertions.assertNotNull(result);
    }

    @Test
    public void testParseWithValidLabelsAndService() {
        final SharedIndexInformer<V1Service> serviceInformer = mock(SharedIndexInformer.class);
        final Indexer<V1Service> serviceIndexer = mock(Indexer.class);
        when(serviceInformer.getIndexer()).thenReturn(serviceIndexer);

        final Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");

        final Map<String, String> labels = new HashMap<>();
        labels.put("shenyu.apache.org/metadata-labels-1", "sofaFindByIdService");

        final Map<String, String> serviceAnnotations = new HashMap<>();
        serviceAnnotations.put("shenyu.apache.org/plugin-sofa-app-name", "sofa");
        serviceAnnotations.put("shenyu.apache.org/plugin-sofa-path", "/sofa/findById");
        serviceAnnotations.put("shenyu.apache.org/plugin-sofa-rpc-type", "sofa");
        serviceAnnotations.put("shenyu.apache.org/plugin-sofa-service-name",
                "org.apache.shenyu.examples.sofa.api.service.SofaTestService");
        serviceAnnotations.put("shenyu.apache.org/plugin-sofa-method-name", "findById");
        serviceAnnotations.put("shenyu.apache.org/plugin-sofa-params-type", "java.lang.String");

        final V1Service sofaService = new V1ServiceBuilder()
                .withNewMetadata().withName("sofaFindByIdService").withNamespace(NAMESPACE)
                .withAnnotations(serviceAnnotations).endMetadata()
                .build();
        when(serviceIndexer.getByKey(NAMESPACE + "/sofaFindByIdService")).thenReturn(sofaService);

        final V1IngressRule rule = new V1IngressRuleBuilder()
                .withNewHttp()
                .withPaths(new V1HTTPIngressPathBuilder()
                        .withPath("/sofa/findById")
                        .withNewBackend()
                        .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(20888).endPort().endService()
                        .endBackend().build())
                .endHttp().build();

        final V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("testIngress").withNamespace(NAMESPACE)
                .withAnnotations(annotations).withLabels(labels).endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .build();

        final Lister<V1Service> serviceLister = new Lister<>(serviceIndexer);
        final Lister<V1Endpoints> endpointsLister = newEndpointsLister();
        final SofaParser parser = new SofaParser(serviceLister, endpointsLister);
        final ShenyuMemoryConfig result = parser.parse(ingress, null);
        Assertions.assertNotNull(result);
        Assertions.assertNotNull(result.getRouteConfigList());
        Assertions.assertFalse(result.getRouteConfigList().isEmpty());
    }
}
