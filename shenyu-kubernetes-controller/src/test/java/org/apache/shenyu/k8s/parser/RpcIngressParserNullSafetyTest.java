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
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1ServiceBuilder;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Null safety tests for RPC ingress parsers.
 */
public final class RpcIngressParserNullSafetyTest {

    private static final String NAMESPACE = "test-namespace";

    private static final String BACKEND_SERVICE = "backend-service";

    @Test
    public void shouldParseIngressWithExistingService() {
        Lister<V1Service> serviceLister = createServiceLister(new V1ServiceBuilder().withNewMetadata().withName(BACKEND_SERVICE)
                .withNamespace(NAMESPACE).withAnnotations(Collections.emptyMap()).endMetadata().build());
        List<K8sResourceParser<V1Ingress>> parsers = createParsers(serviceLister);

        for (K8sResourceParser<V1Ingress> parser : parsers) {
            ShenyuMemoryConfig config = Assertions.assertDoesNotThrow(() -> parser.parse(createIngress(Collections.singletonMap(
                    "service-label", BACKEND_SERVICE)), null));
            Assertions.assertEquals(1, config.getRouteConfigList().size());
            Assertions.assertEquals(1, config.getRouteConfigList().get(0).getRuleDataList().size());
        }
    }

    @Test
    public void shouldSkipMissingServiceReferencedByLabel() {
        Lister<V1Service> serviceLister = createServiceLister(null);
        List<K8sResourceParser<V1Ingress>> parsers = createParsers(serviceLister);

        for (K8sResourceParser<V1Ingress> parser : parsers) {
            ShenyuMemoryConfig config = Assertions.assertDoesNotThrow(() -> parser.parse(createIngress(Collections.singletonMap(
                    "missing-service-label", "missing-service")), null));
            Assertions.assertEquals(1, config.getRouteConfigList().size());
            Assertions.assertTrue(config.getRouteConfigList().get(0).getRuleDataList().isEmpty());
            Assertions.assertTrue(config.getRouteConfigList().get(0).getMetaDataList().isEmpty());
        }
    }

    @Test
    public void shouldIgnoreNullIngressLabels() {
        Lister<V1Service> serviceLister = createServiceLister(null);
        List<K8sResourceParser<V1Ingress>> parsers = createParsers(serviceLister);

        for (K8sResourceParser<V1Ingress> parser : parsers) {
            ShenyuMemoryConfig config = Assertions.assertDoesNotThrow(() -> parser.parse(createIngress(null), null));
            Assertions.assertNotNull(config.getRouteConfigList());
            Assertions.assertTrue(config.getRouteConfigList().isEmpty());
        }
    }

    private List<K8sResourceParser<V1Ingress>> createParsers(final Lister<V1Service> serviceLister) {
        Lister<V1Endpoints> endpointsLister = createEndpointsLister();
        return Arrays.asList(new GrpcParser(serviceLister, endpointsLister),
                new DubboIngressParser(serviceLister, endpointsLister), new SofaParser(serviceLister, endpointsLister));
    }

    private Lister<V1Service> createServiceLister(final V1Service service) {
        Indexer<V1Service> indexer = mock(Indexer.class);
        when(indexer.getByKey(NAMESPACE + "/" + BACKEND_SERVICE)).thenReturn(service);
        return new Lister<>(indexer);
    }

    private Lister<V1Endpoints> createEndpointsLister() {
        Indexer<V1Endpoints> indexer = mock(Indexer.class);
        V1Endpoints endpoints = new V1EndpointsBuilder().withNewMetadata().withName(BACKEND_SERVICE).withNamespace(NAMESPACE)
                .endMetadata().withSubsets(new V1EndpointSubsetBuilder()
                        .withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build()).build();
        when(indexer.getByKey(NAMESPACE + "/" + BACKEND_SERVICE)).thenReturn(endpoints);
        return new Lister<>(indexer);
    }

    private V1Ingress createIngress(final Map<String, String> labels) {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.UPSTREAMS_PROTOCOL_ANNOTATION_KEY, "dubbo://,dubbo://");
        V1Ingress ingress = new V1IngressBuilder().withNewMetadata().withName("test-ingress").withNamespace(NAMESPACE)
                .withAnnotations(annotations).endMetadata()
                .withNewSpec().withRules(new V1IngressRuleBuilder().withNewHttp().withPaths(
                        new V1HTTPIngressPathBuilder().withPath("/test").withPathType("Prefix").withNewBackend()
                                .withNewService().withName(BACKEND_SERVICE).withNewPort().withNumber(8080)
                                .endPort().endService().endBackend().build()).endHttp().build()).endSpec()
                .build();
        ingress.getMetadata().setLabels(labels);
        return ingress;
    }
}
