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

package org.apache.shenyu.k8s;

import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Secret;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1ServiceBuilder;
import org.apache.shenyu.common.config.ssl.ShenyuSniAsyncMapping;
import org.apache.shenyu.k8s.parser.IngressParser;
import org.apache.shenyu.k8s.reconciler.IngressReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * gRPC Ingress Reconciler Test.
 */
public final class GrpcReconcilerTest {

    private SharedIndexInformer<V1Ingress> ingressInformer;

    private SharedIndexInformer<V1Secret> secretInformer;

    private ShenyuCacheRepository shenyuCacheRepository;

    private ShenyuSniAsyncMapping shenyuSniAsyncMapping;

    private SharedIndexInformer<V1Service> serviceInformer;

    private SharedIndexInformer<V1Endpoints> endpointsInformer;

    private IngressReconciler ingressReconciler;

    @BeforeEach
    public void init() {
        ingressInformer = mock(SharedIndexInformer.class);
        secretInformer = mock(SharedIndexInformer.class);
        shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        shenyuSniAsyncMapping = new ShenyuSniAsyncMapping();
        serviceInformer = mock(SharedIndexInformer.class);
        endpointsInformer = mock(SharedIndexInformer.class);

        final V1IngressRule mockedRule = new V1IngressRuleBuilder().withNewHttp().withPaths(
                        new V1HTTPIngressPathBuilder().withPath("/**")
                                .withNewBackend()
                                    .withNewService().withName("grpcTestService").withNewPort().withNumber(50051).endPort().endService()
                                .endBackend().build())
                .endHttp().build();
        Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");
        annotations.put("shenyu.apache.org/plugin-grpc-enabled", "true");
        Map<String, String> labels = new HashMap<>();
        labels.put("shenyu.apache.org/metadata-labels-1", "grpcTestService");
        Map<String, String> serviceAnnotations = new HashMap<>();
        serviceAnnotations.put("shenyu.apache.org/plugin-grpc-enabled", "true");
        serviceAnnotations.put("shenyu.apache.org/plugin-grpc-app-name", "grpc");
        serviceAnnotations.put("shenyu.apache.org/plugin-grpc-path", "/hello");
        serviceAnnotations.put("shenyu.apache.org/plugin-grpc-rpc-type", "grpc");
        serviceAnnotations.put("shenyu.apache.org/plugin-grpc-service-name", "org.apache.shenyu.examples.grpc.api.service.HelloService");
        serviceAnnotations.put("shenyu.apache.org/plugin-grpc-method-name", "sayHello");
        serviceAnnotations.put("shenyu.apache.org/plugin-grpc-params-type", "org.apache.shenyu.examples.grpc.api.service.HelloRequest");
        V1Service grpcTestService = new V1ServiceBuilder().withNewMetadata().withName("grpcTestService").withNamespace("mockedNamespace").withAnnotations(serviceAnnotations).endMetadata()
                .withNewSpec().endSpec().withKind("Service").build();
        V1Ingress mockedIngress = new V1IngressBuilder().withNewMetadata().withName("mockedIngress").withNamespace("mockedNamespace").withAnnotations(annotations).withLabels(labels).endMetadata()
                .withNewSpec().withRules(mockedRule).endSpec()
                .withKind("Ingress").build();
        final Indexer<V1Ingress> ingressIndexer = mock(Indexer.class);
        when(ingressIndexer.getByKey("mockedNamespace/mockedIngress")).thenReturn(mockedIngress);
        when(ingressInformer.getIndexer()).thenReturn(ingressIndexer);
        final Indexer<V1Service> serviceIndexer = mock(Indexer.class);
        when(serviceIndexer.getByKey("mockedNamespace/grpcTestService")).thenReturn(grpcTestService);
        when(serviceInformer.getIndexer()).thenReturn(serviceIndexer);

        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("mockedNamespace").withName("grpcTestService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/grpcTestService")).thenReturn(mockedEndpoints);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);

        IngressParser ingressParser = new IngressParser(serviceInformer, endpointsInformer);
        ApiClient apiClient = mock(ApiClient.class);

        ingressReconciler = new IngressReconciler(ingressInformer, secretInformer, shenyuCacheRepository,
                shenyuSniAsyncMapping, ingressParser, apiClient);
    }

    @Test
    public void testReconcileWithMissingEndpointsDoesNotThrow() {
        Indexer<V1Endpoints> emptyEndpointsIndexer = mock(Indexer.class);
        when(emptyEndpointsIndexer.getByKey("mockedNamespace/grpcTestService")).thenReturn(null);
        when(endpointsInformer.getIndexer()).thenReturn(emptyEndpointsIndexer);

        IngressParser ingressParser = new IngressParser(serviceInformer, endpointsInformer);
        ApiClient apiClient = mock(ApiClient.class);
        IngressReconciler reconciler = new IngressReconciler(ingressInformer, secretInformer, shenyuCacheRepository,
                shenyuSniAsyncMapping, ingressParser, apiClient);

        Assertions.assertDoesNotThrow(() -> reconciler.reconcile(new Request("mockedNamespace", "mockedIngress")));
    }
}
