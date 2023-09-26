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
import io.kubernetes.client.extended.controller.reconciler.Result;
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
import org.apache.shenyu.common.config.ssl.ShenyuSniAsyncMapping;
import org.apache.shenyu.k8s.parser.IngressParser;
import org.apache.shenyu.k8s.reconciler.IngressReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Spring Cloud Ingress Reconciler Test.
 */
public final class WebSocketReconcilerTest {

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

        // mock ingressInformer
        final Indexer<V1Ingress> ingressIndexer = mock(Indexer.class);
        //mock serviceInformer
        final Indexer<V1Service> serviceIndexer = mock(Indexer.class);

        final V1IngressRule mockedRule = new V1IngressRuleBuilder().withNewHttp().withPaths(
                        new V1HTTPIngressPathBuilder().withPath("/**")
                                .withNewBackend()
                                    .withNewService().withName("testService").withNewPort().withNumber(8001).endPort().endService()
                                .endBackend().build())
                .endHttp().build();
        Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");
        annotations.put("shenyu.apache.org/plugin-web-socket-enabled", "true");
        annotations.put("shenyu.apache.org/loadbalancer", "random");

        V1Ingress mockedIngress = new V1IngressBuilder().withNewMetadata().withName("mockedIngress").withNamespace("mockedNamespace").withAnnotations(annotations).endMetadata()
                .withNewSpec().withRules(mockedRule).endSpec()
                .withKind("Ingress").build();

        when(ingressIndexer.getByKey("mockedNamespace/mockedIngress")).thenReturn(mockedIngress);
        when(serviceInformer.getIndexer()).thenReturn(serviceIndexer);
        when(ingressInformer.getIndexer()).thenReturn(ingressIndexer);

        // mock endpointsLister
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);

        IngressParser ingressParser = new IngressParser(serviceInformer, endpointsInformer);
        ApiClient apiClient = mock(ApiClient.class);

        ingressReconciler = new IngressReconciler(ingressInformer, secretInformer, shenyuCacheRepository,
                shenyuSniAsyncMapping, ingressParser, apiClient);
    }

    /**
     * test reconcile.
     */
    @Test
    public void testReconcile() {
        Result result = ingressReconciler.reconcile(new Request("mockedNamespace", "mockedIngress"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository).saveOrUpdateRuleData(any());
    }
}
