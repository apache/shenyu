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
import io.kubernetes.client.openapi.models.CoreV1EndpointPort;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.openapi.models.V1HTTPIngressPath;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Secret;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.common.config.ssl.ShenyuSniAsyncMapping;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.cache.IngressSelectorCache;
import org.apache.shenyu.k8s.cache.ServiceIngressCache;
import org.apache.shenyu.k8s.common.ServiceIngressRelation;
import org.apache.shenyu.k8s.parser.IngressParser;
import org.apache.shenyu.k8s.reconciler.IngressReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for an ingress that routes several paths to the same service while the paths select
 * different service ports.
 */
public final class IngressReconcilerMultiPortPathsTest {

    private static final String NAMESPACE = "multi-port-paths-ns";

    private static final String INGRESS_NAME = "multi-port-paths-ingress";

    private static final String SERVICE_NAME = "multi-port-paths-service";

    private static final int FIRST_PORT = 8001;

    private static final int SECOND_PORT = 8002;

    private IngressReconciler ingressReconciler;

    @BeforeEach
    public void init() {
        final SharedIndexInformer<V1Ingress> ingressInformer = mock(SharedIndexInformer.class);
        final SharedIndexInformer<V1Secret> secretInformer = mock(SharedIndexInformer.class);
        final SharedIndexInformer<V1Service> serviceInformer = mock(SharedIndexInformer.class);
        final SharedIndexInformer<V1Endpoints> endpointsInformer = mock(SharedIndexInformer.class);
        final ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);

        Indexer<V1Ingress> ingressIndexer = mock(Indexer.class);
        V1IngressRule mockedRule = new V1IngressRuleBuilder().withNewHttp().withPaths(
                        path("/first-api", FIRST_PORT),
                        path("/second-api", SECOND_PORT))
                .endHttp().build();
        V1Ingress mockedIngress = new V1IngressBuilder().withNewMetadata().withName(INGRESS_NAME).withNamespace(NAMESPACE).endMetadata()
                .withNewSpec().withIngressClassName("shenyu").withRules(mockedRule).endSpec()
                .withKind("Ingress").build();
        when(ingressIndexer.getByKey(NAMESPACE + "/" + INGRESS_NAME)).thenReturn(mockedIngress);
        when(ingressInformer.getIndexer()).thenReturn(ingressIndexer);

        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder()
                        .withAddresses(new V1EndpointAddress().ip("127.0.0.1"))
                        .withPorts(new CoreV1EndpointPort().port(FIRST_PORT).protocol("TCP"),
                                new CoreV1EndpointPort().port(SECOND_PORT).protocol("TCP"))
                        .build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(mockedEndpoints);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);

        IngressParser ingressParser = new IngressParser(serviceInformer, endpointsInformer);
        ingressReconciler = new IngressReconciler(ingressInformer, secretInformer, shenyuCacheRepository,
                new ShenyuSniAsyncMapping(), ingressParser, mock(ApiClient.class));
    }

    /**
     * test that the paths of an ingress are cached as a single service relation that keeps the port of
     * the first path, the limitation documented on IngressReconciler#parseServiceFromIngress.
     */
    @Test
    public void testOnlyThePortOfTheFirstPathIsCached() {
        Result result = Assertions.assertDoesNotThrow(
                () -> ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME)));

        Assertions.assertEquals(new Result(false), result);
        List<String> selectorIds = IngressSelectorCache.getInstance().get(NAMESPACE, INGRESS_NAME, PluginEnum.DIVIDE.getName());
        Assertions.assertEquals(2, selectorIds.size());
        List<ServiceIngressRelation> relations = ServiceIngressCache.getInstance().getIngressName(NAMESPACE, SERVICE_NAME);
        Assertions.assertEquals(1, relations.size());
        Assertions.assertEquals(INGRESS_NAME, relations.get(0).getIngressName());
        Assertions.assertEquals(Integer.valueOf(FIRST_PORT), relations.get(0).getPort().getNumber());
    }

    private static V1HTTPIngressPath path(final String path, final int port) {
        return new V1HTTPIngressPathBuilder().withPath(path).withPathType("ImplementationSpecific")
                .withNewBackend()
                    .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(port).endPort().endService()
                .endBackend().build();
    }
}
