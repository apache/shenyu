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
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Secret;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.common.config.ssl.ShenyuSniAsyncMapping;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.cache.IngressCache;
import org.apache.shenyu.k8s.cache.IngressSelectorCache;
import org.apache.shenyu.k8s.cache.ServiceIngressCache;
import org.apache.shenyu.k8s.parser.IngressParser;
import org.apache.shenyu.k8s.reconciler.IngressReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for reconciling an ingress that does not carry any annotation, such an ingress selects
 * the controller with spec.ingressClassName instead of the kubernetes.io/ingress.class annotation.
 */
public final class IngressReconcilerMissingAnnotationsTest {

    private static final String NAMESPACE = "missing-annotations-ns";

    private static final String INGRESS_NAME = "missing-annotations-ingress";

    private static final String SERVICE_NAME = "missing-annotations-service";

    private Indexer<V1Ingress> ingressIndexer;

    private ShenyuCacheRepository shenyuCacheRepository;

    private IngressReconciler ingressReconciler;

    @BeforeEach
    public void init() {
        final SharedIndexInformer<V1Ingress> ingressInformer = mock(SharedIndexInformer.class);
        final SharedIndexInformer<V1Secret> secretInformer = mock(SharedIndexInformer.class);
        final SharedIndexInformer<V1Service> serviceInformer = mock(SharedIndexInformer.class);
        final SharedIndexInformer<V1Endpoints> endpointsInformer = mock(SharedIndexInformer.class);
        shenyuCacheRepository = mock(ShenyuCacheRepository.class);

        ingressIndexer = mock(Indexer.class);
        V1IngressRule mockedRule = new V1IngressRuleBuilder().withNewHttp().withPaths(
                        new V1HTTPIngressPathBuilder().withPath("/**").withPathType("ImplementationSpecific")
                                .withNewBackend()
                                    .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(8189).endPort().endService()
                                .endBackend().build())
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
                        .withPorts(new CoreV1EndpointPort().port(8189).protocol("TCP"))
                        .build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(mockedEndpoints);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);

        IngressParser ingressParser = new IngressParser(serviceInformer, endpointsInformer);
        ingressReconciler = new IngressReconciler(ingressInformer, secretInformer, shenyuCacheRepository,
                new ShenyuSniAsyncMapping(), ingressParser, mock(ApiClient.class));
    }

    /**
     * test reconcile an ingress without annotations.
     */
    @Test
    public void testReconcileIngressWithoutAnnotations() {
        Result result = Assertions.assertDoesNotThrow(
                () -> ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME)));

        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, atLeastOnce()).saveOrUpdateSelectorData(any());
        Assertions.assertNotNull(IngressCache.getInstance().get(NAMESPACE, INGRESS_NAME));
        Assertions.assertFalse(ServiceIngressCache.getInstance().getIngressName(NAMESPACE, SERVICE_NAME).isEmpty());
    }

    /**
     * test reconcile the deletion of an ingress without annotations.
     */
    @Test
    public void testReconcileDeletedIngressWithoutAnnotations() {
        ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME));
        when(ingressIndexer.getByKey(NAMESPACE + "/" + INGRESS_NAME)).thenReturn(null);
        when(shenyuCacheRepository.findRuleDataList(anyString())).thenReturn(Collections.emptyList());

        Result result = Assertions.assertDoesNotThrow(
                () -> ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME)));

        Assertions.assertEquals(new Result(false), result);
        Assertions.assertNull(IngressCache.getInstance().get(NAMESPACE, INGRESS_NAME));
        Assertions.assertNull(IngressSelectorCache.getInstance().get(NAMESPACE, INGRESS_NAME, PluginEnum.DIVIDE.getName()));
        Assertions.assertTrue(ServiceIngressCache.getInstance().getIngressName(NAMESPACE, SERVICE_NAME).isEmpty());
        verify(shenyuCacheRepository).deleteSelectorData(eq(PluginEnum.DIVIDE.getName()), anyString());
    }
}
