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

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Dubbo Ingress Reconciler Test.
 */
public final class DubboReconcilerTest {

    private static final String NAMESPACE = "dubboReconcilerNamespace";

    private static final String INGRESS_NAME = "dubboReconcilerIngress";

    private static final String SERVICE_NAME = "dubboReconcilerService";

    private static final String ZOOKEEPER_SERVICE = "dubboReconcilerZookeeperService";

    private static final String METADATA_SERVICE = "dubboReconcilerMetadataService";

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
                                    .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(20888).endPort().endService()
                                .endBackend().build())
                .endHttp().build();
        Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");
        annotations.put("shenyu.apache.org/plugin-dubbo-enabled", "true");
        annotations.put("shenyu.apache.org/zookeeper-register-address", "zookeeper://" + ZOOKEEPER_SERVICE + ":2181");
        annotations.put("shenyu.apache.org/upstreams-protocol", "dubbo://,dubbo://");
        Map<String, String> labels = new HashMap<>();
        labels.put("shenyu.apache.org/metadata-labels-1", METADATA_SERVICE);
        Map<String, String> labelsAnnotations = new HashMap<>();
        labelsAnnotations.put("kubernetes.io/ingress.class", "shenyu");
        labelsAnnotations.put("shenyu.apache.org/plugin-dubbo-enabled", "true");
        labelsAnnotations.put("shenyu.apache.org/plugin-dubbo-app-name", "dubbo");
        labelsAnnotations.put("shenyu.apache.org/plugin-dubbo-path", "/findById");
        labelsAnnotations.put("shenyu.apache.org/plugin-dubbo-rpc-type", "dubbo");
        labelsAnnotations.put("shenyu.apache.org/plugin-dubbo-service-name", "org.apache.shenyu.examples.dubbo.api.service.DubboTestService");
        labelsAnnotations.put("shenyu.apache.org/plugin-dubbo-method-name", "findById");
        labelsAnnotations.put("shenyu.apache.org/plugin-dubbo-params-type", "java.lang.String");
        labelsAnnotations.put("shenyu.apache.org/plugin-dubbo-rpc-expand", "{\"group\":\"\",\"version\":\"v0.0.2\",\"loadbalance\":\"random\","
                    + "\"retries\":2,\"timeout\":10000,\"url\":\"\",\"sent\":false,\"cluster\":\"failover\",\"protocol\":\"dubbo\"}");
        V1Service dubboFindIdService = new V1ServiceBuilder().withNewMetadata().withName(METADATA_SERVICE).withNamespace(NAMESPACE).withAnnotations(labelsAnnotations).endMetadata()
                .withNewSpec().endSpec()
                .withKind("Service").build();

        V1Ingress mockedIngress = new V1IngressBuilder().withNewMetadata().withLabels(labels).withName(INGRESS_NAME).withNamespace(NAMESPACE).withAnnotations(annotations).endMetadata()
                .withNewSpec().withRules(mockedRule).endSpec()
                .withKind("Ingress").build();

        when(ingressIndexer.getByKey(NAMESPACE + "/" + INGRESS_NAME)).thenReturn(mockedIngress);
        when(serviceIndexer.getByKey(NAMESPACE + "/" + METADATA_SERVICE)).thenReturn(dubboFindIdService);
        when(serviceInformer.getIndexer()).thenReturn(serviceIndexer);
        when(ingressInformer.getIndexer()).thenReturn(ingressIndexer);

        //mock endpointsInformer
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        V1Endpoints zookeeperEndpoints = new V1EndpointsBuilder().withNewMetadata().withName(ZOOKEEPER_SERVICE).withNamespace(NAMESPACE).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(mockedEndpoints);
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + ZOOKEEPER_SERVICE)).thenReturn(zookeeperEndpoints);
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
        Result result = ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository).saveOrUpdateRuleData(any());
        verify(shenyuCacheRepository).saveOrUpdateMetaData(any());
    }

    @Test
    public void testParseSkipsContextPathWhenDubboEnabled() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");
        annotations.put("shenyu.apache.org/plugin-dubbo-enabled", "true");
        annotations.put("shenyu.apache.org/zookeeper-register-address", "zookeeper://" + ZOOKEEPER_SERVICE + ":2181");
        annotations.put("shenyu.apache.org/upstreams-protocol", "dubbo://,dubbo://");
        Map<String, String> labels = new HashMap<>();
        labels.put("shenyu.apache.org/metadata-labels-1", METADATA_SERVICE);

        V1Ingress ingress = new V1IngressBuilder().withNewMetadata()
                .withName(INGRESS_NAME)
                .withNamespace(NAMESPACE)
                .withAnnotations(annotations)
                .withLabels(labels)
                .endMetadata()
                .withNewSpec()
                .withRules(new V1IngressRuleBuilder()
                        .withNewHttp()
                        .withPaths(new V1HTTPIngressPathBuilder().withPath("/**")
                                .withNewBackend()
                                .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(20888).endPort().endService()
                                .endBackend().build())
                        .endHttp()
                        .build())
                .endSpec()
                .build();

        IngressParser ingressParser = new IngressParser(serviceInformer, endpointsInformer);
        Assertions.assertEquals(1, ingressParser.parse(ingress, mock(io.kubernetes.client.openapi.apis.CoreV1Api.class)).size());
    }
}
