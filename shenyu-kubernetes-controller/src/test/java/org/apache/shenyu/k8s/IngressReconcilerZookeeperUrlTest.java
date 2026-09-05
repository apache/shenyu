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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.k8s.parser.IngressParser;
import org.apache.shenyu.k8s.reconciler.IngressReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for zookeeper url resolution in IngressReconciler.
 */
public final class IngressReconcilerZookeeperUrlTest {

    private static final String NAMESPACE = "mockedNamespace";

    private static final String ZOOKEEPER_SERVICE = "zookeeperService";

    private ShenyuCacheRepository shenyuCacheRepository;

    private IngressReconciler ingressReconciler;

    private Indexer<V1Endpoints> endpointsIndexer;

    @BeforeEach
    public void init() {
        final SharedIndexInformer<V1Ingress> ingressInformer = mock(SharedIndexInformer.class);
        final SharedIndexInformer<V1Secret> secretInformer = mock(SharedIndexInformer.class);
        shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        final ShenyuSniAsyncMapping shenyuSniAsyncMapping = new ShenyuSniAsyncMapping();
        final SharedIndexInformer<V1Service> serviceInformer = mock(SharedIndexInformer.class);
        final SharedIndexInformer<V1Endpoints> endpointsInformer = mock(SharedIndexInformer.class);

        // mock ingressInformer
        final Indexer<V1Ingress> ingressIndexer = mock(Indexer.class);
        //mock serviceInformer
        final Indexer<V1Service> serviceIndexer = mock(Indexer.class);
        final V1IngressRule mockedRule = new V1IngressRuleBuilder().withNewHttp().withPaths(
                        new V1HTTPIngressPathBuilder().withPath("/**")
                                .withNewBackend()
                                    .withNewService().withName("testService").withNewPort().withNumber(20888).endPort().endService()
                                .endBackend().build())
                .endHttp().build();
        Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");
        annotations.put("shenyu.apache.org/plugin-dubbo-enabled", "true");
        annotations.put("shenyu.apache.org/zookeeper-register-address", "zookeeper://zookeeperService:2181");
        annotations.put("shenyu.apache.org/upstreams-protocol", "dubbo://,dubbo://");
        Map<String, String> labels = new HashMap<>();
        labels.put("shenyu.apache.org/metadata-labels-1", "dubboFindIdService");
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
        V1Service dubboFindIdService = new V1ServiceBuilder().withNewMetadata().withName("dubboFindIdService").withNamespace(NAMESPACE).withAnnotations(labelsAnnotations).endMetadata()
                .withNewSpec().endSpec()
                .withKind("Service").build();

        V1Ingress mockedIngress = new V1IngressBuilder().withNewMetadata().withLabels(labels).withName("mockedIngress").withNamespace(NAMESPACE).withAnnotations(annotations).endMetadata()
                .withNewSpec().withRules(mockedRule).endSpec()
                .withKind("Ingress").build();

        when(ingressIndexer.getByKey(NAMESPACE + "/mockedIngress")).thenReturn(mockedIngress);
        when(serviceIndexer.getByKey(NAMESPACE + "/dubboFindIdService")).thenReturn(dubboFindIdService);
        when(serviceInformer.getIndexer()).thenReturn(serviceIndexer);
        when(ingressInformer.getIndexer()).thenReturn(ingressIndexer);

        //mock endpointsInformer, zookeeperService endpoints are not registered by default
        endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace(NAMESPACE).withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/testService")).thenReturn(mockedEndpoints);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);

        IngressParser ingressParser = new IngressParser(serviceInformer, endpointsInformer);
        ApiClient apiClient = mock(ApiClient.class);

        ingressReconciler = new IngressReconciler(ingressInformer, secretInformer, shenyuCacheRepository,
                shenyuSniAsyncMapping, ingressParser, apiClient);
    }

    /**
     * test reconcile when zookeeper service has no endpoints, should fall back to ShenyuException instead of NPE.
     */
    @Test
    public void testReconcileWithoutZookeeperEndpoints() {
        ShenyuException exception = Assertions.assertThrows(ShenyuException.class,
                () -> ingressReconciler.reconcile(new Request(NAMESPACE, "mockedIngress")));
        Assertions.assertTrue(exception.getMessage().contains("zookeeper url"));
    }

    /**
     * test reconcile when zookeeper service has endpoints, zookeeper url should be resolved to the endpoint ip.
     */
    @Test
    public void testReconcileWithZookeeperEndpoints() {
        V1Endpoints zookeeperEndpoints = new V1EndpointsBuilder().withNewMetadata().withName(ZOOKEEPER_SERVICE).withNamespace(NAMESPACE).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + ZOOKEEPER_SERVICE)).thenReturn(zookeeperEndpoints);

        Result result = ingressReconciler.reconcile(new Request(NAMESPACE, "mockedIngress"));
        Assertions.assertEquals(new Result(false), result);

        ArgumentCaptor<PluginData> captor = ArgumentCaptor.forClass(PluginData.class);
        verify(shenyuCacheRepository, atLeastOnce()).saveOrUpdatePluginData(captor.capture());
        PluginData dubboPluginData = captor.getAllValues().stream()
                .filter(data -> PluginEnum.DUBBO.getName().equals(data.getName()))
                .findFirst()
                .orElse(null);
        Assertions.assertNotNull(dubboPluginData);
        Assertions.assertTrue(dubboPluginData.getConfig().contains("zookeeper://127.0.0.1:2181"));
    }
}
