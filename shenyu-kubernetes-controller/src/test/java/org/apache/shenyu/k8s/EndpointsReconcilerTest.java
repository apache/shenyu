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
import io.kubernetes.client.openapi.models.V1Ingress;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.cache.IngressSelectorCache;
import org.apache.shenyu.k8s.cache.ServiceIngressCache;
import org.apache.shenyu.k8s.reconciler.EndpointsReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Endpoints reconciler test.
 */
public final class EndpointsReconcilerTest {

    /**
     * test websocket selector update.
     */
    @Test
    public void testUpdateWebSocketSelector() {
        SharedIndexInformer<V1Ingress> ingressInformer = mock(SharedIndexInformer.class);
        SharedIndexInformer<V1Endpoints> endpointsInformer = mock(SharedIndexInformer.class);
        Indexer<V1Ingress> ingressIndexer = mock(Indexer.class);
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        when(ingressInformer.getIndexer()).thenReturn(ingressIndexer);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);

        String namespace = "endpoint-websocket-ns";
        String serviceName = "endpoint-websocket-service";
        String ingressName = "endpoint-websocket-ingress";
        String selectorId = "endpoint-websocket-selector";
        V1Endpoints endpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace(namespace).withName(serviceName).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder()
                        .withAddresses(new V1EndpointAddress().ip("127.0.0.1"))
                        .withPorts(new CoreV1EndpointPort().port(8001).protocol("TCP"))
                        .build())
                .build();
        when(endpointsIndexer.getByKey(namespace + "/" + serviceName)).thenReturn(endpoints);

        ServiceIngressCache.getInstance().putIngressName(namespace, serviceName, namespace, ingressName);
        IngressSelectorCache.getInstance().put(namespace, ingressName, PluginEnum.WEB_SOCKET.getName(), selectorId);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SelectorData selectorData = SelectorData.builder()
                .id(selectorId)
                .pluginId(String.valueOf(PluginEnum.WEB_SOCKET.getCode()))
                .pluginName(PluginEnum.WEB_SOCKET.getName())
                .name("/**")
                .handle("[]")
                .enabled(true)
                .build();
        when(shenyuCacheRepository.findSelectorDataList(PluginEnum.DIVIDE.getName())).thenReturn(Collections.emptyList());
        when(shenyuCacheRepository.findSelectorDataList(PluginEnum.WEB_SOCKET.getName())).thenReturn(Collections.singletonList(selectorData));

        EndpointsReconciler endpointsReconciler = new EndpointsReconciler(ingressInformer, endpointsInformer, shenyuCacheRepository, mock(ApiClient.class));
        Result result = endpointsReconciler.reconcile(new Request(namespace, serviceName));

        Assertions.assertEquals(new Result(false), result);
        ArgumentCaptor<SelectorData> selectorCaptor = ArgumentCaptor.forClass(SelectorData.class);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(selectorCaptor.capture());
        SelectorData updatedSelector = selectorCaptor.getValue();
        Assertions.assertEquals(PluginEnum.WEB_SOCKET.getName(), updatedSelector.getPluginName());
        assertThat(updatedSelector.getHandle(), containsString("\"protocol\":\"ws://\""));
        assertThat(updatedSelector.getHandle(), containsString("\"upstreamUrl\":\"127.0.0.1:8001\""));
    }
}
