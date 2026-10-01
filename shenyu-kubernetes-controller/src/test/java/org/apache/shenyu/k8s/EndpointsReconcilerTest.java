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
import io.kubernetes.client.openapi.models.V1ServiceBackendPort;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.cache.IngressSelectorCache;
import org.apache.shenyu.k8s.cache.ServiceIngressCache;
import org.apache.shenyu.k8s.common.IngressBackendPort;
import org.apache.shenyu.k8s.common.ServiceIngressRelation;
import org.apache.shenyu.k8s.reconciler.EndpointsReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Endpoints reconciler test.
 */
public final class EndpointsReconcilerTest {

    private SharedIndexInformer<V1Ingress> ingressInformer;

    private SharedIndexInformer<V1Endpoints> endpointsInformer;

    private Indexer<V1Endpoints> endpointsIndexer;

    private ShenyuCacheRepository shenyuCacheRepository;

    @BeforeEach
    public void init() {
        ingressInformer = mock(SharedIndexInformer.class);
        endpointsInformer = mock(SharedIndexInformer.class);
        Indexer<V1Ingress> ingressIndexer = mock(Indexer.class);
        endpointsIndexer = mock(Indexer.class);
        when(ingressInformer.getIndexer()).thenReturn(ingressIndexer);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);
        shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        when(shenyuCacheRepository.findSelectorDataList(PluginEnum.DIVIDE.getName())).thenReturn(Collections.emptyList());
    }

    /**
     * test websocket selector update.
     */
    @Test
    public void testUpdateWebSocketSelector() {
        String namespace = "endpoint-websocket-ns";
        String serviceName = "endpoint-websocket-service";
        String ingressName = "endpoint-websocket-ingress";
        String selectorId = "endpoint-websocket-selector";
        mockEndpoints(namespace, serviceName, endpointPort(8001, null));
        ServiceIngressCache.getInstance().putIngressName(namespace, serviceName,
                new ServiceIngressRelation(namespace, ingressName, backendPort(8001)));
        IngressSelectorCache.getInstance().put(namespace, ingressName, PluginEnum.WEB_SOCKET.getName(), selectorId);
        mockWebSocketSelectors(selectorId);

        Result result = newReconciler().reconcile(new Request(namespace, serviceName));

        Assertions.assertEquals(new Result(false), result);
        ArgumentCaptor<SelectorData> selectorCaptor = ArgumentCaptor.forClass(SelectorData.class);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(selectorCaptor.capture());
        SelectorData updatedSelector = selectorCaptor.getValue();
        Assertions.assertEquals(PluginEnum.WEB_SOCKET.getName(), updatedSelector.getPluginName());
        assertThat(updatedSelector.getHandle(), containsString("\"protocol\":\"ws://\""));
        assertThat(updatedSelector.getHandle(), containsString("\"upstreamUrl\":\"127.0.0.1:8001\""));
    }

    /**
     * test websocket selectors of two ingresses keep the service port each ingress selects.
     */
    @Test
    public void testUpdateMultiPortSelectors() {
        String namespace = "endpoint-multi-port-ns";
        String serviceName = "endpoint-multi-port-service";
        String firstIngress = "endpoint-multi-port-ingress-1";
        String secondIngress = "endpoint-multi-port-ingress-2";
        mockEndpoints(namespace, serviceName, endpointPort(8001, null), endpointPort(8002, null));
        ServiceIngressCache.getInstance().putIngressName(namespace, serviceName,
                new ServiceIngressRelation(namespace, firstIngress, backendPort(8001)));
        ServiceIngressCache.getInstance().putIngressName(namespace, serviceName,
                new ServiceIngressRelation(namespace, secondIngress, backendPort(8002)));
        String firstSelectorId = "endpoint-multi-port-selector-1";
        IngressSelectorCache.getInstance().put(namespace, firstIngress, PluginEnum.WEB_SOCKET.getName(), firstSelectorId);
        String secondSelectorId = "endpoint-multi-port-selector-2";
        IngressSelectorCache.getInstance().put(namespace, secondIngress, PluginEnum.WEB_SOCKET.getName(), secondSelectorId);
        mockWebSocketSelectors(firstSelectorId, secondSelectorId);

        Result result = newReconciler().reconcile(new Request(namespace, serviceName));

        Assertions.assertEquals(new Result(false), result);
        Map<String, String> handleBySelectorId = captureUpdatedHandles(2);
        assertThat(handleBySelectorId.get(firstSelectorId), containsString("\"upstreamUrl\":\"127.0.0.1:8001\""));
        assertThat(handleBySelectorId.get(firstSelectorId), not(containsString(":8002")));
        assertThat(handleBySelectorId.get(secondSelectorId), containsString("\"upstreamUrl\":\"127.0.0.1:8002\""));
        assertThat(handleBySelectorId.get(secondSelectorId), not(containsString(":8001")));
    }

    /**
     * test the service port can be selected by name.
     */
    @Test
    public void testUpdateSelectorWithNamedBackendPort() {
        String namespace = "endpoint-named-port-ns";
        String serviceName = "endpoint-named-port-service";
        String ingressName = "endpoint-named-port-ingress";
        String selectorId = "endpoint-named-port-selector";
        mockEndpoints(namespace, serviceName, endpointPort(8001, null), endpointPort(8003, "ws"));
        ServiceIngressCache.getInstance().putIngressName(namespace, serviceName,
                new ServiceIngressRelation(namespace, ingressName, namedBackendPort("ws")));
        IngressSelectorCache.getInstance().put(namespace, ingressName, PluginEnum.WEB_SOCKET.getName(), selectorId);
        mockWebSocketSelectors(selectorId);

        Result result = newReconciler().reconcile(new Request(namespace, serviceName));

        Assertions.assertEquals(new Result(false), result);
        Map<String, String> handleBySelectorId = captureUpdatedHandles(1);
        assertThat(handleBySelectorId.get(selectorId), containsString("\"upstreamUrl\":\"127.0.0.1:8003\""));
    }

    /**
     * test the first TCP port is used when no endpoint port matches the selected service port,
     * a service may map the selected port to a different target port.
     */
    @Test
    public void testUpdateSelectorWithUnmatchedBackendPort() {
        String namespace = "endpoint-unmatched-port-ns";
        String serviceName = "endpoint-unmatched-port-service";
        String ingressName = "endpoint-unmatched-port-ingress";
        String selectorId = "endpoint-unmatched-port-selector";
        mockEndpoints(namespace, serviceName, endpointPort(8001, null), endpointPort(8002, null));
        ServiceIngressCache.getInstance().putIngressName(namespace, serviceName,
                new ServiceIngressRelation(namespace, ingressName, backendPort(9000)));
        IngressSelectorCache.getInstance().put(namespace, ingressName, PluginEnum.WEB_SOCKET.getName(), selectorId);
        mockWebSocketSelectors(selectorId);

        Result result = newReconciler().reconcile(new Request(namespace, serviceName));

        Assertions.assertEquals(new Result(false), result);
        Map<String, String> handleBySelectorId = captureUpdatedHandles(1);
        assertThat(handleBySelectorId.get(selectorId), containsString("\"upstreamUrl\":\"127.0.0.1:8001\""));
    }

    private EndpointsReconciler newReconciler() {
        return new EndpointsReconciler(ingressInformer, endpointsInformer, shenyuCacheRepository, mock(ApiClient.class));
    }

    private void mockEndpoints(final String namespace, final String serviceName, final CoreV1EndpointPort... ports) {
        V1Endpoints endpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace(namespace).withName(serviceName).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder()
                        .withAddresses(new V1EndpointAddress().ip("127.0.0.1"))
                        .withPorts(ports)
                        .build())
                .build();
        when(endpointsIndexer.getByKey(namespace + "/" + serviceName)).thenReturn(endpoints);
    }

    private void mockWebSocketSelectors(final String... selectorIds) {
        List<SelectorData> selectorDataList = Arrays.stream(selectorIds)
                .map(selectorId -> SelectorData.builder()
                        .id(selectorId)
                        .pluginId(String.valueOf(PluginEnum.WEB_SOCKET.getCode()))
                        .pluginName(PluginEnum.WEB_SOCKET.getName())
                        .name("/**")
                        .handle("[]")
                        .enabled(true)
                        .build())
                .collect(Collectors.toList());
        when(shenyuCacheRepository.findSelectorDataList(PluginEnum.WEB_SOCKET.getName())).thenReturn(selectorDataList);
    }

    private Map<String, String> captureUpdatedHandles(final int expectedUpdates) {
        ArgumentCaptor<SelectorData> selectorCaptor = ArgumentCaptor.forClass(SelectorData.class);
        verify(shenyuCacheRepository, times(expectedUpdates)).saveOrUpdateSelectorData(selectorCaptor.capture());
        return selectorCaptor.getAllValues().stream()
                .collect(Collectors.toMap(SelectorData::getId, SelectorData::getHandle, (first, second) -> second));
    }

    private CoreV1EndpointPort endpointPort(final int port, final String name) {
        CoreV1EndpointPort endpointPort = new CoreV1EndpointPort().port(port).protocol("TCP");
        if (Objects.nonNull(name)) {
            endpointPort.setName(name);
        }
        return endpointPort;
    }

    private IngressBackendPort backendPort(final int port) {
        return IngressBackendPort.from(new V1ServiceBackendPort().number(port));
    }

    private IngressBackendPort namedBackendPort(final String name) {
        return IngressBackendPort.from(new V1ServiceBackendPort().name(name));
    }
}
