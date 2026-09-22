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
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ssl.ShenyuSniAsyncMapping;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.cache.IngressCache;
import org.apache.shenyu.k8s.cache.IngressSelectorCache;
import org.apache.shenyu.k8s.cache.ServiceIngressCache;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.parser.IngressParser;
import org.apache.shenyu.k8s.reconciler.IngressReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for {@link IngressReconciler}.
 */
public final class IngressReconcilerTest {

    private static final String NAMESPACE = "reconciler-namespace";

    private static final String INGRESS_NAME = "reconciler-ingress";

    private static final String SERVICE_NAME = "reconciler-service";

    private static final int BACKEND_PORT = 8080;

    private static final int ENDPOINT_PORT = 9090;

    private Indexer<V1Ingress> ingressIndexer;

    private ShenyuCacheRepository shenyuCacheRepository;

    private IngressReconciler ingressReconciler;

    private final List<SelectorData> savedSelectorData = new ArrayList<>();

    @BeforeEach
    @SuppressWarnings("unchecked")
    public void init() {
        SharedIndexInformer<V1Ingress> ingressInformer = mock(SharedIndexInformer.class);
        ingressIndexer = mock(Indexer.class);
        when(ingressInformer.getIndexer()).thenReturn(ingressIndexer);

        SharedIndexInformer<V1Service> serviceInformer = mock(SharedIndexInformer.class);
        when(serviceInformer.getIndexer()).thenReturn(mock(Indexer.class));

        SharedIndexInformer<V1Endpoints> endpointsInformer = mock(SharedIndexInformer.class);
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);
        V1Endpoints endpoints = new V1EndpointsBuilder()
                .withKind("Endpoints")
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder()
                        .withAddresses(new V1EndpointAddress().ip("10.0.0.1"))
                        .withPorts(new CoreV1EndpointPort().port(ENDPOINT_PORT).protocol("TCP"))
                        .build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(endpoints);

        shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        when(shenyuCacheRepository.findRuleDataList(anyString())).thenReturn(Collections.emptyList());
        doAnswer(invocation -> {
            savedSelectorData.add(invocation.getArgument(0));
            return null;
        }).when(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        when(shenyuCacheRepository.findSelectorDataList(anyString())).thenAnswer(invocation -> new ArrayList<>(savedSelectorData));

        SharedIndexInformer<V1Secret> secretInformer = mock(SharedIndexInformer.class);
        ingressReconciler = new IngressReconciler(ingressInformer, secretInformer, shenyuCacheRepository,
                new ShenyuSniAsyncMapping(), new IngressParser(serviceInformer, endpointsInformer), mock(ApiClient.class));

        IngressCache.getInstance().remove(NAMESPACE, INGRESS_NAME);
        IngressSelectorCache.getInstance().remove(NAMESPACE, INGRESS_NAME, PluginEnum.DIVIDE.getName());
        IngressSelectorCache.getInstance().remove(NAMESPACE, INGRESS_NAME, PluginEnum.WEB_SOCKET.getName());
        ServiceIngressCache.getInstance().removeAllIngressName(NAMESPACE, SERVICE_NAME);
    }

    @Test
    public void testReconcileNewIngressSavesConfigAndRefreshesUpstreamFromEndpoints() {
        mockIngress(shenyuAnnotations(new HashMap<>()), "/test", "Exact", null);

        Result result = ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME));

        assertEquals(new Result(false), result);
        assertNotNull(IngressCache.getInstance().get(NAMESPACE, INGRESS_NAME));
        List<Pair<String, String>> ingressNames = ServiceIngressCache.getInstance().getIngressName(NAMESPACE, SERVICE_NAME);
        assertNotNull(ingressNames);
        assertTrue(ingressNames.contains(Pair.of(NAMESPACE, INGRESS_NAME)));
        verify(shenyuCacheRepository).saveOrUpdateRuleData(any());

        // the selector is first saved with the ingress backend port and then refreshed with the endpoints port
        assertEquals(2, savedSelectorData.size());
        assertEquals(PluginEnum.DIVIDE.getName(), savedSelectorData.get(0).getPluginName());
        assertThat(savedSelectorData.get(0).getHandle(), containsString("10.0.0.1:" + BACKEND_PORT));
        assertThat(savedSelectorData.get(1).getHandle(), containsString("10.0.0.1:" + ENDPOINT_PORT));
        assertEquals(savedSelectorData.get(0).getId(), savedSelectorData.get(1).getId());
    }

    @Test
    public void testReconcileWebSocketIngressEnablesPluginAndUsesWebSocketUpstream() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_WEB_SOCKET_ENABLED, "true");
        mockIngress(shenyuAnnotations(annotations), "/test", "Prefix", null);

        Result result = ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME));

        assertEquals(new Result(false), result);
        ArgumentCaptor<PluginData> pluginCaptor = ArgumentCaptor.forClass(PluginData.class);
        verify(shenyuCacheRepository, atLeastOnce()).saveOrUpdatePluginData(pluginCaptor.capture());
        assertTrue(pluginCaptor.getAllValues().stream()
                .anyMatch(pluginData -> PluginEnum.WEB_SOCKET.getName().equals(pluginData.getName())));

        SelectorData savedSelector = savedSelectorData.get(savedSelectorData.size() - 1);
        assertEquals(PluginEnum.WEB_SOCKET.getName(), savedSelector.getPluginName());
        assertThat(savedSelector.getHandle(), containsString("\"protocol\":\"ws://\""));
        assertThat(savedSelector.getHandle(), containsString("10.0.0.1:" + ENDPOINT_PORT));
    }

    @Test
    public void testReconcileSkipsIngressOfAnotherIngressClass() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.K8S_INGRESS_CLASS_ANNOTATION_KEY, "nginx");
        mockIngress(annotations, "/test", "Prefix", null);

        Result result = ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME));

        assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertNull(IngressCache.getInstance().get(NAMESPACE, INGRESS_NAME));
    }

    @Test
    public void testReconcileAcceptsIngressClassNameFromSpec() {
        mockIngress(new HashMap<>(), "/test", "Prefix", IngressConstants.SHENYU_INGRESS_CLASS);

        Result result = ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME));

        assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, atLeastOnce()).saveOrUpdateSelectorData(any());
        assertNotNull(IngressCache.getInstance().get(NAMESPACE, INGRESS_NAME));
    }

    @Test
    public void testReconcileUpdatesConfigWhenIngressChanged() {
        mockIngress(shenyuAnnotations(new HashMap<>()), "/test", "Exact", null);
        ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME));
        assertEquals(2, savedSelectorData.size());

        mockIngress(shenyuAnnotations(new HashMap<>()), "/test-changed", "Exact", null);
        Result result = ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME));

        assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, atLeastOnce())
                .deleteSelectorData(eq(PluginEnum.DIVIDE.getName()), eq(savedSelectorData.get(0).getId()));
        // the stale config is deleted and the changed ingress is saved again with a refreshed upstream handle
        assertEquals(4, savedSelectorData.size());
    }

    @Test
    public void testReconcileDeletedIngressRemovesConfig() {
        mockIngress(shenyuAnnotations(new HashMap<>()), "/test", "Exact", null);
        ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME));

        when(ingressIndexer.getByKey(NAMESPACE + "/" + INGRESS_NAME)).thenReturn(null);
        Result result = ingressReconciler.reconcile(new Request(NAMESPACE, INGRESS_NAME));

        assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, atLeastOnce())
                .deleteSelectorData(eq(PluginEnum.DIVIDE.getName()), eq(savedSelectorData.get(0).getId()));
        assertNull(IngressCache.getInstance().get(NAMESPACE, INGRESS_NAME));
        assertNull(IngressSelectorCache.getInstance().get(NAMESPACE, INGRESS_NAME, PluginEnum.DIVIDE.getName()));
        assertTrue(ServiceIngressCache.getInstance().getIngressName(NAMESPACE, SERVICE_NAME).isEmpty());
    }

    private Map<String, String> shenyuAnnotations(final Map<String, String> annotations) {
        Map<String, String> allAnnotations = new HashMap<>();
        allAnnotations.put(IngressConstants.K8S_INGRESS_CLASS_ANNOTATION_KEY, IngressConstants.SHENYU_INGRESS_CLASS);
        allAnnotations.putAll(annotations);
        return allAnnotations;
    }

    private void mockIngress(final Map<String, String> annotations, final String path,
                             final String pathType, final String ingressClassName) {
        V1IngressRule rule = new V1IngressRuleBuilder().withNewHttp().withPaths(new V1HTTPIngressPathBuilder()
                        .withPath(path)
                        .withPathType(pathType)
                        .withNewBackend()
                            .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(BACKEND_PORT).endPort().endService()
                        .endBackend()
                        .build())
                .endHttp().build();
        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName(INGRESS_NAME).withNamespace(NAMESPACE)
                .withAnnotations(annotations).withLabels(new HashMap<>()).endMetadata()
                .withNewSpec().withRules(rule).withIngressClassName(ingressClassName).endSpec()
                .withKind("Ingress")
                .build();
        when(ingressIndexer.getByKey(NAMESPACE + "/" + INGRESS_NAME)).thenReturn(ingress);
    }
}
