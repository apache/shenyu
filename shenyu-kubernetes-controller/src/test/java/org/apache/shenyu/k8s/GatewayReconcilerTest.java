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

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.controller.reconciler.Result;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.GatewayApiCrdVersions;
import org.apache.shenyu.k8s.observability.K8sControllerMetrics;
import org.apache.shenyu.k8s.observability.K8sResourceCorrelation;
import org.apache.shenyu.k8s.observability.K8sResourceCorrelationCache;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.reconciler.GatewayClassReconciler;
import org.apache.shenyu.k8s.reconciler.GatewayReconciler;
import org.apache.shenyu.k8s.reconciler.HTTPRouteReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Gateway Reconciler Test.
 */
public final class GatewayReconcilerTest {

    @BeforeEach
    public void setUp() {
        GatewayRouteCache.getInstance().clear();
        K8sResourceCorrelationCache.getInstance().clear();
        K8sControllerMetrics.getInstance().resetForTesting();
    }

    /** Test ShenYu Gateway creation. */
    @Test
    public void testReconcileShenYuGatewayCreation() throws Exception {
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteIndexer.list()).thenReturn(List.of(httpRoute));
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        RateLimitingQueue<Request> httpRouteWorkQueue = mock(RateLimitingQueue.class);
        RateLimitingQueue<Request> gatewayClassWorkQueue = mock(RateLimitingQueue.class);
        ApiClient apiClient = mockApiClientWithStatusPatch();

        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer,
                httpRouteInformer, shenyuCacheRepository, httpRouteWorkQueue, gatewayClassWorkQueue, apiClient, 9195);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(httpRouteWorkQueue).add(new Request("mockedNamespace", "test-route"));
        // the accept transition re-queues the class so its finalizer is added immediately
        verify(gatewayClassWorkQueue).add(new Request("", "shenyu"));
        verify(apiClient).execute(any(okhttp3.Call.class));
    }

    /** Test Gateway deletion: should cascade delete ShenYu config for associated routes. */
    @Test
    public void testReconcileGatewayDeletion() {
        // gateway not found in indexer → treated as deletion
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(null);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        cache.bindRouteToGateway("mockedNamespace", "shenyu-gateway", Set.of("http"), "mockedNamespace", "test-route");
        cache.putRouteSelectors("mockedNamespace", "test-route", "divide", List.of("sel-1"));
        K8sResourceCorrelationCache.getInstance().recordAccepted("HTTPRoute", "mockedNamespace", "test-route",
                "uid-1", 1L, "v1", Set.of("mockedNamespace/shenyu-gateway"), List.of("sel-1"), List.of());

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);

        RateLimitingQueue<Request> httpRouteWorkQueue = mock(RateLimitingQueue.class);
        RateLimitingQueue<Request> gatewayClassWorkQueue = mock(RateLimitingQueue.class);
        ApiClient apiClient = mock(ApiClient.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer,
                httpRouteInformer, shenyuCacheRepository, httpRouteWorkQueue, gatewayClassWorkQueue, apiClient, 9195);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");
        verify(httpRouteWorkQueue).add(new Request("mockedNamespace", "test-route"));
        Assertions.assertNull(K8sResourceCorrelationCache.getInstance().getCorrelation("HTTPRoute", "mockedNamespace", "test-route"));
        // deletion re-queues every class: the one losing its last Gateway must drop its finalizer
        verify(gatewayClassWorkQueue).add(new Request("", "shenyu"));
        verify(gatewayClassWorkQueue).add(new Request("", "other-class"));
    }

    /**
     * A Gateway whose class carries Accepted=False (ShenYu rejected it) is out of scope: previously served config is
     * cleaned up and the Gateway status is downgraded with the standard Invalid reason.
     */
    @Test
    public void testRejectedGatewayClassMakesGatewayOutOfScope() throws Exception {
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        cache.bindRouteToGateway("mockedNamespace", "shenyu-gateway", Set.of("http"), "mockedNamespace", "test-route");
        cache.putRouteSelectors("mockedNamespace", "test-route", "divide", List.of("sel-1"));

        final ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        final RateLimitingQueue<Request> httpRouteWorkQueue = mock(RateLimitingQueue.class);
        final RateLimitingQueue<Request> gatewayClassWorkQueue = mock(RateLimitingQueue.class);
        ApiClient apiClient = mockApiClientWithStatusPatch();
        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        when(apiClient.buildCall(any(), any(), any(), any(), bodyCaptor.capture(), any(), any(), any(), any(), any()))
                .thenReturn(mock(okhttp3.Call.class));

        final DynamicKubernetesObject rejectedClass = buildGatewayClass("shenyu", "gateway.shenyu.apache.org/shenyu-controller");
        JsonObject rejected = new JsonObject();
        rejected.addProperty("type", "Accepted");
        rejected.addProperty("status", "False");
        rejected.addProperty("reason", "InvalidParameters");
        JsonObject status = new JsonObject();
        status.add("conditions", new JsonArray());
        status.getAsJsonArray("conditions").add(rejected);
        rejectedClass.getRaw().add("status", status);

        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer(rejectedClass);
        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer,
                httpRouteInformer, shenyuCacheRepository, httpRouteWorkQueue, gatewayClassWorkQueue, apiClient, 9195);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");

        JsonObject body = (JsonObject) bodyCaptor.getValue();
        JsonObject downgraded = body.getAsJsonObject("status").getAsJsonArray("conditions").get(0).getAsJsonObject();
        Assertions.assertEquals("False", downgraded.get("status").getAsString());
        Assertions.assertEquals("Invalid", downgraded.get("reason").getAsString());
    }

    /**
     * Test that status update is skipped when the Gateway status already reflects the full desired steady state:
     * Accepted=True and Programmed=True conditions plus a per-listener status entry with the current attachedRoutes
     * count.
     */
    @Test
    public void testReconcileGatewayAlreadyAccepted() throws Exception {
        JsonObject statusObj = new JsonObject();
        statusObj.add("conditions", buildConditions("True", "True"));
        JsonObject listenerStatus = new JsonObject();
        listenerStatus.addProperty("name", "http");
        listenerStatus.addProperty("attachedRoutes", 0);
        listenerStatus.add("conditions", buildConditions("True", "True"));
        JsonArray listeners = new JsonArray();
        listeners.add(listenerStatus);
        statusObj.add("listeners", listeners);

        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        gateway.getRaw().add("status", statusObj);

        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        RateLimitingQueue<Request> httpRouteWorkQueue = mock(RateLimitingQueue.class);
        RateLimitingQueue<Request> gatewayClassWorkQueue = mock(RateLimitingQueue.class);
        ApiClient apiClient = mock(ApiClient.class);

        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer,
                httpRouteInformer, shenyuCacheRepository, httpRouteWorkQueue, gatewayClassWorkQueue, apiClient, 9195);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(apiClient, never()).execute(any(okhttp3.Call.class));
    }

    /** attachedRoutes is defined per listener: a route bound through only one listener of a two-listener Gateway must be counted on that listener alone, not on both. */
    @Test
    public void testAttachedRoutesAreCountedPerListener() throws Exception {
        final DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        JsonObject secondListener = new JsonObject();
        secondListener.addProperty("name", "http2");
        secondListener.addProperty("protocol", "HTTP");
        secondListener.addProperty("port", 9195);
        gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").add(secondListener);

        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        GatewayRouteCache.getInstance().bindRouteToGateway("mockedNamespace", "shenyu-gateway",
                Set.of("http"), "mockedNamespace", "test-route");

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        RateLimitingQueue<Request> httpRouteWorkQueue = mock(RateLimitingQueue.class);
        RateLimitingQueue<Request> gatewayClassWorkQueue = mock(RateLimitingQueue.class);
        ApiClient apiClient = mock(ApiClient.class);
        when(apiClient.getAuthentications()).thenReturn(Map.of());
        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        when(apiClient.buildCall(any(), any(), any(), any(), bodyCaptor.capture(), any(), any(), any(), any(), any()))
                .thenReturn(mock(okhttp3.Call.class));

        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer,
                httpRouteInformer, shenyuCacheRepository, httpRouteWorkQueue, gatewayClassWorkQueue, apiClient, 9195);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);

        JsonArray listeners = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status").getAsJsonArray("listeners");
        Assertions.assertEquals(2, listeners.size());
        for (JsonElement element : listeners) {
            JsonObject listenerStatus = element.getAsJsonObject();
            String name = listenerStatus.get("name").getAsString();
            int attached = listenerStatus.get("attachedRoutes").getAsInt();
            if ("http".equals(name)) {
                Assertions.assertEquals(1, attached, "listener the route attached to must count it");
            } else {
                Assertions.assertEquals(0, attached, "unrelated listener must not count the route");
            }
        }
    }

    /**
     * Production-path probe: accepting a route via HTTPRouteReconciler + HttpRouteParser, then deleting
     * its last managed Gateway, must remove runtime selectors, clear correlation and reverse indexes,
     * and re-queue the route once.
     */
    @Test
    public void testProductionPathLastGatewayDeletionWithdrawsCorrelation() throws Exception {
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        DynamicKubernetesObject route = buildRouteWithPathMatch("mockedNamespace", "test-route",
                List.of("shenyu-gateway"), "testService", 8189);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(route);
        when(httpRouteIndexer.list()).thenReturn(List.of(route));
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository repository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler routeReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, buildReadyParser(), repository, mockApiClientWithStatusPatch(), 9195);

        routeReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        K8sResourceCorrelationCache correlationCache = K8sResourceCorrelationCache.getInstance();
        K8sResourceCorrelation correlation = correlationCache.getCorrelation("HTTPRoute", "mockedNamespace", "test-route");
        Assertions.assertNotNull(correlation);
        final String selectorId = correlation.getSelectorIds().get(0);
        final String ruleId = correlation.getRuleIds().get(0);

        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(null);
        RateLimitingQueue<Request> routeQueue = mock(RateLimitingQueue.class);
        RateLimitingQueue<Request> classQueue = mock(RateLimitingQueue.class);
        GatewayReconciler gwReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer,
                httpRouteInformer, repository, routeQueue, classQueue, mockApiClientWithStatusPatch(), 9195);

        Result result = gwReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(repository).deleteSelectorWithRules("divide", selectorId);
        verify(routeQueue, times(1)).add(new Request("mockedNamespace", "test-route"));
        Assertions.assertNull(GatewayRouteCache.getInstance().getRoutesByGateway("mockedNamespace", "shenyu-gateway"));
        Assertions.assertNull(correlationCache.getCorrelation("HTTPRoute", "mockedNamespace", "test-route"));
        Assertions.assertNull(correlationCache.findCorrelationBySelectorId(selectorId));
        Assertions.assertNull(correlationCache.findCorrelationByRuleId(ruleId));
    }

    /**
     * Multi-Gateway removal: deleting one Gateway keeps runtime config and re-queues the route while
     * another ShenYu Gateway remains bound; removing the last Gateway deletes selectors and correlation.
     */
    @Test
    public void testMultiGatewayPartialThenLastRemoval() throws Exception {
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayIndexer.getByKey("mockedNamespace/gw-1")).thenReturn(buildGateway("mockedNamespace", "gw-1", "shenyu"));
        when(gatewayIndexer.getByKey("mockedNamespace/gw-2")).thenReturn(buildGateway("mockedNamespace", "gw-2", "shenyu"));
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        DynamicKubernetesObject route = buildRouteWithPathMatch("mockedNamespace", "multi-route",
                List.of("gw-1", "gw-2"), "testService", 8189);
        when(httpRouteIndexer.getByKey("mockedNamespace/multi-route")).thenReturn(route);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository repository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler routeReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, buildReadyParser(), repository, mockApiClientWithStatusPatch(), 9195);
        routeReconciler.reconcile(new Request("mockedNamespace", "multi-route"));
        String selectorId = K8sResourceCorrelationCache.getInstance()
                .getCorrelation("HTTPRoute", "mockedNamespace", "multi-route").getSelectorIds().get(0);

        RateLimitingQueue<Request> routeQueue = mock(RateLimitingQueue.class);
        GatewayReconciler gwReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer, httpRouteInformer,
                repository, routeQueue, mock(RateLimitingQueue.class), mockApiClientWithStatusPatch(), 9195);

        when(gatewayIndexer.getByKey("mockedNamespace/gw-1")).thenReturn(null);
        gwReconciler.reconcile(new Request("mockedNamespace", "gw-1"));
        verify(repository, never()).deleteSelectorWithRules("divide", selectorId);
        Assertions.assertNotNull(K8sResourceCorrelationCache.getInstance().findCorrelationBySelectorId(selectorId));

        when(gatewayIndexer.getByKey("mockedNamespace/gw-2")).thenReturn(null);
        gwReconciler.reconcile(new Request("mockedNamespace", "gw-2"));
        verify(repository).deleteSelectorWithRules("divide", selectorId);
        Assertions.assertNull(K8sResourceCorrelationCache.getInstance().findCorrelationBySelectorId(selectorId));
        verify(routeQueue, times(2)).add(new Request("mockedNamespace", "multi-route"));
    }

    /**
     * Controller reassignment on GatewayClass triggers GatewayClassReconciler -> GatewayReconciler ->
     * HTTPRouteReconciler cleanup, withdrawing selectors, correlation state, reverse lookups, and status.
     */
    @Test
    public void testControllerReassignmentWithdrawsCorrelationAndStatus() throws Exception {
        DynamicKubernetesObject shenyuClass = buildGatewayClass("shenyu", "gateway.shenyu.apache.org/shenyu-controller");
        SharedIndexInformer<DynamicKubernetesObject> classInformer = mockGatewayClassInformer(shenyuClass);
        final Indexer<DynamicKubernetesObject> classIndexer = classInformer.getIndexer();

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        gateway.getRaw().add("status", acceptedGatewayStatus());
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayIndexer.list()).thenReturn(List.of(gateway));
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        DynamicKubernetesObject route = buildRouteWithPathMatch("mockedNamespace", "test-route",
                List.of("shenyu-gateway"), "testService", 8189);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(route);
        when(httpRouteIndexer.list()).thenReturn(List.of(route));
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository repository = mock(ShenyuCacheRepository.class);
        HTTPRouteReconciler routeReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                classInformer, buildReadyParser(), repository, mockApiClientWithStatusPatch(), 9195);
        routeReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        K8sResourceCorrelation correlation = K8sResourceCorrelationCache.getInstance()
                .getCorrelation("HTTPRoute", "mockedNamespace", "test-route");
        final String selectorId = correlation.getSelectorIds().get(0);
        final String ruleId = correlation.getRuleIds().get(0);

        DynamicKubernetesObject reassignedClass = buildGatewayClass("shenyu", "example.com/other-controller");
        when(classIndexer.getByKey("shenyu")).thenReturn(reassignedClass);
        when(classIndexer.list()).thenReturn(List.of(reassignedClass));

        RateLimitingQueue<Request> gatewayQueue = mock(RateLimitingQueue.class);
        RateLimitingQueue<Request> routeQueue = mock(RateLimitingQueue.class);
        GatewayClassReconciler classReconciler = new GatewayClassReconciler(classInformer, gatewayInformer,
                gatewayQueue, mockApiClientWithStatusPatch(), new GatewayApiCrdVersions(List.of("v1.5.1"), true));
        classReconciler.reconcile(new Request("", "shenyu"));
        verify(gatewayQueue).add(new Request("mockedNamespace", "shenyu-gateway"));

        GatewayReconciler gwReconciler = new GatewayReconciler(gatewayInformer, classInformer, httpRouteInformer,
                repository, routeQueue, mock(RateLimitingQueue.class), mockApiClientWithStatusPatch(), 9195);
        gwReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));

        verify(repository).deleteSelectorWithRules("divide", selectorId);
        verify(routeQueue, times(1)).add(new Request("mockedNamespace", "test-route"));
        Assertions.assertNull(K8sResourceCorrelationCache.getInstance().getCorrelation("HTTPRoute", "mockedNamespace", "test-route"));
        Assertions.assertNull(K8sResourceCorrelationCache.getInstance().findCorrelationBySelectorId(selectorId));
        Assertions.assertNull(K8sResourceCorrelationCache.getInstance().findCorrelationByRuleId(ruleId));

        routeReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(1L, K8sControllerMetrics.getInstance().getReconcileCount("HTTPRoute", "ignored"));
        Assertions.assertNull(K8sResourceCorrelationCache.getInstance().getCorrelation("HTTPRoute", "mockedNamespace", "test-route"));
    }

    /**
     * Previously rejected routes referencing a removed or reassigned Gateway (not in GatewayRouteCache)
     * must also have their stale correlation cleared and be re-queued once.
     */
    @Test
    public void testRejectedRouteCorrelationWithdrawnOnGatewayDeletion() {
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(null);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        DynamicKubernetesObject route = buildRouteWithPathMatch("mockedNamespace", "bad-route",
                List.of("shenyu-gateway"), "testService", 8189);
        when(httpRouteIndexer.list()).thenReturn(List.of(route));
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        K8sResourceCorrelationCache.getInstance().recordRejected(
                "HTTPRoute", "mockedNamespace", "bad-route", "uid-bad", 1L, "v1",
                Set.of("mockedNamespace/shenyu-gateway"), "NoMatchingListenerHostname", "mismatch");

        RateLimitingQueue<Request> routeQueue = mock(RateLimitingQueue.class);
        GatewayReconciler gwReconciler = new GatewayReconciler(gatewayInformer, mockGatewayClassInformer(),
                httpRouteInformer, mock(ShenyuCacheRepository.class), routeQueue,
                mock(RateLimitingQueue.class), mock(ApiClient.class), 9195);

        gwReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        verify(routeQueue, times(1)).add(new Request("mockedNamespace", "bad-route"));
        Assertions.assertNull(K8sResourceCorrelationCache.getInstance().getCorrelation("HTTPRoute", "mockedNamespace", "bad-route"));
    }

    /** Switching Gateway.spec.gatewayClassName to a foreign class withdraws selectors, correlation, and indexes. */
    @Test
    public void testGatewayClassNameReassignmentWithdrawsCorrelation() throws Exception {
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gw = buildGateway("mockedNamespace", "shenyu-gateway", "other-class");
        gw.getRaw().add("status", acceptedGatewayStatus());
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gw);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        cache.bindRouteToGateway("mockedNamespace", "shenyu-gateway", Set.of("http"), "mockedNamespace", "test-route");
        cache.putRouteSelectors("mockedNamespace", "test-route", "divide", List.of("sel-1"));
        K8sResourceCorrelationCache correlationCache = K8sResourceCorrelationCache.getInstance();
        correlationCache.recordAccepted("HTTPRoute", "mockedNamespace", "test-route", "uid-1", 1L, "v1",
                Set.of("mockedNamespace/shenyu-gateway"), List.of("sel-1"), List.of("rule-1"));

        ShenyuCacheRepository repository = mock(ShenyuCacheRepository.class);
        RateLimitingQueue<Request> routeQueue = mock(RateLimitingQueue.class);
        GatewayReconciler gwReconciler = new GatewayReconciler(gatewayInformer, mockGatewayClassInformer(),
                httpRouteInformer, repository, routeQueue, mock(RateLimitingQueue.class), mockApiClientWithStatusPatch(), 9195);

        gwReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        verify(repository).deleteSelectorWithRules("divide", "sel-1");
        verify(routeQueue, times(1)).add(new Request("mockedNamespace", "test-route"));
        Assertions.assertNull(correlationCache.getCorrelation("HTTPRoute", "mockedNamespace", "test-route"));
        Assertions.assertNull(correlationCache.findCorrelationBySelectorId("sel-1"));
        Assertions.assertNull(correlationCache.findCorrelationByRuleId("rule-1"));
    }

    private JsonObject acceptedGatewayStatus() {
        JsonObject accepted = new JsonObject();
        accepted.addProperty("type", "Accepted");
        accepted.addProperty("status", "True");
        accepted.addProperty("message", "Gateway has been accepted by the ShenYu controller");
        JsonArray conditions = new JsonArray();
        conditions.add(accepted);
        JsonObject status = new JsonObject();
        status.add("conditions", conditions);
        return status;
    }

    private HttpRouteParser buildReadyParser() {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        return new HttpRouteParser(new Lister<>(endpointsIndexer),
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));
    }

    private DynamicKubernetesObject buildRouteWithPathMatch(final String routeNamespace, final String routeName,
                                                            final List<String> gatewayNames, final String serviceName,
                                                            final int port) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", routeNamespace);
        metadata.addProperty("name", routeName);
        JsonArray parentRefs = new JsonArray();
        for (String gwName : gatewayNames) {
            JsonObject ref = new JsonObject();
            ref.addProperty("namespace", routeNamespace);
            ref.addProperty("name", gwName);
            parentRefs.add(ref);
        }
        JsonObject backendRef = new JsonObject();
        backendRef.addProperty("name", serviceName);
        backendRef.addProperty("port", port);
        JsonArray backendRefs = new JsonArray();
        backendRefs.add(backendRef);
        JsonObject pathMatch = new JsonObject();
        pathMatch.addProperty("type", "PathPrefix");
        pathMatch.addProperty("value", "/**");
        JsonObject match = new JsonObject();
        match.add("path", pathMatch);
        JsonArray matches = new JsonArray();
        matches.add(match);
        JsonObject rule = new JsonObject();
        rule.add("backendRefs", backendRefs);
        rule.add("matches", matches);
        JsonArray rules = new JsonArray();
        rules.add(rule);
        JsonObject spec = new JsonObject();
        spec.add("parentRefs", parentRefs);
        spec.add("rules", rules);
        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "HTTPRoute");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
    }

    private JsonArray buildConditions(final String acceptedStatus, final String programmedStatus) {
        JsonObject accepted = new JsonObject();
        accepted.addProperty("type", "Accepted");
        accepted.addProperty("status", acceptedStatus);
        JsonObject programmed = new JsonObject();
        programmed.addProperty("type", "Programmed");
        programmed.addProperty("status", programmedStatus);
        JsonArray conditions = new JsonArray();
        conditions.add(accepted);
        conditions.add(programmed);
        return conditions;
    }

    /**
     * Mock ApiClient whose status-patch pipeline succeeds: authentications resolve empty
     * and buildCall returns a mock call accepted by execute.
     */
    private ApiClient mockApiClientWithStatusPatch() throws ApiException {
        ApiClient apiClient = mock(ApiClient.class);
        when(apiClient.getAuthentications()).thenReturn(Map.of());
        when(apiClient.buildCall(any(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
                .thenReturn(mock(okhttp3.Call.class));
        return apiClient;
    }

    private DynamicKubernetesObject buildGateway(final String namespace, final String name,
                                                  final String gatewayClassName) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", namespace);
        metadata.addProperty("name", name);

        // a single HTTP listener on the served port; allowedRoutes defaults to Same-namespace
        JsonObject listener = new JsonObject();
        listener.addProperty("name", "http");
        listener.addProperty("protocol", "HTTP");
        listener.addProperty("port", 9195);
        JsonArray listeners = new JsonArray();
        listeners.add(listener);

        JsonObject spec = new JsonObject();
        spec.addProperty("gatewayClassName", gatewayClassName);
        spec.add("listeners", listeners);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "Gateway");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
    }

    private DynamicKubernetesObject buildGatewayClass(final String name, final String controllerName) {
        // GatewayClass is cluster-scoped, no namespace
        JsonObject metadata = new JsonObject();
        metadata.addProperty("name", name);

        JsonObject spec = new JsonObject();
        spec.addProperty("controllerName", controllerName);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "GatewayClass");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
    }

    /**
     * Build a mocked gatewayClass informer backed by an Indexer. Lister.get(name) for a
     * cluster-scoped resource resolves to Indexer.getByKey(name), so gateway classes are
     * stubbed via getByKey. "shenyu" is ShenYu-owned; "other-class" is non-ShenYu.
     */
    private SharedIndexInformer<DynamicKubernetesObject> mockGatewayClassInformer() {
        return mockGatewayClassInformer(buildGatewayClass("shenyu", "gateway.shenyu.apache.org/shenyu-controller"));
    }

    /**
     * Same as {@link #mockGatewayClassInformer()} but with a caller-provided "shenyu"
     * class, e.g. one already carrying a rejection status.
     */
    private SharedIndexInformer<DynamicKubernetesObject> mockGatewayClassInformer(
            final DynamicKubernetesObject shenyuClass) {
        SharedIndexInformer<DynamicKubernetesObject> informer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> indexer = mock(Indexer.class);
        DynamicKubernetesObject otherClass = buildGatewayClass("other-class", "example.com/other-controller");
        when(indexer.getByKey("shenyu")).thenReturn(shenyuClass);
        when(indexer.getByKey("other-class")).thenReturn(otherClass);
        // requeueAllGatewayClasses iterates every class after a Gateway deletion
        when(indexer.list()).thenReturn(List.of(shenyuClass, otherClass));
        when(informer.getIndexer()).thenReturn(indexer);
        return informer;
    }

    private DynamicKubernetesObject buildHTTPRoute(final String routeNamespace, final String routeName,
                                                    final String gatewayNamespace, final String gatewayName,
                                                    final String serviceName, final int port) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", routeNamespace);
        metadata.addProperty("name", routeName);

        JsonObject parentRef = new JsonObject();
        parentRef.addProperty("name", gatewayName);
        parentRef.addProperty("namespace", gatewayNamespace);
        JsonArray parentRefs = new JsonArray();
        parentRefs.add(parentRef);

        JsonObject backendRef = new JsonObject();
        backendRef.addProperty("name", serviceName);
        backendRef.addProperty("port", port);
        JsonArray backendRefs = new JsonArray();
        backendRefs.add(backendRef);

        JsonObject rule = new JsonObject();
        rule.add("backendRefs", backendRefs);
        JsonArray rules = new JsonArray();
        rules.add(rule);

        JsonObject spec = new JsonObject();
        spec.add("parentRefs", parentRefs);
        spec.add("rules", rules);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "HTTPRoute");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
    }
}
