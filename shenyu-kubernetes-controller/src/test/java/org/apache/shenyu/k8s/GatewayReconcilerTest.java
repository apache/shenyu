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
import com.google.gson.JsonObject;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.controller.reconciler.Result;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Protocol;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.reconciler.GatewayReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Gateway Reconciler Test.
 */
public final class GatewayReconcilerTest {

    @BeforeEach
    public void setUp() {
        GatewayRouteCache.getInstance().clear();
    }

    /**
     * Test ShenYu Gateway creation.
     */
    @Test
    public void testReconcileShenYuGatewayCreation() throws Exception {
        // mock gateway indexer
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        // mock httpRoute indexer with a route referencing this gateway
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteIndexer.byIndex("namespace", "mockedNamespace")).thenReturn(List.of(httpRoute));
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        RateLimitingQueue<Request> httpRouteWorkQueue = mock(RateLimitingQueue.class);
        ApiClient apiClient = mock(ApiClient.class);
        OkHttpClient httpClient = mock(OkHttpClient.class);
        when(apiClient.getHttpClient()).thenReturn(httpClient);
        when(apiClient.getBasePath()).thenReturn("http://localhost:8080");
        okhttp3.Call call = mock(okhttp3.Call.class);
        when(httpClient.newCall(any(okhttp3.Request.class))).thenReturn(call);
        Response successResponse = new Response.Builder()
                .request(new okhttp3.Request.Builder().url("http://localhost").build())
                .protocol(Protocol.HTTP_1_1).code(200).message("OK")
                .body(ResponseBody.create("{}", MediaType.parse("application/json"))).build();
        when(call.execute()).thenReturn(successResponse);

        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, httpRouteInformer,
                shenyuCacheRepository, httpRouteWorkQueue, apiClient);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(httpRouteWorkQueue).add(new Request("mockedNamespace", "test-route"));
        verify(httpClient).newCall(any(okhttp3.Request.class));
    }

    /**
     * Test non-ShenYu Gateway creation: should skip without re-queuing HTTPRoutes.
     */
    @Test
    public void testReconcileNonShenYuGatewayCreation() {
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "other-gateway", "other-class");
        when(gatewayIndexer.getByKey("mockedNamespace/other-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        RateLimitingQueue<Request> httpRouteWorkQueue = mock(RateLimitingQueue.class);
        ApiClient apiClient = mock(ApiClient.class);

        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, httpRouteInformer,
                shenyuCacheRepository, httpRouteWorkQueue, apiClient);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "other-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(httpRouteWorkQueue, never()).add(any());
    }

    /**
     * Test Gateway deletion: should cascade delete ShenYu config for associated routes.
     */
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

        // pre-populate GatewayRouteCache with a bound route
        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        cache.bindRouteToGateway("mockedNamespace", "shenyu-gateway", "mockedNamespace", "test-route");
        String selectorId = cache.generateSelectorId();
        cache.addRouteSelector("mockedNamespace", "test-route", "divide", selectorId);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        RuleData ruleData = mock(RuleData.class);
        when(ruleData.getId()).thenReturn("rule-1");
        when(shenyuCacheRepository.findRuleDataList(selectorId)).thenReturn(List.of(ruleData));

        RateLimitingQueue<Request> httpRouteWorkQueue = mock(RateLimitingQueue.class);
        ApiClient apiClient = mock(ApiClient.class);
        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, httpRouteInformer,
                shenyuCacheRepository, httpRouteWorkQueue, apiClient);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).deleteRuleData("divide", selectorId, "rule-1");
        verify(shenyuCacheRepository).deleteSelectorData("divide", selectorId);
    }

    /**
     * Test Gateway deletion with no associated routes: should not throw or delete anything.
     */
    @Test
    public void testReconcileGatewayDeletionWithNoAssociatedRoutes() {
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayIndexer.getByKey("mockedNamespace/empty-gateway")).thenReturn(null);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        RateLimitingQueue<Request> httpRouteWorkQueue = mock(RateLimitingQueue.class);
        ApiClient apiClient = mock(ApiClient.class);

        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, httpRouteInformer,
                shenyuCacheRepository, httpRouteWorkQueue, apiClient);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "empty-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).deleteSelectorData(any(), any());
        verify(shenyuCacheRepository, never()).deleteRuleData(any(), any(), any());
    }

    /**
     * Test that status update is skipped when Gateway already has Accepted=True condition.
     */
    @Test
    public void testReconcileGatewayAlreadyAccepted() {
        // Build Accepted=True status JSON first
        JsonObject acceptedCondition = new JsonObject();
        acceptedCondition.addProperty("type", "Accepted");
        acceptedCondition.addProperty("status", "True");
        acceptedCondition.addProperty("reason", "Accepted");
        acceptedCondition.addProperty("message", "Already accepted");
        JsonArray conditions = new JsonArray();
        conditions.add(acceptedCondition);
        JsonObject statusObj = new JsonObject();
        statusObj.add("conditions", conditions);

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
        ApiClient apiClient = mock(ApiClient.class);
        OkHttpClient httpClient = mock(OkHttpClient.class);
        when(apiClient.getHttpClient()).thenReturn(httpClient);

        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, httpRouteInformer,
                shenyuCacheRepository, httpRouteWorkQueue, apiClient);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(httpClient, never()).newCall(any(okhttp3.Request.class));
    }

    private DynamicKubernetesObject buildGateway(final String namespace, final String name,
                                                  final String gatewayClassName) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", namespace);
        metadata.addProperty("name", name);

        JsonObject spec = new JsonObject();
        spec.addProperty("gatewayClassName", gatewayClassName);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "Gateway");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
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
