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
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.reconciler.GatewayReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.List;
import java.util.Set;
import java.util.Map;

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
        ApiClient apiClient = mockApiClientWithStatusPatch();

        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer,
                httpRouteInformer, shenyuCacheRepository, httpRouteWorkQueue, apiClient, 9195);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(httpRouteWorkQueue).add(new Request("mockedNamespace", "test-route"));
        verify(apiClient).execute(any(okhttp3.Call.class));
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

        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        cache.bindRouteToGateway("mockedNamespace", "shenyu-gateway", Set.of("http"), "mockedNamespace", "test-route");
        cache.putRouteSelectors("mockedNamespace", "test-route", "divide", List.of("sel-1"));

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);

        RateLimitingQueue<Request> httpRouteWorkQueue = mock(RateLimitingQueue.class);
        ApiClient apiClient = mock(ApiClient.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer,
                httpRouteInformer, shenyuCacheRepository, httpRouteWorkQueue, apiClient, 9195);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");
    }

    /**
     * Test that status update is skipped when the Gateway status already reflects the full
     * desired steady state: Accepted=True and Programmed=True conditions plus a per-listener
     * status entry with the current attachedRoutes count.
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
        ApiClient apiClient = mock(ApiClient.class);

        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer,
                httpRouteInformer, shenyuCacheRepository, httpRouteWorkQueue, apiClient, 9195);

        Result result = gatewayReconciler.reconcile(new Request("mockedNamespace", "shenyu-gateway"));
        Assertions.assertEquals(new Result(false), result);
        verify(apiClient, never()).execute(any(okhttp3.Call.class));
    }

    /**
     * attachedRoutes is defined per listener: a route bound through only one listener of a
     * two-listener Gateway must be counted on that listener alone, not on both.
     */
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
        ApiClient apiClient = mock(ApiClient.class);
        when(apiClient.getAuthentications()).thenReturn(Map.of());
        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        when(apiClient.buildCall(any(), any(), any(), any(), bodyCaptor.capture(), any(), any(), any(), any(), any()))
                .thenReturn(mock(okhttp3.Call.class));

        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        GatewayReconciler gatewayReconciler = new GatewayReconciler(gatewayInformer, gatewayClassInformer,
                httpRouteInformer, shenyuCacheRepository, httpRouteWorkQueue, apiClient, 9195);

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
        SharedIndexInformer<DynamicKubernetesObject> informer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> indexer = mock(Indexer.class);
        DynamicKubernetesObject shenyuClass = buildGatewayClass("shenyu", "gateway.shenyu.apache.org/shenyu-controller");
        when(indexer.getByKey("shenyu")).thenReturn(shenyuClass);
        DynamicKubernetesObject otherClass = buildGatewayClass("other-class", "example.com/other-controller");
        when(indexer.getByKey("other-class")).thenReturn(otherClass);
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
