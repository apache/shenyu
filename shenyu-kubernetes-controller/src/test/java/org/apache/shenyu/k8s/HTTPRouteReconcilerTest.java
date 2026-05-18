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
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Protocol;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.reconciler.HTTPRouteReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * HTTPRoute Reconciler Test.
 */
public final class HTTPRouteReconcilerTest {

    @BeforeEach
    public void setUp() {
        GatewayRouteCache.getInstance().clear();
    }

    /**
     * Test HTTPRoute bound to a ShenYu Gateway: should create selector and rule.
     */
    @Test
    public void testReconcileBoundHTTPRoute() throws Exception {
        // mock endpoints indexer and lister
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister);

        // mock gateway indexer
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        // mock httpRoute indexer
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        // mock ApiClient for status update
        ApiClient apiClient = mock(ApiClient.class);
        when(apiClient.getBasePath()).thenReturn("http://localhost:8080");
        OkHttpClient httpClient = mock(OkHttpClient.class);
        when(apiClient.getHttpClient()).thenReturn(httpClient);
        okhttp3.Call call = mock(okhttp3.Call.class);
        when(httpClient.newCall(any(okhttp3.Request.class))).thenReturn(call);
        Response response = new Response.Builder()
                .request(new okhttp3.Request.Builder().url("http://localhost").build())
                .protocol(Protocol.HTTP_1_1).code(200).message("OK")
                .body(ResponseBody.create("", MediaType.parse("application/json"))).build();
        when(call.execute()).thenReturn(response);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository).saveOrUpdateRuleData(any());
        verify(httpClient).newCall(any(okhttp3.Request.class));
    }

    /**
     * Test HTTPRoute not bound to any ShenYu Gateway: should skip without creating selector/rule.
     */
    @Test
    public void testReconcileUnboundHTTPRoute() {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister);

        // mock gateway indexer with non-shenyu gateway
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject otherGateway = buildGateway("mockedNamespace", "other-gateway", "other-class");
        when(gatewayIndexer.getByKey("mockedNamespace/other-gateway")).thenReturn(otherGateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        // mock httpRoute indexer
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "other-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        ApiClient apiClient = mock(ApiClient.class);
        OkHttpClient httpClient = mock(OkHttpClient.class);
        when(apiClient.getHttpClient()).thenReturn(httpClient);
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository, never()).saveOrUpdateRuleData(any());
        verify(httpClient, never()).newCall(any(okhttp3.Request.class));
    }

    /**
     * Test HTTPRoute deletion: should clean up selector and rule data.
     */
    @Test
    public void testReconcileHTTPRouteDeletion() {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister);

        // httpRoute not found in indexer → treated as deletion
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(null);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        ApiClient apiClient = mock(ApiClient.class);
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        // No exception should be thrown; deleteConfig handles empty cache gracefully
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
                                                    final String serviceName, final int port,
                                                    final String pathValue) {
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

        JsonObject pathMatch = new JsonObject();
        pathMatch.addProperty("type", "PathPrefix");
        pathMatch.addProperty("value", pathValue);
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
}
