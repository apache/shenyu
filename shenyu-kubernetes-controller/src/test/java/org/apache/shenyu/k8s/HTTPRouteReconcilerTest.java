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
import com.google.gson.JsonParser;
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
import io.kubernetes.client.openapi.models.V1Namespace;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.reconciler.HTTPRouteReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** HTTPRoute Reconciler Test. */
public final class HTTPRouteReconcilerTest {

    @BeforeEach
    public void setUp() {
        GatewayRouteCache.getInstance().clear();
    }

    /** Test HTTPRoute bound to a ShenYu Gateway: should create selector and rule. */
    @Test
    public void testReconcileBoundHTTPRoute() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ApiClient apiClient = mockApiClientWithStatusPatch();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, mockNamespaceInformer(), httpRouteParser, shenyuCacheRepository, apiClient, 9195, mock(RateLimitingQueue.class));

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository).saveOrUpdateRuleData(any());
        verify(apiClient).execute(any(okhttp3.Call.class));
    }

    /** Regression: when the route status already carries Accepted=True and ResolvedRefs=True (capitalized, per K8s condition convention), the reconciler must skip the status patch. */
    @Test
    public void testReconcileSkipsStatusPatchWhenAlreadySet() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder()
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("192.168.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        JsonObject accepted = new JsonObject();
        accepted.addProperty("type", "Accepted");
        accepted.addProperty("status", "True");
        JsonObject resolvedRefs = new JsonObject();
        resolvedRefs.addProperty("type", "ResolvedRefs");
        resolvedRefs.addProperty("status", "True");
        JsonArray conditions = new JsonArray();
        conditions.add(accepted);
        conditions.add(resolvedRefs);
        JsonObject parentRef = new JsonObject();
        parentRef.addProperty("group", "gateway.networking.k8s.io");
        parentRef.addProperty("kind", "Gateway");
        parentRef.addProperty("namespace", "mockedNamespace");
        parentRef.addProperty("name", "shenyu-gateway");
        JsonObject parent = new JsonObject();
        parent.add("parentRef", parentRef);
        parent.addProperty("controllerName", "gateway.shenyu.apache.org/shenyu-controller");
        parent.add("conditions", conditions);
        JsonArray parents = new JsonArray();
        parents.add(parent);
        JsonObject status = new JsonObject();
        status.add("parents", parents);
        httpRoute.getRaw().add("status", status);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ApiClient apiClient = mock(ApiClient.class);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, mockNamespaceInformer(), httpRouteParser, shenyuCacheRepository, apiClient, 9195, mock(RateLimitingQueue.class));

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        verify(apiClient, never()).execute(any(okhttp3.Call.class));
    }

    /**
     * A parentRef carrying a port must only attach through listeners on that port: with the
     * sectionName selecting a listener whose port differs from the parentRef port, the
     * attachment is rejected instead of silently going through the named listener.
     */
    @Test
    public void testParentRefPortMustMatchSelectedListener() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        // parentRef selects the listener by name but demands port 443; the listener serves 9195
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs")
                .get(0).getAsJsonObject().addProperty("port", 443);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ApiClient apiClient = mockApiClientWithStatusPatch();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, mockNamespaceInformer(), httpRouteParser, shenyuCacheRepository, apiClient, 9195, mock(RateLimitingQueue.class));

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
    }

    /** Test HTTPRoute deletion: should clean up selector and rule data. */
    @Test
    public void testReconcileHTTPRouteDeletion() {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        // httpRoute not found in indexer → treated as deletion
        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(null);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        ApiClient apiClient = mock(ApiClient.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, mockNamespaceInformer(), httpRouteParser, shenyuCacheRepository, apiClient, 9195, mock(RateLimitingQueue.class));

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        // No exception should be thrown; deleteConfig handles empty cache gracefully
    }

    /** Idempotent reconcile: re-reconciling an unchanged HTTPRoute must NOT delete any selector, because the deterministic IDs are stable across parses. */
    @Test
    public void testReconcileIsIdempotentOnResync() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ApiClient apiClient = mockApiClientWithStatusPatch();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, mockNamespaceInformer(), httpRouteParser, shenyuCacheRepository, apiClient, 9195, mock(RateLimitingQueue.class));

        httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));

        // The reconciler deletes through deleteSelectorWithRules only, so that is the
        // call an unwanted delete-then-recreate would go through
        verify(shenyuCacheRepository, never()).deleteSelectorWithRules(any(), any());
    }

    /**
     * Cross-namespace parentRef authorized by the listener's allowedRoutes alone: per the
     * Gateway API spec a ReferenceGrant does not apply to a Route's parentRef, so the
     * attachment must be accepted without any grant existing in the Gateway's namespace.
     */
    @Test
    public void testReconcileCrossNamespaceHTTPRouteAcceptedByAllowedRoutes() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("route-ns").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("route-ns/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = allowAllNamespaces(buildGateway("gw-ns", "shenyu-gateway", "shenyu"));
        when(gatewayIndexer.getByKey("gw-ns/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("route-ns", "test-route",
                "gw-ns", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("route-ns/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ApiClient apiClient = mockApiClientWithStatusPatch();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, mockNamespaceInformer(), httpRouteParser, shenyuCacheRepository, apiClient, 9195, mock(RateLimitingQueue.class));

        Result result = httpRouteReconciler.reconcile(new Request("route-ns", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
    }

    /**
     * A Selector policy without a selector or Namespace data fails closed, even for a same-namespace route.
     */
    @Test
    public void testSelectorAllowedRoutesWithoutSelectorFailsClosed() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));
        final SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        JsonObject namespaces = new JsonObject();
        namespaces.addProperty("from", "Selector");
        JsonObject allowedRoutes = new JsonObject();
        allowedRoutes.add("namespaces", namespaces);
        gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners")
                .get(0).getAsJsonObject().add("allowedRoutes", allowedRoutes);
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);
        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);
        ApiClient apiClient = mock(ApiClient.class);
        when(apiClient.getAuthentications()).thenReturn(Map.of());
        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        when(apiClient.buildCall(any(), any(), any(), any(), bodyCaptor.capture(), any(), any(), any(), any(), any()))
                .thenReturn(mock(okhttp3.Call.class));
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, mockNamespaceInformer(), httpRouteParser, shenyuCacheRepository, apiClient, 9195, mock(RateLimitingQueue.class));
        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        JsonArray parents = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status").getAsJsonArray("parents");
        JsonArray conditions = parents.get(0).getAsJsonObject().getAsJsonArray("conditions");
        boolean rejectedByListeners = false;
        for (JsonElement element : conditions) {
            JsonObject condition = element.getAsJsonObject();
            if ("Accepted".equals(condition.get("type").getAsString())
                    && "False".equals(condition.get("status").getAsString())
                    && "NotAllowedByListeners".equals(condition.get("reason").getAsString())) {
                rejectedByListeners = true;
            }
        }
        Assertions.assertTrue(rejectedByListeners, "Accepted=False/NotAllowedByListeners not reported");
    }

    /**
     * A rule with filters (unsupported) must reject the whole route: nothing is programmed,
     * config from a previous valid spec of this route is dropped, and the status patch
     * reports Accepted=False with the spec-defined reason UnsupportedValue.
     */
    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testReconcileUnsupportedRuleProgramsNothing(final boolean invalidTimeout) throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        JsonObject rule = httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("rules").get(0).getAsJsonObject();
        if (invalidTimeout) {
            JsonObject timeouts = new JsonObject();
            timeouts.addProperty("request", "5seconds");
            rule.add("timeouts", timeouts);
        } else {
            JsonObject filter = new JsonObject();
            filter.addProperty("type", "RequestHeaderModifier");
            JsonArray filters = new JsonArray();
            filters.add(filter);
            rule.add("filters", filters);
        }
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        // config programmed by a previous, valid spec of this route
        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        cache.putRouteSelectors("mockedNamespace", "test-route", "divide", List.of("sel-1"));

        ApiClient apiClient = mock(ApiClient.class);
        when(apiClient.getAuthentications()).thenReturn(Map.of());
        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        when(apiClient.buildCall(any(), any(), any(), any(), bodyCaptor.capture(), any(), any(), any(), any(), any()))
                .thenReturn(mock(okhttp3.Call.class));

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, mockNamespaceInformer(), httpRouteParser, shenyuCacheRepository, apiClient, 9195, mock(RateLimitingQueue.class));

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");

        JsonArray parents = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status").getAsJsonArray("parents");
        JsonArray conditions = parents.get(0).getAsJsonObject().getAsJsonArray("conditions");
        boolean rejectedByListeners = false;
        for (JsonElement element : conditions) {
            JsonObject condition = element.getAsJsonObject();
            if ("Accepted".equals(condition.get("type").getAsString())
                    && "False".equals(condition.get("status").getAsString())
                    && "UnsupportedValue".equals(condition.get("reason").getAsString())) {
                rejectedByListeners = true;
            }
        }
        Assertions.assertTrue(rejectedByListeners, "Accepted=False/UnsupportedValue not reported");

        rule.remove(invalidTimeout ? "timeouts" : "filters");
        httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository).saveOrUpdateRuleData(any());
        cache.removeRouteSelectors("mockedNamespace", "test-route", "divide");
        cache.removeRouteGatewayBinding("mockedNamespace", "test-route");
    }

    /** Status entries owned by other controllers must survive ShenYu's status patch: the patch body carries them over, because the merge patch replaces the parents array wholesale. */
    @Test
    public void testStatusPatchPreservesForeignControllerEntries() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        JsonObject foreignRef = new JsonObject();
        foreignRef.addProperty("group", "gateway.networking.k8s.io");
        foreignRef.addProperty("kind", "Gateway");
        foreignRef.addProperty("namespace", "mockedNamespace");
        foreignRef.addProperty("name", "other-gateway");
        JsonObject foreignParent = new JsonObject();
        foreignParent.add("parentRef", foreignRef);
        foreignParent.addProperty("controllerName", "example.net/other-gateway-controller");
        foreignParent.add("conditions", new JsonArray());
        JsonArray existingParents = new JsonArray();
        existingParents.add(foreignParent);
        JsonObject status = new JsonObject();
        status.add("parents", existingParents);
        httpRoute.getRaw().add("status", status);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ApiClient apiClient = mock(ApiClient.class);
        when(apiClient.getAuthentications()).thenReturn(Map.of());
        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        when(apiClient.buildCall(any(), any(), any(), any(), bodyCaptor.capture(), any(), any(), any(), any(), any()))
                .thenReturn(mock(okhttp3.Call.class));

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, mockNamespaceInformer(), httpRouteParser, shenyuCacheRepository, apiClient, 9195, mock(RateLimitingQueue.class));

        httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));

        verify(apiClient).execute(any(okhttp3.Call.class));
        JsonArray patchedParents = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status").getAsJsonArray("parents");
        Assertions.assertEquals(2, patchedParents.size(), "patch must carry the foreign entry plus ShenYu's own");
        boolean foreignKept = false;
        boolean shenyuPresent = false;
        for (JsonElement element : patchedParents) {
            String controllerName = element.getAsJsonObject().get("controllerName").getAsString();
            if ("example.net/other-gateway-controller".equals(controllerName)) {
                foreignKept = true;
            }
            if ("gateway.shenyu.apache.org/shenyu-controller".equals(controllerName)) {
                shenyuPresent = true;
            }
        }
        Assertions.assertTrue(foreignKept, "foreign controller entry was dropped by the patch");
        Assertions.assertTrue(shenyuPresent, "ShenYu's own entry missing from the patch");
    }

    /** Namespace label transitions revoke and restore runtime config without modifying the route. */
    @Test
    public void testNamespaceSelectorRevocationAndRecovery() throws Exception {
        SelectorFixture fixture = new SelectorFixture();
        fixture.namespaceLabels(Map.of("team", "blue"));
        fixture.reconcile();
        Assertions.assertEquals(Set.of("routes/test-route"), fixture.cache.getRoutesByListener("gateways", "shenyu-gateway", "http"));
        fixture.assertAccepted(0, "True", "Accepted");
        verify(fixture.repository).saveOrUpdateSelectorData(any());
        verify(fixture.gatewayQueue).add(new Request("gateways", "shenyu-gateway"));

        clearInvocations(fixture.repository, fixture.gatewayQueue);
        fixture.namespaceLabels(Map.of("team", "red"));
        fixture.reconcile();
        verify(fixture.repository).deleteSelectorWithRules("divide", "selector-id");
        verify(fixture.repository, never()).saveOrUpdateSelectorData(any());
        Assertions.assertNull(fixture.cache.getRouteSelectors("routes", "test-route", "divide"));
        Assertions.assertTrue(fixture.cache.getRoutesByListener("gateways", "shenyu-gateway", "http").isEmpty());
        fixture.assertAccepted(0, "False", "NotAllowedByListeners");
        verify(fixture.gatewayQueue).add(new Request("gateways", "shenyu-gateway"));

        clearInvocations(fixture.repository, fixture.gatewayQueue);
        fixture.namespaceLabels(Map.of("team", "blue"));
        fixture.reconcile();
        verify(fixture.repository).saveOrUpdateSelectorData(any());
        Assertions.assertEquals(Set.of("routes/test-route"), fixture.cache.getRoutesByListener("gateways", "shenyu-gateway", "http"));
        fixture.assertAccepted(0, "True", "Accepted");

        clearInvocations(fixture.repository, fixture.gatewayQueue);
        when(fixture.namespaceIndexer.getByKey("routes")).thenReturn(null);
        fixture.reconcile();
        verify(fixture.repository).deleteSelectorWithRules("divide", "selector-id");
        Assertions.assertNull(fixture.cache.getGatewaysForRoute("routes", "test-route"));
        fixture.assertAccepted(0, "False", "NotAllowedByListeners");
    }

    /** Empty selectors need a present Namespace; route labels cannot grant namespace authorization. */
    @Test
    public void testEmptySelectorAndMissingNamespace() throws Exception {
        SelectorFixture fixture = new SelectorFixture();
        fixture.route.getRaw().getAsJsonObject("metadata").add("labels", JsonParser.parseString("{\"team\":\"blue\"}"));
        fixture.reconcile();
        verify(fixture.repository, never()).saveOrUpdateSelectorData(any());
        fixture.assertAccepted(0, "False", "NotAllowedByListeners");
        fixture.listener().getAsJsonObject("allowedRoutes").getAsJsonObject("namespaces").add("selector", new JsonObject());
        fixture.namespaceLabels(null);
        fixture.reconcile();
        verify(fixture.repository).saveOrUpdateSelectorData(any());
        fixture.assertAccepted(0, "True", "Accepted");
    }

    /** An independently allowed listener keeps the route programmed when the selector stops matching. */
    @Test
    public void testSelectorRejectionPreservesOtherListenerAndParent() throws Exception {
        SelectorFixture fixture = new SelectorFixture();
        DynamicKubernetesObject secondGateway = allowAllNamespaces(buildGateway("gateways", "other", "shenyu"));
        when(fixture.gatewayIndexer.getByKey("gateways/other")).thenReturn(secondGateway);
        JsonObject otherParent = new JsonObject();
        otherParent.addProperty("namespace", "gateways");
        otherParent.addProperty("name", "other");
        fixture.route.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs").add(otherParent);
        fixture.namespaceLabels(Map.of("team", "blue"));
        fixture.reconcile();
        Assertions.assertEquals(Set.of("gateways/shenyu-gateway", "gateways/other"), fixture.cache.getGatewaysForRoute("routes", "test-route"));
        clearInvocations(fixture.repository, fixture.gatewayQueue);
        fixture.namespaceLabels(Map.of("team", "red"));
        fixture.reconcile();
        fixture.assertAccepted(0, "False", "NotAllowedByListeners");
        fixture.assertAccepted(1, "True", "Accepted");
        Assertions.assertEquals(Set.of("gateways/other"), fixture.cache.getGatewaysForRoute("routes", "test-route"));
        verify(fixture.repository, never()).deleteSelectorWithRules(any(), any());
        verify(fixture.gatewayQueue).add(new Request("gateways", "shenyu-gateway"));
        verify(fixture.gatewayQueue).add(new Request("gateways", "other"));

        JsonObject allListener = fixture.listener().deepCopy();
        allListener.addProperty("name", "all");
        allListener.getAsJsonObject("allowedRoutes").getAsJsonObject("namespaces").addProperty("from", "All");
        fixture.gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").add(allListener);
        fixture.reconcile();
        fixture.assertAccepted(0, "True", "Accepted");
        Assertions.assertTrue(fixture.cache.getRoutesByListener("gateways", "shenyu-gateway", "http").isEmpty());
        Assertions.assertEquals(Set.of("routes/test-route"), fixture.cache.getRoutesByListener("gateways", "shenyu-gateway", "all"));
    }

    /** Namespace matching does not override listener kind, hostname, section or served port gates. */
    @Test
    public void testSelectorPreservesOtherListenerGates() throws Exception {
        SelectorFixture fixture = new SelectorFixture();
        fixture.namespaceLabels(Map.of("team", "blue"));
        fixture.listener().addProperty("hostname", "gateway.example.com");
        fixture.route.getRaw().getAsJsonObject("spec").add("hostnames", JsonParser.parseString("[\"other.example.com\"]"));
        fixture.reconcile();
        fixture.assertAccepted(0, "False", "NoMatchingListenerHostname");
        fixture.route.getRaw().getAsJsonObject("spec").remove("hostnames");
        fixture.listener().getAsJsonObject("allowedRoutes").add("kinds", JsonParser.parseString("[{\"kind\":\"TCPRoute\"}]"));
        fixture.reconcile();
        fixture.assertAccepted(0, "False", "NotAllowedByListeners");
        fixture.listener().getAsJsonObject("allowedRoutes").remove("kinds");
        fixture.listener().addProperty("port", 8080);
        fixture.reconcile();
        fixture.assertAccepted(0, "False", "UnsupportedValue");
        fixture.listener().addProperty("port", 9195);
        fixture.route.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs").get(0).getAsJsonObject().addProperty("sectionName", "missing");
        fixture.reconcile();
        fixture.assertAccepted(0, "False", "UnsupportedValue");
        verify(fixture.repository, never()).saveOrUpdateSelectorData(any());
    }

    private SharedIndexInformer<V1Namespace> mockNamespaceInformer() {
        SharedIndexInformer<V1Namespace> informer = mock(SharedIndexInformer.class);
        when(informer.getIndexer()).thenReturn(mock(Indexer.class));
        return informer;
    }

    private ApiClient mockApiClientWithStatusPatch() throws ApiException {
        ApiClient apiClient = mock(ApiClient.class);
        when(apiClient.getAuthentications()).thenReturn(Map.of());
        when(apiClient.buildCall(any(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
                .thenReturn(mock(okhttp3.Call.class));
        return apiClient;
    }

    /**
     * A previously bound HTTPRoute that is no longer bound to any ShenYu Gateway (grant
     * removed, GatewayClass re-pointed, listener removed) must have its programmed
     * selectors deleted and its ShenYu status entries dropped, instead of being skipped
     * with stale config left behind.
     */
    @Test
    public void testReconcileUnboundHTTPRouteCleansUpPreviouslyAppliedConfig() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject otherGateway = buildGateway("mockedNamespace", "other-gateway", "other-class");
        when(gatewayIndexer.getByKey("mockedNamespace/other-gateway")).thenReturn(otherGateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "other-gateway", "testService", 8189, "/**");
        JsonObject shenyuParent = new JsonObject();
        shenyuParent.addProperty("controllerName", "gateway.shenyu.apache.org/shenyu-controller");
        JsonArray parents = new JsonArray();
        parents.add(shenyuParent);
        JsonObject status = new JsonObject();
        status.add("parents", parents);
        httpRoute.getRaw().add("status", status);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        cache.bindRouteToGateway("mockedNamespace", "other-gateway", Set.of("http"), "mockedNamespace", "test-route");
        cache.putRouteSelectors("mockedNamespace", "test-route", "divide", List.of("sel-1"));

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        ApiClient apiClient = mockApiClientWithStatusPatch();

        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, mockNamespaceInformer(), httpRouteParser, shenyuCacheRepository, apiClient, 9195, mock(RateLimitingQueue.class));

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");
        verify(apiClient).execute(any(okhttp3.Call.class));
    }

    private DynamicKubernetesObject buildGateway(final String namespace, final String name,
                                                  final String gatewayClassName) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", namespace);
        metadata.addProperty("name", name);

        // one HTTP listener; allowedRoutes defaults to Same-namespace per the spec
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

    /** Loosen the gateway's listener to accept routes from all namespaces. */
    private DynamicKubernetesObject allowAllNamespaces(final DynamicKubernetesObject gateway) {
        JsonObject namespaces = new JsonObject();
        namespaces.addProperty("from", "All");
        JsonObject allowedRoutes = new JsonObject();
        allowedRoutes.add("namespaces", namespaces);
        gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners")
                .get(0).getAsJsonObject().add("allowedRoutes", allowedRoutes);
        return gateway;
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

    /** Build a mocked gatewayClass informer backed by an Indexer. */
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

    private final class SelectorFixture {
        private final GatewayRouteCache cache = GatewayRouteCache.getInstance();

        private final Indexer<V1Namespace> namespaceIndexer = mock(Indexer.class);

        private final Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);

        private final DynamicKubernetesObject gateway = buildGateway("gateways", "shenyu-gateway", "shenyu");

        private final DynamicKubernetesObject route = buildHTTPRoute("routes", "test-route", "gateways", "shenyu-gateway", "service", 80, "/");

        private final ShenyuCacheRepository repository = mock(ShenyuCacheRepository.class);

        private final RateLimitingQueue<Request> gatewayQueue = mock(RateLimitingQueue.class);

        private final ArgumentCaptor<Object> patches = ArgumentCaptor.forClass(Object.class);

        private final HTTPRouteReconciler reconciler;

        private SelectorFixture() throws Exception {
            SharedIndexInformer<DynamicKubernetesObject> routeInformer = mock(SharedIndexInformer.class);
            Indexer<DynamicKubernetesObject> routeIndexer = mock(Indexer.class);
            when(routeInformer.getIndexer()).thenReturn(routeIndexer);
            when(routeIndexer.getByKey("routes/test-route")).thenReturn(route);
            SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
            when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);
            when(gatewayIndexer.getByKey("gateways/shenyu-gateway")).thenReturn(gateway);
            SharedIndexInformer<V1Namespace> namespaceInformer = mock(SharedIndexInformer.class);
            when(namespaceInformer.getIndexer()).thenReturn(namespaceIndexer);
            listener().add("allowedRoutes", JsonParser.parseString(
                    "{\"namespaces\":{\"from\":\"Selector\",\"selector\":{\"matchLabels\":{\"team\":\"blue\"}}}}"));
            ApiClient client = mockApiClientWithStatusPatch();
            when(client.buildCall(any(), any(), any(), any(), patches.capture(), any(), any(), any(), any(), any())).thenReturn(mock(okhttp3.Call.class));
            HttpRouteParser parser = mock(HttpRouteParser.class);
            ShenyuMemoryConfig config = new ShenyuMemoryConfig();
            SelectorData selector = new SelectorData();
            selector.setId("selector-id");
            IngressConfiguration routeConfig = new IngressConfiguration(selector, List.of(), List.of());
            config.setRouteConfigList(List.of(routeConfig));
            when(parser.parse(any(), any())).thenReturn(config);
            reconciler = new HTTPRouteReconciler(routeInformer, gatewayInformer, mockGatewayClassInformer(), namespaceInformer,
                    parser, repository, client, 9195, gatewayQueue);
        }

        private JsonObject listener() {
            return gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").get(0).getAsJsonObject();
        }

        private void namespaceLabels(final Map<String, String> labels) {
            when(namespaceIndexer.getByKey("routes")).thenReturn(new V1Namespace().metadata(new V1ObjectMeta().name("routes").labels(labels)));
        }

        private void reconcile() {
            reconciler.reconcile(new Request("routes", "test-route"));
        }

        private void assertAccepted(final int parentIndex, final String status, final String reason) {
            JsonObject parent = ((JsonObject) patches.getValue()).getAsJsonObject("status").getAsJsonArray("parents").get(parentIndex).getAsJsonObject();
            JsonObject condition = parent.getAsJsonArray("conditions").get(0).getAsJsonObject();
            Assertions.assertEquals(status, condition.get("status").getAsString());
            Assertions.assertEquals(reason, condition.get("reason").getAsString());
        }
    }
}
