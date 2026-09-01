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
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.reconciler.HTTPRouteReconciler;
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
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository).saveOrUpdateRuleData(any());
        verify(apiClient).execute(any(okhttp3.Call.class));
    }

    /**
     * Regression: when the route status already carries Accepted=True and ResolvedRefs=True
     * (capitalized, per K8s condition convention), the reconciler must skip the status patch.
     * Otherwise every reconcile patches, each patch bumps resourceVersion, the watch
     * re-enqueues the route, and the controller enters an infinite reconcile/patch loop.
     */
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
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

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
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
    }

    /**
     * Test HTTPRoute deletion: should clean up selector and rule data.
     */
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
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        // No exception should be thrown; deleteConfig handles empty cache gracefully
    }

    /**
     * Idempotent reconcile: re-reconciling an unchanged HTTPRoute must NOT delete any selector,
     * because the deterministic IDs are stable across parses. This guards against the data-plane
     * churn window that occurred when every resync deleted and recreated selectors.
     */
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
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));

        // The reconciler deletes through deleteSelectorWithRules only, so that is the
        // call an unwanted delete-then-recreate would go through
        verify(shenyuCacheRepository, never()).deleteSelectorWithRules(any(), any());
    }

    /**
     * Cross-namespace parentRef WITH a matching ReferenceGrant (and a listener permitting
     * all namespaces): the route is accepted and ShenYu config is programmed.
     */
    @Test
    public void testReconcileCrossNamespaceHTTPRouteWithReferenceGrant() throws Exception {
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
        // the listener must also permit cross-namespace routes: a ReferenceGrant alone does
        // not override the listener's allowedRoutes policy (spec default: Same-namespace)
        DynamicKubernetesObject gateway = allowAllNamespaces(buildGateway("gw-ns", "shenyu-gateway", "shenyu"));
        when(gatewayIndexer.getByKey("gw-ns/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("route-ns", "test-route",
                "gw-ns", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("route-ns/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        Indexer<DynamicKubernetesObject> referenceGrantIndexer = referenceGrantInformer.getIndexer();
        DynamicKubernetesObject grant = buildReferenceGrant("gw-ns", "route-ns", "Gateway");
        when(referenceGrantIndexer.byIndex("namespace", "gw-ns")).thenReturn(List.of(grant));

        ApiClient apiClient = mockApiClientWithStatusPatch();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("route-ns", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
    }

    /**
     * A rule with filters (unsupported) must reject the whole route: nothing is programmed,
     * config from a previous valid spec of this route is dropped, and the status patch
     * reports Accepted=False with the spec-defined reason UnsupportedValue.
     */
    @Test
    public void testReconcileUnsupportedFiltersProgramsNothing() throws Exception {
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
        JsonObject filter = new JsonObject();
        filter.addProperty("type", "RequestHeaderModifier");
        JsonArray filters = new JsonArray();
        filters.add(filter);
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("rules")
                .get(0).getAsJsonObject().add("filters", filters);
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
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");

        JsonArray parents = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status").getAsJsonArray("parents");
        JsonArray conditions = parents.get(0).getAsJsonObject().getAsJsonArray("conditions");
        boolean rejectedWithUnsupportedValue = false;
        for (JsonElement element : conditions) {
            JsonObject condition = element.getAsJsonObject();
            if ("Accepted".equals(condition.get("type").getAsString())
                    && "False".equals(condition.get("status").getAsString())
                    && "UnsupportedValue".equals(condition.get("reason").getAsString())) {
                rejectedWithUnsupportedValue = true;
            }
        }
        Assertions.assertTrue(rejectedWithUnsupportedValue, "Accepted=False/UnsupportedValue not reported");
    }

    /**
     * Status entries owned by other controllers must survive ShenYu's status patch: the
     * patch body carries them over, because the merge patch replaces the parents array
     * wholesale.
     */
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
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

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

    /**
     * Mock ApiClient whose status-patch pipeline succeeds: authentications resolve empty
     * and buildCall returns a mock call accepted by execute. The patch body can be captured
     * from buildCall's fifth argument.
     */
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
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

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

    /**
     * Build a mocked referenceGrant informer backed by an empty Indexer. Mockito returns an
     * empty list for {@code byIndex(...)} by default, so a cross-namespace parentRef finds no
     * grant and is rejected. For same-namespace routes the grant check is skipped entirely.
     */
    private SharedIndexInformer<DynamicKubernetesObject> mockReferenceGrantInformer() {
        SharedIndexInformer<DynamicKubernetesObject> informer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> indexer = mock(Indexer.class);
        when(informer.getIndexer()).thenReturn(indexer);
        return informer;
    }

    /**
     * Build a ReferenceGrant dynamic object: {@code spec.from} allows an HTTPRoute from
     * {@code fromNamespace} to reference the {@code toKind} resource living in
     * {@code namespace}. The grant itself lives in {@code namespace}.
     */
    private DynamicKubernetesObject buildReferenceGrant(final String namespace, final String fromNamespace,
                                                        final String toKind) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", namespace);
        metadata.addProperty("name", "allow-" + fromNamespace);

        JsonObject from = new JsonObject();
        from.addProperty("group", "gateway.networking.k8s.io");
        from.addProperty("kind", "HTTPRoute");
        from.addProperty("namespace", fromNamespace);
        JsonArray fromArray = new JsonArray();
        fromArray.add(from);

        JsonObject to = new JsonObject();
        to.addProperty("group", "gateway.networking.k8s.io");
        to.addProperty("kind", toKind);
        JsonArray toArray = new JsonArray();
        toArray.add(to);

        JsonObject spec = new JsonObject();
        spec.add("from", fromArray);
        spec.add("to", toArray);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "ReferenceGrant");
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
