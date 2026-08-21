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
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

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
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

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
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

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
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        verify(apiClient, never()).execute(any(okhttp3.Call.class));
    }

    /**
     * Test HTTPRoute not bound to any ShenYu Gateway: should skip without creating selector/rule.
     */
    @Test
    public void testReconcileUnboundHTTPRoute() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject otherGateway = buildGateway("mockedNamespace", "other-gateway", "other-class");
        when(gatewayIndexer.getByKey("mockedNamespace/other-gateway")).thenReturn(otherGateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "other-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        ApiClient apiClient = mock(ApiClient.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository, never()).saveOrUpdateRuleData(any());
        verify(apiClient, never()).execute(any(okhttp3.Call.class));
    }

    /**
     * Test HTTPRoute deletion: should clean up selector and rule data.
     */
    @Test
    public void testReconcileHTTPRouteDeletion() {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

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
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

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
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

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
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

        httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));

        // The reconciler deletes through deleteSelectorWithRules only, so that is the
        // call an unwanted delete-then-recreate would go through
        verify(shenyuCacheRepository, never()).deleteSelectorWithRules(any(), any());
    }

    /**
     * Cross-namespace parentRef without a ReferenceGrant: the route must be rejected (not
     * bound to a ShenYu Gateway), so no selector/rule is programmed and no status patch is
     * attempted. Per Gateway API, ReferenceGrant is mandatory for cross-namespace parentRefs.
     */
    @Test
    public void testReconcileCrossNamespaceHTTPRouteWithoutReferenceGrant() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("route-ns").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("route-ns/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("gw-ns", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("gw-ns/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("route-ns", "test-route",
                "gw-ns", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("route-ns/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        ApiClient apiClient = mock(ApiClient.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("route-ns", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        verify(apiClient, never()).execute(any(okhttp3.Call.class));
    }

    /**
     * Cross-namespace parentRef WITH a matching ReferenceGrant: the route is accepted and
     * ShenYu config is programmed (selector/rule saved). This is the positive counterpart
     * to {@link #testReconcileCrossNamespaceHTTPRouteWithoutReferenceGrant}.
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
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("gw-ns", "shenyu-gateway", "shenyu");
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
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("route-ns", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
    }

    /**
     * Mixed parentRefs: a valid same-namespace ShenYu Gateway plus a cross-namespace
     * ShenYu Gateway without a ReferenceGrant. The route is accepted via the valid
     * parent, but the unauthorized parent must not be programmed: no gateway binding
     * and no Accepted status entry for it.
     */
    @Test
    public void testReconcileMixedParentRefsSkipUnauthorizedCrossNamespaceParent() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("route-ns").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("route-ns/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayIndexer.getByKey("route-ns/local-gateway")).thenReturn(buildGateway("route-ns", "local-gateway", "shenyu"));
        when(gatewayIndexer.getByKey("gw-ns/remote-gateway")).thenReturn(buildGateway("gw-ns", "remote-gateway", "shenyu"));
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("route-ns", "test-route",
                "route-ns", "local-gateway", "testService", 8189, "/**");
        JsonObject remoteParentRef = new JsonObject();
        remoteParentRef.addProperty("name", "remote-gateway");
        remoteParentRef.addProperty("namespace", "gw-ns");
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs").add(remoteParentRef);
        when(httpRouteIndexer.getByKey("route-ns/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();

        ApiClient apiClient = mockApiClientWithStatusPatch();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("route-ns", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());

        Assertions.assertEquals(java.util.Set.of("route-ns/local-gateway"),
                GatewayRouteCache.getInstance().getGatewaysForRoute("route-ns", "test-route"));

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        verify(apiClient).buildCall(any(), any(), any(), any(), bodyCaptor.capture(), any(), any(), any(), any(), any());
        JsonObject patchBody = (JsonObject) bodyCaptor.getValue();
        JsonArray parents = patchBody.getAsJsonObject("status").getAsJsonArray("parents");
        Assertions.assertEquals(1, parents.size());
        Assertions.assertEquals("local-gateway",
                parents.get(0).getAsJsonObject().getAsJsonObject("parentRef").get("name").getAsString());
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
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

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
        cache.bindRouteToGateway("mockedNamespace", "other-gateway", "mockedNamespace", "test-route");
        cache.putRouteSelectors("mockedNamespace", "test-route", "divide", List.of("sel-1"));

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        ApiClient apiClient = mockApiClientWithStatusPatch();

        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");
        verify(apiClient).execute(any(okhttp3.Call.class));
    }

    /**
     * A bound HTTPRoute without spec.rules (legal per the CRD, e.g. a skeleton created
     * before its rules) must reconcile cleanly: no selector programmed, previously
     * programmed selectors cleaned up, no NPE/requeue loop.
     */
    @Test
    public void testReconcileBoundRouteWithoutRules() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(
                buildGateway("mockedNamespace", "shenyu-gateway", "shenyu"));
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        httpRoute.getRaw().getAsJsonObject("spec").remove("rules");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        GatewayRouteCache.getInstance().putRouteSelectors("mockedNamespace", "test-route", "divide", List.of("sel-1"));

        ApiClient apiClient = mockApiClientWithStatusPatch();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                mockGatewayClassInformer(), mockReferenceGrantInformer(), httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");
    }

    /**
     * A parentRef explicitly pointing at a non-Gateway kind (or a foreign group) is not a
     * ShenYu parent: the route is treated as unbound and nothing is programmed.
     */
    @Test
    public void testReconcileParentRefOfNonGatewayKindRejected() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(
                buildGateway("mockedNamespace", "shenyu-gateway", "shenyu"));
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs")
                .get(0).getAsJsonObject().addProperty("kind", "NotGateway");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        ApiClient apiClient = mock(ApiClient.class);
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                mockGatewayClassInformer(), mockReferenceGrantInformer(), httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        verify(apiClient, never()).execute(any(okhttp3.Call.class));
    }

    /**
     * Cross-namespace backendRef without a ReferenceGrant: no selector is programmed and
     * the status patch reports ResolvedRefs=False with the spec-defined RefNotPermitted
     * reason (not BackendNotFound).
     */
    @Test
    public void testReconcileCrossNamespaceBackendRefReportsRefNotPermitted() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("backend-ns").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("backend-ns/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        httpRoute.getRaw().getAsJsonObject("spec")
                .getAsJsonArray("rules").get(0).getAsJsonObject()
                .getAsJsonArray("backendRefs").get(0).getAsJsonObject()
                .addProperty("namespace", "backend-ns");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ApiClient apiClient = mockApiClientWithStatusPatch();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        verify(apiClient).buildCall(any(), any(), any(), any(), bodyCaptor.capture(), any(), any(), any(), any(), any());
        JsonObject patchBody = (JsonObject) bodyCaptor.getValue();
        Assertions.assertTrue(patchBody.toString().contains("RefNotPermitted"),
                "status patch must carry ResolvedRefs=False/RefNotPermitted");
    }

    /**
     * A parentRef added to an already-accepted route must get a status entry: the skip
     * check verifies every desired ShenYu parent is present, not just that existing
     * entries carry matching conditions.
     */
    @Test
    public void testReconcilePatchesStatusWhenParentEntryMissing() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder()
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        DynamicKubernetesObject secondGateway = buildGateway("mockedNamespace", "shenyu-gateway-2", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway-2")).thenReturn(secondGateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        JsonObject secondParentRef = new JsonObject();
        secondParentRef.addProperty("name", "shenyu-gateway-2");
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs").add(secondParentRef);
        JsonObject accepted = new JsonObject();
        accepted.addProperty("type", "Accepted");
        accepted.addProperty("status", "True");
        JsonObject resolvedRefs = new JsonObject();
        resolvedRefs.addProperty("type", "ResolvedRefs");
        resolvedRefs.addProperty("status", "True");
        JsonArray conditions = new JsonArray();
        conditions.add(accepted);
        conditions.add(resolvedRefs);
        JsonObject parentRefStatus = new JsonObject();
        parentRefStatus.addProperty("namespace", "mockedNamespace");
        parentRefStatus.addProperty("name", "shenyu-gateway");
        JsonObject parent = new JsonObject();
        parent.add("parentRef", parentRefStatus);
        parent.addProperty("controllerName", "gateway.shenyu.apache.org/shenyu-controller");
        parent.add("conditions", conditions);
        JsonArray parents = new JsonArray();
        parents.add(parent);
        JsonObject status = new JsonObject();
        status.add("parents", parents);
        httpRoute.getRaw().add("status", status);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ApiClient apiClient = mockApiClientWithStatusPatch();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(apiClient).execute(any(okhttp3.Call.class));
    }

    /**
     * A stale ShenYu status entry for a parentRef that is no longer in the spec (e.g. its
     * Gateway was deleted) must trigger a patch so the entry is dropped; matching only
     * the desired entries would leave the stale one forever.
     */
    @Test
    public void testReconcilePatchesStatusWhenStaleShenYuEntryExists() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder()
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

        final SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
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
        JsonObject firstRef = new JsonObject();
        firstRef.addProperty("namespace", "mockedNamespace");
        firstRef.addProperty("name", "shenyu-gateway");
        JsonObject first = new JsonObject();
        first.add("parentRef", firstRef);
        first.addProperty("controllerName", "gateway.shenyu.apache.org/shenyu-controller");
        first.add("conditions", conditions);
        JsonObject secondRef = new JsonObject();
        secondRef.addProperty("namespace", "mockedNamespace");
        secondRef.addProperty("name", "shenyu-gateway-2");
        JsonObject second = new JsonObject();
        second.add("parentRef", secondRef);
        second.addProperty("controllerName", "gateway.shenyu.apache.org/shenyu-controller");
        second.add("conditions", conditions);
        JsonArray parentArray = new JsonArray();
        parentArray.add(first);
        parentArray.add(second);
        JsonObject status = new JsonObject();
        status.add("parents", parentArray);
        httpRoute.getRaw().add("status", status);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        final ApiClient apiClient = mockApiClientWithStatusPatch();

        final ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = mockReferenceGrantInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(apiClient).execute(any(okhttp3.Call.class));
    }

    /**
     * When a patch adds an entry for a new parent, an unchanged parent must keep its original
     * lastTransitionTime: the spec requires it to advance only on a status transition.
     */
    @Test
    public void testPatchPreservesTransitionTimeOfUnchangedParent() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder()
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister, new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(
                buildGateway("mockedNamespace", "shenyu-gateway", "shenyu"));
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway-2")).thenReturn(
                buildGateway("mockedNamespace", "shenyu-gateway-2", "shenyu"));
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        JsonObject secondParentRef = new JsonObject();
        secondParentRef.addProperty("name", "shenyu-gateway-2");
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs").add(secondParentRef);

        final String oldTimestamp = "2020-01-01T00:00:00Z";
        JsonArray parents = new JsonArray();
        parents.add(buildShenYuParentStatus("shenyu-gateway", oldTimestamp));
        JsonObject status = new JsonObject();
        status.add("parents", parents);
        httpRoute.getRaw().add("status", status);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ApiClient apiClient = mockApiClientWithStatusPatch();

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                mockGatewayClassInformer(), mockReferenceGrantInformer(), httpRouteParser, shenyuCacheRepository, apiClient);

        httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        verify(apiClient).buildCall(any(), any(), any(), any(), bodyCaptor.capture(), any(), any(), any(), any(), any());
        JsonArray patchedParents = ((JsonObject) bodyCaptor.getValue())
                .getAsJsonObject("status").getAsJsonArray("parents");
        Assertions.assertEquals(2, patchedParents.size());
        for (JsonElement parentEl : patchedParents) {
            JsonObject patched = parentEl.getAsJsonObject();
            String name = patched.getAsJsonObject("parentRef").get("name").getAsString();
            for (JsonElement conditionEl : patched.getAsJsonArray("conditions")) {
                String transitionTime = conditionEl.getAsJsonObject().get("lastTransitionTime").getAsString();
                if ("shenyu-gateway".equals(name)) {
                    Assertions.assertEquals(oldTimestamp, transitionTime,
                            "unchanged parent must keep its original lastTransitionTime");
                } else {
                    Assertions.assertNotEquals(oldTimestamp, transitionTime,
                            "new parent entry must carry a fresh lastTransitionTime");
                }
            }
        }
    }

    /** A ShenYu-owned status.parents entry for one Gateway, all conditions carrying lastTransitionTime. */
    private JsonObject buildShenYuParentStatus(final String parentName, final String lastTransitionTime) {
        JsonObject accepted = new JsonObject();
        accepted.addProperty("type", "Accepted");
        accepted.addProperty("status", "True");
        accepted.addProperty("lastTransitionTime", lastTransitionTime);
        JsonObject resolvedRefs = new JsonObject();
        resolvedRefs.addProperty("type", "ResolvedRefs");
        resolvedRefs.addProperty("status", "True");
        resolvedRefs.addProperty("lastTransitionTime", lastTransitionTime);
        JsonArray conditions = new JsonArray();
        conditions.add(accepted);
        conditions.add(resolvedRefs);
        JsonObject parentRefStatus = new JsonObject();
        parentRefStatus.addProperty("namespace", "mockedNamespace");
        parentRefStatus.addProperty("name", parentName);
        JsonObject parent = new JsonObject();
        parent.add("parentRef", parentRefStatus);
        parent.addProperty("controllerName", "gateway.shenyu.apache.org/shenyu-controller");
        parent.add("conditions", conditions);
        return parent;
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
