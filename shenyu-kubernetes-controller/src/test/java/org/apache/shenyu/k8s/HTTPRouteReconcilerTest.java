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

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
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
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

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
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        verify(apiClient, never()).execute(any(okhttp3.Call.class));
    }

    /**
     * A parentRef carrying a port must only attach through listeners on that port: with the
     * parentRef port matching no listener, the attachment is rejected with the spec-defined
     * Accepted=False/NoMatchingParent rather than being accepted through an unrelated listener.
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
        // parentRef demands port 443; the only listener serves 9195
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs")
                .get(0).getAsJsonObject().addProperty("port", 443);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NoMatchingParent");
    }

    /**
     * A sectionName matching no listener means the parentRef resolves to no parent, which the
     * spec reports as Accepted=False/NoMatchingParent rather than an unsupported value.
     */
    @Test
    public void testUnmatchedSectionNameReportsNoMatchingParent() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
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
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs")
                .get(0).getAsJsonObject().addProperty("sectionName", "does-not-exist");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NoMatchingParent");
    }

    /**
     * sectionName and port must match the same listener: a parentRef naming a listener that serves
     * another port matches no listener at all, so the spec reason is NoMatchingParent, not an
     * acceptance through whichever listener satisfied one of the two criteria.
     */
    @Test
    public void testSectionNameAndPortMustMatchSameListener() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);
        JsonObject altListener = new JsonObject();
        altListener.addProperty("name", "http-alt");
        altListener.addProperty("protocol", "HTTP");
        altListener.addProperty("port", 9196);
        gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").add(altListener);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        // the named listener exists, and the port exists, but not on the same listener
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs").get(0).getAsJsonObject()
                .addProperty("sectionName", "http-alt");
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs").get(0).getAsJsonObject()
                .addProperty("port", 9195);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NoMatchingParent");
    }

    /**
     * A route whose parentRefs mix an acceptable parent with two differently rejected ones must report
     * each verdict on its own parent entry with its own reason, and still program the accepted one:
     * one rejection would drop the reachable parent, and a single shared reason would describe two
     * different failures as one.
     */
    @Test
    public void testPartiallyAcceptedParentsReportPerParentReasons() throws Exception {
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
        // second ShenYu Gateway in another namespace: allowedRoutes=Same denies the cross-namespace attachment
        DynamicKubernetesObject otherGateway = buildGateway("otherNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("otherNamespace/shenyu-gateway")).thenReturn(otherGateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        JsonObject unmatchedParentRef = new JsonObject();
        unmatchedParentRef.addProperty("name", "shenyu-gateway");
        unmatchedParentRef.addProperty("namespace", "mockedNamespace");
        unmatchedParentRef.addProperty("sectionName", "does-not-exist");
        JsonObject deniedParentRef = new JsonObject();
        deniedParentRef.addProperty("name", "shenyu-gateway");
        deniedParentRef.addProperty("namespace", "otherNamespace");
        JsonArray parentRefs = httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs");
        parentRefs.add(unmatchedParentRef);
        parentRefs.add(deniedParentRef);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        // the accepted parent still programs the route
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());

        JsonArray parents = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status").getAsJsonArray("parents");
        Assertions.assertEquals(3, parents.size(), "every parentRef must get its own status entry");
        int accepted = 0;
        Set<String> rejectionReasons = new LinkedHashSet<>();
        for (JsonElement parentElement : parents) {
            for (JsonElement conditionElement : parentElement.getAsJsonObject().getAsJsonArray("conditions")) {
                JsonObject condition = conditionElement.getAsJsonObject();
                if (!"Accepted".equals(condition.get("type").getAsString())) {
                    continue;
                }
                if ("True".equals(condition.get("status").getAsString())) {
                    accepted++;
                } else {
                    rejectionReasons.add(condition.get("reason").getAsString());
                }
            }
        }
        Assertions.assertEquals(1, accepted, "the matching parentRef must stay accepted");
        Assertions.assertEquals(Set.of("NoMatchingParent", "NotAllowedByListeners"), rejectionReasons,
                "each rejected parentRef must carry its own reason");
    }

    /**
     * A served listener denying the route must not claim that no listener permits it when a listener
     * this controller cannot serve would accept it: the truthful reason is the capability limit.
     */
    @Test
    public void testUnservedPermittingListenerReportsUnsupportedValue() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGatewayOnPort("gatewayNamespace", "shenyu-gateway", "shenyu", 9195);
        JsonObject servedListener = gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").get(0).getAsJsonObject();
        servedListener.addProperty("hostname", "a.example.com");
        when(gatewayIndexer.getByKey("gatewayNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);
        JsonObject httpsListener = new JsonObject();
        httpsListener.addProperty("name", "https");
        httpsListener.addProperty("protocol", "HTTPS");
        httpsListener.addProperty("port", 443);
        httpsListener.addProperty("hostname", "b.example.com");
        JsonObject httpsNamespaces = new JsonObject();
        httpsNamespaces.addProperty("from", "All");
        JsonObject httpsAllowedRoutes = new JsonObject();
        httpsAllowedRoutes.add("namespaces", httpsNamespaces);
        httpsListener.add("allowedRoutes", httpsAllowedRoutes);
        gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").add(httpsListener);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("routeNamespace", "test-route",
                "gatewayNamespace", "shenyu-gateway", "testService", 8189, "/**");
        JsonArray hostnames = new JsonArray();
        hostnames.add("b.example.com");
        httpRoute.getRaw().getAsJsonObject("spec").add("hostnames", hostnames);
        when(httpRouteIndexer.getByKey("routeNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("routeNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "UnsupportedValue");
    }

    /**
     * A route already programmed and then rejected by every parent must lose its data-plane config:
     * a rejection that keeps routing would be the silent broadening §5.1 rules out.
     */
    @Test
    public void testAllParentsRejectedDeletesPreviouslyProgrammedConfig() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("gatewayNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("gatewayNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("routeNamespace", "test-route",
                "gatewayNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("routeNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        cache.putRouteSelectors("routeNamespace", "test-route", "divide", List.of("sel-1"));

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("routeNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NotAllowedByListeners");
    }

    /**
     * A denying served listener must not veto a route another served listener accepts: acceptance is
     * checked before any denial, so a Gateway with one restrictive and one permissive listener serves
     * the route instead of rejecting it.
     */
    @Test
    public void testAcceptingServedListenerWinsOverDenyingOne() throws Exception {
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
        JsonObject servingListener = gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").get(0).getAsJsonObject();
        servingListener.addProperty("name", "http-b");
        servingListener.addProperty("hostname", "b.example.com");
        JsonObject denyingListener = new JsonObject();
        denyingListener.addProperty("name", "http-a");
        denyingListener.addProperty("protocol", "HTTP");
        denyingListener.addProperty("port", 9195);
        denyingListener.addProperty("hostname", "a.example.com");
        JsonObject tlsRouteKind = new JsonObject();
        tlsRouteKind.addProperty("group", "gateway.networking.k8s.io");
        tlsRouteKind.addProperty("kind", "TLSRoute");
        JsonArray allowedKinds = new JsonArray();
        allowedKinds.add(tlsRouteKind);
        JsonObject allowedRoutes = new JsonObject();
        allowedRoutes.add("kinds", allowedKinds);
        denyingListener.add("allowedRoutes", allowedRoutes);
        gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").add(denyingListener);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        JsonArray hostnames = new JsonArray();
        hostnames.add("b.example.com");
        httpRoute.getRaw().getAsJsonObject("spec").add("hostnames", hostnames);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
        JsonArray parents = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status").getAsJsonArray("parents");
        Assertions.assertEquals(1, parents.size());
        boolean acceptedSeen = false;
        for (JsonElement conditionElement : parents.get(0).getAsJsonObject().getAsJsonArray("conditions")) {
            JsonObject condition = conditionElement.getAsJsonObject();
            if ("Accepted".equals(condition.get("type").getAsString())) {
                acceptedSeen = true;
                Assertions.assertEquals("True", condition.get("status").getAsString(),
                        "a served listener accepting the route must outrank another served listener denying it");
            }
        }
        Assertions.assertTrue(acceptedSeen, "the status patch must report an Accepted condition");
    }

    /**
     * allowedRoutes defaults to Same namespace: a cross-namespace parentRef that no listener
     * permits is Accepted=False/NotAllowedByListeners. RefNotPermitted is reserved for
     * ResolvedRefs, so a ReferenceGrant could not have fixed this attachment.
     */
    @Test
    public void testCrossNamespaceParentRefNotAllowedReportsNotAllowedByListeners() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("gatewayNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("gatewayNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("routeNamespace", "test-route",
                "gatewayNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("routeNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("routeNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NotAllowedByListeners");
    }

    /**
     * A protocol-incompatible listener can never accept an HTTPRoute, so it must not be treated as the
     * listener that "permits" the route: a TCP listener allowing all namespaces must not turn the
     * serving HTTP listener's namespace denial into UnsupportedValue.
     */
    @Test
    public void testTcpListenerCannotOverrideHttpNamespaceDenial() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("gatewayNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("gatewayNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);
        JsonObject tcpListener = new JsonObject();
        tcpListener.addProperty("name", "tcp");
        tcpListener.addProperty("protocol", "TCP");
        tcpListener.addProperty("port", 9000);
        JsonObject tcpNamespaces = new JsonObject();
        tcpNamespaces.addProperty("from", "All");
        JsonObject tcpAllowedRoutes = new JsonObject();
        tcpAllowedRoutes.add("namespaces", tcpNamespaces);
        tcpListener.add("allowedRoutes", tcpAllowedRoutes);
        gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").add(tcpListener);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("routeNamespace", "test-route",
                "gatewayNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("routeNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("routeNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NotAllowedByListeners");
    }

    /**
     * A listener whose protocol cannot attach an HTTPRoute permits no HTTPRoute even when no
     * allowedRoutes.kinds is declared: a Gateway with only a TLS listener is a kind denial, not a
     * controller-capability limit.
     */
    @Test
    public void testProtocolIncompatibleListenerPermitsNoHttpRoute() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);
        // the Gateway serves only TLS, and declares no allowedRoutes.kinds
        JsonObject tlsListener = gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").get(0).getAsJsonObject();
        tlsListener.addProperty("protocol", "TLS");
        tlsListener.addProperty("port", 443);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NotAllowedByListeners");
    }

    /**
     * A TLS/TCP listener's allowedRoutes values are irrelevant because it can never attach an
     * HTTPRoute: its from=Selector is a kind denial, not an unsupported value the route can act on.
     */
    @Test
    public void testSelectorOnProtocolIncompatibleListenerReportsKindDenial() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("gatewayNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("gatewayNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);
        // the Gateway's only listener speaks TCP and declares the unsupported from=Selector
        JsonObject tcpListener = gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").get(0).getAsJsonObject();
        tcpListener.addProperty("name", "tcp");
        tcpListener.addProperty("protocol", "TCP");
        tcpListener.addProperty("port", 9000);
        JsonObject selectorNamespaces = new JsonObject();
        selectorNamespaces.addProperty("from", "Selector");
        JsonObject tcpAllowedRoutes = new JsonObject();
        tcpAllowedRoutes.add("namespaces", selectorNamespaces);
        tcpListener.add("allowedRoutes", tcpAllowedRoutes);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("routeNamespace", "test-route",
                "gatewayNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("routeNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("routeNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NotAllowedByListeners");
    }

    /**
     * An HTTP listener on a port the data plane does not serve is blocked by that port alone: the route
     * is reported UnsupportedValue, which is what every conformance manifest hits until §7.1 lands.
     */
    @Test
    public void testUnservedPortOnlyReportsUnsupportedValue() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGatewayOnPort("mockedNamespace", "shenyu-gateway", "shenyu", 80);
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "UnsupportedValue");
    }

    /**
     * A listener whose allowedRoutes.kinds excludes HTTPRoute permits no HTTP route, which the
     * spec reports as Accepted=False/NotAllowedByListeners.
     */
    @Test
    public void testAllowedRoutesKindDenialReportsNotAllowedByListeners() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildKindRestrictedGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NotAllowedByListeners");
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
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

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
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

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
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("route-ns", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).saveOrUpdateSelectorData(any());
    }

    /**
     * A listener with allowedRoutes from=Selector is an unsupported value, not a permission denial:
     * the route must be rejected with Accepted=False/UnsupportedValue (never RefNotPermitted, which
     * would suggest a ReferenceGrant could fix it) and nothing may be programmed.
     */
    @Test
    public void testSelectorAllowedRoutesReportsUnsupportedValue() throws Exception {
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
        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);
        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "UnsupportedValue");
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

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);

        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");
        assertRejectedWith(bodyCaptor.getValue(), "UnsupportedValue");
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
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

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
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository).deleteSelectorWithRules("divide", "sel-1");
        verify(apiClient).execute(any(okhttp3.Call.class));
    }

    /**
     * A policy denial must be reported as a denial even when the listener's port is not the served
     * port: the reason has to follow the specification, not the port model, or the conformance
     * cross-namespace test would see UnsupportedValue instead of NotAllowedByListeners.
     */
    @Test
    public void testUnservedListenerPolicyDenialReportsNotAllowedByListeners() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGatewayOnPort("gatewayNamespace", "shenyu-gateway", "shenyu", 80);
        when(gatewayIndexer.getByKey("gatewayNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("routeNamespace", "test-route",
                "gatewayNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("routeNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("routeNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NotAllowedByListeners");
    }

    /** A listener hostname that does not intersect the route hostnames is reported as NoMatchingListenerHostname. */
    @Test
    public void testHostnameMismatchReportsNoMatchingListenerHostname() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").get(0)
                .getAsJsonObject().addProperty("hostname", "other.example.com");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        JsonArray hostnames = new JsonArray();
        hostnames.add("example.com");
        httpRoute.getRaw().getAsJsonObject("spec").add("hostnames", hostnames);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NoMatchingListenerHostname");
    }

    /**
     * Several served listeners denying with different reasons must report the most specific one, no
     * matter where they sit in the listeners array: a policy denial outranks a hostname mismatch, and
     * the first and last listeners here mismatch so neither declaration order nor iteration order wins.
     */
    @Test
    public void testTwoServedDenialsReportHighestPriorityReason() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        DynamicKubernetesObject gateway = buildGateway("mockedNamespace", "shenyu-gateway", "shenyu");
        when(gatewayIndexer.getByKey("mockedNamespace/shenyu-gateway")).thenReturn(gateway);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);
        JsonArray listeners = gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners");
        listeners.get(0).getAsJsonObject().addProperty("name", "http-a");
        listeners.get(0).getAsJsonObject().addProperty("hostname", "a.example.com");
        JsonObject kindDeniedListener = new JsonObject();
        kindDeniedListener.addProperty("name", "http-b");
        kindDeniedListener.addProperty("protocol", "HTTP");
        kindDeniedListener.addProperty("port", 9195);
        JsonObject tlsRouteKind = new JsonObject();
        tlsRouteKind.addProperty("group", "gateway.networking.k8s.io");
        tlsRouteKind.addProperty("kind", "TLSRoute");
        JsonArray allowedKinds = new JsonArray();
        allowedKinds.add(tlsRouteKind);
        JsonObject allowedRoutes = new JsonObject();
        allowedRoutes.add("kinds", allowedKinds);
        kindDeniedListener.add("allowedRoutes", allowedRoutes);
        listeners.add(kindDeniedListener);
        JsonObject secondMismatchListener = new JsonObject();
        secondMismatchListener.addProperty("name", "http-c");
        secondMismatchListener.addProperty("protocol", "HTTP");
        secondMismatchListener.addProperty("port", 9195);
        secondMismatchListener.addProperty("hostname", "c.example.com");
        listeners.add(secondMismatchListener);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        JsonArray hostnames = new JsonArray();
        hostnames.add("b.example.com");
        httpRoute.getRaw().getAsJsonObject("spec").add("hostnames", hostnames);
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NotAllowedByListeners");
    }

    /** A parentRef to a Gateway that does not exist resolves to no parent, reported as NoMatchingParent. */
    @Test
    public void testMissingGatewayReportsNoMatchingParent() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);
        final HttpRouteParser httpRouteParser = new HttpRouteParser(endpointsLister,
                new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer = mock(SharedIndexInformer.class);
        final Indexer<DynamicKubernetesObject> httpRouteIndexer = mock(Indexer.class);
        final DynamicKubernetesObject httpRoute = buildHTTPRoute("mockedNamespace", "test-route",
                "mockedNamespace", "shenyu-gateway", "testService", 8189, "/**");
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(shenyuCacheRepository, never()).saveOrUpdateSelectorData(any());
        assertRejectedWith(bodyCaptor.getValue(), "NoMatchingParent");
    }

    /**
     * A route already reported with the old reason must be patched to the spec reason: the status
     * comparison treats the reason of a False condition as significant, which is what corrects an
     * already-reported route after an upgrade instead of leaving it stale.
     */
    @Test
    public void testReasonChangeIsPatched() throws Exception {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
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
        httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs")
                .get(0).getAsJsonObject().addProperty("sectionName", "does-not-exist");
        httpRoute.getRaw().add("status", staleShenyuStatus("does-not-exist"));
        when(httpRouteIndexer.getByKey("mockedNamespace/test-route")).thenReturn(httpRoute);
        when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);

        ArgumentCaptor<Object> bodyCaptor = ArgumentCaptor.forClass(Object.class);
        ApiClient apiClient = mockApiClientCapturingStatusPatch(bodyCaptor);
        ShenyuCacheRepository shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mockGatewayClassInformer();
        HTTPRouteReconciler httpRouteReconciler = new HTTPRouteReconciler(httpRouteInformer, gatewayInformer,
                gatewayClassInformer, httpRouteParser, shenyuCacheRepository, apiClient, 9195);

        Result result = httpRouteReconciler.reconcile(new Request("mockedNamespace", "test-route"));
        Assertions.assertEquals(new Result(false), result);
        verify(apiClient).execute(any(okhttp3.Call.class));
        assertRejectedWith(bodyCaptor.getValue(), "NoMatchingParent");
    }

    /** Status ShenYu previously wrote for the mockedNamespace/shenyu-gateway parent, using the pre-fix reason. */
    private JsonObject staleShenyuStatus(final String sectionName) {
        JsonObject parentRef = new JsonObject();
        parentRef.addProperty("group", "gateway.networking.k8s.io");
        parentRef.addProperty("kind", "Gateway");
        parentRef.addProperty("namespace", "mockedNamespace");
        parentRef.addProperty("name", "shenyu-gateway");
        parentRef.addProperty("sectionName", sectionName);
        JsonObject accepted = new JsonObject();
        accepted.addProperty("type", "Accepted");
        accepted.addProperty("status", "False");
        accepted.addProperty("reason", "UnsupportedValue");
        JsonObject resolvedRefs = new JsonObject();
        resolvedRefs.addProperty("type", "ResolvedRefs");
        resolvedRefs.addProperty("status", "True");
        resolvedRefs.addProperty("reason", "ResolvedRefs");
        JsonArray conditions = new JsonArray();
        conditions.add(accepted);
        conditions.add(resolvedRefs);
        JsonObject parent = new JsonObject();
        parent.add("parentRef", parentRef);
        parent.addProperty("controllerName", "gateway.shenyu.apache.org/shenyu-controller");
        parent.add("conditions", conditions);
        JsonArray parents = new JsonArray();
        parents.add(parent);
        JsonObject status = new JsonObject();
        status.add("parents", parents);
        return status;
    }

    /** Mock ApiClient whose status-patch pipeline succeeds: authentications resolve empty and buildCall returns a mock call accepted by execute. */
    private ApiClient mockApiClientWithStatusPatch() throws ApiException {
        return mockApiClientCapturingStatusPatch(ArgumentCaptor.forClass(Object.class));
    }

    /** Same as {@link #mockApiClientWithStatusPatch()} but captures the status patch body for assertions. */
    private ApiClient mockApiClientCapturingStatusPatch(final ArgumentCaptor<Object> bodyCaptor) throws ApiException {
        ApiClient apiClient = mock(ApiClient.class);
        when(apiClient.getAuthentications()).thenReturn(Map.of());
        when(apiClient.buildCall(any(), any(), any(), any(), bodyCaptor.capture(), any(), any(), any(), any(), any()))
                .thenReturn(mock(okhttp3.Call.class));
        return apiClient;
    }

    /** Assert the status patch rejects exactly one parent entry with the expected Accepted reason, every entry carrying both conditions. */
    private void assertRejectedWith(final Object patchBody, final String expectedReason) {
        JsonArray parents = ((JsonObject) patchBody).getAsJsonObject("status").getAsJsonArray("parents");
        int rejected = 0;
        for (JsonElement parentElement : parents) {
            JsonArray conditions = parentElement.getAsJsonObject().getAsJsonArray("conditions");
            Assertions.assertEquals(2, conditions.size(), "every parent entry reports Accepted and ResolvedRefs");
            for (JsonElement conditionElement : conditions) {
                JsonObject condition = conditionElement.getAsJsonObject();
                if ("Accepted".equals(condition.get("type").getAsString())
                        && "False".equals(condition.get("status").getAsString())) {
                    rejected++;
                    Assertions.assertEquals(expectedReason, condition.get("reason").getAsString(),
                            "unexpected Accepted reason");
                }
            }
        }
        Assertions.assertEquals(1, rejected, "expected exactly one rejected parent entry in the status patch");
    }

    private DynamicKubernetesObject buildGateway(final String namespace, final String name,
                                                  final String gatewayClassName) {
        return buildGatewayOnPort(namespace, name, gatewayClassName, 9195);
    }

    /** One HTTP listener on the given port; allowedRoutes defaults to Same-namespace per the spec. */
    private DynamicKubernetesObject buildGatewayOnPort(final String namespace, final String name,
                                                        final String gatewayClassName, final int port) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", namespace);
        metadata.addProperty("name", name);

        JsonObject listener = new JsonObject();
        listener.addProperty("name", "http");
        listener.addProperty("protocol", "HTTP");
        listener.addProperty("port", port);
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

    /** A Gateway in the given namespace whose only listener accepts all namespaces but only the TLSRoute kind. */
    private DynamicKubernetesObject buildKindRestrictedGateway(final String namespace, final String name,
                                                               final String gatewayClassName) {
        JsonObject allowedKind = new JsonObject();
        allowedKind.addProperty("group", "gateway.networking.k8s.io");
        allowedKind.addProperty("kind", "TLSRoute");
        JsonArray allowedKinds = new JsonArray();
        allowedKinds.add(allowedKind);
        DynamicKubernetesObject gateway = allowAllNamespaces(buildGateway(namespace, name, gatewayClassName));
        gateway.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").get(0)
                .getAsJsonObject().getAsJsonObject("allowedRoutes").add("kinds", allowedKinds);
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
}
