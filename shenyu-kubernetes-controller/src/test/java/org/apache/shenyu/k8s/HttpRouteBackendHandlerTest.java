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
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1ServiceBuilder;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.reconciler.HttpRouteEndpointsHandler;
import org.apache.shenyu.k8s.reconciler.HttpRouteServiceHandler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The Service and Endpoints event handlers must both enqueue the HTTPRoutes referencing
 * the changed backend through the shared backend-service index: a Service port or
 * targetPort edit does not touch Endpoints, so without the Service handler routes keep
 * the stale pod port until the periodic resync.
 */
public final class HttpRouteBackendHandlerTest {

    private static final String NAMESPACE = "app-ns";

    private static final String SERVICE_NAME = "backend-svc";

    @SuppressWarnings("unchecked")
    private SharedIndexInformer<DynamicKubernetesObject> mockRouteInformer(final Indexer<DynamicKubernetesObject> indexer) {
        SharedIndexInformer<DynamicKubernetesObject> informer = mock(SharedIndexInformer.class);
        when(informer.getIndexer()).thenReturn(indexer);
        return informer;
    }

    /**
     * A Service update must enqueue the routes whose backendRefs target the Service,
     * resolved through the backend-service index.
     */
    @Test
    public void testServiceUpdateEnqueuesReferencingRoutes() {
        Indexer<DynamicKubernetesObject> indexer = mock(Indexer.class);
        when(indexer.getIndexers()).thenReturn(new HashMap<>(Map.of()));
        when(indexer.byIndex("backendService", NAMESPACE + "/" + SERVICE_NAME))
                .thenReturn(List.of(buildRoute("app-ns", "test-route", NAMESPACE, SERVICE_NAME)));
        SharedIndexInformer<DynamicKubernetesObject> informer = mockRouteInformer(indexer);
        RateLimitingQueue<Request> queue = mock(RateLimitingQueue.class);

        new HttpRouteServiceHandler(informer, queue).onUpdate(buildService(), buildService());

        verify(queue).add(new Request("app-ns", "test-route"));
    }

    /** Endpoints events keep working through the same shared index. */
    @Test
    public void testEndpointsUpdateEnqueuesReferencingRoutes() {
        Indexer<DynamicKubernetesObject> indexer = mock(Indexer.class);
        when(indexer.getIndexers()).thenReturn(new HashMap<>(Map.of()));
        when(indexer.byIndex("backendService", NAMESPACE + "/" + SERVICE_NAME))
                .thenReturn(List.of(buildRoute("app-ns", "test-route", NAMESPACE, SERVICE_NAME)));
        SharedIndexInformer<DynamicKubernetesObject> informer = mockRouteInformer(indexer);
        RateLimitingQueue<Request> queue = mock(RateLimitingQueue.class);

        new HttpRouteEndpointsHandler(informer, queue).onUpdate(buildEndpoints(), buildEndpoints());

        verify(queue).add(new Request("app-ns", "test-route"));
    }

    /** Both handlers share one informer index: the second registration must be a no-op. */
    @Test
    public void testSharedIndexRegisteredOnlyOnce() {
        Indexer<DynamicKubernetesObject> indexer = mock(Indexer.class);
        Map<String, Object> indexers = new HashMap<>();
        when(indexer.getIndexers()).thenAnswer(inv -> new HashMap<>(indexers));
        when(indexer.byIndex(anyString(), anyString())).thenReturn(List.of());
        SharedIndexInformer<DynamicKubernetesObject> informer = mockRouteInformer(indexer);
        RateLimitingQueue<Request> queue = mock(RateLimitingQueue.class);

        new HttpRouteEndpointsHandler(informer, queue);
        // after the first handler registered the shared index it is already present
        indexers.put("backendService", new Object());
        new HttpRouteServiceHandler(informer, queue);

        // the registration goes through the informer; the second handler must not repeat it
        verify(informer, times(1)).addIndexers(any());
    }

    /** An event whose Service is referenced by no route enqueues nothing. */
    @Test
    public void testUnreferencedServiceEnqueuesNothing() {
        Indexer<DynamicKubernetesObject> indexer = mock(Indexer.class);
        when(indexer.getIndexers()).thenReturn(new HashMap<>(Map.of()));
        when(indexer.byIndex(anyString(), anyString())).thenReturn(List.of());
        SharedIndexInformer<DynamicKubernetesObject> informer = mockRouteInformer(indexer);
        RateLimitingQueue<Request> queue = mock(RateLimitingQueue.class);

        new HttpRouteServiceHandler(informer, queue).onDelete(buildService(), false);

        verify(queue, never()).add(any(Request.class));
    }

    /**
     * Cross-namespace resolution: a route referencing Service {@code other-ns/svc} by
     * explicit namespace is found when that Service changes.
     */
    @Test
    public void testCrossNamespaceBackendServiceResolved() {
        Indexer<DynamicKubernetesObject> indexer = mock(Indexer.class);
        when(indexer.getIndexers()).thenReturn(new HashMap<>(Map.of()));
        when(indexer.byIndex("backendService", "other-ns/" + SERVICE_NAME))
                .thenReturn(List.of(buildRoute("app-ns", "test-route", "other-ns", SERVICE_NAME)));
        SharedIndexInformer<DynamicKubernetesObject> informer = mockRouteInformer(indexer);
        RateLimitingQueue<Request> queue = mock(RateLimitingQueue.class);

        V1Service otherNamespaceService = new V1ServiceBuilder()
                .withNewMetadata().withNamespace("other-ns").withName(SERVICE_NAME).endMetadata().build();
        new HttpRouteServiceHandler(informer, queue).onAdd(otherNamespaceService);

        ArgumentCaptor<Request> captor = ArgumentCaptor.forClass(Request.class);
        verify(queue).add(captor.capture());
        Assertions.assertEquals("app-ns", captor.getValue().getNamespace());
        Assertions.assertEquals("test-route", captor.getValue().getName());
    }

    private V1Service buildService() {
        return new V1ServiceBuilder()
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata().build();
    }

    private V1Endpoints buildEndpoints() {
        return new V1EndpointsBuilder()
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata().build();
    }

    private DynamicKubernetesObject buildRoute(final String routeNamespace, final String routeName,
                                               final String serviceNamespace, final String serviceName) {
        JsonObject backendRef = new JsonObject();
        backendRef.addProperty("name", serviceName);
        backendRef.addProperty("namespace", serviceNamespace);
        backendRef.addProperty("port", 8080);
        JsonArray backendRefs = new JsonArray();
        backendRefs.add(backendRef);

        JsonObject rule = new JsonObject();
        rule.add("backendRefs", backendRefs);
        JsonArray rules = new JsonArray();
        rules.add(rule);

        JsonObject spec = new JsonObject();
        spec.add("rules", rules);

        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", routeNamespace);
        metadata.addProperty("name", routeName);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "HTTPRoute");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
    }
}
