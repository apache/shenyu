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
import io.kubernetes.client.informer.cache.Cache;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.reconciler.HttpRouteEndpointsHandler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

/**
 * Test cases for HttpRouteEndpointsHandler. Uses a real {@link Cache} as the informer
 * indexer so the backendService index registration and lookups are exercised for real.
 */
public final class HttpRouteEndpointsHandlerTest {

    private SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer;

    private Cache<DynamicKubernetesObject> httpRouteIndexer;

    private RateLimitingQueue<Request> queue;

    private HttpRouteEndpointsHandler handler;

    @BeforeEach
    public void setUp() {
        httpRouteInformer = Mockito.mock(SharedIndexInformer.class);
        httpRouteIndexer = new Cache<>();
        Mockito.when(httpRouteInformer.getIndexer()).thenReturn(httpRouteIndexer);
        Mockito.doAnswer(invocation -> {
            httpRouteIndexer.addIndexers(invocation.getArgument(0));
            return null;
        }).when(httpRouteInformer).addIndexers(Mockito.any());
        queue = Mockito.mock(RateLimitingQueue.class);
        handler = new HttpRouteEndpointsHandler(httpRouteInformer, queue);
    }

    private void givenRoutes(final DynamicKubernetesObject... routes) {
        for (DynamicKubernetesObject route : routes) {
            httpRouteIndexer.add(route);
        }
    }

    private V1Endpoints endpoints(final String namespace, final String name) {
        return new V1Endpoints().metadata(new V1ObjectMeta().namespace(namespace).name(name));
    }

    private DynamicKubernetesObject route(final String namespace, final String name,
                                          final JsonObject... backendRefs) {
        JsonArray refs = new JsonArray();
        for (JsonObject ref : backendRefs) {
            refs.add(ref);
        }
        JsonObject rule = new JsonObject();
        rule.add("backendRefs", refs);
        JsonArray rules = new JsonArray();
        rules.add(rule);

        JsonObject spec = new JsonObject();
        spec.add("rules", rules);

        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", namespace);
        metadata.addProperty("name", name);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "HTTPRoute");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
    }

    private JsonObject serviceRef(final String name) {
        JsonObject ref = new JsonObject();
        ref.addProperty("name", name);
        ref.addProperty("port", 8189);
        return ref;
    }

    @Test
    public void testEnqueuesRoutesReferencingTheService() {
        DynamicKubernetesObject matching = route("default", "route-a", serviceRef("backend-svc"));
        DynamicKubernetesObject other = route("default", "route-b", serviceRef("another-svc"));
        givenRoutes(matching, other);

        handler.onAdd(endpoints("default", "backend-svc"));

        ArgumentCaptor<Request> captor = ArgumentCaptor.forClass(Request.class);
        Mockito.verify(queue).add(captor.capture());
        Assertions.assertEquals("route-a", captor.getValue().getName());
        Assertions.assertEquals("default", captor.getValue().getNamespace());
    }

    @Test
    public void testBackendNamespaceDefaultsToRouteNamespace() {
        DynamicKubernetesObject sameNs = route("default", "route-a", serviceRef("backend-svc"));
        givenRoutes(sameNs);

        // Endpoints in another namespace must not match a default-namespace backendRef
        handler.onAdd(endpoints("other-ns", "backend-svc"));
        Mockito.verify(queue, Mockito.never()).add(Mockito.any(Request.class));

        handler.onDelete(endpoints("default", "backend-svc"), false);
        Mockito.verify(queue).add(new Request("default", "route-a"));
    }

    @Test
    public void testExplicitBackendNamespaceIsMatched() {
        JsonObject ref = serviceRef("backend-svc");
        ref.addProperty("namespace", "shared-ns");
        DynamicKubernetesObject crossNs = route("route-ns", "route-a", ref);
        givenRoutes(crossNs);

        handler.onUpdate(endpoints("shared-ns", "backend-svc"), endpoints("shared-ns", "backend-svc"));
        Mockito.verify(queue).add(new Request("route-ns", "route-a"));
    }

    @Test
    public void testNonServiceBackendRefIsIgnored() {
        JsonObject ref = serviceRef("backend-svc");
        ref.addProperty("kind", "Gateway");
        givenRoutes(route("default", "route-a", ref));

        handler.onAdd(endpoints("default", "backend-svc"));
        Mockito.verify(queue, Mockito.never()).add(Mockito.any(Request.class));
    }
}
