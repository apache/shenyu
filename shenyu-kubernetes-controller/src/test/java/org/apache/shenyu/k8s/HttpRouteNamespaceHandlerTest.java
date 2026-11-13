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
import io.kubernetes.client.openapi.models.V1Namespace;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.reconciler.HttpRouteNamespaceHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Namespace selector events must reconsider all cached routes using the default namespace index.
 */
public final class HttpRouteNamespaceHandlerTest {

    private static final String NAMESPACE = "app-ns";

    private Cache<DynamicKubernetesObject> routes;

    private RateLimitingQueue<Request> queue;

    private HttpRouteNamespaceHandler handler;

    @BeforeEach
    @SuppressWarnings("unchecked")
    public void setUp() {
        routes = new Cache<>();
        SharedIndexInformer<DynamicKubernetesObject> informer = mock(SharedIndexInformer.class);
        when(informer.getIndexer()).thenReturn(routes);
        queue = mock(RateLimitingQueue.class);
        handler = new HttpRouteNamespaceHandler(informer, queue);
    }

    @Test
    public void testAddQueuesUnboundAndRejectedRoutesOnlyInNamespace() {
        routes.add(route(NAMESPACE, "unbound", false));
        routes.add(route(NAMESPACE, "rejected", true));
        routes.add(route("other-ns", "other", false));

        handler.onAdd(namespace(Map.of("team", "blue")));

        verify(queue).add(new Request(NAMESPACE, "unbound"));
        verify(queue).add(new Request(NAMESPACE, "rejected"));
        verify(queue, never()).add(new Request("other-ns", "other"));
    }

    @Test
    public void testLabelChangeQueuesRoutes() {
        routes.add(route(NAMESPACE, "route", false));

        handler.onUpdate(namespace(Map.of("team", "blue")), namespace(Map.of("team", "green")));

        verify(queue).add(new Request(NAMESPACE, "route"));
    }

    @Test
    public void testLabelAdditionQueuesRoutes() {
        routes.add(route(NAMESPACE, "route", false));

        handler.onUpdate(namespace(null), namespace(Map.of("team", "blue")));

        verify(queue).add(new Request(NAMESPACE, "route"));
    }

    @Test
    public void testLabelRemovalQueuesRoutes() {
        routes.add(route(NAMESPACE, "route", false));

        handler.onUpdate(namespace(Map.of("team", "blue")), namespace(null));

        verify(queue).add(new Request(NAMESPACE, "route"));
    }

    @Test
    public void testMetadataOnlyUpdatesDoNotQueueRoutes() {
        routes.add(route(NAMESPACE, "route", false));
        V1Namespace oldNamespace = namespace(Map.of("team", "blue"));
        V1Namespace newNamespace = namespace(Map.of("team", "blue"));
        newNamespace.getMetadata().setResourceVersion("2");
        newNamespace.getMetadata().setAnnotations(Map.of("description", "changed"));

        handler.onUpdate(oldNamespace, newNamespace);

        verifyNoInteractions(queue);
    }

    @Test
    public void testNullAndEmptyLabelsAreEquivalent() {
        routes.add(route(NAMESPACE, "route", false));

        handler.onUpdate(namespace(null), namespace(Map.of()));
        handler.onUpdate(namespace(Map.of()), namespace(null));

        verifyNoInteractions(queue);
    }

    @Test
    public void testDeleteQueuesNamespaceRoutes() {
        routes.add(route(NAMESPACE, "route", false));
        routes.add(route("other-ns", "other", false));

        handler.onDelete(namespace(Map.of("team", "blue")), false);

        verify(queue).add(new Request(NAMESPACE, "route"));
        verify(queue, never()).add(new Request("other-ns", "other"));
    }

    @Test
    public void testUnknownDeleteQueuesNamespaceRoutes() {
        routes.add(route(NAMESPACE, "route", false));

        handler.onDelete(namespace(null), true);

        verify(queue).add(new Request(NAMESPACE, "route"));
    }

    @Test
    public void testUnrelatedNamespaceAndMalformedEventsDoNotQueueRoutes() {
        routes.add(route(NAMESPACE, "route", false));

        handler.onAdd(new V1Namespace().metadata(new V1ObjectMeta().name("other-ns")));
        handler.onAdd(new V1Namespace());
        handler.onDelete(new V1Namespace().metadata(new V1ObjectMeta()), true);
        handler.onAdd(new V1Namespace().metadata(new V1ObjectMeta().name("")));
        handler.onDelete(null, true);

        verify(queue, never()).add(any());
    }

    private V1Namespace namespace(final Map<String, String> labels) {
        return new V1Namespace().metadata(new V1ObjectMeta().name(NAMESPACE).labels(labels));
    }

    private DynamicKubernetesObject route(final String namespace, final String name, final boolean rejected) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", namespace);
        metadata.addProperty("name", name);
        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "HTTPRoute");
        raw.add("metadata", metadata);
        if (rejected) {
            JsonObject condition = new JsonObject();
            condition.addProperty("type", "Accepted");
            condition.addProperty("status", "False");
            condition.addProperty("reason", "NotAllowedByListeners");
            JsonArray conditions = new JsonArray();
            conditions.add(condition);
            JsonObject parent = new JsonObject();
            parent.add("conditions", conditions);
            JsonArray parents = new JsonArray();
            parents.add(parent);
            JsonObject status = new JsonObject();
            status.add("parents", parents);
            raw.add("status", status);
        }
        return new DynamicKubernetesObject(raw);
    }
}
