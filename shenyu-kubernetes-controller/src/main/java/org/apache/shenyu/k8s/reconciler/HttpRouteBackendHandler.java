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

package org.apache.shenyu.k8s.reconciler;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.JsonFields;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Shared machinery of the backend event handlers (Endpoints, Service): both resolve the
 * HTTPRoutes affected by a backend change through the same informer index and enqueue them
 * for reconciliation, so backend changes re-resolve upstreams immediately instead of
 * waiting for the periodic informer resync.
 *
 * <p>An Endpoints object and the Service it backs share their name and namespace. The
 * HTTPRoute informer is indexed by referenced Service ("namespace/name"), so a backend
 * event resolves the affected routes via the index instead of scanning the whole cache.
 * The queue deduplicates, and the reconciler re-checks ShenYu gateway binding, so
 * enqueuing extra routes is harmless.
 */
abstract class HttpRouteBackendHandler {

    /** Index key: the Services ("namespace/name") each route's backendRefs reference. */
    static final String BACKEND_SERVICE_INDEX = "backendService";

    private final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer;

    private final RateLimitingQueue<Request> httpRouteWorkQueue;

    HttpRouteBackendHandler(final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                            final RateLimitingQueue<Request> httpRouteWorkQueue) {
        this.httpRouteInformer = httpRouteInformer;
        this.httpRouteWorkQueue = httpRouteWorkQueue;
        // Indexers must be registered before the informer starts; the handler beans are
        // created (and depended on by the controller manager) before informers run. Both
        // backend handlers share one index, so whichever registers first wins.
        if (!httpRouteInformer.getIndexer().getIndexers().containsKey(BACKEND_SERVICE_INDEX)) {
            this.httpRouteInformer.addIndexers(Map.of(BACKEND_SERVICE_INDEX, HttpRouteBackendHandler::indexBackendServices));
        }
    }

    static List<String> indexBackendServices(final DynamicKubernetesObject route) {
        if (Objects.isNull(route.getMetadata())) {
            return Collections.emptyList();
        }
        String routeNamespace = route.getMetadata().getNamespace();
        JsonObject spec = JsonFields.getJsonObject(route.getRaw(), "spec");
        JsonElement rulesElement = Objects.isNull(spec) ? null : JsonFields.getJsonArray(spec, "rules");
        if (Objects.isNull(rulesElement)) {
            return Collections.emptyList();
        }
        List<String> keys = new ArrayList<>();
        for (JsonElement ruleElement : rulesElement.getAsJsonArray()) {
            JsonObject rule = ruleElement.getAsJsonObject();
            JsonElement refsElement = JsonFields.getJsonArray(rule, "backendRefs");
            if (Objects.isNull(refsElement)) {
                continue;
            }
            for (JsonElement refElement : refsElement.getAsJsonArray()) {
                JsonObject ref = refElement.getAsJsonObject();
                if (!GatewayApiConstants.isServiceRef(ref)) {
                    continue;
                }
                String name = JsonFields.getString(ref, "name");
                String namespace = JsonFields.getString(ref, "namespace");
                if (Objects.isNull(name)) {
                    continue;
                }
                keys.add((Objects.isNull(namespace) ? routeNamespace : namespace) + "/" + name);
            }
        }
        return keys;
    }

    /** Null-safe accessors so each event callback stays a one-liner. */
    static String namespaceOf(final V1ObjectMeta metadata) {
        return Objects.isNull(metadata) ? null : metadata.getNamespace();
    }

    static String nameOf(final V1ObjectMeta metadata) {
        return Objects.isNull(metadata) ? null : metadata.getName();
    }

    /**
     * Enqueue every HTTPRoute whose backendRefs target the Service {@code namespace/name}.
     * Null coordinates (malformed event object) enqueue nothing.
     */
    final void enqueueAffectedRoutes(final String namespace, final String name) {
        if (Objects.isNull(namespace) || Objects.isNull(name)) {
            return;
        }
        for (DynamicKubernetesObject route : httpRouteInformer.getIndexer().byIndex(BACKEND_SERVICE_INDEX, namespace + "/" + name)) {
            if (Objects.isNull(route.getMetadata())) {
                continue;
            }
            httpRouteWorkQueue.add(new Request(route.getMetadata().getNamespace(), route.getMetadata().getName()));
        }
    }
}
