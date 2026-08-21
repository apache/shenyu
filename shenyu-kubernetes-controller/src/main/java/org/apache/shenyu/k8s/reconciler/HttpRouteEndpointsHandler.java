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
import io.kubernetes.client.informer.ResourceEventHandler;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.JsonFields;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Bridges Endpoints events to HTTPRoute reconciliation, so backend address changes
 * re-resolve upstreams immediately instead of waiting for the periodic informer resync.
 *
 * <p>An Endpoints object shares its name and namespace with the Service it backs. The
 * HTTPRoute informer is indexed by referenced Service ("namespace/name"), so an Endpoints
 * event resolves the affected routes via the index instead of scanning the whole cache.
 * The queue deduplicates, and the reconciler re-checks ShenYu gateway binding, so
 * enqueuing extra routes is harmless.
 */
public final class HttpRouteEndpointsHandler implements ResourceEventHandler<V1Endpoints> {

    /** Index key: the Services ("namespace/name") each route's backendRefs reference. */
    static final String BACKEND_SERVICE_INDEX = "backendService";

    private final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer;

    private final RateLimitingQueue<Request> httpRouteWorkQueue;

    public HttpRouteEndpointsHandler(final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                                     final RateLimitingQueue<Request> httpRouteWorkQueue) {
        this.httpRouteInformer = httpRouteInformer;
        this.httpRouteWorkQueue = httpRouteWorkQueue;
        // Indexers must be registered before the informer starts; the handler bean is
        // created (and depended on by the controller manager) before informers run.
        this.httpRouteInformer.addIndexers(Map.of(BACKEND_SERVICE_INDEX, HttpRouteEndpointsHandler::indexBackendServices));
    }

    private static List<String> indexBackendServices(final DynamicKubernetesObject route) {
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

    @Override
    public void onAdd(final V1Endpoints endpoints) {
        enqueueAffectedRoutes(endpoints);
    }

    @Override
    public void onUpdate(final V1Endpoints oldEndpoints, final V1Endpoints newEndpoints) {
        enqueueAffectedRoutes(newEndpoints);
    }

    @Override
    public void onDelete(final V1Endpoints endpoints, final boolean unknownState) {
        enqueueAffectedRoutes(endpoints);
    }

    private void enqueueAffectedRoutes(final V1Endpoints endpoints) {
        if (Objects.isNull(endpoints.getMetadata())) {
            return;
        }
        String namespace = endpoints.getMetadata().getNamespace();
        String name = endpoints.getMetadata().getName();
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
