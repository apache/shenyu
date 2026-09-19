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

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.kubernetes.client.extended.controller.reconciler.Reconciler;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.controller.reconciler.Result;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.common.JsonFields;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

/**
 * Reconciler for ReferenceGrant resources (Gateway API v1). A grant change can make a
 * cross-namespace reference valid or invalid immediately, so every HTTPRoute referencing
 * the grant's namespace is re-queued instead of waiting for the periodic route resync —
 * otherwise a revoked grant would keep unauthorized traffic flowing until the resync.
 */
public class ReferenceGrantReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(ReferenceGrantReconciler.class);

    private final Lister<DynamicKubernetesObject> httpRouteLister;

    private final RateLimitingQueue<Request> httpRouteWorkQueue;

    public ReferenceGrantReconciler(final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                                    final RateLimitingQueue<Request> httpRouteWorkQueue) {
        this.httpRouteLister = new Lister<>(httpRouteInformer.getIndexer());
        this.httpRouteWorkQueue = httpRouteWorkQueue;
    }

    @Override
    public Result reconcile(final Request request) {
        String grantNamespace = request.getNamespace();
        LOG.debug("ReferenceGrant {}/{} changed, re-queuing routes referencing namespace {}",
                grantNamespace, request.getName(), grantNamespace);
        try {
            for (DynamicKubernetesObject route : httpRouteLister.list()) {
                String routeNamespace = Objects.requireNonNull(route.getMetadata()).getNamespace();
                // Same-namespace references never consult grants
                if (grantNamespace.equals(routeNamespace)) {
                    continue;
                }
                if (referencesNamespace(route, grantNamespace)) {
                    httpRouteWorkQueue.add(new Request(routeNamespace, route.getMetadata().getName()));
                    LOG.info("Re-queued HTTPRoute {}/{} due to ReferenceGrant change in namespace {}",
                            routeNamespace, route.getMetadata().getName(), grantNamespace);
                }
            }
            return new Result(false);
        } catch (Exception e) {
            LOG.error("Error reconciling ReferenceGrant {}, will retry", request, e);
            return new Result(true);
        }
    }

    /**
     * Whether the route has a cross-namespace parentRef or backendRef into
     * {@code targetNamespace}: exactly the references a ReferenceGrant in that namespace
     * can permit or deny.
     */
    private boolean referencesNamespace(final DynamicKubernetesObject route, final String targetNamespace) {
        JsonObject spec = JsonFields.getJsonObject(route.getRaw(), "spec");
        if (Objects.isNull(spec)) {
            return false;
        }
        if (referencesNamespace(JsonFields.getJsonArray(spec, "parentRefs"), targetNamespace)) {
            return true;
        }
        JsonArray rules = JsonFields.getJsonArray(spec, "rules");
        if (Objects.isNull(rules)) {
            return false;
        }
        for (JsonElement ruleElement : rules) {
            if (!ruleElement.isJsonObject()) {
                continue;
            }
            if (referencesNamespace(JsonFields.getJsonArray(ruleElement.getAsJsonObject(), "backendRefs"), targetNamespace)) {
                return true;
            }
        }
        return false;
    }

    private boolean referencesNamespace(final JsonArray refs, final String targetNamespace) {
        if (Objects.isNull(refs)) {
            return false;
        }
        for (JsonElement element : refs) {
            if (!element.isJsonObject()) {
                continue;
            }
            if (targetNamespace.equals(JsonFields.getString(element.getAsJsonObject(), "namespace"))) {
                return true;
            }
        }
        return false;
    }
}
