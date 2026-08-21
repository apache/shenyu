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
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.JsonFields;
import org.apache.shenyu.k8s.common.StatusMergePatch;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

public class GatewayReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(GatewayReconciler.class);

    private final Lister<DynamicKubernetesObject> gatewayLister;

    private final Lister<DynamicKubernetesObject> gatewayClassLister;

    private final Lister<DynamicKubernetesObject> httpRouteLister;

    private final ShenyuCacheRepository shenyuCacheRepository;

    private final RateLimitingQueue<Request> httpRouteWorkQueue;

    private final ApiClient apiClient;

    public GatewayReconciler(final SharedIndexInformer<DynamicKubernetesObject> gatewayInformer,
                             final SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer,
                             final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                             final ShenyuCacheRepository shenyuCacheRepository,
                             final RateLimitingQueue<Request> httpRouteWorkQueue,
                             final ApiClient apiClient) {
        this.gatewayLister = new Lister<>(gatewayInformer.getIndexer());
        this.gatewayClassLister = new Lister<>(gatewayClassInformer.getIndexer());
        this.httpRouteLister = new Lister<>(httpRouteInformer.getIndexer());
        this.shenyuCacheRepository = shenyuCacheRepository;
        this.httpRouteWorkQueue = httpRouteWorkQueue;
        this.apiClient = apiClient;
    }

    @Override
    public Result reconcile(final Request request) {
        LOG.info("Starting to reconcile gateway {}", request);
        try {
            DynamicKubernetesObject gateway = gatewayLister.namespace(request.getNamespace()).get(request.getName());

            if (Objects.isNull(gateway)) {
                LOG.info("Gateway {} deleted, cleaning associated routes", request);
                deleteAssociatedRoutes(request.getNamespace(), request.getName());
                return new Result(false);
            }

            if (!GatewayClassReconciler.isShenyuGateway(gateway, gatewayClassLister)) {
                // Gateway conditions carry no controllerName, so an Accepted entry of another
                // controller is indistinguishable from ours. Only Gateways ShenYu previously
                // accepted (they have route bindings) are cleaned up and downgraded; touching
                // anything else would fight the controller that owns it.
                if (CollectionUtils.isEmpty(GatewayRouteCache.getInstance()
                        .getRoutesByGateway(request.getNamespace(), request.getName()))) {
                    LOG.debug("Gateway {} is not managed by ShenYu, skipping", request);
                    return new Result(false);
                }
                LOG.info("Gateway {} is no longer managed by ShenYu, cleaning associated routes", request);
                deleteAssociatedRoutes(request.getNamespace(), request.getName());
                // Accepted must not stay True; the transition also makes a later class
                // restore requeue routes for immediate recovery.
                updateGatewayNotAcceptedStatus(gateway);
                return new Result(false);
            }

            // Requeue only on the Accepted transition (first accept or after losing it):
            // on plain resyncs routes are already reconciled and a cluster scan is wasted.
            boolean wasAccepted = GatewayApiConstants.isConditionTrue(gateway, "Accepted");
            updateGatewayAcceptedStatus(gateway);
            if (!wasAccepted) {
                requeueAffectedHTTPRoutes(request.getNamespace(), request.getName());
            }

            LOG.debug("Gateway {} reconciled successfully", request);
            return new Result(false);
        } catch (Exception e) {
            LOG.error("Error reconciling gateway {}, will retry", request, e);
            return new Result(true);
        }
    }

    /**
     * Re-queue HTTPRoutes whose parentRefs reference this Gateway: covers routes created
     * before the Gateway was accepted, including cross-namespace ones not yet in
     * GatewayRouteCache. Only invoked on the Accepted transition, not on every resync.
     */
    private void requeueAffectedHTTPRoutes(final String gatewayNamespace, final String gatewayName) {
        for (DynamicKubernetesObject route : httpRouteLister.list()) {
            enqueueIfBound(route, gatewayNamespace, gatewayName);
        }
    }

    private void enqueueIfBound(final DynamicKubernetesObject route, final String gatewayNamespace,
                                final String gatewayName) {
        if (!isBoundToGateway(route, gatewayNamespace, gatewayName)) {
            return;
        }
        httpRouteWorkQueue.add(new Request(route.getMetadata().getNamespace(), route.getMetadata().getName()));
        LOG.info("Re-queued HTTPRoute {}/{} due to Gateway {}/{} acceptance",
                route.getMetadata().getNamespace(), route.getMetadata().getName(), gatewayNamespace, gatewayName);
    }

    private boolean isBoundToGateway(final DynamicKubernetesObject httpRoute,
                                     final String gatewayNamespace, final String gatewayName) {
        JsonObject spec = JsonFields.getJsonObject(httpRoute.getRaw(), "spec");
        JsonArray parentRefs = Objects.isNull(spec) ? null : JsonFields.getJsonArray(spec, "parentRefs");
        if (Objects.isNull(parentRefs)) {
            return false;
        }
        String routeNamespace = Objects.requireNonNull(httpRoute.getMetadata()).getNamespace();
        for (JsonElement element : parentRefs) {
            JsonObject parentRef = element.getAsJsonObject();
            String parentName = JsonFields.getString(parentRef, "name");
            String parentNamespace = Optional.ofNullable(JsonFields.getString(parentRef, "namespace"))
                    .orElse(routeNamespace);
            if (gatewayNamespace.equals(parentNamespace) && gatewayName.equals(parentName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * When a Gateway is deleted or no longer ShenYu-managed, clean up ShenYu config for its
     * bound routes. A route still attached to another ShenYu Gateway keeps its config and is
     * re-queued so the next reconcile refreshes its status.
     */
    private void deleteAssociatedRoutes(final String gatewayNamespace, final String gatewayName) {
        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        Set<String> routeKeys = cache.getRoutesByGateway(gatewayNamespace, gatewayName);
        if (CollectionUtils.isEmpty(routeKeys)) {
            return;
        }
        cache.removeRoutesByGateway(gatewayNamespace, gatewayName);
        for (String routeKey : routeKeys) {
            String[] parts = routeKey.split("/", 2);
            if (parts.length != 2) {
                continue;
            }
            String routeNamespace = parts[0];
            String routeName = parts[1];
            if (CollectionUtils.isNotEmpty(cache.getGatewaysForRoute(routeNamespace, routeName))) {
                LOG.info("Route {}/{} still served by another ShenYu Gateway, keeping its config",
                        routeNamespace, routeName);
                httpRouteWorkQueue.add(new Request(routeNamespace, routeName));
                continue;
            }
            deleteRouteConfig(routeNamespace, routeName, cache);
            LOG.info("Deleted ShenYu config for route {}/{} due to Gateway removal", routeNamespace, routeName);
        }
    }

    private void deleteRouteConfig(final String routeNamespace, final String routeName,
                                   final GatewayRouteCache cache) {
        List<String> selectorIds = cache.removeRouteSelectors(routeNamespace, routeName, PluginEnum.DIVIDE.getName());
        if (CollectionUtils.isEmpty(selectorIds)) {
            return;
        }
        for (String selectorId : selectorIds) {
            shenyuCacheRepository.deleteSelectorWithRules(PluginEnum.DIVIDE.getName(), selectorId);
        }
    }

    /** Patch Accepted=False on a Gateway the controller no longer manages; no-op when already False. */
    private void updateGatewayNotAcceptedStatus(final DynamicKubernetesObject gateway) {
        if (isAcceptedCondition(gateway, "False")) {
            return;
        }
        JsonObject condition = new JsonObject();
        condition.addProperty("type", "Accepted");
        condition.addProperty("status", "False");
        condition.addProperty("reason", "NoGatewayClassController");
        condition.addProperty("message", "GatewayClass is missing or not managed by the ShenYu controller");
        condition.addProperty("lastTransitionTime", Instant.now().toString());
        patchGatewayStatus(gateway, condition);
    }

    private boolean isAcceptedCondition(final DynamicKubernetesObject gateway, final String status) {
        JsonObject raw = gateway.getRaw();
        if (!raw.has("status") || raw.get("status").isJsonNull()) {
            return false;
        }
        JsonObject statusObj = raw.getAsJsonObject("status");
        if (!statusObj.has("conditions") || statusObj.get("conditions").isJsonNull()) {
            return false;
        }
        for (JsonElement el : statusObj.getAsJsonArray("conditions")) {
            JsonObject cond = el.getAsJsonObject();
            if ("Accepted".equals(cond.has("type") ? cond.get("type").getAsString() : null)
                    && status.equals(cond.has("status") ? cond.get("status").getAsString() : null)) {
                return true;
            }
        }
        return false;
    }

    /** Update Gateway status with Accepted=True, via merge-patch on the /status subresource. */
    private void updateGatewayAcceptedStatus(final DynamicKubernetesObject gateway) {
        if (GatewayApiConstants.isConditionTrue(gateway, "Accepted")) {
            return;
        }
        JsonObject condition = new JsonObject();
        condition.addProperty("type", "Accepted");
        condition.addProperty("status", "True");
        condition.addProperty("reason", "Accepted");
        condition.addProperty("message", "Gateway has been accepted by the ShenYu controller");
        condition.addProperty("lastTransitionTime", Instant.now().toString());
        patchGatewayStatus(gateway, condition);
    }

    /** Merge-patch the Gateway /status subresource, preserving conditions owned by other controllers. */
    private void patchGatewayStatus(final DynamicKubernetesObject gateway, final JsonObject condition) {
        try {
            final String namespace = gateway.getMetadata().getNamespace();
            final String name = gateway.getMetadata().getName();

            JsonArray conditions = buildGatewayStatusConditions(gateway, condition);

            JsonObject statusObj = new JsonObject();
            statusObj.add("conditions", conditions);

            JsonObject body = new JsonObject();
            body.add("status", statusObj);
            body.addProperty("kind", "Gateway");
            body.addProperty("apiVersion", GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION);

            JsonObject metadata = new JsonObject();
            metadata.addProperty("name", name);
            metadata.addProperty("namespace", namespace);
            body.add("metadata", metadata);

            String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION
                    + "/namespaces/" + namespace + "/gateways/" + name + "/status";

            StatusMergePatch.patch(apiClient, path, body);
            LOG.info("Updated Gateway {}/{} status to Accepted={}", namespace, name, condition.get("status").getAsString());
        } catch (Exception e) {
            LOG.warn("Failed to update Gateway status, will retry on next resync", e);
        }
    }

    /**
     * Build the Gateway status conditions array for the patch body: the Accepted condition
     * plus all existing non-Accepted conditions, and the spec-mandated Programmed default
     * (Unknown/Pending) if missing.
     */
    private JsonArray buildGatewayStatusConditions(final DynamicKubernetesObject gateway,
                                                   final JsonObject acceptedCondition) {
        JsonArray conditions = new JsonArray();
        conditions.add(acceptedCondition);

        boolean hasProgrammed = false;
        JsonObject raw = gateway.getRaw();
        if (raw.has("status") && !raw.get("status").isJsonNull()) {
            JsonObject status = raw.getAsJsonObject("status");
            if (status.has("conditions") && !status.get("conditions").isJsonNull()) {
                JsonArray existingConditions = status.getAsJsonArray("conditions");
                for (JsonElement el : existingConditions) {
                    JsonObject existing = el.getAsJsonObject();
                    String existingType = existing.has("type") ? existing.get("type").getAsString() : null;
                    if ("Programmed".equals(existingType)) {
                        hasProgrammed = true;
                        conditions.add(existing);
                    } else if (!"Accepted".equals(existingType)) {
                        conditions.add(existing);
                    }
                }
            }
        }
        if (!hasProgrammed) {
            JsonObject programmedDefault = new JsonObject();
            programmedDefault.addProperty("type", "Programmed");
            programmedDefault.addProperty("status", "Unknown");
            programmedDefault.addProperty("reason", "Pending");
            programmedDefault.addProperty("message", "Waiting for controller");
            programmedDefault.addProperty("lastTransitionTime", "1970-01-01T00:00:00Z");
            conditions.add(programmedDefault);
        }
        return conditions;
    }

}
