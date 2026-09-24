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
import org.apache.shenyu.k8s.common.ListenerSupport;
import org.apache.shenyu.k8s.common.StatusMergePatch;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

/**
 * Reconciler for Gateway resources (Gateway API v1).
 *
 * <p>Besides the Accepted condition, the reconciler reports Programmed and per-listener
 * status (supportedKinds, attachedRoutes, per-listener Accepted/Programmed). A listener is
 * usable only when it speaks plain HTTP on the port this gateway actually serves
 * ({@code server.port}); anything else is reported with the spec-defined reason instead of
 * being silently ignored. attachedRoutes is per listener and reflects the in-memory
 * listener-level bindings, converging on gateway resyncs.
 */
public class GatewayReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(GatewayReconciler.class);

    private final Lister<DynamicKubernetesObject> gatewayLister;

    private final Lister<DynamicKubernetesObject> gatewayClassLister;

    private final Lister<DynamicKubernetesObject> httpRouteLister;

    private final ShenyuCacheRepository shenyuCacheRepository;

    private final RateLimitingQueue<Request> httpRouteWorkQueue;

    private final ApiClient apiClient;

    /** The port the embedded ShenYu data plane actually listens on ({@code server.port}). */
    private final int servedPort;

    public GatewayReconciler(final SharedIndexInformer<DynamicKubernetesObject> gatewayInformer,
                             final SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer,
                             final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                             final ShenyuCacheRepository shenyuCacheRepository,
                             final RateLimitingQueue<Request> httpRouteWorkQueue,
                             final ApiClient apiClient,
                             final int servedPort) {
        this.gatewayLister = new Lister<>(gatewayInformer.getIndexer());
        this.gatewayClassLister = new Lister<>(gatewayClassInformer.getIndexer());
        this.httpRouteLister = new Lister<>(httpRouteInformer.getIndexer());
        this.shenyuCacheRepository = shenyuCacheRepository;
        this.httpRouteWorkQueue = httpRouteWorkQueue;
        this.apiClient = apiClient;
        this.servedPort = servedPort;
    }

    @Override
    public Result reconcile(final Request request) {
        LOG.debug("Starting to reconcile gateway {}", request);
        try {
            DynamicKubernetesObject gateway = gatewayLister.namespace(request.getNamespace()).get(request.getName());

            if (Objects.isNull(gateway)) {
                LOG.info("Gateway {} deleted, cleaning associated routes", request);
                deleteAssociatedRoutes(request.getNamespace(), request.getName());
                return new Result(false);
            }

            if (!GatewayClassReconciler.isShenyuGateway(gateway, gatewayClassLister)) {
                if (!previouslyServedByShenyu(gateway, request.getNamespace(), request.getName())) {
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

            boolean wasAccepted = GatewayApiConstants.isConditionTrue(gateway, GatewayApiConstants.CONDITION_ACCEPTED);
            Long generation = generationOf(gateway);
            boolean generationObserved = observedGenerationUpToDate(
                    GatewayApiConstants.findCondition(gateway, GatewayApiConstants.CONDITION_ACCEPTED), generation);
            JsonObject desiredStatus = buildAcceptedStatus(gateway);
            preserveTransitionTimes(gateway, desiredStatus);
            if (!gatewayStatusMatches(gateway, desiredStatus)) {
                patchGatewayStatus(gateway, desiredStatus);
            }
            if (!wasAccepted || !generationObserved) {
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
     * GatewayRouteCache, and re-applies listener policy (hostname, allowedRoutes, port)
     * after a spec change. Invoked on the Accepted transition and on unobserved spec
     * generations, not on every resync.
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

    /**
     * Whether a Gateway not (or no longer) owned by a ShenYu GatewayClass was previously
     * served by this controller: either it still has live route bindings, or its status
     * carries the Accepted=True payload this controller writes (the signal for Gateways
     * that legitimately have zero attached routes).
     */
    private boolean previouslyServedByShenyu(final DynamicKubernetesObject gateway, final String namespace, final String name) {
        return CollectionUtils.isNotEmpty(GatewayRouteCache.getInstance().getRoutesByGateway(namespace, name))
                || GatewayApiConstants.isConditionAcceptedByShenyu(gateway, GatewayApiConstants.CONDITION_ACCEPTED);
    }

    private boolean observedGenerationUpToDate(final JsonObject existingCondition, final Long generation) {
        if (Objects.isNull(generation)) {
            return true;
        }
        return Objects.nonNull(existingCondition)
                && generation.equals(JsonFields.getLong(existingCondition, "observedGeneration"));
    }

    /**
     * Carry over the lastTransitionTime of every gateway- and listener-level condition whose
     * (type, status) is unchanged: the spec requires the timestamp to advance only on an
     * actual status transition, not on an attachedRoutes count update or a generation bump.
     */
    private void preserveTransitionTimes(final DynamicKubernetesObject gateway, final JsonObject desiredStatus) {
        JsonObject existingStatus = JsonFields.getJsonObject(gateway.getRaw(), "status");
        JsonArray existingConditions = JsonFields.getJsonArray(existingStatus, "conditions");
        if (Objects.nonNull(existingConditions)) {
            preserveTransitionTimes(existingConditions, desiredStatus.getAsJsonArray("conditions"));
        }
        JsonArray existingListeners = JsonFields.getJsonArray(existingStatus, "listeners");
        JsonArray desiredListeners = desiredStatus.getAsJsonArray("listeners");
        if (Objects.isNull(existingListeners) || Objects.isNull(desiredListeners)) {
            return;
        }
        for (JsonElement desiredElement : desiredListeners) {
            JsonObject desiredListener = desiredElement.getAsJsonObject();
            JsonArray existingListenerConditions = JsonFields.getJsonArray(
                    findListenerStatus(existingListeners, JsonFields.getString(desiredListener, "name")), "conditions");
            if (Objects.nonNull(existingListenerConditions)) {
                preserveTransitionTimes(existingListenerConditions, desiredListener.getAsJsonArray("conditions"));
            }
        }
    }

    private void preserveTransitionTimes(final JsonArray existingConditions, final JsonArray desiredConditions) {
        for (JsonElement desiredElement : desiredConditions) {
            JsonObject desiredCondition = desiredElement.getAsJsonObject();
            for (JsonElement existingElement : existingConditions) {
                if (!existingElement.isJsonObject()) {
                    continue;
                }
                JsonObject existingCondition = existingElement.getAsJsonObject();
                String existingTime = JsonFields.getString(existingCondition, "lastTransitionTime");
                if (Objects.equals(JsonFields.getString(existingCondition, "type"), JsonFields.getString(desiredCondition, "type"))
                        && Objects.equals(JsonFields.getString(existingCondition, "status"), JsonFields.getString(desiredCondition, "status"))
                        && Objects.nonNull(existingTime)) {
                    desiredCondition.addProperty("lastTransitionTime", existingTime);
                    break;
                }
            }
        }
    }

    /** Patch Accepted=False on a Gateway the controller no longer manages; no-op when already False. */
    private void updateGatewayNotAcceptedStatus(final DynamicKubernetesObject gateway) {
        if (isAcceptedCondition(gateway, "False")) {
            return;
        }
        JsonObject condition = new JsonObject();
        condition.addProperty("type", GatewayApiConstants.CONDITION_ACCEPTED);
        condition.addProperty("status", "False");
        condition.addProperty("reason", "NoGatewayClassController");
        condition.addProperty("message", "GatewayClass is missing or not managed by the ShenYu controller");
        Long generation = generationOf(gateway);
        if (Objects.nonNull(generation)) {
            condition.addProperty("observedGeneration", generation);
        }
        condition.addProperty("lastTransitionTime", Instant.now().toString());

        JsonArray conditions = new JsonArray();
        conditions.add(condition);
        JsonObject statusObj = new JsonObject();
        statusObj.add("conditions", conditions);
        patchGatewayStatus(gateway, statusObj);
    }

    private boolean isAcceptedCondition(final DynamicKubernetesObject gateway, final String status) {
        JsonObject statusObj = JsonFields.getJsonObject(gateway.getRaw(), "status");
        JsonArray conditions = JsonFields.getJsonArray(statusObj, "conditions");
        if (Objects.isNull(conditions)) {
            return false;
        }
        for (JsonElement el : conditions) {
            if (!el.isJsonObject()) {
                continue;
            }
            JsonObject cond = el.getAsJsonObject();
            if (GatewayApiConstants.CONDITION_ACCEPTED.equals(JsonFields.getString(cond, "type"))
                    && status.equals(JsonFields.getString(cond, "status"))) {
                return true;
            }
        }
        return false;
    }

    /**
     * Build the desired status of an accepted Gateway: Accepted + Programmed conditions and
     * the per-listener status entries. {@code attachedRoutes} is defined per listener, so
     * each entry counts only the routes bound to that listener.
     */
    private JsonObject buildAcceptedStatus(final DynamicKubernetesObject gateway) {
        Long generation = generationOf(gateway);
        String namespace = gateway.getMetadata().getNamespace();
        String name = gateway.getMetadata().getName();
        GatewayRouteCache cache = GatewayRouteCache.getInstance();

        List<JsonObject> listeners = ListenerSupport.selectListeners(gateway.getRaw(), null);
        JsonArray listenerStatuses = new JsonArray();
        boolean anyUsableListener = false;
        for (JsonObject listener : listeners) {
            boolean protocolOk = ListenerSupport.isSupportedProtocol(listener);
            Long port = ListenerSupport.portOf(listener);
            boolean portOk = Objects.nonNull(port) && port == servedPort;
            boolean usable = protocolOk && portOk;
            anyUsableListener |= usable;

            JsonObject listenerStatus = new JsonObject();
            listenerStatus.addProperty("name", ListenerSupport.nameOf(listener));
            JsonObject kind = new JsonObject();
            kind.addProperty("group", GatewayApiConstants.GATEWAY_API_GROUP);
            kind.addProperty("kind", GatewayApiConstants.HTTP_ROUTE_KIND);
            JsonArray supportedKinds = new JsonArray();
            supportedKinds.add(kind);
            listenerStatus.add("supportedKinds", supportedKinds);
            Set<String> listenerRoutes = cache.getRoutesByListener(namespace, name, ListenerSupport.nameOf(listener));
            listenerStatus.addProperty("attachedRoutes", Objects.isNull(listenerRoutes) ? 0 : listenerRoutes.size());

            JsonArray listenerConditions = new JsonArray();
            if (usable) {
                listenerConditions.add(buildCondition(GatewayApiConstants.CONDITION_ACCEPTED, "True",
                        GatewayApiConstants.CONDITION_ACCEPTED, "Listener is accepted", generation));
                listenerConditions.add(buildCondition(GatewayApiConstants.CONDITION_PROGRAMMED, "True",
                        GatewayApiConstants.REASON_PROGRAMMED, "Listener is programmed", generation));
            } else {
                String reason = protocolOk ? GatewayApiConstants.REASON_PORT_UNAVAILABLE
                        : GatewayApiConstants.REASON_UNSUPPORTED_PROTOCOL;
                String message = protocolOk
                        ? "listener port " + port + " is not served by this gateway (serving " + servedPort + ")"
                        : "listener protocol " + ListenerSupport.protocolOf(listener) + " is not supported, only HTTP";
                listenerConditions.add(buildCondition(GatewayApiConstants.CONDITION_ACCEPTED, "False", reason, message, generation));
                listenerConditions.add(buildCondition(GatewayApiConstants.CONDITION_PROGRAMMED, "False",
                        "Pending", "Listener is not programmed", generation));
            }
            listenerStatus.add("conditions", listenerConditions);
            listenerStatuses.add(listenerStatus);
        }

        JsonArray conditions = new JsonArray();
        conditions.add(buildCondition(GatewayApiConstants.CONDITION_ACCEPTED, "True",
                GatewayApiConstants.CONDITION_ACCEPTED, "Gateway has been accepted by the ShenYu controller", generation));
        if (anyUsableListener) {
            conditions.add(buildCondition(GatewayApiConstants.CONDITION_PROGRAMMED, "True",
                    GatewayApiConstants.REASON_PROGRAMMED, "Gateway is programmed into the embedded ShenYu data plane", generation));
        } else {
            conditions.add(buildCondition(GatewayApiConstants.CONDITION_PROGRAMMED, "False",
                    GatewayApiConstants.REASON_LISTENERS_NOT_VALID, "No listener with a supported protocol (HTTP) and a served port",
                    generation));
        }

        JsonObject status = new JsonObject();
        status.add("conditions", conditions);
        status.add("listeners", listenerStatuses);
        return status;
    }

    /**
     * Whether the current Gateway status already carries our conditions with matching
     * type/status/reason/observedGeneration and per-listener entries with matching
     * attachedRoutes. Timestamps are deliberately ignored to keep the steady state
     * patch-free; observedGeneration is compared so a spec change always produces the
     * patch that acknowledges it.
     */
    private boolean gatewayStatusMatches(final DynamicKubernetesObject gateway, final JsonObject desiredStatus) {
        JsonObject existingStatus = JsonFields.getJsonObject(gateway.getRaw(), "status");
        if (Objects.isNull(existingStatus)) {
            return false;
        }
        if (!conditionsMatch(JsonFields.getJsonArray(existingStatus, "conditions"),
                desiredStatus.getAsJsonArray("conditions"))) {
            return false;
        }
        JsonArray existingListeners = JsonFields.getJsonArray(existingStatus, "listeners");
        JsonArray desiredListeners = desiredStatus.getAsJsonArray("listeners");
        if (Objects.isNull(existingListeners) || existingListeners.size() != desiredListeners.size()) {
            return false;
        }
        for (JsonElement desiredElement : desiredListeners) {
            JsonObject desiredListener = desiredElement.getAsJsonObject();
            JsonObject existingListener = findListenerStatus(existingListeners, JsonFields.getString(desiredListener, "name"));
            if (Objects.isNull(existingListener)
                    || !Objects.equals(JsonFields.getLong(existingListener, "attachedRoutes"),
                    JsonFields.getLong(desiredListener, "attachedRoutes"))
                    || !conditionsMatch(JsonFields.getJsonArray(existingListener, "conditions"),
                    desiredListener.getAsJsonArray("conditions"))) {
                return false;
            }
        }
        return true;
    }

    private JsonObject findListenerStatus(final JsonArray listeners, final String name) {
        for (JsonElement element : listeners) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject listener = element.getAsJsonObject();
            if (Objects.equals(JsonFields.getString(listener, "name"), name)) {
                return listener;
            }
        }
        return null;
    }

    /**
     * Compare by (type, status), plus reason for False conditions and the
     * observedGeneration so a spec change is always acknowledged; ignores timestamps.
     */
    private boolean conditionsMatch(final JsonArray existing, final JsonArray desired) {
        if (Objects.isNull(existing) || Objects.isNull(desired) || existing.size() < desired.size()) {
            return false;
        }
        for (JsonElement desiredElement : desired) {
            JsonObject desiredCondition = desiredElement.getAsJsonObject();
            boolean found = false;
            for (JsonElement existingElement : existing) {
                if (!existingElement.isJsonObject()) {
                    continue;
                }
                JsonObject existingCondition = existingElement.getAsJsonObject();
                boolean reasonMatters = "False".equals(JsonFields.getString(desiredCondition, "status"));
                boolean reasonMatches = !reasonMatters || Objects.equals(
                        JsonFields.getString(existingCondition, "reason"), JsonFields.getString(desiredCondition, "reason"));
                if (Objects.equals(JsonFields.getString(existingCondition, "type"), JsonFields.getString(desiredCondition, "type"))
                        && Objects.equals(JsonFields.getString(existingCondition, "status"), JsonFields.getString(desiredCondition, "status"))
                        && Objects.equals(JsonFields.getLong(existingCondition, "observedGeneration"),
                        JsonFields.getLong(desiredCondition, "observedGeneration"))
                        && reasonMatches) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }
        return true;
    }

    private static JsonObject buildCondition(final String type, final String status, final String reason,
                                             final String message, final Long observedGeneration) {
        JsonObject condition = new JsonObject();
        condition.addProperty("type", type);
        condition.addProperty("status", status);
        condition.addProperty("reason", reason);
        condition.addProperty("message", message);
        if (Objects.nonNull(observedGeneration)) {
            condition.addProperty("observedGeneration", observedGeneration);
        }
        condition.addProperty("lastTransitionTime", Instant.now().toString());
        return condition;
    }

    private static Long generationOf(final DynamicKubernetesObject gateway) {
        return JsonFields.getLong(JsonFields.getJsonObject(gateway.getRaw(), "metadata"), "generation");
    }

    /** Merge-patch the Gateway /status subresource, preserving conditions owned by other controllers. */
    private void patchGatewayStatus(final DynamicKubernetesObject gateway, final JsonObject statusObj) {
        try {
            final String namespace = gateway.getMetadata().getNamespace();
            final String name = gateway.getMetadata().getName();

            JsonObject body = new JsonObject();
            body.add("status", statusObj);
            body.addProperty("kind", GatewayApiConstants.GATEWAY_KIND);
            body.addProperty("apiVersion", GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION);

            JsonObject metadata = new JsonObject();
            metadata.addProperty("name", name);
            metadata.addProperty("namespace", namespace);
            body.add("metadata", metadata);

            String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION
                    + "/namespaces/" + namespace + "/gateways/" + name + "/status";

            StatusMergePatch.patch(apiClient, path, body);
            LOG.info("Updated Gateway {}/{} status", namespace, name);
        } catch (Exception e) {
            LOG.warn("Failed to update Gateway status, will retry on next resync", e);
        }
    }
}
