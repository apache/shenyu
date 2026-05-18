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

import com.google.gson.Gson;
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
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class GatewayReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(GatewayReconciler.class);

    private final Lister<DynamicKubernetesObject> gatewayLister;

    private final Lister<DynamicKubernetesObject> httpRouteLister;

    private final ShenyuCacheRepository shenyuCacheRepository;

    private final RateLimitingQueue<Request> httpRouteWorkQueue;

    private final ApiClient apiClient;

    public GatewayReconciler(final SharedIndexInformer<DynamicKubernetesObject> gatewayInformer,
                             final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                             final ShenyuCacheRepository shenyuCacheRepository,
                             final RateLimitingQueue<Request> httpRouteWorkQueue,
                             final ApiClient apiClient) {
        this.gatewayLister = new Lister<>(gatewayInformer.getIndexer());
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

            if (!isShenyuGateway(gateway)) {
                LOG.info("Gateway {} is not managed by ShenYu, skipping", request);
                return new Result(false);
            }

            updateGatewayAcceptedStatus(gateway);

            // Re-queue HTTPRoutes that reference this Gateway but haven't been applied yet
            requeueAffectedHTTPRoutes(request.getNamespace(), request.getName());

            LOG.info("Gateway {} reconciled successfully", request);
            return new Result(false);
        } catch (Exception e) {
            LOG.error("Error reconciling gateway {}, will retry", request, e);
            return new Result(true);
        }
    }

    /**
     * When a ShenYu Gateway is created/updated, find HTTPRoutes whose parentRefs reference
     * this Gateway and add them to the HTTPRoute controller's work queue for re-reconciliation.
     * This handles the case where an HTTPRoute was created before the Gateway existed.
     * Also handles cross-namespace references where HTTPRoute's parentRef specifies a different namespace.
     */
    private void requeueAffectedHTTPRoutes(final String gatewayNamespace, final String gatewayName) {
        // Search in the gateway's namespace (same-namespace reference)
        List<DynamicKubernetesObject> localRoutes = httpRouteLister.namespace(gatewayNamespace).list();
        for (DynamicKubernetesObject route : localRoutes) {
            if (isBoundToGateway(route, gatewayNamespace, gatewayName)) {
                Request req = new Request(route.getMetadata().getNamespace(), route.getMetadata().getName());
                httpRouteWorkQueue.add(req);
                LOG.info("Re-queued HTTPRoute {}/{} due to Gateway {}/{} reconciliation",
                        route.getMetadata().getNamespace(), route.getMetadata().getName(),
                        gatewayNamespace, gatewayName);
            }
        }
        // Also search all namespaces for cross-namespace references
        for (DynamicKubernetesObject route : httpRouteLister.list()) {
            String routeNamespace = Objects.requireNonNull(route.getMetadata()).getNamespace();
            if (routeNamespace.equals(gatewayNamespace)) {
                // Already handled in local routes search above
                continue;
            }
            if (isBoundToGateway(route, gatewayNamespace, gatewayName)) {
                Request req = new Request(route.getMetadata().getNamespace(), route.getMetadata().getName());
                httpRouteWorkQueue.add(req);
                LOG.info("Re-queued cross-namespace HTTPRoute {}/{} due to Gateway {}/{} reconciliation",
                        route.getMetadata().getNamespace(), route.getMetadata().getName(),
                        gatewayNamespace, gatewayName);
            }
        }
    }

    private boolean isBoundToGateway(final DynamicKubernetesObject httpRoute,
                                     final String gatewayNamespace, final String gatewayName) {
        JsonObject spec = httpRoute.getRaw().getAsJsonObject("spec");
        if (Objects.isNull(spec) || !spec.has("parentRefs")) {
            return false;
        }
        JsonArray parentRefs = spec.getAsJsonArray("parentRefs");
        if (Objects.isNull(parentRefs)) {
            return false;
        }
        String routeNamespace = Objects.requireNonNull(httpRoute.getMetadata()).getNamespace();
        for (JsonElement element : parentRefs) {
            JsonObject parentRef = element.getAsJsonObject();
            String parentName = parentRef.has("name") ? parentRef.get("name").getAsString() : null;
            String parentNamespace = parentRef.has("namespace") ? parentRef.get("namespace").getAsString() : routeNamespace;
            if (gatewayNamespace.equals(parentNamespace) && gatewayName.equals(parentName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * When a Gateway is deleted, cascade delete ShenYu config (selectors/rules) for all associated routes.
     */
    private void deleteAssociatedRoutes(final String gatewayNamespace, final String gatewayName) {
        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        List<String> routeKeys = cache.getRoutesByGateway(gatewayNamespace, gatewayName);
        if (CollectionUtils.isEmpty(routeKeys)) {
            return;
        }
        // Copy to avoid ConcurrentModificationException: removeRouteGatewayBinding modifies the same list
        List<String> routeKeysCopy = new ArrayList<>(routeKeys);
        // Remove gateway-route bindings first to prevent concurrent access issues
        cache.removeRoutesByGateway(gatewayNamespace, gatewayName);
        for (String routeKey : routeKeysCopy) {
            String[] parts = routeKey.split("/", 2);
            if (parts.length != 2) {
                continue;
            }
            String routeNamespace = parts[0];
            String routeName = parts[1];
            List<String> selectorIds = cache.removeRouteSelectors(routeNamespace, routeName, PluginEnum.DIVIDE.getName());
            if (CollectionUtils.isNotEmpty(selectorIds)) {
                for (String selectorId : selectorIds) {
                    List<RuleData> rules = shenyuCacheRepository.findRuleDataList(selectorId);
                    if (CollectionUtils.isNotEmpty(rules)) {
                        for (RuleData rule : rules) {
                            shenyuCacheRepository.deleteRuleData(PluginEnum.DIVIDE.getName(), selectorId, rule.getId());
                        }
                    }
                    shenyuCacheRepository.deleteSelectorData(PluginEnum.DIVIDE.getName(), selectorId);
                }
            }
            LOG.info("Deleted ShenYu config for route {}/{} due to Gateway deletion", routeNamespace, routeName);
        }
    }

    /**
     * Check if the given Gateway object is managed by ShenYu.
     *
     * @param gateway the Gateway dynamic object
     * @return true if the Gateway's gatewayClassName matches ShenYu
     */
    public static boolean isShenyuGateway(final DynamicKubernetesObject gateway) {
        JsonObject spec = gateway.getRaw().getAsJsonObject("spec");
        if (Objects.isNull(spec)) {
            return false;
        }
        if (!spec.has("gatewayClassName") || spec.get("gatewayClassName").isJsonNull()) {
            return false;
        }
        String gatewayClassName = spec.get("gatewayClassName").getAsString();
        return GatewayApiConstants.SHENYU_GATEWAY_CLASS_NAME.equals(gatewayClassName);
    }

    /**
     * Update Gateway status with Accepted=True condition.
     * Uses strategic merge patch on the /status subresource to avoid Gson JsonElement serialization issues
     * with the default updateStatus implementation.
     */
    private void updateGatewayAcceptedStatus(final DynamicKubernetesObject gateway) {
        if (GatewayApiConstants.isConditionTrue(gateway, "Accepted")) {
            return;
        }
        try {
            final String namespace = gateway.getMetadata().getNamespace();
            final String name = gateway.getMetadata().getName();

            JsonObject condition = new JsonObject();
            condition.addProperty("type", "Accepted");
            condition.addProperty("status", "True");
            condition.addProperty("reason", "Accepted");
            condition.addProperty("message", "Gateway has been accepted by the ShenYu controller");
            condition.addProperty("lastTransitionTime", Instant.now().toString());

            JsonArray conditions = new JsonArray();
            conditions.add(condition);

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

            String patchBody = new Gson().toJson(body);
            String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION
                    + "/namespaces/" + namespace + "/gateways/" + name + "/status";

            okhttp3.Request request = new okhttp3.Request.Builder()
                    .url(apiClient.getBasePath() + path)
                    .patch(okhttp3.RequestBody.create(patchBody, okhttp3.MediaType.parse("application/merge-patch+json")))
                    .build();

            try (okhttp3.Response response = apiClient.getHttpClient().newCall(request).execute()) {
                if (response.isSuccessful()) {
                    LOG.info("Updated Gateway {}/{} status to Accepted=True", namespace, name);
                } else {
                    String responseBody = Objects.nonNull(response.body()) ? response.body().string() : "empty";
                    LOG.warn("Failed to update Gateway {}/{} status: {} - {}", namespace, name, response.code(), responseBody);
                }
            }
        } catch (Exception e) {
            LOG.warn("Failed to update Gateway status, will retry on next resync", e);
        }
    }

}
