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
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class HTTPRouteReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(HTTPRouteReconciler.class);

    private final Lister<DynamicKubernetesObject> httpRouteLister;

    private final Lister<DynamicKubernetesObject> gatewayLister;

    private final HttpRouteParser httpRouteParser;

    private final ShenyuCacheRepository shenyuCacheRepository;

    private final ApiClient apiClient;

    public HTTPRouteReconciler(final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                               final SharedIndexInformer<DynamicKubernetesObject> gatewayInformer,
                               final HttpRouteParser httpRouteParser,
                               final ShenyuCacheRepository shenyuCacheRepository,
                               final ApiClient apiClient) {
        this.httpRouteLister = new Lister<>(httpRouteInformer.getIndexer());
        this.gatewayLister = new Lister<>(gatewayInformer.getIndexer());
        this.httpRouteParser = httpRouteParser;
        this.shenyuCacheRepository = shenyuCacheRepository;
        this.apiClient = apiClient;
    }

    @Override
    public Result reconcile(final Request request) {
        LOG.info("Starting to reconcile HTTPRoute {}", request);
        try {
            String namespace = request.getNamespace();
            String routeName = request.getName();
            DynamicKubernetesObject httpRoute = httpRouteLister.namespace(namespace).get(routeName);

            if (Objects.isNull(httpRoute)) {
                deleteConfig(namespace, routeName);
                return new Result(false);
            }

            if (!isBoundToShenyuGateway(httpRoute)) {
                LOG.info("HTTPRoute {} is not bound to a ShenYu Gateway, skipping", request);
                return new Result(false);
            }

            deleteConfig(namespace, routeName);

            ShenyuMemoryConfig config = httpRouteParser.parse(httpRoute);
            applyConfig(config);

            bindToGateway(httpRoute);

            updateHTTPRouteStatus(httpRoute);

            LOG.info("HTTPRoute {} reconciled successfully", request);
            return new Result(false);
        } catch (Exception e) {
            LOG.error("Error reconciling HTTPRoute {}, will retry", request, e);
            return new Result(true);
        }
    }

    private boolean isBoundToShenyuGateway(final DynamicKubernetesObject httpRoute) {
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
            String sectionName = parentRef.has("sectionName") ? parentRef.get("sectionName").getAsString() : null;
            if (Objects.isNull(parentName)) {
                continue;
            }
            DynamicKubernetesObject gateway = gatewayLister.namespace(parentNamespace).get(parentName);
            if (Objects.nonNull(gateway) && GatewayReconciler.isShenyuGateway(gateway)) {
                // If sectionName is specified, verify the Gateway has a matching listener
                if (Objects.nonNull(sectionName) && !hasMatchingListener(gateway, sectionName)) {
                    LOG.info("HTTPRoute references sectionName '{}' but Gateway {}/{} has no matching listener", sectionName, parentNamespace, parentName);
                    continue;
                }
                return true;
            }
        }
        return false;
    }

    private boolean hasMatchingListener(final DynamicKubernetesObject gateway, final String sectionName) {
        JsonObject spec = gateway.getRaw().getAsJsonObject("spec");
        if (Objects.isNull(spec) || !spec.has("listeners")) {
            return false;
        }
        JsonArray listeners = spec.getAsJsonArray("listeners");
        if (Objects.isNull(listeners)) {
            return false;
        }
        for (JsonElement listenerElement : listeners) {
            JsonObject listener = listenerElement.getAsJsonObject();
            if (listener.has("name") && sectionName.equals(listener.get("name").getAsString())) {
                return true;
            }
        }
        return false;
    }

    private void deleteConfig(final String namespace, final String routeName) {
        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        List<String> selectorIds = cache.removeRouteSelectors(namespace, routeName, PluginEnum.DIVIDE.getName());
        if (CollectionUtils.isNotEmpty(selectorIds)) {
            for (String selectorId : selectorIds) {
                List<RuleData> rules = shenyuCacheRepository.findRuleDataList(selectorId);
                if (CollectionUtils.isNotEmpty(rules)) {
                    for (RuleData rule : new ArrayList<>(rules)) {
                        shenyuCacheRepository.deleteRuleData(PluginEnum.DIVIDE.getName(), selectorId, rule.getId());
                    }
                }
                shenyuCacheRepository.deleteSelectorData(PluginEnum.DIVIDE.getName(), selectorId);
            }
        }
        cache.removeRouteGatewayBinding(namespace, routeName);
    }

    private void applyConfig(final ShenyuMemoryConfig config) {
        List<IngressConfiguration> routeConfigs = config.getRouteConfigList();
        if (CollectionUtils.isEmpty(routeConfigs)) {
            return;
        }
        for (IngressConfiguration routeConfig : routeConfigs) {
            SelectorData selectorData = routeConfig.getSelectorData();
            shenyuCacheRepository.saveOrUpdateSelectorData(selectorData);
            for (RuleData ruleData : routeConfig.getRuleDataList()) {
                shenyuCacheRepository.saveOrUpdateRuleData(ruleData);
            }
        }
    }

    private void bindToGateway(final DynamicKubernetesObject httpRoute) {
        JsonObject spec = httpRoute.getRaw().getAsJsonObject("spec");
        if (Objects.isNull(spec) || !spec.has("parentRefs")) {
            return;
        }
        JsonArray parentRefs = spec.getAsJsonArray("parentRefs");
        String routeNamespace = Objects.requireNonNull(httpRoute.getMetadata()).getNamespace();
        String routeName = httpRoute.getMetadata().getName();

        for (JsonElement element : parentRefs) {
            JsonObject parentRef = element.getAsJsonObject();
            String parentName = parentRef.has("name") ? parentRef.get("name").getAsString() : null;
            String parentNamespace = parentRef.has("namespace") ? parentRef.get("namespace").getAsString() : routeNamespace;
            if (Objects.nonNull(parentName)) {
                GatewayRouteCache.getInstance().bindRouteToGateway(parentNamespace, parentName, routeNamespace, routeName);
            }
        }
    }

    /**
     * Update HTTPRoute status with Accepted=True and ResolvedRefs=True for each ShenYu-managed parent.
     * Uses merge-patch on the /status subresource, same approach as GatewayReconciler.
     * Skips the patch if the status is already up-to-date to avoid triggering an infinite reconcile loop.
     */
    private void updateHTTPRouteStatus(final DynamicKubernetesObject httpRoute) {
        if (isRouteStatusAlreadySet(httpRoute)) {
            return;
        }
        try {
            final String routeNamespace = Objects.requireNonNull(httpRoute.getMetadata()).getNamespace();
            final String routeName = httpRoute.getMetadata().getName();

            JsonObject spec = httpRoute.getRaw().getAsJsonObject("spec");
            if (Objects.isNull(spec) || !spec.has("parentRefs")) {
                return;
            }
            JsonArray parentRefs = spec.getAsJsonArray("parentRefs");
            JsonArray parentsStatus = buildParentsStatus(parentRefs, routeNamespace);

            if (parentsStatus.size() == 0) {
                return;
            }
            sendStatusPatch(routeNamespace, routeName, parentsStatus);
        } catch (Exception e) {
            LOG.warn("Failed to update HTTPRoute status, will retry on next resync", e);
        }
    }

    /**
     * Check if the HTTPRoute already has Accepted=True condition from the ShenYu controller
     * in its status.parents, to avoid unnecessary status patches that trigger infinite reconcile loops.
     */
    private boolean isRouteStatusAlreadySet(final DynamicKubernetesObject httpRoute) {
        JsonObject raw = httpRoute.getRaw();
        if (!raw.has("status") || raw.get("status").isJsonNull()) {
            return false;
        }
        JsonObject status = raw.getAsJsonObject("status");
        if (!status.has("parents") || status.get("parents").isJsonNull()) {
            return false;
        }
        JsonArray parents = status.getAsJsonArray("parents");
        for (JsonElement parentElement : parents) {
            JsonObject parent = parentElement.getAsJsonObject();
            if (!parent.has("controllerName") || !GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(parent.get("controllerName").getAsString())) {
                continue;
            }
            if (!parent.has("conditions") || parent.get("conditions").isJsonNull()) {
                continue;
            }
            JsonArray conditions = parent.getAsJsonArray("conditions");
            for (JsonElement condElement : conditions) {
                JsonObject cond = condElement.getAsJsonObject();
                if ("Accepted".equals(cond.has("type") ? cond.get("type").getAsString() : null)
                        && "True".equals(cond.has("status") ? cond.get("status").getAsString() : null)) {
                    return true;
                }
            }
        }
        return false;
    }

    private JsonArray buildParentsStatus(final JsonArray parentRefs, final String routeNamespace) {
        final JsonArray parentsStatus = new JsonArray();
        for (JsonElement element : parentRefs) {
            JsonObject parentRef = element.getAsJsonObject();
            String parentName = parentRef.has("name") ? parentRef.get("name").getAsString() : null;
            String parentNamespace = parentRef.has("namespace") ? parentRef.get("namespace").getAsString() : routeNamespace;

            if (Objects.isNull(parentName)) {
                continue;
            }
            DynamicKubernetesObject gateway = gatewayLister.namespace(parentNamespace).get(parentName);
            if (Objects.isNull(gateway) || !GatewayReconciler.isShenyuGateway(gateway)) {
                continue;
            }
            parentsStatus.add(buildParentStatus(parentNamespace, parentName));
        }
        return parentsStatus;
    }

    private JsonObject buildParentStatus(final String parentNamespace, final String parentName) {
        JsonObject parentRefStatus = new JsonObject();
        parentRefStatus.addProperty("group", GatewayApiConstants.GATEWAY_API_GROUP);
        parentRefStatus.addProperty("kind", GatewayApiConstants.GATEWAY_KIND);
        parentRefStatus.addProperty("namespace", parentNamespace);
        parentRefStatus.addProperty("name", parentName);

        String now = Instant.now().toString();
        JsonArray conditions = buildStatusConditions(now);

        JsonObject parentStatus = new JsonObject();
        parentStatus.add("parentRef", parentRefStatus);
        parentStatus.addProperty("controllerName", GatewayApiConstants.SHENYU_CONTROLLER_NAME);
        parentStatus.add("conditions", conditions);
        return parentStatus;
    }

    private JsonArray buildStatusConditions(final String now) {
        final JsonArray conditions = new JsonArray();
        conditions.add(buildCondition("Accepted", "Route was accepted by the ShenYu controller", now));
        conditions.add(buildCondition("ResolvedRefs", "All references resolved", now));
        return conditions;
    }

    private JsonObject buildCondition(final String type, final String message, final String now) {
        JsonObject condition = new JsonObject();
        condition.addProperty("type", type);
        condition.addProperty("status", "True");
        condition.addProperty("reason", type);
        condition.addProperty("message", message);
        condition.addProperty("lastTransitionTime", now);
        return condition;
    }

    private void sendStatusPatch(final String routeNamespace, final String routeName,
                                  final JsonArray parentsStatus) throws Exception {
        JsonObject statusObj = new JsonObject();
        statusObj.add("parents", parentsStatus);

        JsonObject body = new JsonObject();
        body.add("status", statusObj);
        body.addProperty("kind", GatewayApiConstants.HTTP_ROUTE_KIND);
        body.addProperty("apiVersion", GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION);

        JsonObject metadata = new JsonObject();
        metadata.addProperty("name", routeName);
        metadata.addProperty("namespace", routeNamespace);
        body.add("metadata", metadata);

        String patchBody = new Gson().toJson(body);
        String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION
                + "/namespaces/" + routeNamespace + "/httproutes/" + routeName + "/status";

        okhttp3.Request httpRequest = new okhttp3.Request.Builder()
                .url(apiClient.getBasePath() + path)
                .patch(okhttp3.RequestBody.create(patchBody, okhttp3.MediaType.parse("application/merge-patch+json")))
                .build();

        try (okhttp3.Response response = apiClient.getHttpClient().newCall(httpRequest).execute()) {
            if (response.isSuccessful()) {
                LOG.info("Updated HTTPRoute {}/{} status to Accepted=True", routeNamespace, routeName);
            } else {
                String responseBody = Objects.nonNull(response.body()) ? response.body().string() : "empty";
                LOG.warn("Failed to update HTTPRoute {}/{} status: {} - {}", routeNamespace, routeName, response.code(), responseBody);
            }
        }
    }
}
