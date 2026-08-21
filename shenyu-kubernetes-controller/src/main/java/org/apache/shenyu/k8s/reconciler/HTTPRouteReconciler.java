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
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.JsonFields;
import org.apache.shenyu.k8s.common.ReferenceGrants;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.apache.shenyu.k8s.common.StatusMergePatch;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public class HTTPRouteReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(HTTPRouteReconciler.class);

    private final Lister<DynamicKubernetesObject> httpRouteLister;

    private final Lister<DynamicKubernetesObject> gatewayLister;

    private final Lister<DynamicKubernetesObject> gatewayClassLister;

    private final Lister<DynamicKubernetesObject> referenceGrantLister;

    private final HttpRouteParser httpRouteParser;

    private final ShenyuCacheRepository shenyuCacheRepository;

    private final ApiClient apiClient;

    public HTTPRouteReconciler(final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                               final SharedIndexInformer<DynamicKubernetesObject> gatewayInformer,
                               final SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer,
                               final SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer,
                               final HttpRouteParser httpRouteParser,
                               final ShenyuCacheRepository shenyuCacheRepository,
                               final ApiClient apiClient) {
        this.httpRouteLister = new Lister<>(httpRouteInformer.getIndexer());
        this.gatewayLister = new Lister<>(gatewayInformer.getIndexer());
        this.gatewayClassLister = new Lister<>(gatewayClassInformer.getIndexer());
        this.referenceGrantLister = new Lister<>(referenceGrantInformer.getIndexer());
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

            List<JsonObject> eligibleParents = findEligibleParentRefs(httpRoute);
            if (eligibleParents.isEmpty()) {
                // The route may have been bound before (grant removed, GatewayClass
                // re-pointed, listener removed): clean up whatever was programmed.
                LOG.info("HTTPRoute {} is not bound to a ShenYu Gateway, cleaning up previously applied config", request);
                deleteConfig(namespace, routeName);
                removeShenyuParentStatus(httpRoute);
                return new Result(false);
            }

            GatewayRouteCache cache = GatewayRouteCache.getInstance();
            List<String> oldSelectorIds = new ArrayList<>(
                    Optional.ofNullable(cache.getRouteSelectors(namespace, routeName, PluginEnum.DIVIDE.getName()))
                            .orElse(Collections.emptyList()));

            ShenyuMemoryConfig config = httpRouteParser.parse(httpRoute);

            Set<String> newSelectorIds = config.getRouteConfigList().stream()
                    .map(rc -> rc.getSelectorData().getId())
                    .collect(Collectors.toSet());

            deleteStaleSelectors(namespace, routeName, oldSelectorIds, newSelectorIds);

            applyConfig(config);

            bindToGateways(eligibleParents, namespace, routeName);

            updateHTTPRouteStatus(httpRoute, eligibleParents, namespace, config);

            LOG.debug("HTTPRoute {} reconciled successfully", request);
            return new Result(false);
        } catch (Exception e) {
            LOG.error("Error reconciling HTTPRoute {}, will retry", request, e);
            return new Result(true);
        }
    }

    /**
     * Resolve the eligible ShenYu parents once per reconcile; config programming, gateway
     * binding and status reporting all consume this list, so a mixed parentRefs list (one
     * valid, one unauthorized cross-namespace parent) cannot leak the unauthorized parent.
     */
    private List<JsonObject> findEligibleParentRefs(final DynamicKubernetesObject httpRoute) {
        JsonObject spec = JsonFields.getJsonObject(httpRoute.getRaw(), "spec");
        JsonArray parentRefs = Objects.isNull(spec) ? null : JsonFields.getJsonArray(spec, "parentRefs");
        if (Objects.isNull(parentRefs)) {
            return Collections.emptyList();
        }
        String routeNamespace = Objects.requireNonNull(httpRoute.getMetadata()).getNamespace();
        String routeName = httpRoute.getMetadata().getName();
        List<JsonObject> eligible = new ArrayList<>();
        for (JsonElement element : parentRefs) {
            JsonObject parentRef = element.getAsJsonObject();
            if (isEligibleParentRef(parentRef, routeNamespace, routeName)) {
                eligible.add(parentRef);
            }
        }
        return eligible;
    }

    /**
     * A parentRef is eligible when it references a Gateway (the default group/kind), the
     * Gateway is ShenYu-managed, the optional sectionName matches a Gateway listener, and a
     * cross-namespace reference is permitted by a ReferenceGrant.
     */
    private boolean isEligibleParentRef(final JsonObject parentRef, final String routeNamespace,
                                        final String routeName) {
        String parentName = JsonFields.getString(parentRef, "name");
        if (Objects.isNull(parentName)) {
            return false;
        }
        String parentGroup = Optional.ofNullable(JsonFields.getString(parentRef, "group"))
                .orElse(GatewayApiConstants.GATEWAY_API_GROUP);
        String parentKind = Optional.ofNullable(JsonFields.getString(parentRef, "kind"))
                .orElse(GatewayApiConstants.GATEWAY_KIND);
        if (!GatewayApiConstants.GATEWAY_API_GROUP.equals(parentGroup)
                || !GatewayApiConstants.GATEWAY_KIND.equals(parentKind)) {
            return false;
        }
        String parentNamespace = Optional.ofNullable(JsonFields.getString(parentRef, "namespace"))
                .orElse(routeNamespace);
        String sectionName = JsonFields.getString(parentRef, "sectionName");
        DynamicKubernetesObject gateway = gatewayLister.namespace(parentNamespace).get(parentName);
        if (Objects.isNull(gateway) || !GatewayClassReconciler.isShenyuGateway(gateway, gatewayClassLister)) {
            return false;
        }
        if (Objects.nonNull(sectionName) && !hasMatchingListener(gateway, sectionName)) {
            LOG.info("HTTPRoute {}/{} references sectionName '{}' but Gateway {}/{} has no matching listener",
                    routeNamespace, routeName, sectionName, parentNamespace, parentName);
            return false;
        }
        if (!parentNamespace.equals(routeNamespace)
                && !ReferenceGrants.isGranted(referenceGrantLister, parentNamespace, routeNamespace,
                GatewayApiConstants.GATEWAY_API_GROUP, GatewayApiConstants.GATEWAY_KIND, parentName)) {
            LOG.info("HTTPRoute {}/{} cross-namespace parentRef to Gateway {}/{} rejected: no matching ReferenceGrant",
                    routeNamespace, routeName, parentNamespace, parentName);
            return false;
        }
        return true;
    }

    private boolean hasMatchingListener(final DynamicKubernetesObject gateway, final String sectionName) {
        JsonObject spec = JsonFields.getJsonObject(gateway.getRaw(), "spec");
        JsonArray listeners = Objects.isNull(spec) ? null : JsonFields.getJsonArray(spec, "listeners");
        if (Objects.isNull(listeners)) {
            return false;
        }
        for (JsonElement listenerElement : listeners) {
            JsonObject listener = listenerElement.getAsJsonObject();
            if (sectionName.equals(JsonFields.getString(listener, "name"))) {
                return true;
            }
        }
        return false;
    }

    /**
     * Full cleanup when an HTTPRoute is deleted or detached: remove all its selectors/rules
     * from the data plane and drop the route→gateway bindings.
     */
    private void deleteConfig(final String namespace, final String routeName) {
        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        List<String> selectorIds = cache.removeRouteSelectors(namespace, routeName, PluginEnum.DIVIDE.getName());
        removeSelectors(selectorIds);
        cache.removeRouteGatewayBinding(namespace, routeName);
    }

    /** Remove only selectors absent from the new spec; retained ones are refreshed by applyConfig. */
    private void deleteStaleSelectors(final String namespace, final String routeName,
                                      final List<String> oldSelectorIds, final Set<String> newSelectorIds) {
        List<String> stale = new ArrayList<>();
        for (String id : oldSelectorIds) {
            if (!newSelectorIds.contains(id)) {
                stale.add(id);
            }
        }
        if (stale.isEmpty()) {
            return;
        }
        LOG.info("Deleting {} stale selector(s) for HTTPRoute {}/{}", stale.size(), namespace, routeName);
        removeSelectors(stale);
    }

    private void removeSelectors(final List<String> selectorIds) {
        if (CollectionUtils.isEmpty(selectorIds)) {
            return;
        }
        for (String selectorId : selectorIds) {
            shenyuCacheRepository.deleteSelectorWithRules(PluginEnum.DIVIDE.getName(), selectorId);
        }
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

    private void bindToGateways(final List<JsonObject> eligibleParents, final String routeNamespace,
                                final String routeName) {
        for (JsonObject parentRef : eligibleParents) {
            String parentName = parentRef.get("name").getAsString();
            String parentNamespace = Optional.ofNullable(JsonFields.getString(parentRef, "namespace"))
                    .orElse(routeNamespace);
            GatewayRouteCache.getInstance().bindRouteToGateway(parentNamespace, parentName, routeNamespace, routeName);
        }
    }

    /**
     * Update HTTPRoute status with Accepted=True and a ResolvedRefs condition reflecting the
     * actual backend resolution, via merge-patch on the /status subresource. Skipped when the
     * ShenYu entries already match, so an unchanged resync does not re-trigger reconciliation.
     */
    private void updateHTTPRouteStatus(final DynamicKubernetesObject httpRoute, final List<JsonObject> eligibleParents,
                                       final String routeNamespace, final ShenyuMemoryConfig config) {
        try {
            JsonArray desiredParents = new JsonArray();
            for (JsonObject parentRef : eligibleParents) {
                String parentName = parentRef.get("name").getAsString();
                String parentNamespace = Optional.ofNullable(JsonFields.getString(parentRef, "namespace"))
                        .orElse(routeNamespace);
                desiredParents.add(buildParentStatus(parentNamespace, parentName, config));
            }
            if (existingStatusMatches(httpRoute, desiredParents)) {
                return;
            }

            JsonObject raw = httpRoute.getRaw();
            JsonArray existingParents = raw.has("status") && !raw.get("status").isJsonNull()
                    ? JsonFields.getJsonArray(raw.getAsJsonObject("status"), "parents") : null;
            preserveTransitionTimes(existingParents, desiredParents);

            // Merge-patch replaces arrays, so preserve status.parents entries owned by other controllers
            JsonArray mergedParentsStatus = new JsonArray();
            if (Objects.nonNull(existingParents)) {
                for (JsonElement parentEl : existingParents) {
                    JsonObject parent = parentEl.getAsJsonObject();
                    if (!GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(JsonFields.getString(parent, "controllerName"))) {
                        mergedParentsStatus.add(parentEl);
                    }
                }
            }
            desiredParents.forEach(mergedParentsStatus::add);
            sendStatusPatch(routeNamespace, Objects.requireNonNull(httpRoute.getMetadata()).getName(), mergedParentsStatus);
        } catch (Exception e) {
            LOG.warn("Failed to update HTTPRoute status, will retry on next resync", e);
        }
    }

    /**
     * Drop ShenYu-owned entries from status.parents when the route is no longer attached to
     * any ShenYu Gateway, leaving entries owned by other controllers intact. Skipped when no
     * ShenYu entry remains, so the patch cannot loop reconciliation.
     */
    private void removeShenyuParentStatus(final DynamicKubernetesObject httpRoute) {
        try {
            JsonObject raw = httpRoute.getRaw();
            JsonArray existingParents = raw.has("status") && !raw.get("status").isJsonNull()
                    ? JsonFields.getJsonArray(raw.getAsJsonObject("status"), "parents") : null;
            if (Objects.isNull(existingParents)) {
                return;
            }
            JsonArray remaining = new JsonArray();
            boolean hasShenyuEntry = false;
            for (JsonElement parentEl : existingParents) {
                JsonObject parent = parentEl.getAsJsonObject();
                if (GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(JsonFields.getString(parent, "controllerName"))) {
                    hasShenyuEntry = true;
                } else {
                    remaining.add(parentEl);
                }
            }
            if (!hasShenyuEntry) {
                return;
            }
            sendStatusPatch(Objects.requireNonNull(httpRoute.getMetadata()).getNamespace(),
                    httpRoute.getMetadata().getName(), remaining);
        } catch (Exception e) {
            LOG.warn("Failed to remove ShenYu status from HTTPRoute, will retry on next resync", e);
        }
    }

    /**
     * Replace the freshly built conditions of parents whose status is unchanged with the
     * existing entries, so lastTransitionTime only advances when a condition actually
     * transitions, as the Gateway API spec requires.
     */
    private void preserveTransitionTimes(final JsonArray existingParents, final JsonArray desiredParents) {
        if (Objects.isNull(existingParents)) {
            return;
        }
        for (JsonElement desiredEl : desiredParents) {
            JsonObject desired = desiredEl.getAsJsonObject();
            for (JsonElement existingEl : existingParents) {
                JsonObject existing = existingEl.getAsJsonObject();
                if (!GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(JsonFields.getString(existing, "controllerName"))) {
                    continue;
                }
                if (sameParentRef(JsonFields.getJsonObject(existing, "parentRef"), JsonFields.getJsonObject(desired, "parentRef"))
                        && conditionsMatch(JsonFields.getJsonArray(existing, "conditions"), JsonFields.getJsonArray(desired, "conditions"))) {
                    desired.add("conditions", JsonFields.getJsonArray(existing, "conditions"));
                    break;
                }
            }
        }
    }

    /**
     * The ShenYu entries in status must exactly match the desired ones: every desired parent
     * present with equivalent conditions, and no extra ShenYu entries (a stale entry for a
     * removed parentRef must also trigger a re-patch).
     */
    private boolean existingStatusMatches(final DynamicKubernetesObject httpRoute, final JsonArray desiredParents) {
        JsonObject raw = httpRoute.getRaw();
        JsonArray existingParents = raw.has("status") && !raw.get("status").isJsonNull()
                ? JsonFields.getJsonArray(raw.getAsJsonObject("status"), "parents") : null;
        if (Objects.isNull(existingParents)) {
            return false;
        }
        List<JsonObject> existingShenyuEntries = new ArrayList<>();
        for (JsonElement parentEl : existingParents) {
            JsonObject parent = parentEl.getAsJsonObject();
            if (GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(JsonFields.getString(parent, "controllerName"))) {
                existingShenyuEntries.add(parent);
            }
        }
        if (existingShenyuEntries.size() != desiredParents.size()) {
            return false;
        }
        for (JsonElement desiredEl : desiredParents) {
            JsonObject desired = desiredEl.getAsJsonObject();
            boolean found = false;
            for (JsonObject parent : existingShenyuEntries) {
                if (sameParentRef(JsonFields.getJsonObject(parent, "parentRef"), JsonFields.getJsonObject(desired, "parentRef"))
                        && conditionsMatch(JsonFields.getJsonArray(parent, "conditions"), JsonFields.getJsonArray(desired, "conditions"))) {
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

    private boolean sameParentRef(final JsonObject existing, final JsonObject desired) {
        if (Objects.isNull(existing)) {
            return false;
        }
        return Objects.equals(JsonFields.getString(existing, "namespace"), JsonFields.getString(desired, "namespace"))
                && Objects.equals(JsonFields.getString(existing, "name"), JsonFields.getString(desired, "name"));
    }

    /**
     * Compare desired conditions against existing ones, ignoring lastTransitionTime. For
     * ResolvedRefs=False the reason must match too; reasons of True conditions are not
     * significant.
     */
    private boolean conditionsMatch(final JsonArray existing, final JsonArray desired) {
        if (Objects.isNull(existing)) {
            return false;
        }
        for (JsonElement desiredEl : desired) {
            JsonObject desiredCondition = desiredEl.getAsJsonObject();
            boolean matched = false;
            for (JsonElement existingEl : existing) {
                JsonObject existingCondition = existingEl.getAsJsonObject();
                if (!Objects.equals(JsonFields.getString(desiredCondition, "type"), JsonFields.getString(existingCondition, "type"))
                        || !Objects.equals(JsonFields.getString(desiredCondition, "status"), JsonFields.getString(existingCondition, "status"))) {
                    continue;
                }
                if ("ResolvedRefs".equals(JsonFields.getString(desiredCondition, "type"))
                        && "False".equals(JsonFields.getString(desiredCondition, "status"))
                        && !Objects.equals(JsonFields.getString(desiredCondition, "reason"), JsonFields.getString(existingCondition, "reason"))) {
                    continue;
                }
                matched = true;
                break;
            }
            if (!matched) {
                return false;
            }
        }
        return true;
    }

    private JsonObject buildParentStatus(final String parentNamespace, final String parentName,
                                         final ShenyuMemoryConfig config) {
        JsonObject parentRefStatus = new JsonObject();
        parentRefStatus.addProperty("group", GatewayApiConstants.GATEWAY_API_GROUP);
        parentRefStatus.addProperty("kind", GatewayApiConstants.GATEWAY_KIND);
        parentRefStatus.addProperty("namespace", parentNamespace);
        parentRefStatus.addProperty("name", parentName);

        JsonArray conditions = buildStatusConditions(Instant.now().toString(), config);

        JsonObject parentStatus = new JsonObject();
        parentStatus.add("parentRef", parentRefStatus);
        parentStatus.addProperty("controllerName", GatewayApiConstants.SHENYU_CONTROLLER_NAME);
        parentStatus.add("conditions", conditions);
        return parentStatus;
    }

    private JsonArray buildStatusConditions(final String now, final ShenyuMemoryConfig config) {
        final JsonArray conditions = new JsonArray();
        conditions.add(buildCondition("Accepted", "True", "Accepted",
                "Route was accepted by the ShenYu controller", now));
        if (config.isAllBackendsResolved()) {
            conditions.add(buildCondition("ResolvedRefs", "True", "ResolvedRefs",
                    "All references resolved", now));
        } else {
            String reason = Objects.nonNull(config.getUnresolvedReason())
                    ? config.getUnresolvedReason() : GatewayApiConstants.REASON_BACKEND_NOT_FOUND;
            conditions.add(buildCondition("ResolvedRefs", "False", reason, unresolvedMessage(reason), now));
        }
        return conditions;
    }

    private String unresolvedMessage(final String reason) {
        if (GatewayApiConstants.REASON_REF_NOT_PERMITTED.equals(reason)) {
            return "One or more backendRefs are not permitted by a ReferenceGrant";
        }
        if (GatewayApiConstants.REASON_INVALID_KIND.equals(reason)) {
            return "One or more backendRefs are of an unsupported kind; only Service is supported";
        }
        return "One or more backendRefs could not be resolved to ready endpoints";
    }

    private JsonObject buildCondition(final String type, final String status, final String reason,
                                      final String message, final String now) {
        JsonObject condition = new JsonObject();
        condition.addProperty("type", type);
        condition.addProperty("status", status);
        condition.addProperty("reason", reason);
        condition.addProperty("message", message);
        condition.addProperty("lastTransitionTime", now);
        return condition;
    }

    private void sendStatusPatch(final String routeNamespace, final String routeName,
                                  final JsonArray parentsStatus) throws ApiException {
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

        String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION
                + "/namespaces/" + routeNamespace + "/httproutes/" + routeName + "/status";

        StatusMergePatch.patch(apiClient, path, body);
        LOG.info("Updated HTTPRoute {}/{} status", routeNamespace, routeName);
    }
}
