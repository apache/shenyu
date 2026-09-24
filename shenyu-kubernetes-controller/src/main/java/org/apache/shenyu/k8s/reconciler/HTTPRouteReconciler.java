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
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.JsonFields;
import org.apache.shenyu.k8s.common.ListenerSupport;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.apache.shenyu.k8s.common.StatusMergePatch;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Reconciler for HTTPRoute resources (Gateway API v1).
 *
 * <p>Every parentRef is evaluated individually and reported in status.parents — including
 * rejections (listener policy, hostname mismatch, missing parent) with the spec-defined
 * reason — except parentRefs resolved to Gateways owned by another controller, whose
 * status entries belong to that controller. Cross-namespace attachment is authorized by
 * the listener's allowedRoutes only; ReferenceGrant governs cross-namespace backendRefs,
 * which the {@link HttpRouteParser} validates.
 */
public class HTTPRouteReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(HTTPRouteReconciler.class);

    private final Lister<DynamicKubernetesObject> httpRouteLister;

    private final Lister<DynamicKubernetesObject> gatewayLister;

    private final Lister<DynamicKubernetesObject> gatewayClassLister;

    private final HttpRouteParser httpRouteParser;

    private final ShenyuCacheRepository shenyuCacheRepository;

    private final ApiClient apiClient;

    /** The port the embedded ShenYu data plane actually listens on ({@code server.port}); route attachment is allowed only on listeners the gateway really serves. */
    private final int servedPort;

    public HTTPRouteReconciler(final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                               final SharedIndexInformer<DynamicKubernetesObject> gatewayInformer,
                               final SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer,
                               final HttpRouteParser httpRouteParser,
                               final ShenyuCacheRepository shenyuCacheRepository,
                               final ApiClient apiClient,
                               final int servedPort) {
        this.httpRouteLister = new Lister<>(httpRouteInformer.getIndexer());
        this.gatewayLister = new Lister<>(gatewayInformer.getIndexer());
        this.gatewayClassLister = new Lister<>(gatewayClassInformer.getIndexer());
        this.httpRouteParser = httpRouteParser;
        this.shenyuCacheRepository = shenyuCacheRepository;
        this.apiClient = apiClient;
        this.servedPort = servedPort;
    }

    @Override
    public Result reconcile(final Request request) {
        String namespace = request.getNamespace();
        String routeName = request.getName();
        LOG.debug("Starting to reconcile HTTPRoute {}/{}", namespace, routeName);

        DynamicKubernetesObject httpRoute = httpRouteLister.namespace(namespace).get(routeName);
        if (Objects.isNull(httpRoute)) {
            deleteConfig(namespace, routeName);
            return new Result(false);
        }

        List<ParentDecision> decisions = evaluateParents(httpRoute);
        if (decisions.isEmpty()) {
            // Not attached to any ShenYu-managed Gateway; drop previously programmed config
            // and our status entries, then leave the route to other controllers.
            deleteConfig(namespace, routeName);
            removeShenyuParentStatus(httpRoute, namespace, routeName);
            return new Result(false);
        }

        List<ParentDecision> accepted = new ArrayList<>();
        for (ParentDecision decision : decisions) {
            if (decision.accepted) {
                accepted.add(decision);
            }
        }

        ShenyuMemoryConfig config = null;
        if (accepted.isEmpty()) {
            // All ShenYu parents rejected the attachment: clean up but still report status.
            deleteConfig(namespace, routeName);
        } else {
            config = httpRouteParser.parse(httpRoute, effectiveHostnames(accepted));
            if (config.isHasUnsupportedFilters()) {
                deleteConfig(namespace, routeName);
            } else {
                GatewayRouteCache cache = GatewayRouteCache.getInstance();
                List<String> newSelectorIds = selectorIdsOf(config);
                // read the previous snapshot before putRouteSelectors overwrites it
                List<String> oldSelectorIds = Objects.requireNonNullElse(
                        cache.getRouteSelectors(namespace, routeName, PluginEnum.DIVIDE.getName()), List.of());
                applyConfig(config, namespace, routeName);
                deleteStaleSelectors(namespace, routeName, oldSelectorIds, newSelectorIds);
                cache.putRouteSelectors(namespace, routeName, PluginEnum.DIVIDE.getName(), newSelectorIds);
                rebindGateways(cache, namespace, routeName, accepted);
            }
        }

        updateHTTPRouteStatus(httpRoute, decisions, config, namespace, routeName);
        return new Result(false);
    }

    private List<String> selectorIdsOf(final ShenyuMemoryConfig config) {
        List<String> ids = new ArrayList<>();
        for (IngressConfiguration rc : config.getRouteConfigList()) {
            ids.add(rc.getSelectorData().getId());
        }
        return ids;
    }

    /**
     * Effective hostnames for the data plane: the union of the per-parent intersections of
     * route hostnames with listener hostnames. Empty means "any host" — and one accepting
     * parent with an empty hostname list makes the route host-agnostic overall, so the
     * union with other parents' hostnames must not narrow it back down.
     */
    private List<String> effectiveHostnames(final List<ParentDecision> accepted) {
        Set<String> union = new LinkedHashSet<>();
        for (ParentDecision decision : accepted) {
            if (decision.hostnames.isEmpty()) {
                return List.of();
            }
            union.addAll(decision.hostnames);
        }
        return new ArrayList<>(union);
    }

    /**
     * Rebuild the route→gateway bindings from the currently accepted parents at listener
     * granularity, dropping stale bindings of parentRefs that were removed or became
     * ineligible. The per-listener bindings drive the Gateway listeners'
     * attachedRoutes counts.
     */
    private void rebindGateways(final GatewayRouteCache cache, final String namespace, final String routeName,
                                final List<ParentDecision> accepted) {
        cache.removeRouteGatewayBinding(namespace, routeName);
        for (ParentDecision decision : accepted) {
            cache.bindRouteToGateway(decision.parentNamespace, decision.parentName, decision.listenerNames,
                    namespace, routeName);
        }
    }

    /**
     * Evaluate every parentRef of the route. Only verdicts this controller is responsible
     * for are returned: parentRefs of a foreign kind or group, and parentRefs resolving to
     * a Gateway owned by another controller, are skipped silently.
     */
    private List<ParentDecision> evaluateParents(final DynamicKubernetesObject httpRoute) {
        String routeNamespace = Objects.requireNonNull(httpRoute.getMetadata()).getNamespace();
        JsonObject spec = JsonFields.getJsonObject(httpRoute.getRaw(), "spec");
        JsonArray parentRefs = JsonFields.getJsonArray(spec, "parentRefs");
        List<String> routeHostnames = readStringList(JsonFields.getJsonArray(spec, "hostnames"));
        List<ParentDecision> decisions = new ArrayList<>();
        if (Objects.isNull(parentRefs)) {
            return decisions;
        }
        for (JsonElement element : parentRefs) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject parentRef = element.getAsJsonObject();
            String group = JsonFields.getString(parentRef, "group");
            String kind = JsonFields.getString(parentRef, "kind");
            if ((Objects.nonNull(group) && !GatewayApiConstants.GATEWAY_API_GROUP.equals(group))
                    || (Objects.nonNull(kind) && !GatewayApiConstants.GATEWAY_KIND.equals(kind))) {
                continue;
            }
            String parentName = JsonFields.getString(parentRef, "name");
            if (Objects.isNull(parentName)) {
                continue;
            }
            String parentNamespace = JsonFields.getString(parentRef, "namespace");
            if (Objects.isNull(parentNamespace)) {
                parentNamespace = routeNamespace;
            }
            DynamicKubernetesObject gateway = gatewayLister.namespace(parentNamespace).get(parentName);
            if (Objects.isNull(gateway)) {
                decisions.add(ParentDecision.rejected(parentRef, parentNamespace, parentName,
                        GatewayApiConstants.REASON_NO_MATCHING_PARENT,
                        "Gateway " + parentNamespace + "/" + parentName + " does not exist"));
                continue;
            }
            if (!isShenyuGateway(gateway)) {
                continue;
            }
            // Cross-namespace attachment is authorized by the listener's allowedRoutes
            // policy inside evaluateListeners; per the Gateway API spec a ReferenceGrant
            // does not apply to a Route's parentRef, so no grant check happens here.
            String sectionName = JsonFields.getString(parentRef, "sectionName");
            Long parentPort = JsonFields.getLong(parentRef, "port");
            decisions.add(evaluateListeners(gateway, sectionName, parentPort, routeNamespace, parentNamespace,
                    parentName, routeHostnames, parentRef));
        }
        return decisions;
    }

    /**
     * Attachment evaluation against the selected listeners: the listener must be usable for
     * this gateway (supported protocol on a served port), pass the allowedRoutes
     * namespace/kind policy, and intersect the route hostnames. The route attaches when at
     * least one selected listener accepts it, and the accepting listener names are carried
     * with the decision for the per-listener attachedRoutes binding.
     */
    private ParentDecision evaluateListeners(final DynamicKubernetesObject gateway, final String sectionName,
                                             final Long parentPort, final String routeNamespace,
                                             final String parentNamespace, final String parentName,
                                             final List<String> routeHostnames, final JsonObject parentRef) {
        List<JsonObject> selected = ListenerSupport.selectListeners(gateway.getRaw(), sectionName, parentPort);
        if (selected.isEmpty()) {
            String message;
            if (Objects.nonNull(sectionName)) {
                message = "no listener named '" + sectionName + "' on Gateway " + parentNamespace + "/" + parentName;
                if (Objects.nonNull(parentPort)) {
                    message = message + " serving port " + parentPort;
                }
            } else if (Objects.nonNull(parentPort)) {
                message = "Gateway " + parentNamespace + "/" + parentName + " has no listener on port " + parentPort;
            } else {
                message = "Gateway " + parentNamespace + "/" + parentName + " has no listeners";
            }
            return ParentDecision.rejected(parentRef, parentNamespace, parentName,
                    GatewayApiConstants.REASON_UNSUPPORTED_VALUE, message);
        }
        Set<String> effective = new LinkedHashSet<>();
        Set<String> matchedListeners = new LinkedHashSet<>();
        boolean anyMatched = false;
        boolean notPermitted = false;
        boolean hostnameMismatch = false;
        for (JsonObject listener : selected) {
            // Mirror of the Gateway reconciler's listener usability: a listener on an
            // unserved port is reported PortUnavailable there, so accepting a route on it
            // here would let status and data-plane behavior diverge.
            if (!ListenerSupport.isSupportedProtocol(listener) || !ListenerSupport.servesPort(listener, servedPort)) {
                continue;
            }
            if (!ListenerSupport.allowsNamespace(listener, routeNamespace, parentNamespace)
                    || !ListenerSupport.allowsKind(listener)) {
                notPermitted = true;
                continue;
            }
            List<String> intersect = ListenerSupport.intersectHostnames(ListenerSupport.hostnameOf(listener), routeHostnames);
            if (Objects.isNull(intersect)) {
                hostnameMismatch = true;
                continue;
            }
            // note an empty intersection is still a match: route without hostnames attaching
            // to a listener without hostname means "any host"
            anyMatched = true;
            matchedListeners.add(ListenerSupport.nameOf(listener));
            effective.addAll(intersect);
        }
        if (anyMatched) {
            return ParentDecision.accepted(parentRef, parentNamespace, parentName, new ArrayList<>(effective),
                    matchedListeners);
        }
        if (notPermitted) {
            return ParentDecision.rejected(parentRef, parentNamespace, parentName,
                    GatewayApiConstants.REASON_REF_NOT_PERMITTED,
                    "listener allowedRoutes policy does not permit routes from namespace " + routeNamespace);
        }
        if (hostnameMismatch) {
            return ParentDecision.rejected(parentRef, parentNamespace, parentName,
                    GatewayApiConstants.REASON_NO_MATCHING_LISTENER_HOSTNAME,
                    "route hostnames do not intersect any listener hostname");
        }
        return ParentDecision.rejected(parentRef, parentNamespace, parentName,
                GatewayApiConstants.REASON_UNSUPPORTED_VALUE,
                "no listener with a supported protocol (HTTP) on a served port (serving " + servedPort + ")");
    }

    private List<String> readStringList(final JsonArray array) {
        List<String> result = new ArrayList<>();
        if (Objects.nonNull(array)) {
            for (JsonElement element : array) {
                if (element.isJsonPrimitive()) {
                    result.add(element.getAsString());
                }
            }
        }
        return result;
    }

    private boolean isShenyuGateway(final DynamicKubernetesObject gateway) {
        JsonObject spec = JsonFields.getJsonObject(gateway.getRaw(), "spec");
        String className = JsonFields.getString(spec, "gatewayClassName");
        if (Objects.isNull(className)) {
            return false;
        }
        DynamicKubernetesObject gatewayClass = gatewayClassLister.get(className);
        if (Objects.isNull(gatewayClass)) {
            return false;
        }
        JsonObject classSpec = JsonFields.getJsonObject(gatewayClass.getRaw(), "spec");
        return GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(JsonFields.getString(classSpec, "controllerName"));
    }

    private void applyConfig(final ShenyuMemoryConfig config, final String namespace, final String routeName) {
        for (IngressConfiguration routeConfig : config.getRouteConfigList()) {
            shenyuCacheRepository.saveOrUpdateSelectorData(routeConfig.getSelectorData());
            if (CollectionUtils.isNotEmpty(routeConfig.getRuleDataList())) {
                shenyuCacheRepository.saveOrUpdateRuleData(routeConfig.getRuleDataList().get(0));
            }
        }
        LOG.debug("HTTPRoute {}/{}: applied {} selector(s)", namespace, routeName, config.getRouteConfigList().size());
    }

    /**
     * Delete selectors that were programmed by a previous spec of this route but are no
     * longer part of the current parse. Runs AFTER the new config is applied, so a spec
     * change that shifts deterministic IDs never leaves a live interval with no matching
     * selector; the cache's ID snapshot is committed only after this succeeds.
     */
    private void deleteStaleSelectors(final String namespace, final String routeName,
                                      final List<String> oldSelectorIds, final List<String> newSelectorIds) {
        Set<String> stale = new LinkedHashSet<>(oldSelectorIds);
        stale.removeAll(newSelectorIds);
        for (String selectorId : stale) {
            shenyuCacheRepository.deleteSelectorWithRules(PluginEnum.DIVIDE.getName(), selectorId);
            LOG.info("HTTPRoute {}/{}: removed stale divide selector {}", namespace, routeName, selectorId);
        }
    }

    private void deleteConfig(final String namespace, final String routeName) {
        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        List<String> selectorIds = cache.removeRouteSelectors(namespace, routeName, PluginEnum.DIVIDE.getName());
        if (Objects.nonNull(selectorIds)) {
            for (String selectorId : selectorIds) {
                shenyuCacheRepository.deleteSelectorWithRules(PluginEnum.DIVIDE.getName(), selectorId);
                LOG.info("HTTPRoute {}/{}: deleted divide selector {}", namespace, routeName, selectorId);
            }
        }
        cache.removeRouteGatewayBinding(namespace, routeName);
    }

    private void updateHTTPRouteStatus(final DynamicKubernetesObject httpRoute, final List<ParentDecision> decisions,
                                       final ShenyuMemoryConfig config, final String namespace, final String routeName) {
        Long generation = JsonFields.getLong(JsonFields.getJsonObject(httpRoute.getRaw(), "metadata"), "generation");
        JsonArray desiredParents = new JsonArray();
        for (ParentDecision decision : decisions) {
            desiredParents.add(buildParentStatus(decision, config, generation));
        }

        JsonObject raw = httpRoute.getRaw();
        JsonObject currentStatus = JsonFields.getJsonObject(raw, "status");
        if (Objects.nonNull(currentStatus)) {
            JsonArray existingParents = JsonFields.getJsonArray(currentStatus, "parents");
            retainForeignParentEntries(existingParents, desiredParents);
            if (existingStatusMatches(existingParents, desiredParents)) {
                return;
            }
            preserveTransitionTimes(existingParents, desiredParents);
        }

        JsonObject status = new JsonObject();
        status.add("parents", desiredParents);
        sendStatusPatch(httpRoute, namespace, routeName, status);
    }

    private JsonObject buildParentStatus(final ParentDecision decision, final ShenyuMemoryConfig config,
                                         final Long generation) {
        JsonObject parentRefStatus = new JsonObject();
        parentRefStatus.addProperty("group", GatewayApiConstants.GATEWAY_API_GROUP);
        parentRefStatus.addProperty("kind", GatewayApiConstants.GATEWAY_KIND);
        parentRefStatus.addProperty("namespace", decision.parentNamespace);
        parentRefStatus.addProperty("name", decision.parentName);
        String sectionName = JsonFields.getString(decision.parentRef, "sectionName");
        if (Objects.nonNull(sectionName)) {
            parentRefStatus.addProperty("sectionName", sectionName);
        }
        Long parentPort = JsonFields.getLong(decision.parentRef, "port");
        if (Objects.nonNull(parentPort)) {
            parentRefStatus.addProperty("port", parentPort);
        }

        JsonObject parent = new JsonObject();
        parent.add("parentRef", parentRefStatus);
        parent.addProperty("controllerName", GatewayApiConstants.SHENYU_CONTROLLER_NAME);
        parent.add("conditions", buildStatusConditions(decision, config, generation));
        return parent;
    }

    private JsonArray buildStatusConditions(final ParentDecision decision, final ShenyuMemoryConfig config,
                                            final Long generation) {
        JsonArray conditions = new JsonArray();

        boolean unsupportedFilters = Objects.nonNull(config) && config.isHasUnsupportedFilters();
        if (!decision.accepted) {
            conditions.add(buildCondition(GatewayApiConstants.CONDITION_ACCEPTED, "False", decision.reason,
                    decision.message, generation));
        } else if (unsupportedFilters) {
            conditions.add(buildCondition(GatewayApiConstants.CONDITION_ACCEPTED, "False",
                    GatewayApiConstants.REASON_UNSUPPORTED_VALUE,
                    "route rules declare filters which are not supported by ShenYu", generation));
        } else {
            conditions.add(buildCondition(GatewayApiConstants.CONDITION_ACCEPTED, "True", "Accepted",
                    "HTTPRoute is accepted by the ShenYu Gateway", generation));
        }

        boolean resolved = Objects.isNull(config) || config.isAllBackendsResolved();
        if (resolved) {
            conditions.add(buildCondition(GatewayApiConstants.CONDITION_RESOLVED_REFS, "True", "ResolvedRefs",
                    "All references have been resolved", generation));
        } else {
            String reason = Objects.nonNull(config.getUnresolvedReason())
                    ? config.getUnresolvedReason() : GatewayApiConstants.REASON_BACKEND_NOT_FOUND;
            conditions.add(buildCondition(GatewayApiConstants.CONDITION_RESOLVED_REFS, "False", reason,
                    "Some services were not found or not permitted", generation));
        }
        return conditions;
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
        condition.addProperty("lastTransitionTime", java.time.Instant.now().toString());
        return condition;
    }

    /**
     * Whether the existing parents already cover every desired parent with matching
     * conditions; used to skip no-op patches that would otherwise cause an infinite
     * watch/patch loop (each patch bumps resourceVersion, re-enqueueing the route).
     */
    private boolean existingStatusMatches(final JsonArray existingParents, final JsonArray desiredParents) {
        if (Objects.isNull(existingParents) || existingParents.size() != desiredParents.size()) {
            return false;
        }
        List<JsonObject> unmatched = new ArrayList<>();
        for (JsonElement element : existingParents) {
            if (element.isJsonObject()) {
                unmatched.add(element.getAsJsonObject());
            }
        }
        for (JsonElement desiredElement : desiredParents) {
            JsonObject desired = desiredElement.getAsJsonObject();
            boolean found = false;
            for (int i = 0; i < unmatched.size(); i++) {
                JsonObject existing = unmatched.get(i);
                if (sameParentRef(JsonFields.getJsonObject(existing, "parentRef"),
                        JsonFields.getJsonObject(desired, "parentRef"))
                        && Objects.equals(JsonFields.getString(desired, "controllerName"),
                        JsonFields.getString(existing, "controllerName"))
                        && conditionsMatch(JsonFields.getJsonArray(existing, "conditions"),
                        JsonFields.getJsonArray(desired, "conditions"))) {
                    unmatched.remove(i);
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
        if (Objects.isNull(existing) || Objects.isNull(desired)) {
            return false;
        }
        return Objects.equals(JsonFields.getString(existing, "namespace"), JsonFields.getString(desired, "namespace"))
                && Objects.equals(JsonFields.getString(existing, "name"), JsonFields.getString(desired, "name"))
                && Objects.equals(JsonFields.getString(existing, "sectionName"), JsonFields.getString(desired, "sectionName"))
                && Objects.equals(JsonFields.getLong(existing, "port"), JsonFields.getLong(desired, "port"));
    }

    private boolean conditionsMatch(final JsonArray existing, final JsonArray desired) {
        if (Objects.isNull(existing) || Objects.isNull(desired) || existing.size() != desired.size()) {
            return false;
        }
        for (JsonElement desiredElement : desired) {
            JsonObject desiredCondition = desiredElement.getAsJsonObject();
            boolean found = false;
            for (JsonElement existingElement : existing) {
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

    /**
     * Keep the lastTransitionTime of conditions whose (type, status) is unchanged, as the
     * spec requires the timestamp to advance only on an actual status transition.
     */
    private void preserveTransitionTimes(final JsonArray existingParents, final JsonArray desiredParents) {
        for (JsonElement desiredElement : desiredParents) {
            JsonObject desired = desiredElement.getAsJsonObject();
            JsonObject desiredRef = JsonFields.getJsonObject(desired, "parentRef");
            JsonArray desiredConditions = JsonFields.getJsonArray(desired, "conditions");
            for (JsonElement existingElement : existingParents) {
                JsonObject existing = existingElement.getAsJsonObject();
                if (!GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(JsonFields.getString(existing, "controllerName"))) {
                    continue;
                }
                if (!sameParentRef(JsonFields.getJsonObject(existing, "parentRef"), desiredRef)) {
                    continue;
                }
                JsonArray existingConditions = JsonFields.getJsonArray(existing, "conditions");
                if (Objects.isNull(existingConditions) || Objects.isNull(desiredConditions)) {
                    continue;
                }
                for (JsonElement desiredConditionElement : desiredConditions) {
                    JsonObject desiredCondition = desiredConditionElement.getAsJsonObject();
                    for (JsonElement existingConditionElement : existingConditions) {
                        JsonObject existingCondition = existingConditionElement.getAsJsonObject();
                        if (Objects.equals(JsonFields.getString(existingCondition, "type"), JsonFields.getString(desiredCondition, "type"))
                                && Objects.equals(JsonFields.getString(existingCondition, "status"), JsonFields.getString(desiredCondition, "status"))
                                && Objects.nonNull(JsonFields.getString(existingCondition, "lastTransitionTime"))) {
                            desiredCondition.addProperty("lastTransitionTime",
                                    JsonFields.getString(existingCondition, "lastTransitionTime"));
                            break;
                        }
                    }
                }
            }
        }
    }

    /**
     * Carry over status.parents entries owned by other controllers into the desired
     * parents: the merge patch replaces the array wholesale, so foreign entries must be
     * part of the patch body to survive it.
     */
    private void retainForeignParentEntries(final JsonArray existingParents, final JsonArray desiredParents) {
        if (Objects.isNull(existingParents)) {
            return;
        }
        for (JsonElement element : existingParents) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject parent = element.getAsJsonObject();
            if (!GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(JsonFields.getString(parent, "controllerName"))) {
                desiredParents.add(element);
            }
        }
    }

    /**
     * Remove ShenYu's status.parents entries (used when the route is no longer attached to
     * any ShenYu Gateway), preserving entries owned by other controllers.
     */    private void removeShenyuParentStatus(final DynamicKubernetesObject httpRoute, final String namespace,
                                          final String routeName) {
        JsonObject status = JsonFields.getJsonObject(httpRoute.getRaw(), "status");
        JsonArray existingParents = JsonFields.getJsonArray(status, "parents");
        if (Objects.isNull(existingParents)) {
            return;
        }
        JsonArray kept = new JsonArray();
        boolean hasShenyuEntry = false;
        for (JsonElement element : existingParents) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject parent = element.getAsJsonObject();
            if (GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(JsonFields.getString(parent, "controllerName"))) {
                hasShenyuEntry = true;
            } else {
                kept.add(parent);
            }
        }
        if (!hasShenyuEntry) {
            return;
        }
        JsonObject newStatus = new JsonObject();
        newStatus.add("parents", kept);
        sendStatusPatch(httpRoute, namespace, routeName, newStatus);
    }

    private void sendStatusPatch(final DynamicKubernetesObject httpRoute, final String namespace,
                                 final String routeName, final JsonObject status) {
        JsonObject patch = new JsonObject();
        patch.addProperty("apiVersion", GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION);
        patch.addProperty("kind", GatewayApiConstants.HTTP_ROUTE_KIND);
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", namespace);
        metadata.addProperty("name", routeName);
        patch.add("metadata", metadata);
        patch.add("status", status);

        String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION
                + "/namespaces/" + namespace + "/httproutes/" + routeName + "/status";
        try {
            StatusMergePatch.patch(apiClient, path, patch);
            LOG.debug("Updated status of HTTPRoute {}/{}", namespace, routeName);
        } catch (ApiException e) {
            LOG.warn("Failed to update status of HTTPRoute {}/{}: {}", namespace, routeName, e.getMessage());
        }
    }

    /**
     * The verdict for one parentRef: acceptance with the effective hostnames and the
     * listener names that accepted the route, or rejection with the spec-defined reason
     * reported in status.
     */
    private static final class ParentDecision {

        private final JsonObject parentRef;

        private final String parentNamespace;

        private final String parentName;

        private final boolean accepted;

        private final String reason;

        private final String message;

        private final List<String> hostnames;

        private final Set<String> listenerNames;

        private ParentDecision(final JsonObject parentRef, final String parentNamespace, final String parentName,
                               final boolean accepted, final String reason, final String message,
                               final List<String> hostnames, final Set<String> listenerNames) {
            this.parentRef = parentRef;
            this.parentNamespace = parentNamespace;
            this.parentName = parentName;
            this.accepted = accepted;
            this.reason = reason;
            this.message = message;
            this.hostnames = hostnames;
            this.listenerNames = listenerNames;
        }

        static ParentDecision accepted(final JsonObject parentRef, final String parentNamespace, final String parentName,
                                       final List<String> hostnames, final Set<String> listenerNames) {
            return new ParentDecision(parentRef, parentNamespace, parentName, true, null, null, hostnames, listenerNames);
        }

        static ParentDecision rejected(final JsonObject parentRef, final String parentNamespace, final String parentName,
                                       final String reason, final String message) {
            return new ParentDecision(parentRef, parentNamespace, parentName, false, reason, message, List.of(), Set.of());
        }
    }
}
