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

package org.apache.shenyu.k8s.parser;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubset;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.JsonFields;
import org.apache.shenyu.k8s.common.ReferenceGrants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

public class HttpRouteParser {

    private static final Logger LOG = LoggerFactory.getLogger(HttpRouteParser.class);

    /** Prefix isolating Gateway API IDs from the numeric ID space of the Ingress reconciler. */
    private static final String ID_PREFIX = "gwapi-";

    /** Stable hostname slot for rules without a hostname, keeping deterministic IDs well-defined. */
    private static final String NO_HOSTNAME_PLACEHOLDER = "_";

    private final Lister<V1Endpoints> endpointsLister;

    private final Lister<DynamicKubernetesObject> referenceGrantLister;

    public HttpRouteParser(final Lister<V1Endpoints> endpointsLister,
                           final Lister<DynamicKubernetesObject> referenceGrantLister) {
        this.endpointsLister = endpointsLister;
        this.referenceGrantLister = referenceGrantLister;
    }

    public ShenyuMemoryConfig parse(final DynamicKubernetesObject httpRoute) {
        ShenyuMemoryConfig res = new ShenyuMemoryConfig();
        String namespace = Objects.requireNonNull(httpRoute.getMetadata()).getNamespace();
        String routeName = httpRoute.getMetadata().getName();
        List<IngressConfiguration> routeConfigList = new ArrayList<>();
        res.setRouteConfigList(routeConfigList);

        JsonObject raw = httpRoute.getRaw();
        JsonObject spec = raw.getAsJsonObject("spec");
        if (Objects.nonNull(spec)) {
            JsonArray hostnames = spec.getAsJsonArray("hostnames");
            JsonArray rules = spec.getAsJsonArray("rules");
            ResolveState resolveState = new ResolveState();
            for (int ruleIndex = 0; Objects.nonNull(rules) && ruleIndex < rules.size(); ruleIndex++) {
                processRule(rules.get(ruleIndex).getAsJsonObject(), hostnames, namespace, routeName, ruleIndex,
                        routeConfigList, resolveState);
            }
            res.setAllBackendsResolved(!resolveState.anyUnresolved);
            res.setUnresolvedReason(resolveState.reason);
        }

        List<String> selectorIds = new ArrayList<>();
        for (IngressConfiguration rc : routeConfigList) {
            selectorIds.add(rc.getSelectorData().getId());
        }
        GatewayRouteCache.getInstance().putRouteSelectors(namespace, routeName,
                PluginEnum.DIVIDE.getName(), selectorIds);
        return res;
    }

    private void processRule(final JsonObject rule, final JsonArray hostnames, final String namespace,
                             final String routeName, final int ruleIndex,
                             final List<IngressConfiguration> routeConfigList,
                             final ResolveState resolveState) {
        // A rule without backendRefs has no ShenYu equivalent; skipping it leaves matching
        // requests unmatched instead of programming an empty upstream list.
        JsonArray backendRefs = rule.getAsJsonArray("backendRefs");
        if (Objects.isNull(backendRefs) || backendRefs.isEmpty()) {
            return;
        }

        BackendResolveResult result = parseBackendRefs(backendRefs, namespace, routeName);
        List<DivideUpstream> upstreamList = result.upstreams;
        if (result.unresolvedCount > 0) {
            resolveState.anyUnresolved = true;
            if (Objects.isNull(resolveState.reason)) {
                resolveState.reason = result.unresolvedReason;
            }
            LOG.warn("HTTPRoute {}/{} rule {} has {} unresolved backendRef(s)",
                    namespace, routeName, ruleIndex, result.unresolvedCount);
        }

        // An empty (handle="[]") selector would make matching requests 5xx; no match is safer
        if (upstreamList.isEmpty()) {
            return;
        }

        // One selector+rule per hostname: a request matches at most one hostname, and the
        // selector's AND semantics cannot express "any of these hostnames".
        List<ConditionData> hostnameConditions = new ArrayList<>();
        if (Objects.nonNull(hostnames) && !hostnames.isEmpty()) {
            for (JsonElement hostname : hostnames) {
                hostnameConditions.add(buildHostnameCondition(hostname.getAsString()));
            }
        }

        JsonArray matches = rule.getAsJsonArray("matches");
        if (Objects.nonNull(matches) && !matches.isEmpty()) {
            for (int matchIndex = 0; matchIndex < matches.size(); matchIndex++) {
                JsonObject match = matches.get(matchIndex).getAsJsonObject();
                List<ConditionData> matchConditions = new ArrayList<>();
                appendMatchConditions(matchConditions, match);
                if (hostnameConditions.isEmpty()) {
                    addSelectorRule(routeConfigList, namespace, routeName, ruleIndex, null,
                            matchIndex, matchConditions, upstreamList);
                } else {
                    for (ConditionData hostCondition : hostnameConditions) {
                        addSelectorRule(routeConfigList, namespace, routeName, ruleIndex,
                                hostCondition.getParamValue(), matchIndex,
                                composeConditions(hostCondition, matchConditions), upstreamList);
                    }
                }
            }
        } else {
            if (hostnameConditions.isEmpty()) {
                addSelectorRule(routeConfigList, namespace, routeName, ruleIndex, null,
                        0, new ArrayList<>(), upstreamList);
            } else {
                for (ConditionData hostCondition : hostnameConditions) {
                    addSelectorRule(routeConfigList, namespace, routeName, ruleIndex,
                            hostCondition.getParamValue(), 0,
                            composeConditions(hostCondition, new ArrayList<>()), upstreamList);
                }
            }
        }
    }

    private void addSelectorRule(final List<IngressConfiguration> routeConfigList,
                                 final String namespace, final String routeName, final int ruleIndex,
                                 final String hostname, final int matchIndex,
                                 final List<ConditionData> conditions, final List<DivideUpstream> upstreamList) {
        String selectorId = deterministicSelectorId(namespace, routeName, ruleIndex, hostname, matchIndex);
        String ruleId = deterministicRuleId(selectorId, matchIndex);
        String hostComponent = Objects.isNull(hostname) ? "" : "-" + hostname;
        String selectorName = routeName + "-rule-" + ruleIndex + hostComponent + "-m" + matchIndex;
        SelectorData selectorData = buildSelectorData(selectorId, selectorName, conditions, upstreamList);
        RuleData ruleData = buildRuleData(ruleId, selectorId, selectorName, conditions);
        routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
    }

    private List<ConditionData> composeConditions(final ConditionData hostCondition,
                                                  final List<ConditionData> matchConditions) {
        List<ConditionData> conditions = new ArrayList<>();
        conditions.add(hostCondition);
        conditions.addAll(matchConditions);
        return conditions;
    }

    /**
     * Deterministic selector ID derived from the route coordinates, so the same spec always
     * yields the same ID and a resync upserts instead of delete-then-create on the data plane.
     */
    private String deterministicSelectorId(final String namespace, final String routeName, final int ruleIndex,
                                           final String hostname, final int matchIndex) {
        String hostComponent = Objects.isNull(hostname) ? NO_HOSTNAME_PLACEHOLDER : hostname;
        String key = namespace + "/" + routeName + "/r" + ruleIndex + "/h" + hostComponent + "/m" + matchIndex;
        return ID_PREFIX + UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8));
    }

    /** Derive a deterministic rule ID from its parent selector ID; stays under varchar(128). */
    private String deterministicRuleId(final String selectorId, final int matchIndex) {
        return selectorId + "/rule-m" + matchIndex;
    }

    /**
     * Exact hostnames use EQ. A wildcard ({@code *.example.com}) is a suffix match per the
     * Gateway API spec: it matches {@code test.example.com} and {@code foo.test.example.com}
     * but not {@code example.com} — impossible to express with EQ's exact comparison, hence REGEX.
     */
    private ConditionData buildHostnameCondition(final String hostname) {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.DOMAIN.getName());
        if (hostname.startsWith("*.")) {
            String suffix = hostname.substring(2).replace(".", "\\.");
            condition.setOperator(OperatorEnum.REGEX.getAlias());
            condition.setParamValue("^([^.]+\\.)+" + suffix + "$");
        } else {
            condition.setOperator(OperatorEnum.EQ.getAlias());
            condition.setParamValue(hostname);
        }
        return condition;
    }

    private SelectorData buildSelectorData(final String selectorId, final String selectorName,
                                           final List<ConditionData> conditions, final List<DivideUpstream> upstreamList) {
        return SelectorData.builder()
                .id(selectorId)
                .pluginId(String.valueOf(PluginEnum.DIVIDE.getCode()))
                .pluginName(PluginEnum.DIVIDE.getName())
                .name(selectorName)
                .sort(1)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .enabled(true)
                .logged(false)
                .continued(true)
                .conditionList(conditions)
                .handle(GsonUtils.getInstance().toJson(upstreamList))
                .build();
    }

    private RuleData buildRuleData(final String ruleId, final String selectorId,
                                   final String selectorName, final List<ConditionData> conditions) {
        DivideRuleHandle divideRuleHandle = new DivideRuleHandle();
        divideRuleHandle.setLoadBalance(LoadBalanceEnum.RANDOM.getName());
        divideRuleHandle.setRetry(3);
        divideRuleHandle.setTimeout(3000L);

        return RuleData.builder()
                .id(ruleId)
                .selectorId(selectorId)
                .name(selectorName)
                .pluginName(PluginEnum.DIVIDE.getName())
                .sort(1)
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(conditions)
                .handle(GsonUtils.getInstance().toJson(divideRuleHandle))
                .loged(false)
                .enabled(true)
                .build();
    }

    /**
     * Resolve the rule's backendRefs into upstream addresses. Service is the only supported
     * kind (the default when absent); anything else is unresolved with reason InvalidKind.
     * A cross-namespace Service requires a ReferenceGrant in that namespace; a Service whose
     * Endpoints are missing or have no ready addresses is unresolved with reason
     * BackendNotFound. Both drive ResolvedRefs=False.
     */
    private BackendResolveResult parseBackendRefs(final JsonArray backendRefs, final String namespace,
                                                  final String routeName) {
        List<DivideUpstream> upstreamList = new ArrayList<>();
        int unresolvedCount = 0;
        String unresolvedReason = null;
        for (JsonElement element : backendRefs) {
            JsonObject backendRef = element.getAsJsonObject();
            String serviceName = JsonFields.getString(backendRef, "name");
            String backendNamespace = JsonFields.getString(backendRef, "namespace");
            if (Objects.isNull(backendNamespace)) {
                backendNamespace = namespace;
            }
            // Gateway API spec: an omitted weight defaults to 1, so it mixes 1:1 with an explicit one
            int weight = backendRef.has("weight") ? backendRef.get("weight").getAsInt() : 1;
            Integer port = backendRef.has("port") ? backendRef.get("port").getAsInt() : null;

            if (Objects.isNull(serviceName)) {
                continue;
            }

            if (!GatewayApiConstants.isServiceRef(backendRef)) {
                LOG.warn("HTTPRoute {}/{} backendRef to kind '{}' is not supported, only Service",
                        namespace, routeName, JsonFields.getString(backendRef, "kind"));
                unresolvedCount++;
                if (Objects.isNull(unresolvedReason)) {
                    unresolvedReason = GatewayApiConstants.REASON_INVALID_KIND;
                }
                continue;
            }

            if (!backendNamespace.equals(namespace)
                    && !ReferenceGrants.isGranted(referenceGrantLister, backendNamespace, namespace,
                    GatewayApiConstants.CORE_API_GROUP, GatewayApiConstants.SERVICE_KIND, serviceName)) {
                LOG.warn("HTTPRoute {}/{} backendRef to Service {}/{} is not permitted by a ReferenceGrant",
                        namespace, routeName, backendNamespace, serviceName);
                unresolvedCount++;
                if (Objects.isNull(unresolvedReason)) {
                    unresolvedReason = GatewayApiConstants.REASON_REF_NOT_PERMITTED;
                }
                continue;
            }

            V1Endpoints v1Endpoints = endpointsLister.namespace(backendNamespace).get(serviceName);
            if (Objects.isNull(v1Endpoints) || CollectionUtils.isEmpty(v1Endpoints.getSubsets())) {
                LOG.warn("Cannot find endpoints for service {}/{}", backendNamespace, serviceName);
                unresolvedCount++;
                if (Objects.isNull(unresolvedReason)) {
                    unresolvedReason = GatewayApiConstants.REASON_BACKEND_NOT_FOUND;
                }
                continue;
            }

            int before = upstreamList.size();
            for (V1EndpointSubset subset : v1Endpoints.getSubsets()) {
                if (CollectionUtils.isEmpty(subset.getAddresses())) {
                    continue;
                }
                for (V1EndpointAddress address : subset.getAddresses()) {
                    String ip = address.getIp();
                    if (Objects.nonNull(ip)) {
                        DivideUpstream upstream = new DivideUpstream();
                        upstream.setUpstreamUrl(Objects.nonNull(port) ? ip + ":" + port : ip);
                        upstream.setWeight(weight);
                        upstream.setProtocol("http://");
                        upstream.setWarmup(0);
                        upstream.setStatus(true);
                        upstream.setUpstreamHost("");
                        upstreamList.add(upstream);
                    }
                }
            }
            // Endpoints existed but yielded no ready address → treat as unresolved
            if (upstreamList.size() == before) {
                unresolvedCount++;
                if (Objects.isNull(unresolvedReason)) {
                    unresolvedReason = GatewayApiConstants.REASON_BACKEND_NOT_FOUND;
                }
            }
        }
        return new BackendResolveResult(upstreamList, unresolvedCount, unresolvedReason);
    }

    private void appendMatchConditions(final List<ConditionData> conditions, final JsonObject match) {
        JsonObject path = match.getAsJsonObject("path");
        if (Objects.nonNull(path) && path.has("value")) {
            ConditionData pathCondition = new ConditionData();
            pathCondition.setParamType(ParamTypeEnum.URI.getName());
            pathCondition.setOperator(mapPathType(JsonFields.getString(path, "type")));
            pathCondition.setParamValue(path.get("value").getAsString());
            conditions.add(pathCondition);
        }

        String method = JsonFields.getString(match, "method");
        if (Objects.nonNull(method)) {
            ConditionData methodCondition = new ConditionData();
            methodCondition.setParamType(ParamTypeEnum.REQUEST_METHOD.getName());
            methodCondition.setOperator(OperatorEnum.EQ.getAlias());
            methodCondition.setParamValue(method);
            conditions.add(methodCondition);
        }

        JsonArray headers = match.getAsJsonArray("headers");
        if (Objects.nonNull(headers)) {
            for (JsonElement headerElement : headers) {
                JsonObject header = headerElement.getAsJsonObject();
                ConditionData headerCondition = new ConditionData();
                headerCondition.setParamType(ParamTypeEnum.HEADER.getName());
                headerCondition.setOperator(exactOrRegex(JsonFields.getString(header, "type")));
                headerCondition.setParamName(JsonFields.getString(header, "name"));
                headerCondition.setParamValue(JsonFields.getString(header, "value"));
                conditions.add(headerCondition);
            }
        }

        JsonArray queryParams = match.getAsJsonArray("queryParams");
        if (Objects.nonNull(queryParams)) {
            for (JsonElement queryElement : queryParams) {
                JsonObject queryParam = queryElement.getAsJsonObject();
                ConditionData queryCondition = new ConditionData();
                queryCondition.setParamType(ParamTypeEnum.QUERY.getName());
                queryCondition.setOperator(exactOrRegex(JsonFields.getString(queryParam, "type")));
                queryCondition.setParamName(JsonFields.getString(queryParam, "name"));
                queryCondition.setParamValue(JsonFields.getString(queryParam, "value"));
                conditions.add(queryCondition);
            }
        }
    }

    private String mapPathType(final String pathType) {
        if ("Exact".equals(pathType)) {
            return OperatorEnum.EQ.getAlias();
        }
        if ("PathPrefix".equals(pathType)) {
            return OperatorEnum.STARTS_WITH.getAlias();
        }
        if ("RegularExpression".equals(pathType)) {
            return OperatorEnum.REGEX.getAlias();
        }
        return OperatorEnum.STARTS_WITH.getAlias();
    }

    /**
     * Header and query match types are Exact or RegularExpression; the spec defaults an
     * absent type to Exact. Regex must map to the REGEX operator: the MATCH judge compares
     * by substring containment (Ant path patterns for URI), which never implements regex
     * semantics.
     */
    private String exactOrRegex(final String matchType) {
        return "RegularExpression".equals(matchType) ? OperatorEnum.REGEX.getAlias() : OperatorEnum.EQ.getAlias();
    }

    /**
     * Result of resolving a rule's backendRefs: the reachable upstreams, how many
     * backendRefs failed, and the Gateway API reason (BackendNotFound, RefNotPermitted
     * or InvalidKind) of the first failure. A non-zero {@code unresolvedCount} means
     * the reconciler should report ResolvedRefs=False.
     */
    private static final class BackendResolveResult {

        private final List<DivideUpstream> upstreams;

        private final int unresolvedCount;

        private final String unresolvedReason;

        BackendResolveResult(final List<DivideUpstream> upstreams, final int unresolvedCount,
                             final String unresolvedReason) {
            this.upstreams = upstreams;
            this.unresolvedCount = unresolvedCount;
            this.unresolvedReason = unresolvedReason;
        }
    }

    /** Backend resolution failures across rules; the first reason becomes the route-level one. */
    private static final class ResolveState {

        private boolean anyUnresolved;

        private String reason;
    }
}
