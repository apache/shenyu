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
import io.kubernetes.client.custom.IntOrString;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.models.CoreV1EndpointPort;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubset;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1ServicePort;
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
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.JsonFields;
import org.apache.shenyu.k8s.common.ReferenceGrants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Pattern;

/**
 * Parses an HTTPRoute into ShenYu divide selectors/rules; pure, no cache or data-plane
 * side effects. Match precedence follows the spec (hostname, path, method presence, header
 * count, query count, first rule) and is encoded in the selector sort; because the data
 * plane groups matches by condition count BEFORE comparing sort
 * ({@code AbstractShenyuPlugin#manyMatchSelector}), every selector's condition list is
 * padded to a fixed length with always-true duplicates so the sort alone decides. The
 * traffic share of invalid weighted backendRefs goes to a loopback fail-target upstream:
 * the connection is refused instantly, so the request fails with a 500 instead of
 * re-flowing onto healthy backends.
 */
public class HttpRouteParser {

    private static final Logger LOG = LoggerFactory.getLogger(HttpRouteParser.class);

    private static final String ID_PREFIX = "gwapi-";

    private static final String NO_HOSTNAME_PLACEHOLDER = "_";

    /** Base of the packed precedence sort (hostname 3b, path 11b, method 1b, headers 4b, query 4b, rule index 4b); lower sort wins. */
    private static final int SORT_PRECEDENCE_BASE = 1 << 28;

    /** Condition-list length selectors are padded to, defeating the data plane's count-first tie-breaking. */
    private static final int CONDITION_COUNT_FLOOR = 16;

    /** Loopback port 1 has no listener in the pod: connections are refused instantly, failing the invalid share with a 500. */
    private static final String FAIL_TARGET_URL = "127.0.0.1:1";

    private final Lister<V1Endpoints> endpointsLister;

    private final Lister<V1Service> serviceLister;

    private final Lister<DynamicKubernetesObject> referenceGrantLister;

    public HttpRouteParser(final Lister<V1Endpoints> endpointsLister,
                           final Lister<V1Service> serviceLister,
                           final Lister<DynamicKubernetesObject> referenceGrantLister) {
        this.endpointsLister = endpointsLister;
        this.serviceLister = serviceLister;
        this.referenceGrantLister = referenceGrantLister;
    }

    /**
     * Parse the HTTPRoute into a ShenYu config snapshot.
     * @param hostnames effective hostnames (route × listener intersection); empty means "any host"
     * @return the parsed config, never null
     */
    public ShenyuMemoryConfig parse(final DynamicKubernetesObject httpRoute, final List<String> hostnames) {
        ShenyuMemoryConfig res = new ShenyuMemoryConfig();
        String namespace = Objects.requireNonNull(httpRoute.getMetadata()).getNamespace();
        String routeName = httpRoute.getMetadata().getName();
        List<IngressConfiguration> routeConfigList = new ArrayList<>();
        res.setRouteConfigList(routeConfigList);

        JsonObject spec = JsonFields.getJsonObject(httpRoute.getRaw(), "spec");
        if (Objects.nonNull(spec)) {
            JsonArray rules = JsonFields.getJsonArray(spec, "rules");
            ResolveState resolveState = new ResolveState();
            for (int ruleIndex = 0; Objects.nonNull(rules) && ruleIndex < rules.size(); ruleIndex++) {
                if (rules.get(ruleIndex).isJsonObject()) {
                    processRule(rules.get(ruleIndex).getAsJsonObject(), hostnames, namespace, routeName, ruleIndex,
                            routeConfigList, resolveState);
                }
            }
            res.setAllBackendsResolved(!resolveState.anyUnresolved);
            res.setUnresolvedReason(resolveState.reason);
            res.setHasUnsupportedFilters(resolveState.unsupportedFilters);
        }
        return res;
    }

    private void processRule(final JsonObject rule, final List<String> hostnames, final String namespace,
                             final String routeName, final int ruleIndex,
                             final List<IngressConfiguration> routeConfigList,
                             final ResolveState resolveState) {
        // Unsupported filters must surface as Accepted=False/UnsupportedValue, never apply partially.
        JsonArray filters = JsonFields.getJsonArray(rule, "filters");
        if (Objects.nonNull(filters) && !filters.isEmpty()) {
            resolveState.unsupportedFilters = true;
            LOG.warn("HTTPRoute {}/{} rule {} declares filters which are not supported; the rule is not programmed",
                    namespace, routeName, ruleIndex);
            return;
        }

        // Without backendRefs there is no ShenYu equivalent; leave requests unmatched.
        JsonArray backendRefs = JsonFields.getJsonArray(rule, "backendRefs");
        if (Objects.isNull(backendRefs) || backendRefs.isEmpty()) {
            return;
        }

        BackendResolveResult result = parseBackendRefs(backendRefs, namespace, routeName);
        if (result.unresolvedCount > 0) {
            resolveState.anyUnresolved = true;
            if (Objects.isNull(resolveState.reason)) {
                resolveState.reason = result.unresolvedReason;
            }
            LOG.warn("HTTPRoute {}/{} rule {} has {} unresolved backendRef(s)",
                    namespace, routeName, ruleIndex, result.unresolvedCount);
        }

        // All backends valid with weight 0: removed from rotation, nothing to program.
        if (result.upstreams.isEmpty()) {
            return;
        }
        emitRuleSelectors(rule, hostnames, namespace, routeName, ruleIndex, routeConfigList, result.upstreams);
    }

    /**
     * Fan the rule out into selectors/rules: one selector per (hostname, match) pair, all
     * sharing the rule's upstream list.
     */
    private void emitRuleSelectors(final JsonObject rule, final List<String> hostnames, final String namespace,
                                   final String routeName, final int ruleIndex,
                                   final List<IngressConfiguration> routeConfigList,
                                   final List<DivideUpstream> upstreamList) {
        // One selector per hostname: AND semantics cannot express "any of these hostnames".
        JsonArray matches = JsonFields.getJsonArray(rule, "matches");
        if (Objects.nonNull(matches) && !matches.isEmpty()) {
            for (int matchIndex = 0; matchIndex < matches.size(); matchIndex++) {
                if (!matches.get(matchIndex).isJsonObject()) {
                    continue;
                }
                JsonObject match = matches.get(matchIndex).getAsJsonObject();
                List<ConditionData> matchConditions = new ArrayList<>();
                appendMatchConditions(matchConditions, match);
                if (hostnames.isEmpty()) {
                    addSelectorRule(routeConfigList, namespace, routeName, ruleIndex, null,
                            matchIndex, selectorSort(null, match, ruleIndex), matchConditions, upstreamList);
                } else {
                    for (String hostname : hostnames) {
                        addSelectorRule(routeConfigList, namespace, routeName, ruleIndex,
                                hostname, matchIndex, selectorSort(hostname, match, ruleIndex),
                                composeConditions(hostname, matchConditions), upstreamList);
                    }
                }
            }
        } else {
            // Spec: a rule without matches matches everything, like PathPrefix /
            JsonObject noMatch = new JsonObject();
            if (hostnames.isEmpty()) {
                addSelectorRule(routeConfigList, namespace, routeName, ruleIndex, null,
                        0, selectorSort(null, noMatch, ruleIndex), new ArrayList<>(), upstreamList);
            } else {
                for (String hostname : hostnames) {
                    addSelectorRule(routeConfigList, namespace, routeName, ruleIndex,
                            hostname, 0, selectorSort(hostname, noMatch, ruleIndex),
                            composeConditions(hostname, new ArrayList<>()), upstreamList);
                }
            }
        }
    }

    private void addSelectorRule(final List<IngressConfiguration> routeConfigList,
                                 final String namespace, final String routeName, final int ruleIndex,
                                 final String hostname, final int matchIndex, final int sort,
                                 final List<ConditionData> conditions, final List<DivideUpstream> upstreamList) {
        // A CUSTOM_FLOW selector with no conditions never matches; match-all fills the gap.
        if (conditions.isEmpty()) {
            conditions.add(matchAllCondition());
        }
        String selectorId = deterministicSelectorId(namespace, routeName, ruleIndex, hostname, matchIndex);
        String ruleId = deterministicRuleId(selectorId, matchIndex);
        String hostComponent = Objects.isNull(hostname) ? "" : "-" + hostname;
        String selectorName = routeName + "-rule-" + ruleIndex + hostComponent + "-m" + matchIndex;
        // Only the selector is padded; the rule's trie cache would just bloat with duplicates.
        SelectorData selectorData = buildSelectorData(selectorId, selectorName, sort,
                padToConditionFloor(conditions), upstreamList);
        RuleData ruleData = buildRuleData(ruleId, selectorId, selectorName, conditions);
        routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
    }

    /** Pad to the fixed condition count so the precedence-encoded sort alone decides (see class javadoc). */
    private List<ConditionData> padToConditionFloor(final List<ConditionData> conditions) {
        if (conditions.size() >= CONDITION_COUNT_FLOOR) {
            return conditions;
        }
        List<ConditionData> padded = new ArrayList<>(conditions);
        while (padded.size() < CONDITION_COUNT_FLOOR) {
            padded.add(matchAllCondition());
        }
        return padded;
    }

    /** Every request path starts with '/', so this condition matches all requests. */
    private ConditionData matchAllCondition() {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.URI.getName());
        condition.setOperator(OperatorEnum.STARTS_WITH.getAlias());
        condition.setParamValue("/");
        return condition;
    }

    private List<ConditionData> composeConditions(final String hostname,
                                                  final List<ConditionData> matchConditions) {
        List<ConditionData> conditions = new ArrayList<>();
        conditions.add(buildHostnameCondition(hostname));
        conditions.addAll(matchConditions);
        return conditions;
    }

    /** Deterministic ID: a resync upserts instead of delete-then-create on the data plane. */
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

    /** Exact hostnames use EQ; a wildcard is a multi-label suffix match, expressible only as REGEX. */
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

    private SelectorData buildSelectorData(final String selectorId, final String selectorName, final int sort,
                                           final List<ConditionData> conditions, final List<DivideUpstream> upstreamList) {
        return SelectorData.builder()
                .id(selectorId)
                .pluginId(String.valueOf(PluginEnum.DIVIDE.getCode()))
                .pluginName(PluginEnum.DIVIDE.getName())
                .name(selectorName)
                .sort(sort)
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

    /** Resolve backendRefs to upstream addresses; non-Service kinds, unauthorized cross-namespace refs and missing Endpoints stay unresolved (ResolvedRefs=False). */
    private BackendResolveResult parseBackendRefs(final JsonArray backendRefs, final String namespace,
                                                  final String routeName) {
        List<ResolvedBackend> backends = new ArrayList<>();
        int unresolvedCount = 0;
        int invalidWeightedShare = 0;
        String unresolvedReason = null;
        for (JsonElement element : backendRefs) {
            if (!element.isJsonObject()) {
                continue;
            }
            BackendRefOutcome outcome = resolveBackendRef(element.getAsJsonObject(), namespace, routeName);
            if (Objects.nonNull(outcome.unresolvedReason)) {
                unresolvedCount++;
                invalidWeightedShare += Math.max(0, outcome.declaredWeight);
                if (Objects.isNull(unresolvedReason)) {
                    unresolvedReason = outcome.unresolvedReason;
                }
                continue;
            }
            backends.add(new ResolvedBackend(outcome.declaredWeight, outcome.urls));
        }
        // One fail-target entry with exactly the invalid share keeps the split proportional.
        if (invalidWeightedShare > 0) {
            backends.add(new ResolvedBackend(invalidWeightedShare, List.of(FAIL_TARGET_URL)));
        }
        return new BackendResolveResult(buildUpstreams(backends), unresolvedCount, unresolvedReason);
    }

    /** Spread declared weights over endpoints with one common scale factor, so aggregate weights stay proportional regardless of replica counts. */
    private List<DivideUpstream> buildUpstreams(final List<ResolvedBackend> backends) {
        long scale = 1;
        for (ResolvedBackend backend : backends) {
            if (!backend.urls.isEmpty()) {
                scale = Math.max(scale, divideRoundingUp(backend.urls.size(), backend.declaredWeight));
            }
        }
        List<DivideUpstream> upstreams = new ArrayList<>();
        for (ResolvedBackend backend : backends) {
            if (backend.urls.isEmpty()) {
                continue;
            }
            int perEndpointWeight = Math.max(1, (int) Math.min(Integer.MAX_VALUE,
                    Math.round(backend.declaredWeight * (double) scale / backend.urls.size())));
            for (String url : backend.urls) {
                DivideUpstream upstream = new DivideUpstream();
                upstream.setUpstreamUrl(url);
                upstream.setWeight(perEndpointWeight);
                upstream.setProtocol("http://");
                upstream.setWarmup(0);
                upstream.setStatus(true);
                upstream.setUpstreamHost("");
                // Constant timestamp keeps the handle json byte-identical for the unchanged-check.
                upstream.setTimestamp(0L);
                upstreams.add(upstream);
            }
        }
        return upstreams;
    }

    private long divideRoundingUp(final long dividend, final long divisor) {
        return (dividend + divisor - 1) / divisor;
    }

    /** Resolve one backendRef into upstream URLs or the Gateway API reason of its failure. */
    private BackendRefOutcome resolveBackendRef(final JsonObject backendRef, final String namespace,
                                                final String routeName) {
        String serviceName = JsonFields.getString(backendRef, "name");
        if (Objects.isNull(serviceName)) {
            return BackendRefOutcome.ok(List.of(), 0);
        }
        // An omitted weight defaults to 1
        int weight = backendRef.has("weight") && backendRef.get("weight").isJsonPrimitive()
                ? backendRef.get("weight").getAsInt() : 1;
        String backendNamespace = JsonFields.getString(backendRef, "namespace");
        if (Objects.isNull(backendNamespace)) {
            backendNamespace = namespace;
        }
        if (!GatewayApiConstants.isServiceRef(backendRef)) {
            LOG.warn("HTTPRoute {}/{} backendRef to group '{}' kind '{}' is not supported, only core Service",
                    namespace, routeName, JsonFields.getString(backendRef, "group"),
                    JsonFields.getString(backendRef, "kind"));
            return BackendRefOutcome.unresolved(GatewayApiConstants.REASON_INVALID_KIND, weight);
        }
        if (!backendNamespace.equals(namespace)
                && !ReferenceGrants.isGranted(referenceGrantLister, backendNamespace, namespace,
                GatewayApiConstants.CORE_API_GROUP, GatewayApiConstants.SERVICE_KIND, serviceName)) {
            LOG.warn("HTTPRoute {}/{} backendRef to Service {}/{} is not permitted by a ReferenceGrant",
                    namespace, routeName, backendNamespace, serviceName);
            return BackendRefOutcome.unresolved(GatewayApiConstants.REASON_REF_NOT_PERMITTED, weight);
        }
        V1Endpoints v1Endpoints = endpointsLister.namespace(backendNamespace).get(serviceName);
        if (Objects.isNull(v1Endpoints) || CollectionUtils.isEmpty(v1Endpoints.getSubsets())) {
            LOG.warn("Cannot find endpoints for service {}/{}", backendNamespace, serviceName);
            return BackendRefOutcome.unresolved(GatewayApiConstants.REASON_BACKEND_NOT_FOUND, weight);
        }
        List<String> readyIps = new ArrayList<>();
        Set<Long> endpointPorts = new LinkedHashSet<>();
        Map<String, Long> endpointPortsByName = new HashMap<>();
        for (V1EndpointSubset subset : v1Endpoints.getSubsets()) {
            if (Objects.nonNull(subset.getPorts())) {
                for (CoreV1EndpointPort endpointPort : subset.getPorts()) {
                    if (Objects.nonNull(endpointPort.getPort())) {
                        endpointPorts.add(endpointPort.getPort().longValue());
                        if (Objects.nonNull(endpointPort.getName())) {
                            endpointPortsByName.putIfAbsent(endpointPort.getName(), endpointPort.getPort().longValue());
                        }
                    }
                }
            }
            if (CollectionUtils.isEmpty(subset.getAddresses())) {
                continue;
            }
            for (V1EndpointAddress address : subset.getAddresses()) {
                if (Objects.nonNull(address.getIp())) {
                    readyIps.add(address.getIp());
                }
            }
        }
        // Endpoints existed but yielded no ready address → treat as unresolved
        if (readyIps.isEmpty()) {
            return BackendRefOutcome.unresolved(GatewayApiConstants.REASON_BACKEND_NOT_FOUND, weight);
        }
        V1Service service = serviceLister.namespace(backendNamespace).get(serviceName);
        Long targetPort = resolveTargetPort(service, endpointPorts, endpointPortsByName,
                JsonFields.getLong(backendRef, "port"), namespace, routeName, backendNamespace, serviceName);
        if (Objects.isNull(targetPort)) {
            return BackendRefOutcome.unresolved(GatewayApiConstants.REASON_BACKEND_NOT_FOUND, weight);
        }
        // Spec: weight 0 removes the backend from rotation.
        if (weight == 0) {
            return BackendRefOutcome.ok(List.of(), 0);
        }
        List<String> urls = new ArrayList<>();
        for (String ip : readyIps) {
            urls.add(ip + ":" + targetPort);
        }
        return BackendRefOutcome.ok(urls, weight);
    }

    /**
     * Map the backendRef port to the pod port via the Service spec (targetPort numeric or
     * named, resolved against Endpoints); without a cached Service, fall back to a
     * single-endpoint-port heuristic and report ambiguity as BackendNotFound.
     */
    private Long resolveTargetPort(final V1Service service, final Set<Long> endpointPorts,
                                   final Map<String, Long> endpointPortsByName, final Long servicePort,
                                   final String namespace, final String routeName,
                                   final String backendNamespace, final String serviceName) {
        List<V1ServicePort> servicePorts = Objects.isNull(service) || Objects.isNull(service.getSpec())
                || Objects.isNull(service.getSpec().getPorts()) ? List.of() : service.getSpec().getPorts();
        if (!servicePorts.isEmpty()) {
            V1ServicePort selected = selectServicePort(servicePorts, servicePort);
            if (Objects.isNull(selected)) {
                LOG.warn("HTTPRoute {}/{} backendRef to Service {}/{}: no service port matches {}",
                        namespace, routeName, backendNamespace, serviceName,
                        Objects.isNull(servicePort) ? "the multiple ports of the service" : servicePort);
                return null;
            }
            IntOrString targetPort = selected.getTargetPort();
            if (Objects.nonNull(targetPort) && targetPort.isInteger()) {
                return targetPort.getIntValue().longValue();
            }
            if (Objects.nonNull(targetPort)) {
                Long resolved = endpointPortsByName.get(targetPort.getStrValue());
                if (Objects.nonNull(resolved)) {
                    return resolved;
                }
                LOG.warn("HTTPRoute {}/{} backendRef to Service {}/{}: named targetPort '{}' not found in endpoints",
                        namespace, routeName, backendNamespace, serviceName, targetPort.getStrValue());
                return null;
            }
            return Objects.isNull(selected.getPort()) ? null : selected.getPort().longValue();
        }
        if (endpointPorts.size() == 1) {
            return endpointPorts.iterator().next();
        }
        if (Objects.nonNull(servicePort) && (endpointPorts.isEmpty() || endpointPorts.contains(servicePort))) {
            return servicePort;
        }
        LOG.warn("HTTPRoute {}/{} backendRef to Service {}/{}: cannot map service port {} to an endpoint port {}",
                namespace, routeName, backendNamespace, serviceName, servicePort, endpointPorts);
        return null;
    }

    /** The Service port entry a backendRef port selects; required unless the Service has exactly one port. */
    private V1ServicePort selectServicePort(final List<V1ServicePort> servicePorts, final Long servicePort) {
        if (Objects.nonNull(servicePort)) {
            for (V1ServicePort port : servicePorts) {
                if (Objects.nonNull(port.getPort()) && port.getPort().longValue() == servicePort) {
                    return port;
                }
            }
            return null;
        }
        return servicePorts.size() == 1 ? servicePorts.get(0) : null;
    }

    private void appendMatchConditions(final List<ConditionData> conditions, final JsonObject match) {
        JsonObject path = JsonFields.getJsonObject(match, "path");
        String pathValue = JsonFields.getString(path, "value");
        if (Objects.nonNull(pathValue)) {
            ConditionData pathCondition = new ConditionData();
            pathCondition.setParamType(ParamTypeEnum.URI.getName());
            String pathType = JsonFields.getString(path, "type");
            if (Objects.isNull(pathType) || "PathPrefix".equals(pathType)) {
                // Prefixes match on element boundaries (/foo ≠ /foobar); raw startsWith cannot, hence regex.
                pathCondition.setOperator(OperatorEnum.REGEX.getAlias());
                pathCondition.setParamValue(prefixRegex(pathValue));
            } else {
                pathCondition.setOperator(mapPathType(pathType));
                pathCondition.setParamValue(pathValue);
            }
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

        JsonArray headers = JsonFields.getJsonArray(match, "headers");
        if (Objects.nonNull(headers)) {
            for (JsonElement headerElement : headers) {
                if (!headerElement.isJsonObject()) {
                    continue;
                }
                JsonObject header = headerElement.getAsJsonObject();
                ConditionData headerCondition = new ConditionData();
                headerCondition.setParamType(ParamTypeEnum.HEADER.getName());
                headerCondition.setOperator(exactOrRegex(JsonFields.getString(header, "type")));
                headerCondition.setParamName(JsonFields.getString(header, "name"));
                headerCondition.setParamValue(JsonFields.getString(header, "value"));
                conditions.add(headerCondition);
            }
        }

        JsonArray queryParams = JsonFields.getJsonArray(match, "queryParams");
        if (Objects.nonNull(queryParams)) {
            for (JsonElement queryElement : queryParams) {
                if (!queryElement.isJsonObject()) {
                    continue;
                }
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

    /** Anchored full-match regex for a prefix: {@code /foo} → {@code ^\Q/foo\E(/.*)?$}; root {@code /} is the catch-all. */
    private String prefixRegex(final String prefix) {
        String stripped = prefix.length() > 1 && prefix.endsWith("/") ? prefix.substring(0, prefix.length() - 1) : prefix;
        if ("/".equals(stripped)) {
            return "^/.*$";
        }
        return "^" + Pattern.quote(stripped) + "(/.*)?$";
    }

    /** Full spec precedence packed into the sort, rule index as the final tie-break; lower sort wins. */
    private int selectorSort(final String hostname, final JsonObject match, final int ruleIndex) {
        int score = hostnameScore(hostname) << 24
                | pathScore(match) << 13
                | (Objects.isNull(JsonFields.getString(match, "method")) ? 0 : 1) << 12
                | matchCount(match, "headers") << 8
                | matchCount(match, "queryParams") << 4
                | 15 - Math.min(ruleIndex, 15);
        return SORT_PRECEDENCE_BASE - score;
    }

    /** Exact beats wildcard, more wildcard suffix labels beat fewer; no hostname sorts below both. */
    private int hostnameScore(final String hostname) {
        if (Objects.isNull(hostname)) {
            return 0;
        }
        if (hostname.startsWith("*.")) {
            return Math.min(hostname.substring(2).split("\\.").length, 6);
        }
        return 7;
    }

    /** Exact beats prefix, longer prefix beats shorter; the 11-bit field covers the CRD's 1024-char path maximum. */
    private int pathScore(final JsonObject match) {
        JsonObject path = JsonFields.getJsonObject(match, "path");
        String pathValue = JsonFields.getString(path, "value");
        if (Objects.isNull(pathValue)) {
            return 0;
        }
        String pathType = JsonFields.getString(path, "type");
        if ("Exact".equals(pathType)) {
            return 2047;
        }
        if ("RegularExpression".equals(pathType)) {
            return 1;
        }
        return 2 + Math.min(pathValue.length() - 1, 2044);
    }

    /** Number of header/query matches (4 bits each, capped), higher is more specific. */
    private int matchCount(final JsonObject match, final String field) {
        JsonArray array = JsonFields.getJsonArray(match, field);
        return Objects.isNull(array) ? 0 : Math.min(array.size(), 15);
    }

    private String mapPathType(final String pathType) {
        if ("Exact".equals(pathType)) {
            return OperatorEnum.EQ.getAlias();
        }
        if ("RegularExpression".equals(pathType)) {
            return OperatorEnum.REGEX.getAlias();
        }
        return OperatorEnum.STARTS_WITH.getAlias();
    }

    /** Regex must map to REGEX: the MATCH judge compares by substring containment, not regex semantics. */
    private String exactOrRegex(final String matchType) {
        return "RegularExpression".equals(matchType) ? OperatorEnum.REGEX.getAlias() : OperatorEnum.EQ.getAlias();
    }

    /** One backendRef outcome: URLs with declared weight, or the failure reason. */
    private static final class BackendRefOutcome {

        private final List<String> urls;

        private final int declaredWeight;

        private final String unresolvedReason;

        private BackendRefOutcome(final List<String> urls, final int declaredWeight, final String unresolvedReason) {
            this.urls = urls;
            this.declaredWeight = declaredWeight;
            this.unresolvedReason = unresolvedReason;
        }

        static BackendRefOutcome ok(final List<String> urls, final int declaredWeight) {
            return new BackendRefOutcome(urls, declaredWeight, null);
        }

        static BackendRefOutcome unresolved(final String reason, final int declaredWeight) {
            return new BackendRefOutcome(List.of(), declaredWeight, reason);
        }
    }

    /** One resolved backendRef: its declared weight and the pod addresses it fans out to. */
    private static final class ResolvedBackend {

        private final int declaredWeight;

        private final List<String> urls;

        ResolvedBackend(final int declaredWeight, final List<String> urls) {
            this.declaredWeight = declaredWeight;
            this.urls = urls;
        }
    }

    /** Rule-level result; non-zero unresolvedCount means the reconciler reports ResolvedRefs=False. */
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

        private boolean unsupportedFilters;
    }
}
