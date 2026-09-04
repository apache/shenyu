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
 * Parses an HTTPRoute into ShenYu divide selectors/rules. The parser is pure: it neither
 * touches GatewayRouteCache nor the data plane, so a reconcile can compute status from a
 * parse result without side effects.
 *
 * <p>Match precedence follows the Gateway API spec: hostname specificity, then path
 * specificity (exact beats longer prefix beats shorter prefix beats regex beats no path),
 * then HTTP method presence, then header match count, then query param match count. ShenYu's
 * data plane first groups matching selectors by their number of AND conditions and only then
 * compares the selector sort ({@code AbstractShenyuPlugin#manyMatchSelector}), which would
 * let a raw two-header match outrank a method match regardless of the sort. The parser
 * therefore pads every selector's condition list to one fixed length with always-true
 * duplicate conditions, turning the count grouping into a tie so the sort — which encodes
 * the full precedence above — decides. A single match declaring more header/query matches
 * than the floor accommodates exceeds it and falls back to count-dominant precedence.
 *
 * <p>The spec requires the traffic share of an invalid weighted backendRef to receive an
 * HTTP 500 while the valid shares stay routable. The invalid share is represented as a
 * dedicated fail-target upstream entry: the connection to it is refused immediately and —
 * under the default {@code current} retry strategy, which retries timeout-class errors only
 * — the request fails fast with a 500 through the global error handler instead of silently
 * re-flowing onto the healthy backends.
 */
public class HttpRouteParser {

    private static final Logger LOG = LoggerFactory.getLogger(HttpRouteParser.class);

    /** Prefix isolating Gateway API IDs from the numeric ID space of the Ingress reconciler. */
    private static final String ID_PREFIX = "gwapi-";

    /** Stable hostname slot for rules without a hostname, keeping deterministic IDs well-defined. */
    private static final String NO_HOSTNAME_PLACEHOLDER = "_";

    /**
     * Base of the packed precedence sort; lower sort wins on the ShenYu data plane, so a
     * higher precedence score subtracts to a lower sort. High enough to stay positive and
     * above the magnitude of hand-configured sorts.
     */
    private static final int SORT_PRECEDENCE_BASE = 1 << 20;

    /**
     * Fixed condition-list length every generated selector is padded to, neutralizing the
     * data plane's condition-count-first selector tie-breaking (see class javadoc). Chosen
     * to cover the natural condition count of any realistic match (hostname + path +
     * method + a dozen header/query matches).
     */
    private static final int CONDITION_COUNT_FLOOR = 16;

    /**
     * Stand-in upstream for the traffic share of invalid weighted backendRefs: loopback
     * port 1 has no listener in the gateway pod, so the connection is refused instantly
     * and the request fails with a 500 (see class javadoc) instead of re-flowing.
     */
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
     *
     * @param httpRoute the route object
     * @param hostnames effective hostnames (route hostnames intersected with the listener
     *                  hostnames of every accepting Gateway); empty means "any host"
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
        // Filters are not implemented. Per the spec an unsupported filter MUST surface as
        // Accepted=False/UnsupportedValue and the rule MUST NOT be applied partially.
        JsonArray filters = JsonFields.getJsonArray(rule, "filters");
        if (Objects.nonNull(filters) && !filters.isEmpty()) {
            resolveState.unsupportedFilters = true;
            LOG.warn("HTTPRoute {}/{} rule {} declares filters which are not supported; the rule is not programmed",
                    namespace, routeName, ruleIndex);
            return;
        }

        // A rule without backendRefs has no ShenYu equivalent; skipping it leaves matching
        // requests unmatched instead of programming an empty upstream list.
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

        // Empty means neither a valid weighted backend nor an invalid weighted share to
        // fail: all backends are valid with weight 0 (spec: removed from rotation).
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
        // One selector+rule per hostname: a request matches at most one hostname, and the
        // selector's AND semantics cannot express "any of these hostnames".
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
                            matchIndex, selectorSort(null, match), matchConditions, upstreamList);
                } else {
                    for (String hostname : hostnames) {
                        addSelectorRule(routeConfigList, namespace, routeName, ruleIndex,
                                hostname, matchIndex, selectorSort(hostname, match),
                                composeConditions(hostname, matchConditions), upstreamList);
                    }
                }
            }
        } else {
            // Spec: a rule without matches matches everything, like PathPrefix /
            JsonObject noMatch = new JsonObject();
            if (hostnames.isEmpty()) {
                addSelectorRule(routeConfigList, namespace, routeName, ruleIndex, null,
                        0, selectorSort(null, noMatch), new ArrayList<>(), upstreamList);
            } else {
                for (String hostname : hostnames) {
                    addSelectorRule(routeConfigList, namespace, routeName, ruleIndex,
                            hostname, 0, selectorSort(hostname, noMatch),
                            composeConditions(hostname, new ArrayList<>()), upstreamList);
                }
            }
        }
    }

    private void addSelectorRule(final List<IngressConfiguration> routeConfigList,
                                 final String namespace, final String routeName, final int ruleIndex,
                                 final String hostname, final int matchIndex, final int sort,
                                 final List<ConditionData> conditions, final List<DivideUpstream> upstreamList) {
        // A CUSTOM_FLOW selector with an empty condition list never matches in ShenYu, so a
        // rule without matches (spec: matches everything, like PathPrefix /) needs an
        // explicit match-all condition.
        if (conditions.isEmpty()) {
            conditions.add(matchAllCondition());
        }
        String selectorId = deterministicSelectorId(namespace, routeName, ruleIndex, hostname, matchIndex);
        String ruleId = deterministicRuleId(selectorId, matchIndex);
        String hostComponent = Objects.isNull(hostname) ? "" : "-" + hostname;
        String selectorName = routeName + "-rule-" + ruleIndex + hostComponent + "-m" + matchIndex;
        // Only the selector is padded: the rule's own conditions feed the data plane's
        // trie cache, which duplicate conditions would only bloat.
        SelectorData selectorData = buildSelectorData(selectorId, selectorName, sort,
                padToConditionFloor(conditions), upstreamList);
        RuleData ruleData = buildRuleData(ruleId, selectorId, selectorName, conditions);
        routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
    }

    /**
     * ShenYu's selector match first groups matching selectors by their number of AND
     * conditions and only then compares the sort ({@code AbstractShenyuPlugin#manyMatchSelector}),
     * so a raw two-header match would outrank a method match regardless of the encoded
     * precedence. Padding every generated selector to one fixed condition count with
     * always-true duplicates turns the grouping into a tie so the precedence-encoded sort
     * decides. Returns the list as-is when it already reached the floor (never shrinks).
     */
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

    /**
     * Resolve the rule's backendRefs into upstream addresses. Service is the only supported
     * kind (the default when absent); anything else is unresolved with reason InvalidKind.
     * A cross-namespace Service requires a ReferenceGrant in that namespace; a Service whose
     * Endpoints are missing or have no ready addresses is unresolved with reason
     * BackendNotFound. Both drive ResolvedRefs=False.
     */
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
        // Spec: the proportion of requests destined for invalid backends MUST receive a 500
        // while the valid shares stay routable. One fail-target entry carrying exactly the
        // invalid share keeps the weighted split proportional — see buildUpstreams.
        if (invalidWeightedShare > 0) {
            backends.add(new ResolvedBackend(invalidWeightedShare, List.of(FAIL_TARGET_URL)));
        }
        return new BackendResolveResult(buildUpstreams(backends), unresolvedCount,
                invalidWeightedShare, unresolvedReason);
    }

    /**
     * Spread the declared backend weights over the endpoints so each backend's aggregate
     * weight stays proportional to its declared weight regardless of replica counts.
     * Dividing each backend independently lets flooring distort the ratios (weight 9 and 1
     * over two endpoints each yield totals 8:2, and a weight-1 backend with many replicas
     * can outweigh a weight-9 one), so every backend is scaled by one common factor: the
     * smallest factor that lifts each backend's per-endpoint share to at least 1.
     */
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
                // Constant timestamp: the handle json must be byte-identical across resyncs,
                // otherwise the unchanged-check in ShenyuCacheRepository never triggers.
                upstream.setTimestamp(0L);
                upstreams.add(upstream);
            }
        }
        return upstreams;
    }

    private long divideRoundingUp(final long dividend, final long divisor) {
        return (dividend + divisor - 1) / divisor;
    }

    /**
     * Resolve one backendRef into upstream URLs, or into the Gateway API reason of its
     * failure (InvalidKind / RefNotPermitted / BackendNotFound).
     */
    private BackendRefOutcome resolveBackendRef(final JsonObject backendRef, final String namespace,
                                                final String routeName) {
        String serviceName = JsonFields.getString(backendRef, "name");
        if (Objects.isNull(serviceName)) {
            return BackendRefOutcome.ok(List.of(), 0);
        }
        // Gateway API spec: an omitted weight defaults to 1, so it mixes 1:1 with an explicit one
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
     * Map the backendRef (Service) port to the port the pods actually listen on, using the
     * Service spec the way the Ingress path does: the backendRef port selects
     * {@code spec.ports[]}, whose targetPort is either the pod port directly or a name
     * resolved against the Endpoints subsets' named ports; a Service port without
     * targetPort forwards to itself. When the Service is not in the informer cache, fall
     * back to the Endpoints-only heuristic: a single distinct endpoint port wins; a
     * servicePort matching one of the endpoint ports is used as-is; no port information at
     * all falls back to the servicePort (legacy behavior); anything else is ambiguous and
     * reported BackendNotFound instead of being silently misrouted.
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
                // Spec: a path prefix matches on element boundaries — /foo matches /foo and
                // /foo/bar but NOT /foobar — and a trailing '/' in the prefix is ignored.
                // ShenYu's raw startsWith cannot express this, hence an anchored regex.
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

    /**
     * Anchored full-match regex for a path prefix (the REGEX judge is a full match):
     * {@code /foo} → {@code ^\Q/foo\E(/.*)?$}, matching {@code /foo} and everything under it.
     * The root prefix {@code /} is the spec's catch-all and matches every absolute path, so
     * it must not go through the element-boundary form (which would only match {@code /} and
     * paths starting with {@code //}).
     */
    private String prefixRegex(final String prefix) {
        String stripped = prefix.length() > 1 && prefix.endsWith("/") ? prefix.substring(0, prefix.length() - 1) : prefix;
        if ("/".equals(stripped)) {
            return "^/.*$";
        }
        return "^" + Pattern.quote(stripped) + "(/.*)?$";
    }

    /**
     * Sort encoding the full Gateway API match precedence (lower sort wins on the data
     * plane, hence the inversion of the precedence score): hostname specificity beats path
     * specificity beats HTTP method presence beats header match count beats query param
     * match count — so for equal paths a match with a method outranks a match with two
     * headers, independently of the raw condition count (see padToConditionFloor).
     */
    private int selectorSort(final String hostname, final JsonObject match) {
        int score = hostnameScore(hostname) << 13
                | pathScore(match) << 9
                | (Objects.isNull(JsonFields.getString(match, "method")) ? 0 : 1) << 8
                | matchCount(match, "headers") << 4
                | matchCount(match, "queryParams");
        return SORT_PRECEDENCE_BASE - score;
    }

    /**
     * Hostname specificity (3 bits): an exact hostname beats any wildcard, a wildcard with
     * more suffix labels beats one with fewer; a selector without a hostname condition
     * matches any host and sorts below both.
     */
    private int hostnameScore(final String hostname) {
        if (Objects.isNull(hostname)) {
            return 0;
        }
        if (hostname.startsWith("*.")) {
            return Math.min(hostname.substring(2).split("\\.").length, 6);
        }
        return 7;
    }

    /**
     * Path specificity (4 bits): exact beats any prefix, a longer prefix beats a shorter
     * one (prefixes above twelve characters tie), regex beats no path at all. Two exact or
     * two method-distinct matches can never match the same request, so their scores never
     * need to differ.
     */
    private int pathScore(final JsonObject match) {
        JsonObject path = JsonFields.getJsonObject(match, "path");
        String pathValue = JsonFields.getString(path, "value");
        if (Objects.isNull(pathValue)) {
            return 0;
        }
        String pathType = JsonFields.getString(path, "type");
        if ("Exact".equals(pathType)) {
            return 15;
        }
        if ("RegularExpression".equals(pathType)) {
            return 1;
        }
        return 2 + Math.min(pathValue.length() - 1, 11);
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
     * Outcome of resolving one backendRef: either its upstream URLs with the declared
     * weight (possibly an empty URL list for a weight-0 backend) or the Gateway API reason
     * of the failure together with the declared weight (0 when the weight could not be
     * read at all).
     */
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

    /**
     * Result of resolving a rule's backendRefs: the reachable upstreams — including the
     * fail-target entry standing in for the invalid weighted share — how many backendRefs
     * failed, the summed declared weight of the failed backends holding a traffic share,
     * and the Gateway API reason (BackendNotFound, RefNotPermitted or InvalidKind) of the
     * first failure. A non-zero {@code unresolvedCount} means the reconciler should report
     * ResolvedRefs=False.
     */
    private static final class BackendResolveResult {

        private final List<DivideUpstream> upstreams;

        private final int unresolvedCount;

        private final int invalidWeightedShare;

        private final String unresolvedReason;

        BackendResolveResult(final List<DivideUpstream> upstreams, final int unresolvedCount,
                             final int invalidWeightedShare, final String unresolvedReason) {
            this.upstreams = upstreams;
            this.unresolvedCount = unresolvedCount;
            this.invalidWeightedShare = invalidWeightedShare;
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
