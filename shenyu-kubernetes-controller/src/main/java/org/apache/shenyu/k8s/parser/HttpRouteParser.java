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
import org.apache.shenyu.k8s.common.IngressConfiguration;
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

    /**
     * Prefix for all Gateway API generated selector/rule IDs, isolating them from the
     * numeric ID space used by the Ingress reconciler and avoiding any collision.
     */
    private static final String ID_PREFIX = "gwapi-";

    /**
     * Placeholder used when an HTTPRoute rule has no hostname, so that the deterministic
     * ID derivation still has a stable component for the hostname slot.
     */
    private static final String NO_HOSTNAME_PLACEHOLDER = "_";

    private final Lister<V1Endpoints> endpointsLister;

    public HttpRouteParser(final Lister<V1Endpoints> endpointsLister) {
        this.endpointsLister = endpointsLister;
    }

    public ShenyuMemoryConfig parse(final DynamicKubernetesObject httpRoute) {
        ShenyuMemoryConfig res = new ShenyuMemoryConfig();
        String namespace = Objects.requireNonNull(httpRoute.getMetadata()).getNamespace();
        String routeName = httpRoute.getMetadata().getName();

        JsonObject raw = httpRoute.getRaw();
        JsonObject spec = raw.getAsJsonObject("spec");
        if (Objects.isNull(spec)) {
            return res;
        }

        JsonArray hostnames = spec.getAsJsonArray("hostnames");
        JsonArray rules = spec.getAsJsonArray("rules");
        if (Objects.isNull(rules) || rules.isEmpty()) {
            return res;
        }

        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        List<IngressConfiguration> routeConfigList = new ArrayList<>();

        for (int ruleIndex = 0; ruleIndex < rules.size(); ruleIndex++) {
            processRule(rules.get(ruleIndex).getAsJsonObject(), hostnames, namespace, routeName, ruleIndex,
                    routeConfigList);
        }

        // Replace the route→selectors index atomically with the freshly parsed IDs.
        // Deterministic IDs make this an idempotent replacement on resync.
        List<String> selectorIds = new ArrayList<>();
        for (IngressConfiguration rc : routeConfigList) {
            selectorIds.add(rc.getSelectorData().getId());
        }
        cache.putRouteSelectors(namespace, routeName, PluginEnum.DIVIDE.getName(), selectorIds);

        res.setRouteConfigList(routeConfigList);
        return res;
    }

    private void processRule(final JsonObject rule, final JsonArray hostnames, final String namespace,
                             final String routeName, final int ruleIndex,
                             final List<IngressConfiguration> routeConfigList) {
        JsonArray backendRefs = rule.getAsJsonArray("backendRefs");
        if (Objects.isNull(backendRefs) || backendRefs.isEmpty()) {
            return;
        }

        List<DivideUpstream> upstreamList = parseBackendRefs(backendRefs, namespace);

        // Build a list of individual hostname conditions.
        // Each hostname generates a separate selector+rule to avoid AND logic contradiction
        // (a request can only match one hostname at a time).
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
                    // No hostname: one selector+rule for this match
                    String selectorId = deterministicSelectorId(namespace, routeName, ruleIndex, null, matchIndex);
                    String ruleId = deterministicRuleId(selectorId, matchIndex);
                    String selectorName = routeName + "-rule-" + ruleIndex;
                    SelectorData selectorData = buildSelectorData(selectorId, selectorName, matchConditions, upstreamList);
                    RuleData ruleData = buildRuleData(ruleId, selectorId, selectorName, matchConditions);
                    routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
                } else {
                    // One selector+rule per hostname to keep AND semantics correct
                    for (ConditionData hostCondition : hostnameConditions) {
                        List<ConditionData> conditions = new ArrayList<>();
                        conditions.add(hostCondition);
                        conditions.addAll(matchConditions);

                        String selectorId = deterministicSelectorId(namespace, routeName, ruleIndex,
                                hostCondition.getParamValue(), matchIndex);
                        String ruleId = deterministicRuleId(selectorId, matchIndex);
                        String selectorName = routeName + "-rule-" + ruleIndex + "-" + hostCondition.getParamValue();
                        SelectorData selectorData = buildSelectorData(selectorId, selectorName, conditions, upstreamList);
                        RuleData ruleData = buildRuleData(ruleId, selectorId, selectorName, conditions);
                        routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
                    }
                }
            }
        } else {
            // No matches: a single selector+rule derived from the rule index only.
            // matchIndex 0 is reused as the derivation component so IDs stay stable.
            if (hostnameConditions.isEmpty()) {
                String selectorId = deterministicSelectorId(namespace, routeName, ruleIndex, null, 0);
                String ruleId = deterministicRuleId(selectorId, 0);
                String selectorName = routeName + "-rule-" + ruleIndex;
                SelectorData selectorData = buildSelectorData(selectorId, selectorName, new ArrayList<>(), upstreamList);
                RuleData ruleData = buildRuleData(ruleId, selectorId, selectorName, new ArrayList<>());
                routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
            } else {
                for (ConditionData hostCondition : hostnameConditions) {
                    String selectorId = deterministicSelectorId(namespace, routeName, ruleIndex,
                            hostCondition.getParamValue(), 0);
                    String ruleId = deterministicRuleId(selectorId, 0);
                    String selectorName = routeName + "-rule-" + ruleIndex + "-" + hostCondition.getParamValue();
                    List<ConditionData> conditions = new ArrayList<>();
                    conditions.add(hostCondition);
                    SelectorData selectorData = buildSelectorData(selectorId, selectorName, conditions, upstreamList);
                    RuleData ruleData = buildRuleData(ruleId, selectorId, selectorName, conditions);
                    routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
                }
            }
        }
    }

    /**
     * Derive a deterministic selector ID from the route coordinates so the same HTTPRoute
     * spec always yields the same ID. This makes reconcile idempotent: on informer resync
     * the reconciler re-parses and upserts selectors with unchanged IDs, avoiding the
     * delete-then-create churn that briefly left routes unmatched on the data plane.
     *
     * @param namespace  route namespace
     * @param routeName  route name
     * @param ruleIndex  index of the rule within the route
     * @param hostname   hostname the selector is scoped to, or null when the route has no hostnames
     * @param matchIndex index of the match within the rule (0 when the rule has no matches)
     * @return stable prefixed UUID, e.g. "gwapi-550e8400-e29b-..."
     */
    private String deterministicSelectorId(final String namespace, final String routeName, final int ruleIndex,
                                           final String hostname, final int matchIndex) {
        String hostComponent = Objects.isNull(hostname) ? NO_HOSTNAME_PLACEHOLDER : hostname;
        String key = namespace + "/" + routeName + "/r" + ruleIndex + "/h" + hostComponent + "/m" + matchIndex;
        return ID_PREFIX + UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Derive a deterministic rule ID from its parent selector ID and match index.
     * Stays well under the varchar(128) limit for any plausible selector ID.
     */
    private String deterministicRuleId(final String selectorId, final int matchIndex) {
        return selectorId + "/rule-m" + matchIndex;
    }

    /**
     * Build a domain match condition for an HTTPRoute hostname.
     *
     * <p>Per Gateway API semantics:
     * <ul>
     *   <li>Exact hostname (e.g. {@code example.com}) → EQ match.</li>
     *   <li>Wildcard hostname (e.g. {@code *.example.com}) → matches a single DNS label
     *       subdomain (e.g. {@code api.example.com} but not {@code a.b.example.com}).
     *       Translated to an anchored regex used with the MATCH operator, since the data
     *       plane's EQ judge does exact string equality and the MATCH judge for DOMAIN
     *       falls back to {@code contains} (both wrong for wildcards).</li>
     * </ul>
     *
     * @param hostname the HTTPRoute hostname entry
     * @return a condition data matching the hostname semantics
     */
    private ConditionData buildHostnameCondition(final String hostname) {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.DOMAIN.getName());
        if (hostname.startsWith("*.")) {
            // *.example.com → ^[^.]+\.example\.com$ (single-label subdomain)
            String suffix = hostname.substring(2).replace(".", "\\.");
            condition.setOperator(OperatorEnum.MATCH.getAlias());
            condition.setParamValue("^[^.]+\\." + suffix + "$");
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

    private List<DivideUpstream> parseBackendRefs(final JsonArray backendRefs, final String namespace) {
        List<DivideUpstream> upstreamList = new ArrayList<>();
        for (JsonElement element : backendRefs) {
            JsonObject backendRef = element.getAsJsonObject();
            String serviceName = getStringField(backendRef, "name");
            int weight = backendRef.has("weight") ? backendRef.get("weight").getAsInt() : 100;
            Integer port = backendRef.has("port") ? backendRef.get("port").getAsInt() : null;

            if (Objects.isNull(serviceName)) {
                continue;
            }

            V1Endpoints v1Endpoints = endpointsLister.namespace(namespace).get(serviceName);
            if (Objects.isNull(v1Endpoints) || CollectionUtils.isEmpty(v1Endpoints.getSubsets())) {
                LOG.warn("Cannot find endpoints for service {}/{}", namespace, serviceName);
                continue;
            }

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
        }
        return upstreamList;
    }

    private void appendMatchConditions(final List<ConditionData> conditions, final JsonObject match) {
        JsonObject path = match.getAsJsonObject("path");
        if (Objects.nonNull(path) && path.has("value")) {
            ConditionData pathCondition = new ConditionData();
            pathCondition.setParamType(ParamTypeEnum.URI.getName());
            pathCondition.setOperator(mapPathType(getStringField(path, "type")));
            pathCondition.setParamValue(path.get("value").getAsString());
            conditions.add(pathCondition);
        }

        JsonArray headers = match.getAsJsonArray("headers");
        if (Objects.nonNull(headers)) {
            for (JsonElement headerElement : headers) {
                JsonObject header = headerElement.getAsJsonObject();
                ConditionData headerCondition = new ConditionData();
                headerCondition.setParamType(ParamTypeEnum.HEADER.getName());
                headerCondition.setOperator("Exact".equals(getStringField(header, "type"))
                        ? OperatorEnum.EQ.getAlias() : OperatorEnum.MATCH.getAlias());
                headerCondition.setParamName(getStringField(header, "name"));
                headerCondition.setParamValue(getStringField(header, "value"));
                conditions.add(headerCondition);
            }
        }

        JsonArray queryParams = match.getAsJsonArray("queryParams");
        if (Objects.nonNull(queryParams)) {
            for (JsonElement queryElement : queryParams) {
                JsonObject queryParam = queryElement.getAsJsonObject();
                ConditionData queryCondition = new ConditionData();
                queryCondition.setParamType(ParamTypeEnum.QUERY.getName());
                queryCondition.setOperator("Exact".equals(getStringField(queryParam, "type"))
                        ? OperatorEnum.EQ.getAlias() : OperatorEnum.MATCH.getAlias());
                queryCondition.setParamName(getStringField(queryParam, "name"));
                queryCondition.setParamValue(getStringField(queryParam, "value"));
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
            return OperatorEnum.MATCH.getAlias();
        }
        return OperatorEnum.STARTS_WITH.getAlias();
    }

    private String getStringField(final JsonObject obj, final String field) {
        if (Objects.isNull(obj) || !obj.has(field) || obj.get(field).isJsonNull()) {
            return null;
        }
        return obj.get(field).getAsString();
    }
}
