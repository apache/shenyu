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

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class HttpRouteParser {

    private static final Logger LOG = LoggerFactory.getLogger(HttpRouteParser.class);

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
            processRule(rules.get(ruleIndex).getAsJsonObject(), hostnames, namespace, routeName, ruleIndex, cache, routeConfigList);
        }

        res.setRouteConfigList(routeConfigList);
        return res;
    }

    private void processRule(final JsonObject rule, final JsonArray hostnames, final String namespace,
                             final String routeName, final int ruleIndex, final GatewayRouteCache cache,
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
                ConditionData hostCondition = new ConditionData();
                hostCondition.setParamType(ParamTypeEnum.DOMAIN.getName());
                hostCondition.setOperator(OperatorEnum.EQ.getAlias());
                hostCondition.setParamValue(hostname.getAsString());
                hostnameConditions.add(hostCondition);
            }
        }

        JsonArray matches = rule.getAsJsonArray("matches");
        if (Objects.nonNull(matches) && !matches.isEmpty()) {
            for (JsonElement matchElement : matches) {
                JsonObject match = matchElement.getAsJsonObject();
                List<ConditionData> matchConditions = new ArrayList<>();
                appendMatchConditions(matchConditions, match);

                if (hostnameConditions.isEmpty()) {
                    // No hostname: one selector+rule for this match
                    String selectorId = cache.generateSelectorId();
                    String selectorName = routeName + "-rule-" + ruleIndex;
                    SelectorData selectorData = buildSelectorData(selectorId, selectorName, matchConditions, upstreamList);
                    RuleData ruleData = buildRuleData(cache.generateRuleId(), selectorId, selectorName, matchConditions);
                    cache.addRouteSelector(namespace, routeName, PluginEnum.DIVIDE.getName(), selectorId);
                    routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
                } else {
                    // One selector+rule per hostname to keep AND semantics correct
                    for (ConditionData hostCondition : hostnameConditions) {
                        List<ConditionData> conditions = new ArrayList<>();
                        conditions.add(hostCondition);
                        conditions.addAll(matchConditions);

                        String selectorId = cache.generateSelectorId();
                        String selectorName = routeName + "-rule-" + ruleIndex + "-" + hostCondition.getParamValue();
                        SelectorData selectorData = buildSelectorData(selectorId, selectorName, conditions, upstreamList);
                        RuleData ruleData = buildRuleData(cache.generateRuleId(), selectorId, selectorName, conditions);
                        cache.addRouteSelector(namespace, routeName, PluginEnum.DIVIDE.getName(), selectorId);
                        routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
                    }
                }
            }
        } else {
            if (hostnameConditions.isEmpty()) {
                String selectorId = cache.generateSelectorId();
                String selectorName = routeName + "-rule-" + ruleIndex;
                SelectorData selectorData = buildSelectorData(selectorId, selectorName, new ArrayList<>(), upstreamList);
                RuleData ruleData = buildRuleData(cache.generateRuleId(), selectorId, selectorName, new ArrayList<>());
                cache.addRouteSelector(namespace, routeName, PluginEnum.DIVIDE.getName(), selectorId);
                routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
            } else {
                for (ConditionData hostCondition : hostnameConditions) {
                    String selectorId = cache.generateSelectorId();
                    String selectorName = routeName + "-rule-" + ruleIndex + "-" + hostCondition.getParamValue();
                    List<ConditionData> conditions = new ArrayList<>();
                    conditions.add(hostCondition);
                    SelectorData selectorData = buildSelectorData(selectorId, selectorName, conditions, upstreamList);
                    RuleData ruleData = buildRuleData(cache.generateRuleId(), selectorId, selectorName, conditions);
                    cache.addRouteSelector(namespace, routeName, PluginEnum.DIVIDE.getName(), selectorId);
                    routeConfigList.add(new IngressConfiguration(selectorData, List.of(ruleData), null));
                }
            }
        }
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
