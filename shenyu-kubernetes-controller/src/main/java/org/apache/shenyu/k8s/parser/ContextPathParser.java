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

import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1HTTPIngressPath;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ContextMappingRuleHandle;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Parser of Ingress ContextPath Annotations.
 */
public class ContextPathParser implements K8sResourceParser<V1Ingress> {

    private static final Logger LOG = LoggerFactory.getLogger(ContextPathParser.class);

    private final Lister<V1Service> serviceLister;

    private final Lister<V1Endpoints> endpointsLister;

    /**
     * IngressParser Constructor.
     *
     * @param serviceLister serviceLister
     * @param endpointsLister endpointsLister
     */
    public ContextPathParser(final Lister<V1Service> serviceLister, final Lister<V1Endpoints> endpointsLister) {
        this.serviceLister = serviceLister;
        this.endpointsLister = endpointsLister;
    }

    /**
     * Parse ingress to ShenyuMemoryConfig.
     *
     * @param ingress ingress resource
     * @param coreV1Api coreV1Api
     * @return ShenyuMemoryConfig
     */
    @Override
    public ShenyuMemoryConfig parse(final V1Ingress ingress, final CoreV1Api coreV1Api) {
        ShenyuMemoryConfig res = new ShenyuMemoryConfig();

        if (Objects.nonNull(ingress.getSpec())) {
            // if rules is null, context path Plugin will not execute the
            List<V1IngressRule> rules = ingress.getSpec().getRules();
            if (Objects.nonNull(rules)) {
                List<IngressConfiguration> routeList = new ArrayList<>(rules.size());
                for (V1IngressRule ingressRule : rules) {
                    List<IngressConfiguration> routes = parseIngressRule(ingressRule, ingress.getMetadata().getAnnotations());
                    routeList.addAll(routes);
                }
                res.setRouteConfigList(routeList);
            }
        }
        return res;
    }

    private List<IngressConfiguration> parseIngressRule(final V1IngressRule ingressRule, final Map<String, String> annotations) {
        List<IngressConfiguration> res = new ArrayList<>();
        ConditionData hostCondition = Objects.nonNull(ingressRule.getHost()) ? createHostCondition(ingressRule.getHost()) : null;
        if (Objects.nonNull(ingressRule.getHttp())) {
            List<V1HTTPIngressPath> paths = ingressRule.getHttp().getPaths();
            if (Objects.nonNull(paths)) {
                for (V1HTTPIngressPath path : paths) {
                    String pathPath = path.getPath();
                    if (Objects.isNull(pathPath)) {
                        continue;
                    }
                    OperatorEnum operator = getOperator(path.getPathType());
                    ConditionData pathCondition = createPathCondition(pathPath, operator);
                    List<ConditionData> conditionList = new ArrayList<>(2);
                    if (Objects.nonNull(hostCondition)) {
                        conditionList.add(hostCondition);
                    }
                    conditionList.add(pathCondition);
                    SelectorData selectorData = createSelectorData(pathPath, conditionList);
                    ContextMappingRuleHandle contextMappingRuleHandle = createContextMappingRuleHandle(annotations);
                    List<RuleData> ruleDataList = new ArrayList<>();
                    List<ConditionData> ruleConditionList = getRuleConditionList(annotations);
                    RuleData ruleData = createRuleData(annotations, contextMappingRuleHandle, ruleConditionList);
                    ruleDataList.add(ruleData);
                    res.add(new IngressConfiguration(selectorData, ruleDataList, null));
                }
            }
        }
        return res;
    }

    private ConditionData createHostCondition(final String host) {
        ConditionData hostCondition = new ConditionData();
        hostCondition.setParamType(ParamTypeEnum.DOMAIN.getName());
        hostCondition.setOperator(OperatorEnum.EQ.getAlias());
        hostCondition.setParamValue(host);
        return hostCondition;
    }

    private OperatorEnum getOperator(final String pathType) {
        if ("ImplementationSpecific".equals(pathType)) {
            return OperatorEnum.MATCH;
        } else if ("Prefix".equals(pathType)) {
            return OperatorEnum.STARTS_WITH;
        } else if ("Exact".equals(pathType)) {
            return OperatorEnum.EQ;
        } else {
            LOG.info("Invalid path type, set it with match operator");
            return OperatorEnum.MATCH;
        }
    }

    private ConditionData createPathCondition(final String path, final OperatorEnum operator) {
        ConditionData pathCondition = new ConditionData();
        pathCondition.setOperator(operator.getAlias());
        pathCondition.setParamType(ParamTypeEnum.URI.getName());
        pathCondition.setParamValue(path);
        return pathCondition;
    }

    private SelectorData createSelectorData(final String path, final List<ConditionData> conditionList) {
        return SelectorData.builder()
                .pluginId(String.valueOf(PluginEnum.CONTEXT_PATH.getCode()))
                .pluginName(PluginEnum.CONTEXT_PATH.getName())
                .name(path)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .enabled(true)
                .logged(false)
                .continued(true)
                .conditionList(conditionList)
                .build();
    }

    private ContextMappingRuleHandle createContextMappingRuleHandle(final Map<String, String> annotations) {
        ContextMappingRuleHandle ruleHandle = new ContextMappingRuleHandle();
        ruleHandle.setContextPath(annotations.get(IngressConstants.PLUGIN_CONTEXT_PATH_PATH));
        ruleHandle.setAddPrefix(annotations.get(IngressConstants.PLUGIN_CONTEXT_PATH_ADD_PREFIX));
        ruleHandle.setAddPrefixed(Boolean.parseBoolean(annotations.getOrDefault(IngressConstants.PLUGIN_CONTEXT_PATH_ADD_PREFIXED, "false")));
        return ruleHandle;
    }

    private RuleData createRuleData(final Map<String, String> annotations, final ContextMappingRuleHandle ruleHandle, final List<ConditionData> ruleConditionList) {
        return RuleData.builder()
                .name(annotations.get(IngressConstants.PLUGIN_CONTEXT_PATH_PATH))
                .pluginName(PluginEnum.CONTEXT_PATH.getName())
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(ruleConditionList)
                .handle(GsonUtils.getInstance().toJson(ruleHandle))
                .loged(true)
                .enabled(true)
                .build();
    }

    private List<ConditionData> getRuleConditionList(final Map<String, String> annotations) {
        final List<ConditionData> ruleConditionList = new ArrayList<>();
        ConditionData ruleCondition = new ConditionData();
        ruleCondition.setOperator(OperatorEnum.PATH_PATTERN.getAlias());
        ruleCondition.setParamType(ParamTypeEnum.URI.getName());
        ruleCondition.setParamValue(annotations.get(IngressConstants.PLUGIN_CONTEXT_PATH_PATH) + "/**");
        ruleConditionList.add(ruleCondition);
        return ruleConditionList;
    }
}
