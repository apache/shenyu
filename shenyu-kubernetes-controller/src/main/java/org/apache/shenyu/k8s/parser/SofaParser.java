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
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1HTTPIngressPath;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBackend;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressTLS;
import io.kubernetes.client.openapi.models.V1Secret;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ssl.SslCrtAndKeyStream;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.SofaRuleHandle;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Parser of Ingress Sofa Annotations.
 */
public class SofaParser implements K8sResourceParser<V1Ingress> {

    private static final Logger LOG = LoggerFactory.getLogger(SofaParser.class);

    private final Lister<V1Service> serviceLister;

    private final Lister<V1Endpoints> endpointsLister;

    /**
     * SofaIngressParser Constructor.
     *
     * @param serviceInformer   serviceInformer
     * @param endpointsInformer endpointsInformer
     */
    public SofaParser(final Lister<V1Service> serviceInformer, final Lister<V1Endpoints> endpointsInformer) {
        this.serviceLister = serviceInformer;
        this.endpointsLister = endpointsInformer;
    }

    /**
     * Parse ingress to ShenyuMemoryConfig.
     *
     * @param ingress   ingress resource
     * @param coreV1Api coreV1Api
     * @return ShenyuMemoryConfig
     */
    @Override
    public ShenyuMemoryConfig parse(final V1Ingress ingress, final CoreV1Api coreV1Api) {
        ShenyuMemoryConfig res = new ShenyuMemoryConfig();

        if (Objects.nonNull(ingress.getSpec())) {
            // Parse the sofa backend
            V1IngressBackend sofaBackend = ingress.getSpec().getDefaultBackend();
            List<V1IngressRule> rules = ingress.getSpec().getRules();
            List<V1IngressTLS> tlsList = ingress.getSpec().getTls();

            String namespace = Objects.requireNonNull(ingress.getMetadata()).getNamespace();

            if (Objects.isNull(rules) || CollectionUtils.isEmpty(rules)) {
                // if rules is null, sofaBackend become global default
                if (Objects.nonNull(sofaBackend) && Objects.nonNull(sofaBackend.getService())) {
                    IngressConfiguration defaultRouteConfig = getSofaRouteConfig(ingress.getMetadata().getAnnotations());
                    res.setGlobalDefaultBackend(Pair.of(Pair.of(namespace + "/" + ingress.getMetadata().getName(), sofaBackend.getService().getName()),
                            defaultRouteConfig));
                }
            } else {
                // if rules is not null, sofaBackend is default in this ingress
                List<IngressConfiguration> routeList = new ArrayList<>(rules.size());
                for (V1IngressRule ingressRule : rules) {
                    List<IngressConfiguration> routes = parseIngressRule(ingressRule,
                            Objects.requireNonNull(ingress.getMetadata()).getNamespace(), ingress.getMetadata().getAnnotations(), ingress.getMetadata().getLabels());
                    routeList.addAll(routes);
                }
                res.setRouteConfigList(routeList);
            }

            // Parse tls
            if (Objects.nonNull(tlsList) && CollectionUtils.isNotEmpty(tlsList)) {
                List<SslCrtAndKeyStream> sslList = new ArrayList<>();
                for (V1IngressTLS tls : tlsList) {
                    if (Objects.nonNull(tls.getSecretName()) && Objects.nonNull(tls.getHosts()) && CollectionUtils.isNotEmpty(tls.getHosts())) {
                        try {
                            V1Secret secret = coreV1Api.readNamespacedSecret(tls.getSecretName(), namespace, "ture");
                            Map<String, byte[]> secretData = secret.getData();
                            if (MapUtils.isNotEmpty(secretData)) {
                                InputStream keyCertChainInputStream = new ByteArrayInputStream(secretData.get("tls.crt"));
                                InputStream keyInputStream = new ByteArrayInputStream(secretData.get("tls.key"));
                                tls.getHosts().forEach(host ->
                                        sslList.add(new SslCrtAndKeyStream(host, keyCertChainInputStream, keyInputStream))
                                );
                            }
                        } catch (ApiException e) {
                            LOG.error("parse tls failed ", e);
                        }
                    }
                }
                res.setTlsConfigList(sslList);
            }
        }
        return res;
    }

    private List<IngressConfiguration> parseIngressRule(final V1IngressRule ingressRule,
                                                        final String namespace,
                                                        final Map<String, String> annotations,
                                                        final Map<String, String> labels) {
        List<IngressConfiguration> res = new ArrayList<>();
        ConditionData hostCondition = Objects.nonNull(ingressRule.getHost()) ? createHostCondition(ingressRule.getHost()) : null;
        if (Objects.nonNull(ingressRule.getHttp())) {
            List<V1HTTPIngressPath> paths = ingressRule.getHttp().getPaths();
            if (Objects.nonNull(paths)) {
                for (V1HTTPIngressPath path : paths) {
                    if (path.getPath() == null) {
                        continue;
                    }
                    OperatorEnum operator = getOperator(path.getPathType());
                    ConditionData pathCondition = createPathCondition(path.getPath(), operator);
                    List<ConditionData> conditionList = new ArrayList<>(2);
                    if (Objects.nonNull(hostCondition)) {
                        conditionList.add(hostCondition);
                    }
                    conditionList.add(pathCondition);

                    SelectorData selectorData = createSelectorData(path.getPath(), conditionList);
                    List<RuleData> ruleDataList = new ArrayList<>();
                    List<MetaData> metaDataList = new ArrayList<>();
                    for (String label : labels.keySet()) {
                        Map<String, String> metadataAnnotations = serviceLister.namespace(namespace).get(labels.get(label)).getMetadata().getAnnotations();
                        SofaRuleHandle ruleHandle = createSofaRuleHandle(annotations);
                        List<ConditionData> ruleConditionList = getRuleConditionList(metadataAnnotations);
                        RuleData ruleData = createRuleData(metadataAnnotations, ruleHandle, ruleConditionList);
                        MetaData metaData = parseMetaData(metadataAnnotations);
                        ruleDataList.add(ruleData);
                        metaDataList.add(metaData);
                    }
                    res.add(new IngressConfiguration(selectorData, ruleDataList, metaDataList));
                }
            }
        }
        return res;
    }

    private List<ConditionData> getRuleConditionList(final Map<String, String> annotations) {
        final List<ConditionData> ruleConditionList = new ArrayList<>();
        ConditionData ruleCondition = new ConditionData();
        ruleCondition.setOperator(OperatorEnum.EQ.getAlias());
        ruleCondition.setParamType(ParamTypeEnum.URI.getName());
        ruleCondition.setParamValue(annotations.get(IngressConstants.PLUGIN_SOFA_PATH));
        ruleConditionList.add(ruleCondition);
        return ruleConditionList;
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

    private SofaRuleHandle createSofaRuleHandle(final Map<String, String> annotations) {
        SofaRuleHandle sofaRuleHandle = new SofaRuleHandle();
        if (Objects.nonNull(annotations)) {
            sofaRuleHandle.setLoadBalance(annotations.getOrDefault(IngressConstants.LOADBALANCER_ANNOTATION_KEY, LoadBalanceEnum.RANDOM.getName()));
            sofaRuleHandle.setTimeout(Long.parseLong(annotations.getOrDefault(IngressConstants.TIMEOUT_ANNOTATION_KEY, "3000")));
            sofaRuleHandle.setRetries(Integer.parseInt(annotations.getOrDefault(IngressConstants.RETRY_ANNOTATION_KEY, "3")));
        }
        return sofaRuleHandle;
    }

    private SelectorData createSelectorData(final String path, final List<ConditionData> conditionList) {
        return SelectorData.builder()
                .pluginId(String.valueOf(PluginEnum.SOFA.getCode()))
                .pluginName(PluginEnum.SOFA.getName())
                .name(path)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .enabled(true)
                .logged(false)
                .continued(true)
                .conditionList(conditionList)
                .build();
    }

    private RuleData createRuleData(final Map<String, String> metadataAnnotations, final SofaRuleHandle ruleHandle, final List<ConditionData> ruleConditionList) {
        return RuleData.builder()
                .name(metadataAnnotations.get(IngressConstants.PLUGIN_SOFA_PATH))
                .pluginName(PluginEnum.SOFA.getName())
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(ruleConditionList)
                .handle(GsonUtils.getInstance().toJson(ruleHandle))
                .loged(true)
                .enabled(true)
                .build();
    }

    private MetaData parseMetaData(final Map<String, String> annotations) {
        return MetaData.builder()
                .appName(annotations.get(IngressConstants.PLUGIN_SOFA_APP_NAME))
                .path(annotations.get(IngressConstants.PLUGIN_SOFA_PATH))
                .rpcType(annotations.get(IngressConstants.PLUGIN_SOFA_RPC_TYPE))
                .rpcExt(annotations.get(IngressConstants.PLUGIN_SOFA_RPC_EXPAND))
                .serviceName(annotations.get(IngressConstants.PLUGIN_SOFA_SERVICE_NAME))
                .methodName(annotations.get(IngressConstants.PLUGIN_SOFA_METHOD_NAME))
                .parameterTypes(annotations.get(IngressConstants.PLUGIN_SOFA_PARAMS_TYPE))
                .enabled(true)
                .build();
    }

    private IngressConfiguration getSofaRouteConfig(final Map<String, String> annotations) {
        final ConditionData conditionData = new ConditionData();
        conditionData.setParamName("sofa");
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.PATH_PATTERN.getAlias());
        conditionData.setParamValue("/**");

        final SelectorData selectorData = SelectorData.builder()
                .name("sofa-selector")
                .sort(Integer.MAX_VALUE)
                .conditionList(Collections.singletonList(conditionData))
                .enabled(true)
                .id(IngressConstants.ID)
                .pluginName(PluginEnum.SOFA.getName())
                .pluginId(String.valueOf(PluginEnum.SOFA.getCode()))
                .logged(false)
                .continued(true)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.FULL_FLOW.getCode()).build();

        final RuleData ruleData = RuleData.builder()
                .selectorId(IngressConstants.ID)
                .pluginName(PluginEnum.SOFA.getName())
                .name("sofa-rule")
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(Collections.singletonList(conditionData))
                .loged(false)
                .enabled(true)
                .sort(Integer.MAX_VALUE).build();

        MetaData metaData = new MetaData();
        if (Objects.nonNull(annotations)) {
            metaData.setAppName(annotations.getOrDefault(IngressConstants.PLUGIN_SOFA_APP_NAME, "sofa"));
            metaData.setMethodName(annotations.getOrDefault(IngressConstants.PLUGIN_SOFA_METHOD_NAME, "methodName"));
            metaData.setPath(annotations.getOrDefault(IngressConstants.PLUGIN_SOFA_PATH, "/sofa/findAll"));
            metaData.setRpcType(annotations.getOrDefault(IngressConstants.PLUGIN_SOFA_RPC_TYPE, RpcTypeEnum.SOFA.getName()));
            metaData.setServiceName(annotations.getOrDefault(IngressConstants.PLUGIN_SOFA_SERVICE_NAME, "findAll"));
            metaData.setContextPath(annotations.getOrDefault(IngressConstants.PLUGIN_SOFA_CONTEXT_PATH, "/sofa"));
            metaData.setRpcExt(annotations.getOrDefault(IngressConstants.PLUGIN_SOFA_RPC_EXT, "{\"loadbalance\":\"hash\",\"retries\":3,\"timeout\":-1}"));
            metaData.setParameterTypes(annotations.getOrDefault(IngressConstants.PLUGIN_SOFA_PARAMETER_TYPE, ""));
            metaData.setEnabled(Boolean.parseBoolean(annotations.getOrDefault(IngressConstants.PLUGIN_SOFA_ENABLED, "true")));
        }
        return new IngressConfiguration(selectorData, Arrays.asList(ruleData), Arrays.asList(metaData));
    }
}
