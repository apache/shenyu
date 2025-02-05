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
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ssl.SslCrtAndKeyStream;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
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

public class MotanIngressParser implements K8sResourceParser<V1Ingress> {
    private static final Logger LOG = LoggerFactory.getLogger(MotanIngressParser.class);

    private final Lister<V1Service> serviceLister;

    private final Lister<V1Endpoints> endpointsLister;

    /**
     * IngressParser Constructor.
     *
     * @param serviceLister   serviceLister
     * @param endpointsLister endpointsLister
     */
    public MotanIngressParser(final Lister<V1Service> serviceLister, final Lister<V1Endpoints> endpointsLister) {
        this.serviceLister = serviceLister;
        this.endpointsLister = endpointsLister;
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
            // Parse the default backend
            V1IngressBackend defaultBackend = ingress.getSpec().getDefaultBackend();
            List<V1IngressRule> rules = ingress.getSpec().getRules();
            List<V1IngressTLS> tlsList = ingress.getSpec().getTls();

            String namespace = Objects.requireNonNull(ingress.getMetadata()).getNamespace();

            if (Objects.isNull(rules) || CollectionUtils.isEmpty(rules)) {
                // if rules is null, defaultBackend become global default
                if (Objects.nonNull(defaultBackend) && Objects.nonNull(defaultBackend.getService())) {
                    IngressConfiguration defaultRouteConfig = getDefaultRouteConfig(ingress.getMetadata().getAnnotations());
                    res.setGlobalDefaultBackend(Pair.of(Pair.of(namespace + "/" + ingress.getMetadata().getName(), defaultBackend.getService().getName()),
                            defaultRouteConfig));
                }
            } else {
                // if rules is not null, defaultBackend is default in this ingress
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
                    String secretName = tls.getSecretName();
                    List<String> hosts = tls.getHosts();
                    if (Objects.nonNull(secretName) && CollectionUtils.isNotEmpty(hosts)) {
                        try {
                            V1Secret secret = coreV1Api.readNamespacedSecret(secretName, namespace, "ture");
                            if (Objects.nonNull(secret.getData())) {
                                InputStream keyCertChainInputStream = new ByteArrayInputStream(secret.getData().get("tls.crt"));
                                InputStream keyInputStream = new ByteArrayInputStream(secret.getData().get("tls.key"));
                                hosts.forEach(host ->
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

        ConditionData hostCondition = null;
        if (Objects.nonNull(ingressRule.getHost())) {
            hostCondition = new ConditionData();
            hostCondition.setParamType(ParamTypeEnum.URI.getName());
            hostCondition.setOperator(OperatorEnum.EQ.getAlias());
            hostCondition.setParamValue(ingressRule.getHost());
        }
        if (Objects.nonNull(ingressRule.getHttp())) {
            List<V1HTTPIngressPath> paths = ingressRule.getHttp().getPaths();
            if (Objects.nonNull(paths)) {
                for (V1HTTPIngressPath path : paths) {
                    String pathPath = path.getPath();
                    if (Objects.isNull(pathPath)) {
                        continue;
                    }

                    OperatorEnum operator;
                    if ("ImplementationSpecific".equals(path.getPathType())) {
                        operator = OperatorEnum.MATCH;
                    } else if ("Prefix".equals(path.getPathType())) {
                        operator = OperatorEnum.STARTS_WITH;
                    } else if ("Exact".equals(path.getPathType())) {
                        operator = OperatorEnum.EQ;
                    } else {
                        LOG.info("Invalid path type, set it with match operator");
                        operator = OperatorEnum.MATCH;
                    }

                    ConditionData pathCondition = new ConditionData();
                    pathCondition.setOperator(operator.getAlias());
                    pathCondition.setParamType(ParamTypeEnum.URI.getName());
                    pathCondition.setParamValue(pathPath);
                    List<ConditionData> conditionList = new ArrayList<>(2);
                    if (Objects.nonNull(hostCondition)) {
                        conditionList.add(hostCondition);
                    }
                    conditionList.add(pathCondition);
                    ConditionData ruleConditionData = new ConditionData();
                    ruleConditionData.setParamType(ParamTypeEnum.URI.getName());
                    ruleConditionData.setOperator(OperatorEnum.EQ.getAlias());
                    ruleConditionData.setParamName(annotations.getOrDefault(IngressConstants.PLUGIN_MOTAN_PATH, pathPath));
                    List<ConditionData> ruleConditionDataList = new ArrayList<>();
                    ruleConditionDataList.add(ruleConditionData);

                    SelectorData selectorData = SelectorData.builder()
                            .pluginId(String.valueOf(PluginEnum.MOTAN.getCode()))
                            .pluginName(PluginEnum.MOTAN.getName())
                            .name(pathPath)
                            .matchMode(MatchModeEnum.AND.getCode())
                            .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                            .enabled(true)
                            .logged(false)
                            .continued(true)
                            .conditionList(conditionList).build();

                    List<RuleData> ruleDataList = new ArrayList<>();
                    List<MetaData> metaDataList = new ArrayList<>();
                    for (String label : labels.keySet()) {
                        Map<String, String> metadataAnnotations = serviceLister.namespace(namespace).get(labels.get(label)).getMetadata().getAnnotations();
                        List<ConditionData> ruleConditionList = getRuleConditionList(metadataAnnotations);
                        RuleData ruleData = createRuleData(metadataAnnotations, ruleConditionList);
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
        ruleCondition.setParamValue(annotations.get(IngressConstants.PLUGIN_MOTAN_PATH));
        ruleConditionList.add(ruleCondition);
        return ruleConditionList;
    }

    private RuleData createRuleData(final Map<String, String> metadataAnnotations, final List<ConditionData> ruleConditionList) {
        return RuleData.builder()
                .name(metadataAnnotations.get(IngressConstants.PLUGIN_MOTAN_PATH))
                .pluginName(PluginEnum.MOTAN.getName())
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(ruleConditionList)
                .loged(true)
                .enabled(true)
                .build();
    }

    private MetaData parseMetaData(final Map<String, String> annotations) {
        return MetaData.builder()
                .appName(annotations.get(IngressConstants.PLUGIN_MOTAN_APP_NAME))
                .path(annotations.get(IngressConstants.PLUGIN_MOTAN_PATH))
                .rpcType(annotations.get(IngressConstants.PLUGIN_MOTAN_RPC_TYPE))
                .rpcExt(annotations.get(IngressConstants.PLUGIN_MOTAN_RPC_EXPAND))
                .serviceName(annotations.get(IngressConstants.PLUGIN_MOTAN_SREVICE_NAME))
                .methodName(annotations.get(IngressConstants.PLUGIN_MOTAN_METHOD_NAME))
                .parameterTypes(annotations.get(IngressConstants.PLUGIN_MOTAN_PARAMS_TYPE))
                .enabled(true)
                .build();
    }

    private IngressConfiguration getDefaultRouteConfig(final Map<String, String> annotations) {
        final ConditionData conditionData = new ConditionData();
        conditionData.setParamName("default");
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.PATH_PATTERN.getAlias());
        conditionData.setParamValue("/**");

        final SelectorData selectorData = SelectorData.builder()
                .name("default-selector")
                .sort(Integer.MAX_VALUE)
                .conditionList(Collections.singletonList(conditionData))
                .enabled(true)
                .id(IngressConstants.ID)
                .pluginName(PluginEnum.MOTAN.getName())
                .pluginId(String.valueOf(PluginEnum.MOTAN.getCode()))
                .logged(false)
                .continued(true)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.FULL_FLOW.getCode()).build();

        final RuleData ruleData = RuleData.builder()
                .selectorId(IngressConstants.ID)
                .pluginName(PluginEnum.MOTAN.getName())
                .name("default-rule")
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(Collections.singletonList(conditionData))
                .loged(false)
                .enabled(true)
                .sort(Integer.MAX_VALUE).build();

        if (Objects.isNull(annotations.get(IngressConstants.PLUGIN_MOTAN_APP_NAME))
                || Objects.isNull(annotations.get(IngressConstants.PLUGIN_MOTAN_METHOD_NAME))
                || Objects.isNull(annotations.get(IngressConstants.PLUGIN_MOTAN_PATH))
                || Objects.isNull(annotations.get(IngressConstants.PLUGIN_MOTAN_SREVICE_NAME))
                || Objects.isNull(annotations.get(IngressConstants.PLUGIN_MOTAN_RPC_TYPE))) {
            LOG.error("motan metadata is error, please check motan service. MetaData: [{}]", annotations);
            throw new ShenyuException(annotations + " is is missing.");
        }
        MetaData metaData = MetaData.builder()
                .appName(annotations.get(IngressConstants.PLUGIN_MOTAN_APP_NAME))
                .path(annotations.get(IngressConstants.PLUGIN_MOTAN_PATH))
                .rpcType(annotations.get(IngressConstants.PLUGIN_MOTAN_RPC_TYPE))
                .rpcExt(annotations.getOrDefault(IngressConstants.PLUGIN_MOTAN_RPC_EXPAND, ""))
                .serviceName(annotations.get(IngressConstants.PLUGIN_MOTAN_SREVICE_NAME))
                .methodName(annotations.get(IngressConstants.PLUGIN_MOTAN_METHOD_NAME))
                .parameterTypes(annotations.getOrDefault(IngressConstants.PLUGIN_MOTAN_PARAMS_TYPE, ""))
                .enabled(true)
                .build();
        return new IngressConfiguration(selectorData, Arrays.asList(ruleData), Arrays.asList(metaData));
    }
}
