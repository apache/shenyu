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

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubset;
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
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
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

import static org.apache.shenyu.common.utils.IpUtils.isCompleteHost;

public class BrpcParser implements K8sResourceParser<V1Ingress> {
    private static final Logger LOG = LoggerFactory.getLogger(BrpcParser.class);

    private final Lister<V1Service> serviceLister;

    private final Lister<V1Endpoints> endpointsLister;

    /**
     * BrpcIngressParser Constructor.
     *
     * @param serviceInformer   serviceInformer
     * @param endpointsInformer endpointsInformer
     */
    public BrpcParser(final Lister<V1Service> serviceInformer, final Lister<V1Endpoints> endpointsInformer) {
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
            // parse the brpc backend
            V1IngressBackend brpcBackend = ingress.getSpec().getDefaultBackend();
            List<V1IngressRule> rules = ingress.getSpec().getRules();
            List<V1IngressTLS> tlsList = ingress.getSpec().getTls();

            String namespace = Objects.requireNonNull(ingress.getMetadata()).getNamespace();

            if (Objects.isNull(rules) || CollectionUtils.isEmpty(rules)) {
                // if rules is null, brpcBackend become global default
                if (Objects.nonNull(brpcBackend) && Objects.nonNull(brpcBackend.getService())) {
                    IngressConfiguration defaultRouteConfig = getRpcRouteConfig(ingress.getMetadata().getAnnotations());
                    res.setGlobalDefaultBackend(Pair.of(Pair.of(namespace + "/" + ingress.getMetadata().getName(), brpcBackend.getService().getName()),
                            defaultRouteConfig));
                }
            } else {
                // if rules is not null, brpcBackend is default in this ingress
                List<IngressConfiguration> routeList = new ArrayList<>(rules.size());
                for (V1IngressRule ingressRule : rules) {
                    List<IngressConfiguration> routes = parseIngressRule(ingressRule,
                            Objects.requireNonNull(ingress.getMetadata()).getNamespace(), ingress.getMetadata().getLabels());
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
                            if (secret.getData() != null) {
                                InputStream keyCertChainInputStream = new ByteArrayInputStream(secret.getData().get("tls.crt"));
                                InputStream keyInputStream = new ByteArrayInputStream(secret.getData().get("tls.key"));
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
                        List<ConditionData> ruleConditionList = getRuleConditionList(metadataAnnotations);
                        RuleData ruleData = createRuleData(metadataAnnotations, ruleConditionList);
                        MetaData metaData = parseMetaData(metadataAnnotations, namespace);
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
        ruleCondition.setParamValue(annotations.get(IngressConstants.PLUGIN_BRPC_PATH));
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

    private SelectorData createSelectorData(final String path, final List<ConditionData> conditionList) {
        return SelectorData.builder()
                .pluginId(String.valueOf(PluginEnum.BRPC.getCode()))
                .pluginName(PluginEnum.BRPC.getName())
                .name(path)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .enabled(true)
                .logged(false)
                .continued(true)
                .conditionList(conditionList)
                .build();
    }

    private RuleData createRuleData(final Map<String, String> metadataAnnotations, final List<ConditionData> ruleConditionList) {
        return RuleData.builder()
                .name(metadataAnnotations.get(IngressConstants.PLUGIN_BRPC_PATH))
                .pluginName(PluginEnum.BRPC.getName())
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(ruleConditionList)
                .loged(true)
                .enabled(true)
                .build();
    }

    private MetaData parseMetaData(final Map<String, String> annotations, final String namespace) {
        String rpcExt = null;
        if (Objects.nonNull(annotations.get(IngressConstants.PLUGIN_BRPC_RPC_EXPAND))) {
            rpcExt = annotations.get(IngressConstants.PLUGIN_BRPC_RPC_EXPAND);
            JsonParser parser = new JsonParser();
            JsonObject jsonObject = parser.parse(rpcExt).getAsJsonObject();
            String host = jsonObject.get("host").getAsString();
            if (!isCompleteHost(host)) {
                V1Endpoints v1Endpoints = endpointsLister.namespace(namespace).get(host);
                List<V1EndpointSubset> subsets = v1Endpoints.getSubsets();
                if (Objects.isNull(subsets) || CollectionUtils.isEmpty(subsets)) {
                    LOG.info("Endpoints do not have subsets");
                } else {
                    for (V1EndpointSubset subset : subsets) {
                        List<V1EndpointAddress> addresses = subset.getAddresses();
                        if (Objects.isNull(addresses) || addresses.isEmpty()) {
                            continue;
                        }
                        for (V1EndpointAddress address : addresses) {
                            host = address.getIp();
                            jsonObject.addProperty("host", host);
                            rpcExt = jsonObject.toString();
                        }
                    }
                }
            }
        }
        return MetaData.builder()
                .appName(annotations.get(IngressConstants.PLUGIN_BRPC_APP_NAME))
                .path(annotations.get(IngressConstants.PLUGIN_BRPC_PATH))
                .contextPath(annotations.get(IngressConstants.PLUGIN_CONTEXT_PATH_PATH))
                .rpcType(annotations.get(IngressConstants.PLUGIN_BRPC_RPC_TYPE))
                .rpcExt(rpcExt)
                .serviceName(annotations.get(IngressConstants.PLUGIN_BRPC_SERVICE_NAME))
                .methodName(annotations.get(IngressConstants.PLUGIN_BRPC_METHOD_NAME))
                .parameterTypes(annotations.get(IngressConstants.PLUGIN_BRPC_PARAMS_TYPE))
                .enabled(true)
                .build();
    }

    private IngressConfiguration getRpcRouteConfig(final Map<String, String> annotations) {
        final ConditionData conditionData = new ConditionData();
        conditionData.setParamName("brpc");
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.PATH_PATTERN.getAlias());
        conditionData.setParamValue("/**");

        final SelectorData selectorData = SelectorData.builder()
                .name("brpc-selector")
                .sort(Integer.MAX_VALUE)
                .conditionList(Collections.singletonList(conditionData))
                .enabled(true)
                .id(IngressConstants.ID)
                .pluginName(PluginEnum.BRPC.getName())
                .pluginId(String.valueOf(PluginEnum.BRPC.getCode()))
                .logged(false)
                .continued(true)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.FULL_FLOW.getCode()).build();

        final RuleData ruleData = RuleData.builder()
                .selectorId(IngressConstants.ID)
                .pluginName(PluginEnum.BRPC.getName())
                .name("brpc-rule")
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(Collections.singletonList(conditionData))
                .loged(false)
                .enabled(true)
                .sort(Integer.MAX_VALUE).build();

        MetaData metaData = new MetaData();
        if (Objects.nonNull(annotations)) {
            metaData.setAppName(annotations.getOrDefault(IngressConstants.PLUGIN_BRPC_APP_NAME, "brpc"));
            metaData.setMethodName(annotations.getOrDefault(IngressConstants.PLUGIN_BRPC_METHOD_NAME, "allName"));
            metaData.setPath(annotations.getOrDefault(IngressConstants.PLUGIN_BRPC_PATH, "/brpc/allName"));
            metaData.setRpcType(annotations.getOrDefault(IngressConstants.PLUGIN_BRPC_RPC_TYPE, RpcTypeEnum.BRPC.getName()));
            metaData.setServiceName(annotations.getOrDefault(IngressConstants.PLUGIN_BRPC_SERVICE_NAME, "org.apache.shenyu.examples.brpc.api.service.BrpcDemoService"));
            metaData.setContextPath(annotations.getOrDefault(IngressConstants.PLUGIN_BRPC_CONTEXT_PATH, "/brpc"));
            metaData.setRpcExt(annotations.getOrDefault(IngressConstants.PLUGIN_BRPC_RPC_EXT, "{\\\"methodInfo\\\":[{\\\"methodName\\\":\\\"allName\\\",\\\"paramTypes\\\":[]}]}"));
            metaData.setServiceName(annotations.getOrDefault(IngressConstants.PLUGIN_BRPC_SERVICE_NAME, "org.apache.shenyu.examples.brpc.api.service.BrpcDemoService"));
            metaData.setParameterTypes(annotations.getOrDefault(IngressConstants.PLUGIN_BRPC_PARAMETER_TYPE, ""));
            metaData.setEnabled(Boolean.parseBoolean(annotations.getOrDefault(IngressConstants.PLUGIN_BRPC_ENABLED, "true")));
        }
        return new IngressConfiguration(selectorData, Arrays.asList(ruleData), Arrays.asList(metaData));
    }
}
