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
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubset;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1HTTPIngressPath;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBackend;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressServiceBackend;
import io.kubernetes.client.openapi.models.V1IngressTLS;
import io.kubernetes.client.openapi.models.V1Secret;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ssl.SslCrtAndKeyStream;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class SpringCloudParser implements K8sResourceParser<V1Ingress> {

    private static final Logger LOG = LoggerFactory.getLogger(SpringCloudParser.class);

    private final Lister<V1Service> serviceLister;

    private final Lister<V1Endpoints> endpointsLister;

    /**
     * SpringCloudParser Constructor.
     *
     * @param serviceInformer   serviceInformer
     * @param endpointsInformer endpointsInformer
     */
    public SpringCloudParser(final Lister<V1Service> serviceInformer, final Lister<V1Endpoints> endpointsInformer) {
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
            // Parse the spring cloud backend
            V1IngressBackend springCloudBackend = ingress.getSpec().getDefaultBackend();
            List<V1IngressRule> rules = ingress.getSpec().getRules();
            List<V1IngressTLS> tlsList = ingress.getSpec().getTls();

            String namespace = Objects.requireNonNull(ingress.getMetadata()).getNamespace();

            if (Objects.isNull(rules) || CollectionUtils.isEmpty(rules)) {
                // if rules is null, springCloudBackend become global default
                if (Objects.nonNull(springCloudBackend) && Objects.nonNull(springCloudBackend.getService())) {
                    IngressConfiguration defaultRouteConfig = getSpringCloudRouteConfig(ingress.getMetadata().getAnnotations());
                    res.setGlobalDefaultBackend(Pair.of(Pair.of(namespace + "/" + ingress.getMetadata().getName(), springCloudBackend.getService().getName()),
                            defaultRouteConfig));
                }
            } else {
                // if rules is not null, springCloudBackend is default in this ingress
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

    private List<IngressConfiguration> parseIngressRule(final V1IngressRule ingressRule, final String namespace, final Map<String, String> annotations, final Map<String, String> labels) {
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

                    SpringCloudSelectorHandle springCloudSelectorHandle = createSpringCloudSelectorHandle(annotations, path, namespace);
                    SelectorData selectorData = createSelectorData(pathPath, conditionList, springCloudSelectorHandle);
                    SpringCloudRuleHandle ruleHandle = createSpringCloudRuleHandle(annotations);
                    List<RuleData> ruleDataList = new ArrayList<>();
                    List<MetaData> metaDataList = new ArrayList<>();
                    for (String label : labels.keySet()) {
                        Map<String, String> metadataAnnotations = serviceLister.namespace(namespace).get(labels.get(label)).getMetadata().getAnnotations();
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

    private SpringCloudSelectorHandle createSpringCloudSelectorHandle(final Map<String, String> annotations, final V1HTTPIngressPath path, final String namespace) {
        SpringCloudSelectorHandle springCloudSelectorHandle = new SpringCloudSelectorHandle();
        springCloudSelectorHandle.setServiceId(annotations.getOrDefault(IngressConstants.PLUGIN_SPRING_CLOUD_SERVICE_ID, "springCloud-test"));
        springCloudSelectorHandle.setGray(Boolean.parseBoolean(annotations.getOrDefault(IngressConstants.PLUGIN_SPRING_CLOUD_GRAY, "false")));
        springCloudSelectorHandle.setDivideUpstreams(parseDivideUpstream(path, namespace, annotations));
        return springCloudSelectorHandle;
    }

    private SelectorData createSelectorData(final String path, final List<ConditionData> conditionList, final SpringCloudSelectorHandle springCloudSelectorHandle) {
        return SelectorData.builder()
                .pluginId(String.valueOf(PluginEnum.SPRING_CLOUD.getCode()))
                .pluginName(PluginEnum.SPRING_CLOUD.getName())
                .name(path)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .enabled(true)
                .logged(false)
                .continued(true)
                .conditionList(conditionList)
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .build();
    }

    private SpringCloudRuleHandle createSpringCloudRuleHandle(final Map<String, String> annotations) {
        SpringCloudRuleHandle ruleHandle = new SpringCloudRuleHandle();
        ruleHandle.setPath(annotations.get(IngressConstants.PATH_ANNOTATION_KEY));
        ruleHandle.setTimeout(Long.parseLong(annotations.getOrDefault(IngressConstants.TIMEOUT_ANNOTATION_KEY, "3000")));
        ruleHandle.setLoadBalance(annotations.getOrDefault(IngressConstants.LOADBALANCER_ANNOTATION_KEY, LoadBalanceEnum.ROUND_ROBIN.getName()));
        return ruleHandle;
    }

    private RuleData createRuleData(final Map<String, String> metadataAnnotations, final SpringCloudRuleHandle ruleHandle, final List<ConditionData> ruleConditionList) {
        return RuleData.builder()
                .name(metadataAnnotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_PATH))
                .pluginName(PluginEnum.SPRING_CLOUD.getName())
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
        ruleCondition.setOperator(OperatorEnum.EQ.getAlias());
        ruleCondition.setParamType(ParamTypeEnum.URI.getName());
        ruleCondition.setParamValue(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_PATH));
        ruleConditionList.add(ruleCondition);
        return ruleConditionList;
    }

    private String parsePort(final V1IngressServiceBackend service) {
        if (Objects.nonNull(service.getPort())) {
            if (service.getPort().getNumber() != null && service.getPort().getNumber() > 0) {
                return String.valueOf(service.getPort().getNumber());
            } else if (service.getPort().getName() != null && StringUtils.isNoneBlank(service.getPort().getName().trim())) {
                return service.getPort().getName().trim();
            }
        }
        return null;
    }

    private List<DivideUpstream> parseDivideUpstream(final V1HTTPIngressPath path, final String namespace, final Map<String, String> annotations) {
        List<DivideUpstream> upstreamList = new ArrayList<>();
        if (Objects.nonNull(path) && Objects.nonNull(path.getBackend().getService()) && Objects.nonNull(path.getBackend().getService().getName())) {
            // shenyu routes directly to the container
            String serviceName = path.getBackend().getService().getName();
            V1Endpoints v1Endpoints = endpointsLister.namespace(namespace).get(serviceName);
            List<V1EndpointSubset> subsets = v1Endpoints.getSubsets();
            String[] protocols = annotations.get(IngressConstants.UPSTREAMS_PROTOCOL_ANNOTATION_KEY).split(",");
            if (Objects.isNull(subsets) || CollectionUtils.isEmpty(subsets)) {
                LOG.info("Endpoints {} do not have subsets", serviceName);
            } else {
                for (V1EndpointSubset subset : subsets) {
                    List<V1EndpointAddress> addresses = subset.getAddresses();
                    if (Objects.isNull(addresses) || addresses.isEmpty()) {
                        continue;
                    }
                    int i = 0;
                    for (V1EndpointAddress address : addresses) {
                        String upstreamIp = address.getIp();
                        String defaultPort = parsePort(path.getBackend().getService());
                        if (Objects.nonNull(defaultPort)) {
                            DivideUpstream upstream = new DivideUpstream();
                            upstream.setUpstreamUrl(upstreamIp + ":" + defaultPort);
                            upstream.setWeight(100);
                            upstream.setProtocol(Objects.isNull(protocols[i++]) ? "http://" : protocols[i++]);
                            upstream.setWarmup(0);
                            upstream.setStatus(true);
                            upstream.setUpstreamHost("");
                            upstreamList.add(upstream);
                        }
                    }
                }
            }
        }
        return upstreamList;
    }

    private MetaData parseMetaData(final Map<String, String> annotations) {
        return MetaData.builder()
                .appName(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_APP_NAME))
                .contextPath(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_CONTEXT_PATH))
                .path(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_PATH))
                .rpcType(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_RPC_TYPE))
                .serviceName(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_SERVICE_NAME))
                .methodName(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_METHOD_NAME))
                .rpcExt(annotations.getOrDefault(IngressConstants.PLUGIN_SPRING_CLOUD_RPC_EXT, null))
                .parameterTypes(annotations.getOrDefault(IngressConstants.PLUGIN_SPRING_CLOUD_PARAMENT_TYPE, null))
                .enabled(true)
                .build();
    }

    private IngressConfiguration getSpringCloudRouteConfig(final Map<String, String> annotations) {
        final ConditionData conditionData = new ConditionData();
        conditionData.setParamName("/springCloud");
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.STARTS_WITH.getAlias());
        conditionData.setParamValue("/springcloud");

        final SelectorData selectorData = SelectorData.builder()
                .name("/springcloud")
                .sort(1)
                .conditionList(Collections.singletonList(conditionData))
                .enabled(true)
                .id(IngressConstants.ID)
                .pluginName(PluginEnum.SPRING_CLOUD.getName())
                .pluginId(String.valueOf(PluginEnum.SPRING_CLOUD.getCode()))
                .logged(false)
                .continued(true)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.FULL_FLOW.getCode()).build();
        SpringCloudSelectorHandle springCloudSelectorHandle = new SpringCloudSelectorHandle();
        springCloudSelectorHandle.setServiceId(annotations.getOrDefault(IngressConstants.PLUGIN_SPRING_CLOUD_SERVICE_ID, "springCloud-test"));
        springCloudSelectorHandle.setGray(Boolean.parseBoolean(annotations.getOrDefault(IngressConstants.PLUGIN_SPRING_CLOUD_GRAY, "false")));
        selectorData.setHandle(GsonUtils.getInstance().toJson(springCloudSelectorHandle));

        SpringCloudRuleHandle springCloudRuleHandle = new SpringCloudRuleHandle();
        if (Objects.nonNull(annotations)) {
            springCloudRuleHandle.setPath(annotations.getOrDefault(IngressConstants.PATH_ANNOTATION_KEY, ""));
            springCloudRuleHandle.setTimeout(Long.parseLong(annotations.getOrDefault(IngressConstants.TIMEOUT_ANNOTATION_KEY, "3000")));
            springCloudRuleHandle.setLoadBalance(annotations.getOrDefault(IngressConstants.LOADBALANCER_ANNOTATION_KEY, LoadBalanceEnum.ROUND_ROBIN.getName()));
        }

        final ConditionData ruleConditionData = new ConditionData();
        conditionData.setParamName(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_PATH));
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_PATH));

        final RuleData ruleData = RuleData.builder()
                .selectorId(IngressConstants.ID)
                .pluginName(PluginEnum.SPRING_CLOUD.getName())
                .name("spring-cloud-rule")
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(Collections.singletonList(ruleConditionData))
                .loged(false)
                .enabled(true)
                .sort(Integer.MAX_VALUE).build();

        if (Objects.isNull(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_APP_NAME))
                || Objects.isNull(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_PATH))
                || Objects.isNull(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_RPC_TYPE))
                || Objects.isNull(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_SERVICE_NAME))
                || Objects.isNull(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_METHOD_NAME))) {
            LOG.error("spring cloud metadata is error, please check spring cloud service. annotations: [{}]", annotations);
            throw new ShenyuException(annotations + " is is missing.");
        }
        MetaData metaData = MetaData.builder()
                .appName(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_APP_NAME))
                .path(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_PATH))
                .rpcType(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_RPC_TYPE))
                .serviceName(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_SERVICE_NAME))
                .methodName(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_METHOD_NAME))
                .rpcExt(annotations.getOrDefault(IngressConstants.PLUGIN_SPRING_CLOUD_RPC_EXT, ""))
                .parameterTypes(annotations.getOrDefault(IngressConstants.PLUGIN_SPRING_CLOUD_PARAMENT_TYPE, ""))
                .enabled(true)
                .build();
        return new IngressConfiguration(selectorData, Collections.singletonList(ruleData), Collections.singletonList(metaData));
    }
}
