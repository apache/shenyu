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
import io.kubernetes.client.openapi.models.V1ServiceBackendPort;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ssl.SslCrtAndKeyStream;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.GrpcRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
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

public class GrpcParser implements K8sResourceParser<V1Ingress> {
    private static final Logger LOG = LoggerFactory.getLogger(GrpcParser.class);

    private final Lister<V1Service> serviceLister;

    private final Lister<V1Endpoints> endpointsLister;

    /**
     * GrpcIngressParser Constructor.
     *
     * @param serviceInformer   serviceInformer
     * @param endpointsInformer endpointsInformer
     */
    public GrpcParser(final Lister<V1Service> serviceInformer, final Lister<V1Endpoints> endpointsInformer) {
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
            // parse the grpc backend
            V1IngressBackend grpcBackend = ingress.getSpec().getDefaultBackend();
            List<V1IngressRule> rules = ingress.getSpec().getRules();
            List<V1IngressTLS> tlsList = ingress.getSpec().getTls();

            String namespace = Objects.requireNonNull(ingress.getMetadata()).getNamespace();
            List<GrpcUpstream> grpcDefaultUpstreamList = parseDefaultService(grpcBackend, namespace);

            if (Objects.isNull(rules) || CollectionUtils.isEmpty(rules)) {
                // if rules is null, grpcBackend become global default
                if (Objects.nonNull(grpcBackend) && Objects.nonNull(grpcBackend.getService())) {
                    IngressConfiguration defaultRouteConfig = getRpcRouteConfig(grpcDefaultUpstreamList, ingress.getMetadata().getAnnotations());
                    res.setGlobalDefaultBackend(Pair.of(Pair.of(namespace + "/" + ingress.getMetadata().getName(), grpcBackend.getService().getName()),
                            defaultRouteConfig));
                }
            } else {
                // if rules is not null, grpcBackend is default in this ingress
                List<IngressConfiguration> routeList = new ArrayList<>(rules.size());
                for (V1IngressRule ingressRule : rules) {
                    List<IngressConfiguration> routes = parseIngressRule(ingressRule,
                            Objects.requireNonNull(ingress.getMetadata()).getNamespace(),
                            ingress.getMetadata().getAnnotations(), ingress.getMetadata().getLabels());
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
                            if (Objects.nonNull(secret.getData())) {
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

    private List<GrpcUpstream> parseDefaultService(final V1IngressBackend defaultBackend, final String namespace) {
        List<GrpcUpstream> defaultUpstreamList = new ArrayList<>();
        if (Objects.nonNull(defaultBackend) && Objects.nonNull(defaultBackend.getService())) {
            String serviceName = defaultBackend.getService().getName();
            // shenyu routes directly to the container
            V1Endpoints v1Endpoints = endpointsLister.namespace(namespace).get(serviceName);
            List<V1EndpointSubset> subsets = v1Endpoints.getSubsets();
            if (Objects.isNull(subsets) || CollectionUtils.isEmpty(subsets)) {
                LOG.info("Endpoints {} do not have subsets", serviceName);
            } else {
                for (V1EndpointSubset subset : subsets) {
                    List<V1EndpointAddress> addresses = subset.getAddresses();
                    if (Objects.isNull(addresses) || CollectionUtils.isEmpty(addresses)) {
                        continue;
                    }
                    for (V1EndpointAddress address : addresses) {
                        String upstreamIp = address.getIp();
                        String defaultPort = parsePort(defaultBackend.getService());
                        if (Objects.nonNull(defaultPort)) {
                            GrpcUpstream upstream = GrpcUpstream.builder()
                                    .upstreamUrl(upstreamIp + ":" + defaultPort)
                                    .weight(50).build();
                            defaultUpstreamList.add(upstream);
                        }
                    }
                }
            }
        }
        return defaultUpstreamList;
    }

    private List<GrpcUpstream> parseUpstream(final V1IngressBackend backend, final String namespace) {
        List<GrpcUpstream> upstreamList = new ArrayList<>();
        if (Objects.nonNull(backend) && Objects.nonNull(backend.getService()) && Objects.nonNull(backend.getService().getName())) {
            String serviceName = backend.getService().getName();
            // shenyu routes directly to the container
            V1Endpoints v1Endpoints = endpointsLister.namespace(namespace).get(serviceName);
            List<V1EndpointSubset> subsets = v1Endpoints.getSubsets();

            if (Objects.isNull(subsets) || CollectionUtils.isEmpty(subsets)) {
                LOG.info("Endpoints {} do not have subsets", serviceName);
            } else {
                for (V1EndpointSubset subset : subsets) {
                    List<V1EndpointAddress> addresses = subset.getAddresses();
                    if (Objects.isNull(addresses) || addresses.isEmpty()) {
                        continue;
                    }
                    for (V1EndpointAddress address : addresses) {
                        String upstreamIp = address.getIp();
                        String defaultPort = parsePort(backend.getService());
                        if (Objects.nonNull(defaultPort)) {
                            GrpcUpstream upstream = GrpcUpstream.builder()
                                    .upstreamUrl(upstreamIp + ":" + defaultPort)
                                    .weight(100).build();
                            upstreamList.add(upstream);
                        }
                    }
                }
            }
        }
        return upstreamList;
    }

    private String parsePort(final V1IngressServiceBackend service) {
        V1ServiceBackendPort servicePort = service.getPort();
        if (Objects.nonNull(servicePort)) {
            Integer portNumber = servicePort.getNumber();
            if (Objects.nonNull(portNumber) && portNumber > 0) {
                return String.valueOf(portNumber);
            } else {
                String servicePortName = servicePort.getName();
                if (Objects.nonNull(servicePortName) && StringUtils.isNoneBlank(servicePortName.trim())) {
                    return servicePortName.trim();
                }
            }
        }
        return null;
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
                    List<GrpcUpstream> grpcUpstreamList = parseUpstream(path.getBackend(), namespace);

                    SelectorData selectorData = createSelectorData(pathPath, conditionList, grpcUpstreamList);
                    List<RuleData> ruleDataList = new ArrayList<>();
                    List<MetaData> metaDataList = new ArrayList<>();
                    for (String label : labels.keySet()) {
                        Map<String, String> metadataAnnotations = serviceLister.namespace(namespace).get(labels.get(label)).getMetadata().getAnnotations();
                        List<ConditionData> ruleConditionList = getRuleConditionList(metadataAnnotations);
                        RuleData ruleData = createRuleData(metadataAnnotations, ruleConditionList, annotations);
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
        ruleCondition.setParamValue(annotations.get(IngressConstants.PLUGIN_GRPC_PATH));
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

    private SelectorData createSelectorData(final String path, final List<ConditionData> conditionList, final List<GrpcUpstream> grpcUpstreamList) {
        return SelectorData.builder()
                .pluginId(String.valueOf(PluginEnum.GRPC.getCode()))
                .pluginName(PluginEnum.GRPC.getName())
                .name(path)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .handle(GsonUtils.getInstance().toJson(grpcUpstreamList))
                .enabled(true)
                .logged(false)
                .continued(true)
                .conditionList(conditionList)
                .build();
    }

    private RuleData createRuleData(final Map<String, String> metadataAnnotations, final List<ConditionData> ruleConditionList, final Map<String, String> annotations) {
        GrpcRuleHandle grpcRuleHandle = new GrpcRuleHandle();
        if (Objects.nonNull(annotations)) {
            grpcRuleHandle.setLoadBalance(annotations.getOrDefault(IngressConstants.LOADBALANCER_ANNOTATION_KEY, "random"));
        }
        return RuleData.builder()
                .name(metadataAnnotations.get(IngressConstants.PLUGIN_GRPC_PATH))
                .pluginName(PluginEnum.GRPC.getName())
                .matchMode(MatchModeEnum.AND.getCode())
                .handle(GsonUtils.getInstance().toJson(grpcRuleHandle))
                .conditionDataList(ruleConditionList)
                .loged(true)
                .enabled(true)
                .build();
    }

    private MetaData parseMetaData(final Map<String, String> annotations) {
        return MetaData.builder()
                .appName(annotations.get(IngressConstants.PLUGIN_GRPC_APP_NAME))
                .path(annotations.get(IngressConstants.PLUGIN_GRPC_PATH))
                .contextPath(annotations.get(IngressConstants.PLUGIN_CONTEXT_PATH_PATH))
                .rpcType(annotations.get(IngressConstants.PLUGIN_GRPC_RPC_TYPE))
                .rpcExt(annotations.get(IngressConstants.PLUGIN_GRPC_RPC_EXPAND))
                .serviceName(annotations.get(IngressConstants.PLUGIN_GRPC_SERVICE_NAME))
                .methodName(annotations.get(IngressConstants.PLUGIN_GRPC_METHOD_NAME))
                .parameterTypes(annotations.get(IngressConstants.PLUGIN_GRPC_PARAMS_TYPE))
                .enabled(true)
                .build();
    }

    private IngressConfiguration getRpcRouteConfig(final List<GrpcUpstream> grpcUpStream, final Map<String, String> annotations) {
        final ConditionData conditionData = new ConditionData();
        conditionData.setParamName("grpc");
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.PATH_PATTERN.getAlias());
        conditionData.setParamValue("/**");

        final SelectorData selectorData = SelectorData.builder()
                .name("grpc-selector")
                .sort(Integer.MAX_VALUE)
                .conditionList(Collections.singletonList(conditionData))
                .handle(GsonUtils.getInstance().toJson(grpcUpStream))
                .enabled(true)
                .id(IngressConstants.ID)
                .pluginName(PluginEnum.GRPC.getName())
                .pluginId(String.valueOf(PluginEnum.GRPC.getCode()))
                .logged(false)
                .continued(true)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.FULL_FLOW.getCode()).build();

        GrpcRuleHandle grpcRuleHandle = new GrpcRuleHandle();
        if (Objects.nonNull(annotations)) {
            grpcRuleHandle.setLoadBalance(annotations.getOrDefault(IngressConstants.LOADBALANCER_ANNOTATION_KEY, LoadBalanceEnum.RANDOM.getName()));
        }

        final RuleData ruleData = RuleData.builder()
                .selectorId(IngressConstants.ID)
                .pluginName(PluginEnum.GRPC.getName())
                .name("grpc-rule")
                .handle(GsonUtils.getInstance().toJson(grpcRuleHandle))
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(Collections.singletonList(conditionData))
                .loged(false)
                .enabled(true)
                .sort(Integer.MAX_VALUE).build();

        MetaData metaData = new MetaData();
        if (Objects.nonNull(annotations)) {
            metaData.setAppName(annotations.getOrDefault(IngressConstants.PLUGIN_GRPC_APP_NAME, "grpc"));
            metaData.setMethodName(annotations.getOrDefault(IngressConstants.PLUGIN_GRPC_METHOD_NAME, "hello"));
            metaData.setPath(annotations.getOrDefault(IngressConstants.PLUGIN_GRPC_PATH, "/grpc/helloService/hello"));
            metaData.setRpcType(annotations.getOrDefault(IngressConstants.PLUGIN_GRPC_RPC_TYPE, RpcTypeEnum.GRPC.getName()));
            metaData.setServiceName(annotations.getOrDefault(IngressConstants.PLUGIN_GRPC_SERVICE_NAME, "hello.HelloService"));
            metaData.setContextPath(annotations.getOrDefault(IngressConstants.PLUGIN_GRPC_CONTEXT_PATH, "/grpc"));
            metaData.setRpcExt(annotations.getOrDefault(IngressConstants.PLUGIN_GRPC_RPC_EXPAND, "{\"timeout\":5000,\"methodType\":\"UNARY\"}"));
            metaData.setServiceName(annotations.getOrDefault(IngressConstants.PLUGIN_GRPC_SERVICE_NAME, "hello.HelloService"));
            metaData.setParameterTypes(annotations.getOrDefault(IngressConstants.PLUGIN_GRPC_PARAMETER_TYPE, "hello.HelloRequest,io.grpc.stub.StreamObserver"));
            metaData.setEnabled(Boolean.parseBoolean(annotations.getOrDefault(IngressConstants.PLUGIN_GRPC_ENABLED, "true")));
        }
        return new IngressConfiguration(selectorData, Arrays.asList(ruleData), Arrays.asList(metaData));
    }
}
