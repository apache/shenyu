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
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ssl.SslCrtAndKeyStream;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
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

/**
 * Parser of Ingress Dubbo Annotations
 */
public class DubboIngressParser implements K8sResourceParser<V1Ingress> {

    private static final Logger LOG = LoggerFactory.getLogger(DubboIngressParser.class);

    private final Lister<V1Service> serviceLister;

    private final Lister<V1Endpoints> endpointsLister;

    /**
     * DubboIngressParser Constructor.
     *
     * @param serviceInformer   serviceInformer
     * @param endpointsInformer endpointsInformer
     */
    public DubboIngressParser(final Lister<V1Service> serviceInformer, final Lister<V1Endpoints> endpointsInformer) {
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
            // Parse the dubbo backend
            V1IngressBackend dubboBackend = ingress.getSpec().getDefaultBackend();
            List<V1IngressRule> rules = ingress.getSpec().getRules();
            List<V1IngressTLS> tlsList = ingress.getSpec().getTls();

            String namespace = Objects.requireNonNull(ingress.getMetadata()).getNamespace();
            List<DubboUpstream> dubboUpstreamList = parseDubboService(dubboBackend, namespace);

            if (Objects.isNull(rules) || CollectionUtils.isEmpty(rules)) {
                // if rules is null, dubboBackend become global default
                if (Objects.nonNull(dubboBackend) && Objects.nonNull(dubboBackend.getService())) {
                    Pair<SelectorData, RuleData> defaultRouteConfig = getDubboRouteConfig(dubboUpstreamList, ingress.getMetadata().getAnnotations());
                    res.setGlobalDefaultBackend(Pair.of(Pair.of(namespace + "/" + ingress.getMetadata().getName(), dubboBackend.getService().getName()),
                            defaultRouteConfig));
                }
            } else {
                // if rules is not null, dubboBackend is default in this ingress
                List<Pair<SelectorData, RuleData>> routeList = new ArrayList<>(rules.size());
                for (V1IngressRule ingressRule : rules) {
                    List<Pair<SelectorData, RuleData>> routes = parseIngressRule(ingressRule, dubboUpstreamList,
                            Objects.requireNonNull(ingress.getMetadata()).getNamespace(), ingress.getMetadata().getAnnotations());
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

    private List<DubboUpstream> parseDubboService(final V1IngressBackend dubboBackend, final String namespace) {
        List<DubboUpstream> dubboUpstreamList = new ArrayList<>();
        if (Objects.nonNull(dubboBackend) && Objects.nonNull(dubboBackend.getService())) {
            String serviceName = dubboBackend.getService().getName();
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
                        String defaultPort = parsePort(dubboBackend.getService());
                        if (Objects.nonNull(defaultPort)) {
                            DubboUpstream upstream = DubboUpstream.builder().protocol("http://").upstreamHost("").upstreamUrl(upstreamIp + ":" + defaultPort)
                                    .status(true).warmup(50).timestamp(0).weight(100).build();
                            dubboUpstreamList.add(upstream);
                        }
                    }
                }
            }
        }
        return dubboUpstreamList;
    }

    private List<Pair<SelectorData, RuleData>> parseIngressRule(final V1IngressRule ingressRule,
                                                                final List<DubboUpstream> dubboUpstream,
                                                                final String namespace,
                                                                final Map<String, String> annotations) {
        List<Pair<SelectorData, RuleData>> res = new ArrayList<>();

        ConditionData hostCondition = null;
        if (Objects.nonNull(ingressRule.getHost())) {
            hostCondition = new ConditionData();
            hostCondition.setParamType(ParamTypeEnum.DOMAIN.getName());
            hostCondition.setOperator(OperatorEnum.EQ.getAlias());
            hostCondition.setParamValue(ingressRule.getHost());
        }
        if (Objects.nonNull(ingressRule.getHttp())) {
            List<V1HTTPIngressPath> paths = ingressRule.getHttp().getPaths();
            if (paths != null) {
                for (V1HTTPIngressPath path : paths) {
                    if (path.getPath() == null) {
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
                    pathCondition.setParamValue(path.getPath());
                    List<ConditionData> conditionList = new ArrayList<>(2);
                    if (Objects.nonNull(hostCondition)) {
                        conditionList.add(hostCondition);
                    }
                    conditionList.add(pathCondition);

                    SelectorData selectorData = SelectorData.builder()
                            .pluginId(String.valueOf(PluginEnum.DUBBO.getCode()))
                            .pluginName(PluginEnum.DUBBO.getName())
                            .name(path.getPath())
                            .matchMode(MatchModeEnum.AND.getCode())
                            .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                            .enabled(true)
                            .logged(false)
                            .continued(true)
                            .conditionList(conditionList).build();
                    List<DubboUpstream> upstreamList = parseUpstream(path.getBackend(), namespace);
                    if (upstreamList.isEmpty()) {
                        upstreamList = dubboUpstream;
                    }
                    selectorData.setHandle(GsonUtils.getInstance().toJson(upstreamList));

                    DubboRuleHandle dubboRuleHandle = new DubboRuleHandle();
                    if (Objects.nonNull(annotations)) {
                        dubboRuleHandle.setLoadbalance(annotations.getOrDefault(IngressConstants.PLUGIN_DUBBO_LOADBALANCE_ANNOTATION_KEY, LoadBalanceEnum.HASH.getName()));
                    }
                    RuleData ruleData = RuleData.builder()
                            .name(path.getPath())
                            .pluginName(PluginEnum.DUBBO.getName())
                            .matchMode(MatchModeEnum.AND.getCode())
                            .conditionDataList(conditionList)
                            .handle(GsonUtils.getInstance().toJson(dubboRuleHandle))
                            .loged(false)
                            .enabled(true).build();

                    res.add(Pair.of(selectorData, ruleData));
                }
            }
        }
        return res;
    }

    private String parsePort(final V1IngressServiceBackend service) {
        if (Objects.nonNull(service.getPort())) {
            if (Objects.nonNull(service.getPort().getNumber()) && service.getPort().getNumber() > 0) {
                return String.valueOf(service.getPort().getNumber());
            } else if (Objects.nonNull(service.getPort().getName()) && !"".equals(service.getPort().getName().trim())) {
                return service.getPort().getName().trim();
            }
        }
        return null;
    }

    private List<DubboUpstream> parseUpstream(final V1IngressBackend backend, final String namespace) {
        List<DubboUpstream> upstreamList = new ArrayList<>();
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
                    if (addresses == null || addresses.isEmpty()) {
                        continue;
                    }
                    for (V1EndpointAddress address : addresses) {
                        String upstreamIp = address.getIp();
                        String defaultPort = parsePort(backend.getService());
                        if (Objects.nonNull(defaultPort)) {
                            DubboUpstream upstream = DubboUpstream.builder().protocol("http://").upstreamHost("").upstreamUrl(upstreamIp + ":" + defaultPort)
                                    .status(true).warmup(50).timestamp(0).weight(100).build();
                            upstreamList.add(upstream);
                        }
                    }
                }
            }
        }
        return upstreamList;
    }

    private Pair<SelectorData, RuleData> getDubboRouteConfig(final List<DubboUpstream> duobboUpstream, final Map<String, String> annotations) {
        String id = UUIDUtils.getInstance().generateShortUuid();
        final ConditionData conditionData = new ConditionData();
        conditionData.setParamName("dubbo");
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.PATH_PATTERN.getAlias());
        conditionData.setParamValue("/**");

        final SelectorData selectorData = SelectorData.builder()
                .name("dubbo-selector")
                .sort(Integer.MAX_VALUE)
                .conditionList(Collections.singletonList(conditionData))
                .handle(GsonUtils.getInstance().toJson(duobboUpstream))
                .enabled(true)
                .id(id)
                .pluginName(PluginEnum.DUBBO.getName())
                .pluginId(String.valueOf(PluginEnum.DUBBO.getCode()))
                .logged(false)
                .continued(true)
                .matchMode(MatchModeEnum.AND.getCode())
                .type(SelectorTypeEnum.FULL_FLOW.getCode()).build();

        DubboRuleHandle dubboRuleHandle = new DubboRuleHandle();
        if (Objects.nonNull(annotations)) {
            dubboRuleHandle.setLoadbalance(annotations.getOrDefault(IngressConstants.PLUGIN_DUBBO_LOADBALANCE_ANNOTATION_KEY, LoadBalanceEnum.HASH.getName()));
        }
        final RuleData ruleData = RuleData.builder()
                .selectorId(id)
                .pluginName(PluginEnum.DIVIDE.getName())
                .name("dubbo-rule")
                .matchMode(MatchModeEnum.AND.getCode())
                .conditionDataList(Collections.singletonList(conditionData))
                .handle(GsonUtils.getInstance().toJson(dubboRuleHandle))
                .loged(false)
                .enabled(true)
                .sort(Integer.MAX_VALUE).build();

        return Pair.of(selectorData, ruleData);
    }
}
