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

package org.apache.shenyu.k8s.reconciler;

import io.kubernetes.client.extended.controller.reconciler.Reconciler;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.controller.reconciler.Result;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubset;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1HTTPIngressPath;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1Secret;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ssl.ShenyuSniAsyncMapping;
import org.apache.shenyu.common.config.ssl.SslCrtAndKeyStream;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.PluginRoleEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.k8s.cache.IngressCache;
import org.apache.shenyu.k8s.cache.IngressSecretCache;
import org.apache.shenyu.k8s.cache.IngressSelectorCache;
import org.apache.shenyu.k8s.cache.ServiceIngressCache;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.apache.shenyu.k8s.parser.IngressParser;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The Reconciler of Ingress.
 */
public class IngressReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(IngressReconciler.class);

    // ingressName serviceName selectorData ruleData
    private static Pair<Pair<String, String>, IngressConfiguration> globalDefaultBackend;

    private final Lister<V1Ingress> ingressLister;

    private final Lister<V1Secret> secretLister;

    private final ShenyuCacheRepository shenyuCacheRepository;

    private final ShenyuSniAsyncMapping shenyuSniAsyncMapping;

    private final IngressParser ingressParser;

    private final ApiClient apiClient;

    /**
     * IngressReconciler Constructor.
     *
     * @param ingressInformer       ingressInformer
     * @param secretInformer        secretInformer
     * @param shenyuCacheRepository shenyuCacheRepository
     * @param shenyuSniAsyncMapping shenyuSniAsyncMapping
     * @param ingressParser         ingressParser
     * @param apiClient             apiClient
     */
    public IngressReconciler(final SharedIndexInformer<V1Ingress> ingressInformer,
                             final SharedIndexInformer<V1Secret> secretInformer,
                             final ShenyuCacheRepository shenyuCacheRepository,
                             final ShenyuSniAsyncMapping shenyuSniAsyncMapping,
                             final IngressParser ingressParser,
                             final ApiClient apiClient) {
        this.ingressLister = new Lister<>(ingressInformer.getIndexer());
        this.secretLister = new Lister<>(secretInformer.getIndexer());
        this.shenyuCacheRepository = shenyuCacheRepository;
        this.shenyuSniAsyncMapping = shenyuSniAsyncMapping;
        this.ingressParser = ingressParser;
        this.apiClient = apiClient;
        initPlugins(shenyuCacheRepository);
    }

    /**
     * Reconcile cycle.
     *
     * @param request request
     * @return reconcile result
     */
    @Override
    public Result reconcile(final Request request) {
        LOG.info("Starting to reconcile ingress {}", request);

        // Do not modify current ingress object directly
        final V1Ingress v1Ingress = this.ingressLister.namespace(request.getNamespace()).get(request.getName());
        final V1Ingress oldIngress = IngressCache.getInstance().get(request.getNamespace(), request.getName());
        Map<String, String> annotations = v1Ingress.getMetadata().getAnnotations();
        if (Objects.equals(annotations.get(IngressConstants.PLUGIN_DUBBO_ENABLED), "true")) {
            String zookeeperUrl = getZookeeperUrl(annotations, request);
            enablePlugin(shenyuCacheRepository, PluginEnum.DUBBO, zookeeperUrl);
        } else if (Objects.equals(annotations.get(IngressConstants.PLUGIN_MOTAN_ENABLED), "true")) {
            String zookeeperUrl = getZookeeperUrl(annotations, request);
            enablePlugin(shenyuCacheRepository, PluginEnum.MOTAN, zookeeperUrl);
        } else if (Objects.equals(annotations.get(IngressConstants.PLUGIN_SPRING_CLOUD_ENABLED), "true")) {
            enablePlugin(shenyuCacheRepository, PluginEnum.SPRING_CLOUD, null);
        } else if (Objects.equals(annotations.get(IngressConstants.PLUGIN_WEB_SOCKET_ENABLED), "true")) {
            enablePlugin(shenyuCacheRepository, PluginEnum.WEB_SOCKET, null);
        }
        if (Objects.isNull(v1Ingress)) {
            if (Objects.nonNull(oldIngress)) {
                // Delete ingress binding selectors
                doDeleteConfigByIngress(request, oldIngress);

                // Remove ssl config
                Set<String> sslDomainSet = IngressSecretCache.getInstance().getDomainByIngress(request.getNamespace(), request.getName());
                if (Objects.nonNull(sslDomainSet) && !sslDomainSet.isEmpty()) {
                    for (String sslDomain : sslDomainSet) {
                        Integer preDomainSslNums = IngressSecretCache.getInstance().getAndDecrementDomainNums(sslDomain);
                        if (preDomainSslNums == 1) {
                            shenyuSniAsyncMapping.removeSslCertificate(sslDomain);
                            LOG.info("Remove ssl config for domain {}", sslDomain);
                        }
                    }
                }
                IngressSecretCache.getInstance().removeDomainByIngress(request.getNamespace(), request.getName());

                IngressCache.getInstance().remove(request.getNamespace(), request.getName());
                LOG.info("Delete selector and rule for ingress {}", request);
            } else {
                LOG.info("Cannot find ingress {}", request);
            }
            return new Result(false);
        }

        if (!checkIngressClass(v1Ingress)) {
            LOG.info("IngressClass is not match {}", request);
            return new Result(false);
        }

        if (Objects.isNull(oldIngress)) {
            try {
                addNewIngressConfigToShenyu(v1Ingress, new CoreV1Api(apiClient));
            } catch (IOException e) {
                LOG.error("add new ingress config error", e);
            }
        } else if (needUpdate(oldIngress, v1Ingress)) {
            // Update logic
            // 1. clean old config
            doDeleteConfigByIngress(request, oldIngress);

            // 2. add new config
            try {
                addNewIngressConfigToShenyu(v1Ingress, new CoreV1Api(apiClient));
            } catch (IOException e) {
                LOG.error("add new ingress config error", e);
            }
        }
        IngressCache.getInstance().put(request.getNamespace(), request.getName(), v1Ingress);
        List<Pair<String, String>> serviceList = parseServiceFromIngress(v1Ingress);
        Objects.requireNonNull(serviceList).forEach(pair -> {
            ServiceIngressCache.getInstance().putIngressName(pair.getLeft(), pair.getRight(), request.getNamespace(), request.getName());
            LOG.info("Add service cache {} for ingress {}", pair.getLeft() + "/" + pair.getRight(), request.getNamespace() + "/" + request.getName());
        });

        return new Result(false);
    }

    private void doDeleteConfigByIngress(final Request request, final V1Ingress oldIngress) {
        List<String> selectorList = new ArrayList<>();
        if (Objects.equals(oldIngress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_DUBBO_ENABLED), "true")) {
            selectorList = deleteSelectorByIngressName(request.getNamespace(), request.getName(), PluginEnum.DUBBO.getName(),
                    oldIngress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_DUBBO_CONTEXT_PATH));
        } else if (Objects.equals(oldIngress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_MOTAN_ENABLED), "true")) {
            selectorList = deleteSelectorByIngressName(request.getNamespace(), request.getName(), PluginEnum.MOTAN.getName(),
                    oldIngress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_MOTAN_CONTEXT_PATH));
        } else if (Objects.equals(oldIngress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_SPRING_CLOUD_ENABLED), "true")) {
            selectorList = deleteSelectorByIngressName(request.getNamespace(), request.getName(), PluginEnum.SPRING_CLOUD.getName(), "");
        } else if (Objects.equals(oldIngress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_WEB_SOCKET_ENABLED), "true")) {
            selectorList = deleteSelectorByIngressName(request.getNamespace(), request.getName(), PluginEnum.WEB_SOCKET.getName(), "");
        } else {
            selectorList = deleteSelectorByIngressName(request.getNamespace(), request.getName(), PluginEnum.DIVIDE.getName(), "");
        }
        if (Objects.nonNull(selectorList) && !selectorList.isEmpty()) {
            if (Objects.equals(oldIngress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_DUBBO_ENABLED), "true")) {
                IngressSelectorCache.getInstance().remove(request.getNamespace(), request.getName(), PluginEnum.DUBBO.getName());
            } else if (Objects.equals(oldIngress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_MOTAN_ENABLED), "true")) {
                IngressSelectorCache.getInstance().remove(request.getNamespace(), request.getName(), PluginEnum.MOTAN.getName());
            } else if (Objects.equals(oldIngress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_SPRING_CLOUD_ENABLED), "true")) {
                IngressSelectorCache.getInstance().remove(request.getNamespace(), request.getName(), PluginEnum.SPRING_CLOUD.getName());
            } else if (Objects.equals(oldIngress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_WEB_SOCKET_ENABLED), "true")) {
                IngressSelectorCache.getInstance().remove(request.getNamespace(), request.getName(), PluginEnum.WEB_SOCKET.getName());
            } else {
                IngressSelectorCache.getInstance().remove(request.getNamespace(), request.getName(), PluginEnum.DIVIDE.getName());
            }
        }
        List<Pair<String, String>> serviceList = parseServiceFromIngress(oldIngress);
        Objects.requireNonNull(serviceList).forEach(pair -> {
            ServiceIngressCache.getInstance().removeSpecifiedIngressName(pair.getLeft(), pair.getRight(), request.getNamespace(), request.getName());
            LOG.info("Delete service cache {} for ingress {}", pair.getLeft() + "/" + pair.getRight(), request.getNamespace() + "/" + request.getName());
        });
        deleteGlobalDefaultBackend(request.getNamespace(), request.getName());
    }

    private void deleteGlobalDefaultBackend(final String namespace, final String name) {
        if (Objects.nonNull(globalDefaultBackend) && (namespace + "/" + name).equals(globalDefaultBackend.getLeft().getLeft())) {
            globalDefaultBackend = null;
        }
    }

    private void initPlugins(final ShenyuCacheRepository shenyuCacheRepository) {
        enablePlugin(shenyuCacheRepository, PluginEnum.GLOBAL, null);
        enablePlugin(shenyuCacheRepository, PluginEnum.URI, null);
        enablePlugin(shenyuCacheRepository, PluginEnum.NETTY_HTTP_CLIENT, null);
        enablePlugin(shenyuCacheRepository, PluginEnum.DIVIDE, null);
        enablePlugin(shenyuCacheRepository, PluginEnum.GENERAL_CONTEXT, null);
    }

    private void enablePlugin(final ShenyuCacheRepository shenyuCacheRepository, final PluginEnum pluginEnum, final String zookeeperUrl) {
        PluginData pluginData = PluginData.builder()
                .id(String.valueOf(pluginEnum.getCode()))
                .name(pluginEnum.getName())
                .config(getPluginConfig(pluginEnum, zookeeperUrl))
                .role(PluginRoleEnum.SYS.getName())
                .enabled(true)
                .sort(pluginEnum.getCode())
                .build();
        shenyuCacheRepository.saveOrUpdatePluginData(pluginData);
    }

    private String getPluginConfig(final PluginEnum pluginEnum, final String zookeeperUrl) {
        switch (pluginEnum) {
            case DIVIDE:
                return "{multiSelectorHandle: 1, multiRuleHandle:0}";
            case DUBBO:
                return "{\"register\":\"" + zookeeperUrl + "\",\"multiSelectorHandle\":\"1\",\"threadpool\":\"shared\",\"corethreads\":0,\"threads\":2147483647,\"queues\":0}";
            case MOTAN:
                return "{\"registerProtocol\":\"zk\",\"registerAddress\":\"" + zookeeperUrl + "\",\"corethreads\":0,\"threads\":2147483647,\"queues\":0,\"threadpool\":\"shared\"}";
            case WEB_SOCKET:
                return "{multiSelectorHandle: 1}";
            default:
                return null;
        }
    }

    private String getZookeeperUrl(final Map<String, String> annotations, final Request request) {
        String zookeeperK8sUrl = annotations.get(IngressConstants.ZOOKEEPER_REGISTER_ADDRESS);
        String zookeeperUrl = null;
        String zookeeperK8sIpUrl = extractTarget(zookeeperK8sUrl);
        if (isCorrectIp(zookeeperK8sIpUrl)) {
            zookeeperUrl = annotations.get(IngressConstants.ZOOKEEPER_REGISTER_ADDRESS);
        } else {
            Lister<V1Endpoints> endpointsLister = ingressParser.getEndpointsLister();
            V1Endpoints v1Endpoints = endpointsLister.namespace(request.getNamespace()).get(zookeeperK8sIpUrl);
            List<V1EndpointSubset> subsets = v1Endpoints.getSubsets();
            if (Objects.isNull(subsets) || CollectionUtils.isEmpty(subsets)) {
                LOG.info("Endpoints {} do not have subsets", v1Endpoints);
            } else {
                for (V1EndpointSubset subset : subsets) {
                    List<V1EndpointAddress> addresses = subset.getAddresses();
                    if (Objects.isNull(addresses) || addresses.isEmpty()) {
                        continue;
                    }
                    for (V1EndpointAddress address : addresses) {
                        zookeeperUrl = address.getIp();
                    }
                }
            }
        }
        if (!isCorrectIp(zookeeperUrl)) {
            LOG.info("Please enter the correct zookeeperUrl address");
            throw new ShenyuException("zookeeper url:" + zookeeperUrl + " is is error.");
        }
        zookeeperUrl = replaceAndCombine(zookeeperK8sUrl, zookeeperUrl);
        return zookeeperUrl;
    }

    private String extractTarget(final String inputString) {
        Pattern pattern = Pattern.compile("://(.*?)(:|$)");
        Matcher matcher = pattern.matcher(inputString);

        if (matcher.find()) {
            return matcher.group(1);
        } else {
            return null;
        }
    }

    private String replaceAndCombine(final String inputString, final String targetHost) {
        Pattern pattern = Pattern.compile("://(.*?)(:|$)");
        Matcher matcher = pattern.matcher(inputString);

        if (matcher.find()) {
            String originalHost = matcher.group(1);
            String replacedString = inputString.replace(originalHost, targetHost);
            return replacedString;
        } else {
            return inputString;
        }
    }

    private boolean isCorrectIp(final String ipString) {
        if (ipString.length() < 7 || ipString.length() > 15) {
            return false;
        }
        String[] ipArray = ipString.split("\\.");
        if (ipArray.length != 4) {
            return false;
        }
        for (int i = 0; i < ipArray.length; i++) {
            try {
                int number = Integer.parseInt(ipArray[i]);
                if (number < 0 || number > 255) {
                    return false;
                }
            } catch (Exception e) {
                return false;
            }
        }
        return true;
    }

    /**
     * Check whether the IngressClass is shenyu, check the annotation first.
     *
     * @param v1Ingress v1Ingress
     * @return boolean
     */
    private boolean checkIngressClass(final V1Ingress v1Ingress) {
        if (Objects.nonNull(v1Ingress.getMetadata())) {
            Map<String, String> annotations = v1Ingress.getMetadata().getAnnotations();
            if (Objects.nonNull(annotations)
                    && Objects.nonNull(annotations.get(IngressConstants.K8S_INGRESS_CLASS_ANNOTATION_KEY))) {
                return IngressConstants.SHENYU_INGRESS_CLASS.equals(annotations.get(IngressConstants.K8S_INGRESS_CLASS_ANNOTATION_KEY));
            } else {
                return Objects.nonNull(v1Ingress.getSpec()) && IngressConstants.SHENYU_INGRESS_CLASS.equals(v1Ingress.getSpec().getIngressClassName());
            }
        } else {
            return false;
        }
    }

    private List<String> deleteSelectorByIngressName(final String namespace, final String name,
                                                     final String pluginName, final String path) {
        final List<String> selectorList = IngressSelectorCache.getInstance().get(namespace, name, pluginName);
        if (Objects.nonNull(selectorList) && !selectorList.isEmpty()) {
            for (String selectorId : selectorList) {
                List<RuleData> ruleList = shenyuCacheRepository.findRuleDataList(selectorId);
                // To avoid ConcurrentModificationException, copy the ruleId to list
                List<String> ruleIdList = new ArrayList<>();
                ruleList.forEach(rule -> ruleIdList.add(rule.getId()));
                for (String id : ruleIdList) {
                    MetaData metaData = shenyuCacheRepository.findMetaData(path);
                    if (Objects.nonNull(metaData)) {
                        shenyuCacheRepository.deleteMetaData(metaData);
                    }
                    shenyuCacheRepository.deleteRuleData(pluginName, selectorId, id);
                }
                shenyuCacheRepository.deleteSelectorData(pluginName, selectorId);
            }
        }
        return selectorList;
    }

    private List<Pair<String, String>> parseServiceFromIngress(final V1Ingress ingress) {
        List<Pair<String, String>> res = new ArrayList<>();
        if (Objects.isNull(ingress) || Objects.isNull(ingress.getSpec())) {
            return res;
        }
        String namespace = Objects.requireNonNull(ingress.getMetadata()).getNamespace();
        String name = ingress.getMetadata().getName();
        String namespacedName = namespace + "/" + name;
        String defaultService = null;
        if (ingress.getSpec().getDefaultBackend() != null && ingress.getSpec().getDefaultBackend().getService() != null) {
            defaultService = ingress.getSpec().getDefaultBackend().getService().getName();
            if (Objects.isNull(ingress.getSpec().getRules())) {
                if (globalDefaultBackend != null) {
                    if (globalDefaultBackend.getLeft().getLeft().equals(namespacedName)) {
                        res.add(Pair.of(namespace, defaultService));
                    }
                } else {
                    res.add(Pair.of(namespace, defaultService));
                }
                return res;
            }
        }
        Set<String> deduplicateSet = new HashSet<>();
        if (Objects.isNull(ingress.getSpec().getRules())) {
            return res;
        }
        for (V1IngressRule rule : ingress.getSpec().getRules()) {
            if (Objects.nonNull(rule.getHttp()) && Objects.nonNull(rule.getHttp().getPaths())) {
                for (V1HTTPIngressPath path : rule.getHttp().getPaths()) {
                    if (Objects.nonNull(path.getBackend()) && Objects.nonNull(path.getBackend().getService())) {
                        if (!deduplicateSet.contains(path.getBackend().getService().getName())) {
                            res.add(Pair.of(namespace, path.getBackend().getService().getName()));
                            deduplicateSet.add(path.getBackend().getService().getName());
                        }
                    } else {
                        if (Objects.nonNull(defaultService) && !deduplicateSet.contains(defaultService)) {
                            res.add(Pair.of(namespace, defaultService));
                            deduplicateSet.add(defaultService);
                        }
                    }
                }
            }
        }
        return res;
    }

    private boolean needUpdate(final V1Ingress oldIngress, final V1Ingress currentIngress) {
        return !oldIngress.equals(currentIngress);
    }

    private void addNewIngressConfigToShenyu(final V1Ingress v1Ingress, final CoreV1Api apiClient) throws IOException {
        V1Ingress ingressCopy = new V1IngressBuilder(v1Ingress).build();
        ShenyuMemoryConfig shenyuMemoryConfig = ingressParser.parse(ingressCopy, apiClient);
        String pluginName = getPluginName(ingressCopy);
        if (Objects.nonNull(shenyuMemoryConfig)) {
            List<IngressConfiguration> routeConfigList = shenyuMemoryConfig.getRouteConfigList();
            if (Objects.isNull(routeConfigList)) {
                return;
            }
            routeConfigList.forEach(routeConfig -> {
                SelectorData selectorData = routeConfig.getSelectorData();
                if (Objects.isNull(selectorData)) {
                    return;
                }
                selectorData.setId(IngressSelectorCache.getInstance().generateSelectorId());
                selectorData.setSort(100);
                shenyuCacheRepository.saveOrUpdateSelectorData(selectorData);
                List<RuleData> ruleDataList = routeConfig.getRuleDataList();
                ruleDataList.forEach(ruleData -> {
                    if (Objects.isNull(ruleData)) {
                        shenyuCacheRepository.deleteSelectorData(selectorData.getPluginName(), selectorData.getId());
                        return;
                    }
                    ruleData.setId(IngressSelectorCache.getInstance().generateRuleId());
                    ruleData.setSelectorId(selectorData.getId());
                    ruleData.setSort(100);
                    shenyuCacheRepository.saveOrUpdateRuleData(ruleData);
                    IngressSelectorCache.getInstance().put(Objects.requireNonNull(v1Ingress.getMetadata()).getNamespace(),
                            v1Ingress.getMetadata().getName(), pluginName, selectorData.getId());
                });
                List<MetaData> metaDataList = routeConfig.getMetaDataList();
                if (Objects.nonNull(metaDataList)) {
                    metaDataList.forEach(metaData -> {
                        if (Objects.nonNull(metaData)) {
                            metaData.setId(IngressSelectorCache.getInstance().generateMetaDataId());
                            shenyuCacheRepository.saveOrUpdateMetaData(metaData);
                        }
                    });
                }
            });
            if (Objects.nonNull(shenyuMemoryConfig.getGlobalDefaultBackend())) {
                synchronized (IngressReconciler.class) {
                    if (globalDefaultBackend == null) {
                        // Add a default backend
                        shenyuCacheRepository.saveOrUpdateSelectorData(shenyuMemoryConfig.getGlobalDefaultBackend().getRight().getSelectorData());
                        shenyuMemoryConfig.getGlobalDefaultBackend().getRight().getRuleDataList().forEach(shenyuCacheRepository::saveOrUpdateRuleData);
                        shenyuMemoryConfig.getGlobalDefaultBackend().getRight().getMetaDataList().forEach(shenyuCacheRepository::saveOrUpdateMetaData);
                        globalDefaultBackend = shenyuMemoryConfig.getGlobalDefaultBackend();
                        IngressSelectorCache.getInstance().put(Objects.requireNonNull(v1Ingress.getMetadata()).getNamespace(),
                                v1Ingress.getMetadata().getName(), pluginName, shenyuMemoryConfig.getGlobalDefaultBackend().getRight().getSelectorData().getId());
                    }
                }
            }
            List<SslCrtAndKeyStream> tlsConfigList = shenyuMemoryConfig.getTlsConfigList();
            if (Objects.nonNull(tlsConfigList)) {
                final String namespace = Objects.requireNonNull(v1Ingress.getMetadata()).getNamespace();
                final String ingressName = v1Ingress.getMetadata().getName();
                Set<String> oldDomainSet = Optional.ofNullable(IngressSecretCache.getInstance().removeDomainByIngress(namespace, ingressName)).orElse(new HashSet<>());
                Set<String> newDomainSet = new HashSet<>();
                for (SslCrtAndKeyStream sslCrtAndKeyStream : tlsConfigList) {
                    final String domain = sslCrtAndKeyStream.getDomain();
                    if (!oldDomainSet.contains(domain)) {
                        if (IngressSecretCache.getInstance().getAndIncrementDomainNums(domain) == 0) {
                            shenyuSniAsyncMapping.addSslCertificate(sslCrtAndKeyStream);
                            LOG.info("Add ssl config for domain {}", domain);
                        }
                    }
                    newDomainSet.add(domain);
                }
                oldDomainSet.removeAll(newDomainSet);
                oldDomainSet.forEach(domain -> {
                    if (IngressSecretCache.getInstance().getAndDecrementDomainNums(domain) == 1) {
                        shenyuSniAsyncMapping.removeSslCertificate(domain);
                        LOG.info("Remove ssl config for domain {}", domain);
                    }
                });
                IngressSecretCache.getInstance().putDomainByIngress(namespace, ingressName, newDomainSet);
            }
        }
    }

    private String getPluginName(final V1Ingress ingress) {
        String pluginName;
        String pluginDubboEnabled = ingress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_DUBBO_ENABLED);
        String pluginMotanEnabled = ingress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_MOTAN_ENABLED);
        String pluginSpringCloudEnabled = ingress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_SPRING_CLOUD_ENABLED);
        String pluginWebSocketEnabled = ingress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_WEB_SOCKET_ENABLED);
        if ((Boolean.TRUE.toString()).equals(pluginDubboEnabled)) {
            pluginName = PluginEnum.DUBBO.getName();
        } else if ((Boolean.TRUE.toString()).equals(pluginMotanEnabled)) {
            pluginName = PluginEnum.MOTAN.getName();
        } else if ((Boolean.TRUE.toString()).equals(pluginSpringCloudEnabled)) {
            pluginName = PluginEnum.SPRING_CLOUD.getName();
        } else if ((Boolean.TRUE.toString()).equals(pluginWebSocketEnabled)) {
            pluginName = PluginEnum.WEB_SOCKET.getName();
        } else {
            pluginName = PluginEnum.DIVIDE.getName();
        }
        return pluginName;
    }
}
