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
import io.kubernetes.client.openapi.models.V1HTTPIngressPath;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1Secret;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ssl.ShenyuSniAsyncMapping;
import org.apache.shenyu.common.config.ssl.SslCrtAndKeyStream;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.PluginRoleEnum;
import org.apache.shenyu.k8s.cache.IngressCache;
import org.apache.shenyu.k8s.cache.IngressSecretCache;
import org.apache.shenyu.k8s.cache.IngressSelectorCache;
import org.apache.shenyu.k8s.cache.ServiceIngressCache;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.apache.shenyu.k8s.parser.IngressParser;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Optional;

/**
 * The Reconciler of Ingress.
 */
public class IngressReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(IngressReconciler.class);

    // ingressName serviceName selectorData ruleData
    private static Pair<Pair<String, String>, Pair<SelectorData, RuleData>> globalDefaultBackend;

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
        List<String> selectorList = deleteSelectorByIngressName(request.getNamespace(), request.getName(), PluginEnum.DIVIDE.getName());
        if (Objects.nonNull(selectorList) && !selectorList.isEmpty()) {
            IngressSelectorCache.getInstance().remove(request.getNamespace(), request.getName(), PluginEnum.DIVIDE.getName());
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
        //GLOBAL
        PluginData globalPlugin = PluginData.builder()
            .id(String.valueOf(PluginEnum.GLOBAL.getCode()))
            .name(PluginEnum.GLOBAL.getName())
            .config("")
            .role(PluginRoleEnum.SYS.getName())
            .enabled(true)
            .sort(PluginEnum.GLOBAL.getCode())
            .build();
        shenyuCacheRepository.saveOrUpdatePluginData(globalPlugin);
        //uri
        PluginData uriPlugin = PluginData.builder()
            .id(String.valueOf(PluginEnum.URI.getCode()))
            .name(PluginEnum.URI.getName())
            .config("")
            .role(PluginRoleEnum.SYS.getName())
            .enabled(true)
            .sort(PluginEnum.URI.getCode())
            .build();
        shenyuCacheRepository.saveOrUpdatePluginData(uriPlugin);
        //nettyHttpClient
        PluginData webclientPlugin = PluginData.builder()
            .id(String.valueOf(PluginEnum.NETTY_HTTP_CLIENT.getCode()))
            .config("")
            .name(PluginEnum.NETTY_HTTP_CLIENT.getName())
            .role(PluginRoleEnum.SYS.getName())
            .enabled(true)
            .sort(PluginEnum.NETTY_HTTP_CLIENT.getCode())
            .build();
        shenyuCacheRepository.saveOrUpdatePluginData(webclientPlugin);
        //divide
        PluginData dividePlugin = PluginData.builder()
            .id(String.valueOf(PluginEnum.DIVIDE.getCode()))
            .name(PluginEnum.DIVIDE.getName())
            .config("{multiSelectorHandle: 1, multiRuleHandle:0}")
            .role(PluginRoleEnum.SYS.getName())
            .enabled(true)
            .sort(PluginEnum.DIVIDE.getCode())
            .build();
        shenyuCacheRepository.saveOrUpdatePluginData(dividePlugin);
        //GeneralContextPlugin
        PluginData generalContextPlugin = PluginData.builder()
            .id(String.valueOf(PluginEnum.GENERAL_CONTEXT.getCode()))
            .config("")
            .name(PluginEnum.GENERAL_CONTEXT.getName())
            .role(PluginRoleEnum.SYS.getName())
            .enabled(true)
            .sort(PluginEnum.GENERAL_CONTEXT.getCode())
            .build();
        shenyuCacheRepository.saveOrUpdatePluginData(generalContextPlugin);
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
        final String pluginName) {
        final List<String> selectorList = IngressSelectorCache.getInstance().get(namespace, name, pluginName);
        if (Objects.nonNull(selectorList) && !selectorList.isEmpty()) {
            for (String selectorId : selectorList) {
                List<RuleData> ruleList = shenyuCacheRepository.findRuleDataList(selectorId);
                // To avoid ConcurrentModificationException, copy the ruleId to list
                List<String> ruleIdList = new ArrayList<>();
                ruleList.forEach(rule -> ruleIdList.add(rule.getId()));
                for (String id : ruleIdList) {
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
        if (Objects.nonNull(shenyuMemoryConfig)) {
            List<Pair<SelectorData, RuleData>> routeConfigList = shenyuMemoryConfig.getRouteConfigList();
            List<SslCrtAndKeyStream> tlsConfigList = shenyuMemoryConfig.getTlsConfigList();

            if (Objects.nonNull(routeConfigList)) {
                routeConfigList.forEach(routeConfig -> {
                    SelectorData selectorData = routeConfig.getLeft();
                    RuleData ruleData = routeConfig.getRight();
                    if (Objects.nonNull(selectorData)) {
                        selectorData.setId(IngressSelectorCache.getInstance().generateSelectorId());
                        selectorData.setSort(100);
                        shenyuCacheRepository.saveOrUpdateSelectorData(selectorData);
                        if (Objects.nonNull(ruleData)) {
                            ruleData.setId(selectorData.getId());
                            ruleData.setSelectorId(selectorData.getId());
                            ruleData.setSort(100);
                            shenyuCacheRepository.saveOrUpdateRuleData(ruleData);
                            IngressSelectorCache.getInstance().put(Objects.requireNonNull(v1Ingress.getMetadata()).getNamespace(),
                                v1Ingress.getMetadata().getName(), PluginEnum.DIVIDE.getName(), selectorData.getId());
                        } else {
                            shenyuCacheRepository.deleteSelectorData(selectorData.getPluginName(), selectorData.getId());
                        }
                    }
                });
            }

            if (Objects.nonNull(shenyuMemoryConfig.getGlobalDefaultBackend())) {
                synchronized (IngressReconciler.class) {
                    if (globalDefaultBackend == null) {
                        // Add a default backend
                        shenyuCacheRepository.saveOrUpdateSelectorData(shenyuMemoryConfig.getGlobalDefaultBackend().getRight().getLeft());
                        shenyuCacheRepository.saveOrUpdateRuleData(shenyuMemoryConfig.getGlobalDefaultBackend().getRight().getRight());
                        globalDefaultBackend = shenyuMemoryConfig.getGlobalDefaultBackend();
                        IngressSelectorCache.getInstance().put(Objects.requireNonNull(v1Ingress.getMetadata()).getNamespace(),
                            v1Ingress.getMetadata().getName(), PluginEnum.DIVIDE.getName(), shenyuMemoryConfig.getGlobalDefaultBackend().getRight().getLeft().getId());
                    }
                }
            }

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
}
