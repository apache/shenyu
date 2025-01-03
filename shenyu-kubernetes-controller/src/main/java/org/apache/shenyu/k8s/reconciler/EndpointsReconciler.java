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
import io.kubernetes.client.openapi.models.CoreV1EndpointPort;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubset;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1Ingress;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.cache.IngressSelectorCache;
import org.apache.shenyu.k8s.cache.ServiceIngressCache;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * The Reconciler of Endpoints.
 */
public class EndpointsReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(EndpointsReconciler.class);

    private final Lister<V1Ingress> ingressLister;

    private final Lister<V1Endpoints> endpointsLister;

    private final ShenyuCacheRepository shenyuCacheRepository;

    private final ApiClient apiClient;

    /**
     * EndpointsReconciler Constructor.
     *
     * @param ingressInformer       ingressInformer
     * @param endpointsInformer     endpointsInformer
     * @param shenyuCacheRepository shenyuCacheRepository
     * @param apiClient             apiClient
     */
    public EndpointsReconciler(final SharedIndexInformer<V1Ingress> ingressInformer,
                               final SharedIndexInformer<V1Endpoints> endpointsInformer,
                               final ShenyuCacheRepository shenyuCacheRepository,
                               final ApiClient apiClient) {
        this.ingressLister = new Lister<>(ingressInformer.getIndexer());
        this.endpointsLister = new Lister<>(endpointsInformer.getIndexer());
        this.shenyuCacheRepository = shenyuCacheRepository;
        this.apiClient = apiClient;
    }

    /**
     * Reconcile cycle.
     *
     * @param request request
     * @return reconcile result
     */
    @Override
    public Result reconcile(final Request request) {
        List<Pair<String, String>> ingressList = ServiceIngressCache.getInstance().getIngressName(request.getNamespace(), request.getName());
        if (CollectionUtils.isEmpty(ingressList)) {
            return new Result(false);
        }

        V1Endpoints v1Endpoints = endpointsLister.namespace(request.getNamespace()).get(request.getName());
        if (Objects.isNull(v1Endpoints)) {
            // The deletion event is not processed, because deleting all upstreams in the Selector has
            // the same effect as not deleting them, and they cannot be accessed
            LOG.info("Cannot find endpoints {}", request);
            return new Result(false);
        }

        // 1. Obtain upstream according to endpoints
        List<DivideUpstream> upstreamList = getUpstreamFromEndpoints(v1Endpoints);

        // 2. Update the handler of the selector
        List<SelectorData> totalSelectors = shenyuCacheRepository.findSelectorDataList(PluginEnum.DIVIDE.getName());
        Set<String> needUpdateSelectorId = new HashSet<>();
        //TODO Adaptation of other plugins
        ingressList.forEach(item -> {
            List<String> selectorIdList = IngressSelectorCache.getInstance().get(item.getLeft(), item.getRight(), PluginEnum.DIVIDE.getName());
            needUpdateSelectorId.addAll(selectorIdList);
        });
        totalSelectors.forEach(selectorData -> {
            if (needUpdateSelectorId.contains(selectorData.getId())) {
                SelectorData newSelectorData = SelectorData.builder().id(selectorData.getId())
                        .pluginId(selectorData.getPluginId())
                        .pluginName(selectorData.getPluginName())
                        .name(selectorData.getName())
                        .matchMode(selectorData.getMatchMode())
                        .type(selectorData.getType())
                        .sort(selectorData.getSort())
                        .enabled(selectorData.getEnabled())
                        .logged(selectorData.getLogged())
                        .continued(selectorData.getContinued())
                        .handle(GsonUtils.getInstance().toJson(upstreamList))
                        .conditionList(selectorData.getConditionList())
                        .matchRestful(selectorData.getMatchRestful()).build();
                shenyuCacheRepository.saveOrUpdateSelectorData(newSelectorData);
            }
        });
        LOG.info("Update selector for endpoint {}", request);

        return new Result(false);
    }

    private List<DivideUpstream> getUpstreamFromEndpoints(final V1Endpoints v1Endpoints) {
        List<DivideUpstream> res = new ArrayList<>();
        List<V1EndpointSubset> subsets = v1Endpoints.getSubsets();
        if (CollectionUtils.isNotEmpty(subsets)) {
            for (V1EndpointSubset subset : subsets) {
                List<CoreV1EndpointPort> ports = subset.getPorts();
                List<V1EndpointAddress> addresses = subset.getAddresses();
                if (CollectionUtils.isEmpty(ports) || CollectionUtils.isEmpty(addresses)) {
                    continue;
                }
                CoreV1EndpointPort endpointPort = ports.stream()
                        .filter(coreV1EndpointPort -> "TCP".equals(coreV1EndpointPort.getProtocol()))
                        .findFirst()
                        .orElseThrow(() -> new ShenyuException("Can't find port from endpoints"));
                String port = null;
                if (endpointPort.getPort() > 0) {
                    port = String.valueOf(endpointPort.getPort());
                } else {
                    String endpointPortName = endpointPort.getName();
                    if (Objects.nonNull(endpointPortName)) {
                        port = endpointPortName;
                    }
                }
                for (V1EndpointAddress address : addresses) {
                    String ip = address.getIp();
                    if (Objects.nonNull(ip)) {
                        DivideUpstream upstream = new DivideUpstream();
                        upstream.setUpstreamUrl(ip + ":" + port);
                        upstream.setWeight(100);
                        // TODO support config protocol in annotation
                        upstream.setProtocol("http://");
                        upstream.setWarmup(0);
                        upstream.setStatus(true);
                        upstream.setUpstreamHost("");
                        res.add(upstream);
                    }
                }
            }
        }
        return res;
    }
}
