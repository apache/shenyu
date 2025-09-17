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

package org.apache.shenyu.k8s.cache;

import com.google.common.collect.Maps;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The cache for mapping service name to ingress name.
 */
public final class ServiceIngressCache {

    private static final ServiceIngressCache INSTANCE = new ServiceIngressCache();

    private static final Map<String, List<Pair<String, String>>> INGRESS_MAP = Maps.newConcurrentMap();

    private ServiceIngressCache() {
    }

    /**
     * Get singleton of ServiceIngressCache.
     *
     * @return ServiceIngressCache
     */
    public static ServiceIngressCache getInstance() {
        return INSTANCE;
    }

    /**
     * Get ingress namespace and name by service namespace and namespace.
     *
     * @param namespace namespace
     * @param serviceName service name
     * @return ingress namespace and name
     */
    public List<Pair<String, String>> getIngressName(final String namespace, final String serviceName) {
        return INGRESS_MAP.get(getKey(namespace, serviceName));
    }

    /**
     * Put ingress by service namespace and name.
     *
     * @param namespace service namespace
     * @param serviceName service name
     * @param ingressNamespace ingress namespace
     * @param ingressName ingress name
     */
    public void putIngressName(final String namespace, final String serviceName, final String ingressNamespace, final String ingressName) {
        List<Pair<String, String>> list = INGRESS_MAP.computeIfAbsent(getKey(namespace, serviceName), k -> new ArrayList<>());
        list.add(Pair.of(ingressNamespace, ingressName));
    }

    /**
     * Remove all ingress by service namespace and name.
     *
     * @param namespace service namespace
     * @param serviceName service name
     * @return the ingress list removed
     */
    public List<Pair<String, String>> removeAllIngressName(final String namespace, final String serviceName) {
        return INGRESS_MAP.remove(getKey(namespace, serviceName));
    }

    /**
     * Remove specified ingress by service and ingress.
     *
     * @param namespace service namespace
     * @param serviceName service name
     * @param ingressNamespace ingress namespace
     * @param ingressName ingress name
     */
    public void removeSpecifiedIngressName(final String namespace, final String serviceName, final String ingressNamespace, final String ingressName) {
        List<Pair<String, String>> list = INGRESS_MAP.get(getKey(namespace, serviceName));
        if (Objects.nonNull(list)) {
            list.removeIf(item -> item.getLeft().equals(ingressNamespace) && item.getRight().equals(ingressName));
        }
    }

    private String getKey(final String namespace, final String name) {
        return String.format("%s-%s", namespace, name);
    }
}
