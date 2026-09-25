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
import org.apache.shenyu.k8s.common.ServiceIngressRelation;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The cache for mapping service name to the ingress relations that reference the service.
 */
public final class ServiceIngressCache {

    private static final ServiceIngressCache INSTANCE = new ServiceIngressCache();

    private static final Map<String, List<ServiceIngressRelation>> INGRESS_MAP = Maps.newConcurrentMap();

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
     * Get the ingress relations of the service, each relation keeps the service port selected by the ingress.
     *
     * @param namespace service namespace
     * @param serviceName service name
     * @return the ingress relations of the service, empty if the service is not referenced
     */
    public List<ServiceIngressRelation> getIngressName(final String namespace, final String serviceName) {
        List<ServiceIngressRelation> res = INGRESS_MAP.get(getKey(namespace, serviceName));
        return Objects.isNull(res) ? Collections.emptyList() : res;
    }

    /**
     * Put the ingress that references the service, the previous relation of the same ingress is
     * replaced so that a changed backend service port does not leave a stale relation behind.
     *
     * @param namespace service namespace
     * @param serviceName service name
     * @param relation ingress relation of the service
     */
    public void putIngressName(final String namespace, final String serviceName, final ServiceIngressRelation relation) {
        INGRESS_MAP.compute(getKey(namespace, serviceName), (key, relations) -> {
            List<ServiceIngressRelation> res = Objects.isNull(relations) ? new ArrayList<>() : relations;
            res.removeIf(item -> item.isSameIngress(relation.getIngressNamespace(), relation.getIngressName()));
            res.add(relation);
            return res;
        });
    }

    /**
     * Remove all ingress relations by service namespace and name.
     *
     * @param namespace service namespace
     * @param serviceName service name
     * @return the ingress relation list removed
     */
    public List<ServiceIngressRelation> removeAllIngressName(final String namespace, final String serviceName) {
        return INGRESS_MAP.remove(getKey(namespace, serviceName));
    }

    /**
     * Remove specified ingress relation by service and ingress.
     *
     * @param namespace service namespace
     * @param serviceName service name
     * @param ingressNamespace ingress namespace
     * @param ingressName ingress name
     */
    public void removeSpecifiedIngressName(final String namespace, final String serviceName, final String ingressNamespace, final String ingressName) {
        List<ServiceIngressRelation> list = INGRESS_MAP.get(getKey(namespace, serviceName));
        if (Objects.nonNull(list)) {
            list.removeIf(item -> item.isSameIngress(ingressNamespace, ingressName));
        }
    }

    private String getKey(final String namespace, final String name) {
        return String.format("%s-%s", namespace, name);
    }
}
