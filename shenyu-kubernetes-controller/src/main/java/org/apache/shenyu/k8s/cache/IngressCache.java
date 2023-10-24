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
import io.kubernetes.client.openapi.models.V1Ingress;

import java.util.Map;

/**
 * The cache for V1Ingress.
 */
public final class IngressCache implements K8sResourceCache<V1Ingress> {

    private static final IngressCache INSTANCE = new IngressCache();

    private static final Map<String, V1Ingress> INGRESS_MAP = Maps.newConcurrentMap();

    private IngressCache() {
    }

    /**
     * Get singleton of IngressCache.
     *
     * @return IngressCache
     */
    public static IngressCache getInstance() {
        return INSTANCE;
    }

    /**
     * Put ingress.
     *
     * @param namespace namespace
     * @param name name
     * @param resource resource
     */
    @Override
    public void put(final String namespace, final String name, final V1Ingress resource) {
        INGRESS_MAP.put(getKey(namespace, name), resource);
    }

    /**
     * Get ingress.
     *
     * @param namespace namespace
     * @param name name
     * @return V1Ingress
     */
    @Override
    public V1Ingress get(final String namespace, final String name) {
        return INGRESS_MAP.get(getKey(namespace, name));
    }

    /**
     * Remove ingress.
     *
     * @param namespace namespace
     * @param name name
     * @return V1Ingress
     */
    @Override
    public V1Ingress remove(final String namespace, final String name) {
        return INGRESS_MAP.remove(getKey(namespace, name));
    }

    private String getKey(final String namespace, final String name) {
        return String.format("%s-%s", namespace, name);
    }
}
