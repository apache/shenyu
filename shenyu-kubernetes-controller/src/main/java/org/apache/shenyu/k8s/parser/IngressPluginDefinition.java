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

import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * External ingress plugin definition.
 *
 * <p>When more than one external definition matches an ingress, the first
 * definition in the injected list wins. Spring wiring uses ordered stream
 * semantics, so extensions can use {@code @Order} to make overlap deterministic.
 */
public interface IngressPluginDefinition {

    /**
     * Whether this definition owns the ingress.
     *
     * @param ingress ingress
     * @return true when this definition owns the ingress
     */
    boolean matchesIngress(V1Ingress ingress);

    /**
     * Plugin name used for selector cache and repository operations.
     *
     * @return plugin name
     */
    String pluginName();

    /**
     * Metadata path used when deleting metadata for selectors.
     *
     * @param ingress ingress
     * @return metadata path, or empty when no metadata deletion path is needed
     */
    String contextPath(V1Ingress ingress);

    /**
     * Metadata paths owned by this ingress definition.
     *
     * @param ingress ingress
     * @param serviceLister service lister
     * @param endpointsLister endpoints lister
     * @return metadata paths owned by this ingress
     */
    default List<String> metadataPaths(final V1Ingress ingress, final Lister<V1Service> serviceLister,
                                       final Lister<V1Endpoints> endpointsLister) {
        String path = contextPath(ingress);
        return Objects.isNull(path) || path.isEmpty() ? Collections.emptyList() : Collections.singletonList(path);
    }

    /**
     * Build plugin data for this ingress.
     *
     * @param ingress ingress
     * @param request reconcile request
     * @param endpointsLister endpoints lister
     * @return plugin data
     */
    PluginData pluginData(V1Ingress ingress, Request request, Lister<V1Endpoints> endpointsLister);

    /**
     * Parse ingress config for this plugin.
     *
     * @param ingress ingress
     * @param coreV1Api core v1 api
     * @param serviceLister service lister
     * @param endpointsLister endpoints lister
     * @return memory config
     */
    ShenyuMemoryConfig parse(V1Ingress ingress, CoreV1Api coreV1Api, Lister<V1Service> serviceLister, Lister<V1Endpoints> endpointsLister);
}
