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

import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

/**
 * Parser of Ingress.
 */
public class IngressParser implements K8sResourceParser<V1Ingress> {

    private static final Logger LOG = LoggerFactory.getLogger(IngressParser.class);

    private final Lister<V1Service> serviceLister;

    private final Lister<V1Endpoints> endpointsLister;

    /**
     * IngressParser Constructor.
     *
     * @param serviceInformer serviceInformer
     * @param endpointsInformer endpointsInformer
     */
    public IngressParser(final SharedIndexInformer<V1Service> serviceInformer, final SharedIndexInformer<V1Endpoints> endpointsInformer) {
        this.serviceLister = new Lister<>(serviceInformer.getIndexer());
        this.endpointsLister = new Lister<>(endpointsInformer.getIndexer());
    }

    /**
     * Parse ingress to ShenyuMemoryConfig.
     *
     * @param ingress ingress resource
     * @param coreV1Api coreV1Api
     * @return ShenyuMemoryConfig
     */
    @Override
    public ShenyuMemoryConfig parse(final V1Ingress ingress, final CoreV1Api coreV1Api) {
        if (Objects.equals(ingress.getMetadata().getAnnotations().get(IngressConstants.PLUGIN_DUBBO_ENABLED), "true")) {
            DubboIngressParser dubboIngressParser = new DubboIngressParser(serviceLister, endpointsLister);
            return dubboIngressParser.parse(ingress, coreV1Api);
        } else {
            DivideIngressParser divideIngressParser = new DivideIngressParser(serviceLister, endpointsLister);
            return divideIngressParser.parse(ingress, coreV1Api);
        }
    }
}
