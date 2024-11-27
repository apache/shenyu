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
import io.kubernetes.client.openapi.models.V1HTTPIngressPath;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Parser of Ingress.
 */
public class IngressParser implements K8sResourceListParser<V1Ingress> {

    private static final Logger LOG = LoggerFactory.getLogger(IngressParser.class);

    private final Lister<V1Service> serviceLister;

    private final Lister<V1Endpoints> endpointsLister;

    /**
     * IngressParser Constructor.
     *
     * @param serviceInformer   serviceInformer
     * @param endpointsInformer endpointsInformer
     */
    public IngressParser(final SharedIndexInformer<V1Service> serviceInformer, final SharedIndexInformer<V1Endpoints> endpointsInformer) {
        this.serviceLister = new Lister<>(serviceInformer.getIndexer());
        this.endpointsLister = new Lister<>(endpointsInformer.getIndexer());
    }

    /**
     * Parse ingress to ShenyuMemoryConfig.
     *
     * @param ingress   ingress resource
     * @param coreV1Api coreV1Api
     * @return ShenyuMemoryConfig
     */
    @Override
    public List<ShenyuMemoryConfig> parse(final V1Ingress ingress, final CoreV1Api coreV1Api) {
        List<ShenyuMemoryConfig> shenyuMemoryConfigList = new ArrayList<>();
        boolean dubboEnabled = getBooleanAnnotation(ingress, IngressConstants.PLUGIN_DUBBO_ENABLED);
        boolean motanEnabled = getBooleanAnnotation(ingress, IngressConstants.PLUGIN_MOTAN_ENABLED);
        boolean webSocketEnabled = getBooleanAnnotation(ingress, IngressConstants.PLUGIN_WEB_SOCKET_ENABLED);
        boolean brpcEnabled = getBooleanAnnotation(ingress, IngressConstants.PLUGIN_BRPC_ENABLED);
        boolean grpcEnabled = getBooleanAnnotation(ingress, IngressConstants.PLUGIN_GRPC_ENABLED);
        boolean sofaEnabled = getBooleanAnnotation(ingress, IngressConstants.PLUGIN_SOFA_ENABLED);

        if (!dubboEnabled || !motanEnabled || !sofaEnabled) {
            contextPathParse(ingress, shenyuMemoryConfigList, coreV1Api);
        }
        if (dubboEnabled) {
            DubboIngressParser dubboIngressParser = new DubboIngressParser(serviceLister, endpointsLister);
            shenyuMemoryConfigList.add(dubboIngressParser.parse(ingress, coreV1Api));
        } else if (motanEnabled) {
            MotanIngressParser motanIngressParser = new MotanIngressParser(serviceLister, endpointsLister);
            shenyuMemoryConfigList.add(motanIngressParser.parse(ingress, coreV1Api));
        } else if (webSocketEnabled) {
            WebSocketParser webSocketParser = new WebSocketParser(serviceLister, endpointsLister);
            shenyuMemoryConfigList.add(webSocketParser.parse(ingress, coreV1Api));
        } else if (grpcEnabled) {
            GrpcParser grpcParser = new GrpcParser(serviceLister, endpointsLister);
            shenyuMemoryConfigList.add(grpcParser.parse(ingress, coreV1Api));
        } else if (sofaEnabled) {
            SofaParser sofaParser = new SofaParser(serviceLister, endpointsLister);
            shenyuMemoryConfigList.add(sofaParser.parse(ingress, coreV1Api));
        } else {
            DivideIngressParser divideIngressParser = new DivideIngressParser(serviceLister, endpointsLister);
            shenyuMemoryConfigList.add(divideIngressParser.parse(ingress, coreV1Api));
        }
        return shenyuMemoryConfigList;
    }

    private boolean getBooleanAnnotation(final V1Ingress ingress, final String annotationKey) {
        String annotationValue = ingress.getMetadata().getAnnotations().get(annotationKey);
        return annotationValue != null && Boolean.parseBoolean(annotationValue);
    }

    private void contextPathParse(final V1Ingress ingress, final List<ShenyuMemoryConfig> shenyuMemoryConfigList, final CoreV1Api coreV1Api) {
        List<V1IngressRule> rules = ingress.getSpec().getRules();
        for (V1IngressRule rule : rules) {
            List<V1HTTPIngressPath> paths = rule.getHttp().getPaths();
            for (V1HTTPIngressPath path : paths) {
                if ("Prefix".equals(path.getPathType())) {
                    ContextPathParser contextPathParser = new ContextPathParser(serviceLister, endpointsLister);
                    shenyuMemoryConfigList.add(contextPathParser.parse(ingress, coreV1Api));
                }
            }
        }
    }


    /**
     * get endpointsLister.
     * @return endpointsLister
     */
    public Lister<V1Endpoints> getEndpointsLister() {
        return endpointsLister;
    }
}
