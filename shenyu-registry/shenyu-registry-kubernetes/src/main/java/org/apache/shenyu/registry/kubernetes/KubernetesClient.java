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

package org.apache.shenyu.registry.kubernetes;

import org.springframework.util.CollectionUtils;
import org.springframework.web.client.RestTemplate;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * kubernetes client.
 */
public class KubernetesClient {

    private RestTemplate rest;

    private KubernetesConfig kubernetesConfig;

    public KubernetesClient(final KubernetesConfig kubernetesConfig) {
        this.kubernetesConfig = kubernetesConfig;
        this.rest = new RestTemplate();
    }

    /**
     * get all serviceInstance.
     * @param serviceId service identifier
     * @return list of serviceInstance
     */
    public List<KubernetesInstance> selectInstances(final String serviceId) {
        List<KubernetesInstance> response = Collections.emptyList();
        KubernetesInstance[] responseBody = this.rest.getForEntity(this.kubernetesConfig.getDiscoveryServerUrl() + "/apps/" + serviceId,
                KubernetesInstance[].class, new Object[0]).getBody();
        if (Objects.nonNull(responseBody) && responseBody.length > 0) {
            response = Arrays.stream(responseBody).filter(this::matchNamespaces).collect(Collectors.toList());
        }

        return response;
    }

    private boolean matchNamespaces(final KubernetesInstance kubernetesInstance) {
        return CollectionUtils.isEmpty(this.kubernetesConfig.getNamespaces()) ? true : this.kubernetesConfig.getNamespaces().contains(kubernetesInstance.getNamespace());
    }
}
