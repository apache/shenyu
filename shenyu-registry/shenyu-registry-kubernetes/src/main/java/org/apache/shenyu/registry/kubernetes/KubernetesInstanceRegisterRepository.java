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

import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.spi.Join;

import java.net.URI;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * The type kubernetes instance register repository.
 */
@Join
public class KubernetesInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private KubernetesClient kubernetesClient;

    @Override
    public void init(final RegisterConfig config) {
        Properties properties = config.getProps();
        KubernetesConfig kubernetesConfig = new KubernetesConfig();
        kubernetesConfig.setDiscoveryServerUrl(config.getServerLists());
        kubernetesConfig.setEnabled(config.getEnabled());
        kubernetesConfig.setNamespaces(Arrays.asList(properties.getProperty("namespaces").split(",")));
        this.kubernetesClient = new KubernetesClient(kubernetesConfig);
    }

    @Override
    public void persistInstance(final InstanceEntity instance) {

    }

    @Override
    public List<InstanceEntity> selectInstances(final String selectKey) {
        List<KubernetesInstance> instanceList = kubernetesClient.selectInstances(selectKey);
        return instanceList.stream().map(instance -> InstanceEntity.builder()
                .appName(instance.getServiceId())
                .host(instance.getHost())
                .port(instance.getPort())
                .uri(getURI(instance))
                .build()).collect(Collectors.toList());
    }

    private URI getURI(final KubernetesInstance instance) {
        boolean secure = instance.isSecure();
        String scheme = secure ? "https" : "http";
        int port = instance.getPort();
        if (port <= 0) {
            port = secure ? 443 : 80;
        }
        String uri = String.format("%s://%s:%s", scheme, instance.getHost(), port);
        return URI.create(uri);
    }

    @Override
    public void close() {
        ShenyuInstanceRegisterRepository.super.close();
    }
}
