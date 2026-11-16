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

package org.apache.shenyu.client.core.register.registrar;

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.utils.ApiBeanUtil;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.springframework.context.EnvironmentAware;
import org.springframework.core.env.Environment;
import org.springframework.core.env.StandardEnvironment;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Publishes local instance membership once per namespace and instance identity.
 * Repository failback owns transport retries after a URI has been enqueued.
 *
 * @see URIRegisterDTO
 */
public class UriApiRegistrarImplImpl extends BaseApiRegistrarImpl implements EnvironmentAware {

    private static final int MAX_PORT = 65535;

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final ClientRegisterConfig clientRegisterConfig;

    private final Set<List<?>> publishedIdentities = new HashSet<>();

    private Environment environment = new StandardEnvironment();

    public UriApiRegistrarImplImpl(final ClientRegisterConfig clientRegisterConfig) {
        this.clientRegisterConfig = clientRegisterConfig;
    }

    @Override
    public void setEnvironment(final Environment environment) {
        this.environment = environment;
    }

    @Override
    public Class<?> registerDataType() {
        return URIRegisterDTO.class;
    }

    @Override
    public void register(final ApiBean apiBean) {
        if (ShenyuClientConstants.DISCOVERY_LOCAL_MODE.equals(environment.getProperty("shenyu.discovery.type", ShenyuClientConstants.DISCOVERY_LOCAL_MODE))) {
            super.register(apiBean);
        }
    }

    @Override
    protected void doRegisterApi(final ApiBean.ApiDefinition api) {
        registerUri(ApiBeanUtil.getRpcType(api));
    }

    @Override
    protected void doRegisterBean(final ApiBean apiBean) {
        registerUri(ApiBeanUtil.getRpcType(apiBean));
    }

    private synchronized void registerUri(final String rpcType) {
        int port = clientRegisterConfig.getPort();
        if (port <= 0 || port > MAX_PORT) {
            throw new IllegalArgumentException("Local URI registration requires a port between 1 and " + MAX_PORT);
        }
        for (String namespaceId : clientRegisterConfig.getNamespace()) {
            URIRegisterDTO uri = URIRegisterDTO.builder()
                    .namespaceId(namespaceId)
                    .contextPath(clientRegisterConfig.getContextPath())
                    .appName(clientRegisterConfig.getAppName())
                    .eventType(EventType.REGISTER)
                    .host(clientRegisterConfig.getHost())
                    .port(port)
                    .rpcType(rpcType)
                    .build();
            // Snapshot identity: heartbeat mutates the DTO, and enqueue is not an Admin acknowledgement.
            List<?> identity = Arrays.asList(namespaceId, rpcType, uri.getAppName(), uri.getContextPath(), uri.getHost(), port);
            if (!publishedIdentities.contains(identity)) {
                publisher.publishEvent(uri);
                publishedIdentities.add(identity);
            }
        }
    }
}
