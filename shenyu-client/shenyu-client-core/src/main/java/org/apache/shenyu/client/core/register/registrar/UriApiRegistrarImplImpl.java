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

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.utils.ApiBeanUtil;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Set;

/**
 * Publishes local instance membership once per namespace and instance identity.
 * Repository failback owns transport retries after a URI has been enqueued.
 *
 * @see URIRegisterDTO
 */
public class UriApiRegistrarImplImpl extends BaseApiRegistrarImpl {

    private static final Logger LOG = LoggerFactory.getLogger(UriApiRegistrarImplImpl.class);

    private static final int MAX_PORT = 65535;

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final ClientRegisterConfig clientRegisterConfig;

    private final Set<UriIdentity> publishedIdentities = new HashSet<>();

    private boolean awaitingPort;

    public UriApiRegistrarImplImpl(final ClientRegisterConfig clientRegisterConfig) {
        this.clientRegisterConfig = clientRegisterConfig;
    }

    @Override
    public Class<?> registerDataType() {
        return URIRegisterDTO.class;
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
        if (isPendingPort(rpcType, port)) {
            if (!awaitingPort) {
                LOG.warn("Skipping local URI registration until the advertised port is available");
                awaitingPort = true;
            }
            return;
        }
        if (port <= 0 || port > MAX_PORT) {
            throw new IllegalArgumentException("Local URI registration requires a port between 1 and " + MAX_PORT);
        }
        awaitingPort = false;
        for (String namespaceId : clientRegisterConfig.getNamespace()) {
            URIRegisterDTO uri = URIRegisterDTO.builder()
                    .namespaceId(namespaceId)
                    .contextPath(clientRegisterConfig.getContextPath())
                    .appName(clientRegisterConfig.getAppName())
                    .eventType(EventType.REGISTER)
                    .host(clientRegisterConfig.getHost())
                    .port(port)
                    .rpcType(rpcType)
                    .protocol(clientRegisterConfig.getProtocol())
                    .build();
            publish(uri);
        }
    }

    private static boolean isPendingPort(final String rpcType, final int port) {
        boolean supportsAutoPort = RpcTypeEnum.HTTP.getName().equals(rpcType) || RpcTypeEnum.WEB_SOCKET.getName().equals(rpcType);
        return supportsAutoPort && (port == 0 || port == -1);
    }

    private void publish(final URIRegisterDTO uri) {
        // The enqueue key is a snapshot since the subscriber can mutate the DTO.
        UriIdentity identity = new UriIdentity(uri.getNamespaceId(), uri.getRpcType(), uri.getAppName(), uri.getContextPath(), uri.getHost(), uri.getPort());
        if (publishedIdentities.contains(identity)) {
            return;
        }
        try {
            publisher.publishEvent(uri);
            publishedIdentities.add(identity);
        } catch (RuntimeException ex) {
            LOG.warn("Could not enqueue local URI registration; it can be retried on rediscovery");
        }
    }

    private record UriIdentity(String namespaceId, String rpcType, String appName, String contextPath, String host, int port) {
    }
}
