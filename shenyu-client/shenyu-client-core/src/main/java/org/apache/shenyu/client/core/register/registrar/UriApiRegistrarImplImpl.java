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
import java.util.List;
import java.util.Objects;
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

    private static final String INVALID_PORT = "Local URI registration requires a port between 1 and " + MAX_PORT;

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final ClientRegisterConfig clientRegisterConfig;

    private final List<ClientRegisterConfig> protocolConfigurations;

    private final Set<UriIdentity> publishedIdentities = new HashSet<>();

    private final Set<String> awaitingPorts = new HashSet<>();

    public UriApiRegistrarImplImpl(final ClientRegisterConfig clientRegisterConfig) {
        this(clientRegisterConfig, List.of());
    }

    /**
     * Construct a registrar with the client configurations wired by the active starters.
     *
     * @param clientRegisterConfig the primary configuration, preferred for its own RPC type
     * @param protocolConfigurations the available protocol configurations
     */
    public UriApiRegistrarImplImpl(final ClientRegisterConfig clientRegisterConfig, final List<ClientRegisterConfig> protocolConfigurations) {
        this.clientRegisterConfig = clientRegisterConfig;
        this.protocolConfigurations = List.copyOf(protocolConfigurations);
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
        ClientRegisterConfig configuration = configurationFor(rpcType);
        int port = advertisedPort(configuration);
        if (isPendingPort(rpcType, port)) {
            if (awaitingPorts.add(rpcType)) {
                LOG.warn("Skipping local URI registration until the advertised port is available");
            }
            return;
        }
        if (port <= 0 || port > MAX_PORT) {
            throw new IllegalArgumentException(INVALID_PORT);
        }
        awaitingPorts.remove(rpcType);
        for (String namespaceId : configuration.getNamespace()) {
            URIRegisterDTO uri = URIRegisterDTO.builder()
                    .namespaceId(namespaceId)
                    .contextPath(configuration.getContextPath())
                    .appName(configuration.getAppName())
                    .eventType(EventType.REGISTER)
                    .host(configuration.getHost())
                    .port(port)
                    .rpcType(rpcType)
                    .protocol(configuration.getProtocol())
                    .build();
            publish(uri);
        }
    }

    private ClientRegisterConfig configurationFor(final String rpcType) {
        if (protocolConfigurations.isEmpty() || matchesProtocol(clientRegisterConfig, rpcType)) {
            return clientRegisterConfig;
        }
        List<ClientRegisterConfig> matching = protocolConfigurations.stream().filter(configuration -> matchesProtocol(configuration, rpcType)).toList();
        if (matching.size() != 1) {
            throw new IllegalStateException("Local URI registration requires one client configuration for RPC type " + rpcType);
        }
        return matching.get(0);
    }

    private static boolean matchesProtocol(final ClientRegisterConfig configuration, final String rpcType) {
        RpcTypeEnum protocol = configuration.getRpcTypeEnum();
        return Objects.nonNull(protocol) && protocol.getName().equals(rpcType);
    }

    private static int advertisedPort(final ClientRegisterConfig configuration) {
        try {
            Integer port = configuration.getPort();
            if (Objects.isNull(port)) {
                throw new IllegalArgumentException(INVALID_PORT);
            }
            return port;
        } catch (NumberFormatException ignored) {
            throw new IllegalArgumentException(INVALID_PORT);
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
