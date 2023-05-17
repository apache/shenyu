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

package org.apache.shenyu.protocol.tcp.connection;

import reactor.core.publisher.Mono;
import reactor.netty.Connection;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.tcp.TcpClient;

import java.time.Duration;
import java.util.Properties;

/**
 * ConnectionContext.
 */
public class ConnectionContext {

    private final ClientConnectionConfigProvider connectionConfigProvider;

    private ConnectionProvider connectionProvider;

    public ConnectionContext(final ClientConnectionConfigProvider connectionConfigProvider) {
        this.connectionConfigProvider = connectionConfigProvider;
    }

    /**
     * init.
     *
     * @param props props
     */
    public void init(final Properties props) {
        final String tcpProxyClientName = props.getProperty("Name", "shenyu-tcp-connection-pool-client");
        final String maxConnections = props.getProperty("maxConnections", "20");
        final String maxIdleTimeMs = props.getProperty("maxIdleTimeMs", "30");
        final String maxLifeTimeMs = props.getProperty("maxLifeTimeMs", "60");
        final String pendingAcquireTimeout = props.getProperty("pendingAcquireTimeout", "5");
        final String pendingAcquireMaxCount = props.getProperty("pendingAcquireMaxCount", "5");
        connectionProvider = ConnectionProvider.builder(tcpProxyClientName)
                .maxConnections(Integer.parseInt(maxConnections))
                .pendingAcquireTimeout(Duration.ofSeconds(Integer.parseInt(pendingAcquireTimeout)))
                .pendingAcquireMaxCount(Integer.parseInt(pendingAcquireMaxCount))
                .maxIdleTime(Duration.ofSeconds(Integer.parseInt(maxIdleTimeMs)))
                .maxLifeTime(Duration.ofSeconds(Integer.parseInt(maxLifeTimeMs)))
                .build();
    }

    /**
     * getTcpClientConnection.
     *
     * @param ip       ip
     * @param observer observer
     * @return MonoConnection
     */
    public Mono<Connection> getTcpClientConnection(final String ip, final ActivityConnectionObserver observer) {
        return Mono.just(connectionConfigProvider.getProxiedService(ip))
                .flatMap(url ->
                        TcpClient.create(connectionProvider)
                                .host(url.getHost())
                                .port(url.getPort())
                                .observe(observer)
                                .connect()
                );
    }

}
