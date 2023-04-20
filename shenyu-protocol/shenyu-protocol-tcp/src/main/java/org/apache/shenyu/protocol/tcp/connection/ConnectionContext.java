package org.apache.shenyu.protocol.tcp.connection;

import reactor.core.publisher.Mono;
import reactor.netty.Connection;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.tcp.TcpClient;

import java.time.Duration;
import java.util.Properties;

/**
 *
 */
public class ConnectionContext {
    private final ClientConnectionConfigProvider connectionConfigProvider;
    private ConnectionProvider connectionProvider;

    public ConnectionContext(final ClientConnectionConfigProvider connectionConfigProvider) {
        this.connectionConfigProvider = connectionConfigProvider;
    }

    public void init(Properties props) {
        final String maxTotal = props.getProperty("tcpProxy.maxConnections", "800");
        final String maxIdleTimeMs = props.getProperty("tcpProxy.maxIdleTimeMs", "100");
        final String tcpProxyClientName = props.getProperty("tcpProxy.Name", "shenyu-tcp-connection-pool-client");
        final String disposeTimeoutMs = props.getProperty("tcpProxy.disposeTimeoutMs", "2000");
        connectionProvider = ConnectionProvider.builder(tcpProxyClientName)
                .maxConnections(Integer.parseInt(maxTotal))
                .maxIdleTime(Duration.ofMillis(Integer.parseInt(maxIdleTimeMs)))
                .disposeTimeout(Duration.ofMillis(Integer.parseInt(disposeTimeoutMs)))
                .build();
    }

    public Mono<Connection> getTcpClientConnection(String ip) {
        return Mono.just(connectionConfigProvider.getProxiedService(ip))
                .flatMap(url ->
                        TcpClient.create(connectionProvider)
                                .host(url.getHost())
                                .port(url.getPort())
                                .connect()
                );
    }


    public String getClientConnectionId() {

        return "TEST_CLIENT";
    }


}
