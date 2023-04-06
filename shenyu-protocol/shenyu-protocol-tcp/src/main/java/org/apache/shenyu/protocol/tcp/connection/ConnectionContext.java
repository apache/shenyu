package org.apache.shenyu.protocol.tcp.connection;

import reactor.netty.Connection;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.tcp.TcpClient;

import java.net.InetSocketAddress;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ConnectionContext {

    static ConnectionProvider connectionProvider;

    static Map<String, Connection> cache = new ConcurrentHashMap<>();

    static Map<String, InetSocketAddress> mappingTable = new ConcurrentHashMap<>();

    static {
        connectionProvider = ConnectionProvider.builder("my-pool-client")
                .maxConnections(10)
                .maxIdleTime(Duration.ofMinutes(5))
                .build();
        mappingTable.put("127.0.0.1:9124", new InetSocketAddress("127.0.0.1", 9124));
    }

    public static Connection getTcpClientConnection(String clientIp, Integer clientPort) {
        String key = clientIp + ":" + clientPort;
        if (cache.containsKey(key)) {
            return cache.get(key);
        }
        InetSocketAddress inetSocketAddress = mappingTable.get(key);
        Connection connection = TcpClient.create(connectionProvider)
                .host(inetSocketAddress.getHostString()).port(inetSocketAddress.getPort())
                .connect().block();
        cache.put(key, connection);
        return connection;
    }


}
