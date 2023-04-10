package org.apache.shenyu.protocol.tcp;

import io.netty.handler.logging.LogLevel;
import io.netty.handler.logging.LoggingHandler;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.protocol.tcp.connection.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;
import reactor.netty.DisposableServer;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.TcpServer;

import java.util.Properties;


public class BootstrapServer {

    private static final Logger LOG = LoggerFactory.getLogger(BootstrapServer.class);
    public static final Linked TCP_LINKED = new TcpConnectionLinked();

    private ConnectionContext connectionContext;


    public void init(Properties properties) {
        try {
            ClientConnectionConfigProviderFactory factory = ClientConnectionConfigProviderFactory.getInstance();
            ClientConnectionConfigProvider provider = factory.getClientConnectionConfigProviderByType(SyancType.HTTP);
            connectionContext = new ConnectionContext(provider);
            connectionContext.init(properties);
        } catch (Exception ex) {
            throw new ShenyuException(ex);
        }
    }

    public void start() {
        ShenyuTcpConfig shenyuTcpConfig = new ShenyuTcpConfig();
        shenyuTcpConfig.setBossThreads(1);
        shenyuTcpConfig.setWorkerThreads(10);
        LoopResources loopResources = LoopResources.create("my-elg", shenyuTcpConfig.getBossThreads(), shenyuTcpConfig.getWorkerThreads(), true);
        TcpServer tcpServer = TcpServer.create()
                .doOnChannelInit((connectionObserver, channel, remoteAddress) -> {
                    channel.pipeline().addFirst(new LoggingHandler(LogLevel.DEBUG));
                })
                .wiretap(true)
                .doOnConnection(this::bridgeConnections)
                .port(9123)
                .runOn(loopResources);
        DisposableServer server = tcpServer.bindNow();
        server.onDispose().block();
    }

    private void bridgeConnections(final Connection serverConn) {
        LOG.debug("Starting proxy client");
        Mono<Connection> client = connectionContext.getTcpClientConnection();
        // Connect to client, and react when connection becomes available
        client
                .subscribe((clientConn) -> {
                    LOG.debug("Bridging connection with {}", TCP_LINKED);
                    TCP_LINKED.link(serverConn, clientConn);
                });
    }


}
