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

import java.util.ArrayList;
import java.util.Properties;


public class BootstrapServer {
    private static final Logger LOG = LoggerFactory.getLogger(BootstrapServer.class);
    public Bridge bridge;
    private ConnectionContext connectionContext;

    private ConnectionHolder holder;

    public void init(Properties properties) {
        try {
            ClientConnectionConfigProviderFactory factory = ClientConnectionConfigProviderFactory.getInstance();
            this.bridge = new TcpConnectionBridge();
            this.holder = new ConnectionHolder();
            ClientConnectionConfigProvider provider = factory.getClientConnectionConfigProviderByType(SyancType.HTTP);
            provider.init(properties);
            connectionContext = new ConnectionContext(provider);
            connectionContext.init(properties);
        } catch (Exception ex) {
            throw new ShenyuException(ex);
        }
    }

    public void start() {
        TcpServerConfiguration tcpServerConfiguration = new TcpServerConfiguration();
        tcpServerConfiguration.setBossGroupThreadCount(1);
        tcpServerConfiguration.setWorkerGroupThreadCount(10);
        LoopResources loopResources = LoopResources.create("shenyu-tcp-bootstrap-server", tcpServerConfiguration.getBossGroupThreadCount(),
                tcpServerConfiguration.getWorkerGroupThreadCount(), true);
        TcpServer tcpServer = TcpServer.create()
                .doOnChannelInit((connectionObserver, channel, remoteAddress) -> {
                    channel.pipeline().addFirst(new LoggingHandler(LogLevel.DEBUG));
                })
                .wiretap(true)
                .doOnConnection(this::bridgeConnections)
                .port(9123)
                .runOn(loopResources);
        DisposableServer server = tcpServer.bindNow();
        triggerJob();
        server.onDispose().block();
    }

    private void bridgeConnections(final Connection serverConn) {
        LOG.debug("Starting proxy client");
        Mono<Connection> client = connectionContext.getTcpClientConnection();
        String clientConnectionKey = connectionContext.getClientConnectionKey();
        holder.put(clientConnectionKey, serverConn);
        client.subscribe((clientConn) -> {
            LOG.debug("Bridging connection with {}", bridge);
            bridge.bridge(serverConn, clientConn);
        });
    }


    public void triggerJob() {
        Runnable runnable = () -> {
            while (true) {
                try {
                    LOG.info("job trigger start");
                    ArrayList<Connection> testClient1 = new ArrayList<>(holder.getConnectionList("TEST_CLIENT"));
                    for (Connection serverCon : testClient1) {
                        serverCon.disposeNow();
                        bridgeConnections(serverCon);
                    }
                    LOG.info("job trigger end");
                } catch (Throwable tx) {
                    LOG.error("error", tx);
                }
                try {
                    Thread.sleep(10000);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
        };
        new Thread(runnable).start();
    }

    public static void main(String[] args) {
        BootstrapServer bootstrapServer = new BootstrapServer();
        bootstrapServer.init(new Properties());
        bootstrapServer.start();

    }


}
