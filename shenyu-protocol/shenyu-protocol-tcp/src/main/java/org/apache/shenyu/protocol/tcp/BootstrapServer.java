package org.apache.shenyu.protocol.tcp;

import io.netty.handler.logging.LogLevel;
import io.netty.handler.logging.LoggingHandler;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.loadbalancer.spi.LoadBalancer;
import org.apache.shenyu.protocol.tcp.connection.*;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;
import reactor.netty.DisposableServer;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.TcpServer;

import java.util.ArrayList;
import java.util.Properties;


/**
 * BootstrapServer.
 */
public class BootstrapServer {
    private static final Logger LOG = LoggerFactory.getLogger(BootstrapServer.class);
    private Bridge bridge;
    private ConnectionContext connectionContext;
    private ConnectionHolder holder;

    private DisposableServer server;

    private void init(Properties properties) {
        try {
            ShenyuDiscoveryService shenyuDiscoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin("zookeeper");
            LoadBalancer loadBalancer = ExtensionLoader.getExtensionLoader(LoadBalancer.class).getJoin("zookeeper");
            DefaultConnectionConfigProvider connectionConfigProvider = new DefaultConnectionConfigProvider(shenyuDiscoveryService, loadBalancer);
            this.bridge = new TcpConnectionBridge();
            this.holder = new ConnectionHolder();
            connectionContext = new ConnectionContext(connectionConfigProvider);
            connectionContext.init(properties);
        } catch (Exception ex) {
            throw new ShenyuException(ex);
        }
    }

    public void start(DiscoveryConfig discoveryConfig, TcpServerConfiguration tcpServerConfiguration) {
        ShenyuDiscoveryService shenyuDiscoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin("zookeeper");
        LoadBalancer radmon = ExtensionLoader.getExtensionLoader(LoadBalancer.class).getJoin("radmon");
        shenyuDiscoveryService.init(discoveryConfig);
        String data = shenyuDiscoveryService.getData("/shenyu/plugin/tcp");
        init(tcpServerConfiguration.getProps());
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
        server = tcpServer.bindNow();
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

    private void triggerJob() {
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


    /**
     * shutdown.
     */
    public void shutdown() {
        server.disposeNow();
    }

}
