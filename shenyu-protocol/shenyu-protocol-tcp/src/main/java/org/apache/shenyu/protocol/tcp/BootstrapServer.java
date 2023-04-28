package org.apache.shenyu.protocol.tcp;

import io.netty.handler.logging.LogLevel;
import io.netty.handler.logging.LoggingHandler;
import org.apache.shenyu.protocol.tcp.connection.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;
import reactor.netty.DisposableServer;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.TcpServer;

import java.net.SocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;


/**
 * BootstrapServer.
 */
public class BootstrapServer {
    private static final Logger LOG = LoggerFactory.getLogger(BootstrapServer.class);
    private Bridge bridge;
    private ConnectionContext connectionContext;
    private DisposableServer server;
    private final Map<String, Connection> connectionCache = new ConcurrentHashMap<>();

    public void start(TcpServerConfiguration tcpServerConfiguration) {
        String loadBalanceAlgorithm = tcpServerConfiguration.getProps().getOrDefault("shenyu.tcpPlugin.tcpServerConfiguration.props.loadBalanceAlgorithm", "random").toString();
        DefaultConnectionConfigProvider connectionConfigProvider = new DefaultConnectionConfigProvider(loadBalanceAlgorithm, tcpServerConfiguration.getPluginSelectorName());
        this.bridge = new TcpConnectionBridge();
        connectionContext = new ConnectionContext(connectionConfigProvider);
        connectionContext.init(tcpServerConfiguration.getProps());
        LoopResources loopResources = LoopResources.create("shenyu-tcp-bootstrap-server", tcpServerConfiguration.getBossGroupThreadCount(),
                tcpServerConfiguration.getWorkerGroupThreadCount(), true);
        TcpServer tcpServer = TcpServer.create()
                .doOnChannelInit((connectionObserver, channel, remoteAddress) -> {
                    channel.pipeline().addFirst(new LoggingHandler(LogLevel.DEBUG));
                })
                .wiretap(true)
                .observe((c, s) -> {
                    LOG.info("connection={}|status={}", c.toString(), s.toString());
                })
                //todo 使用 child 去监控
                .childObserve(new ActivityConnectionObserver("TcpServer"))
                .doOnConnection(this::bridgeConnections)
                .port(tcpServerConfiguration.getPort())
                .runOn(loopResources);
        server = tcpServer.bindNow();
        triggerJob();
    }

    private void bridgeConnections(final Connection serverConn) {
        LOG.info("Starting proxy client ={}", serverConn);
        SocketAddress socketAddress = serverConn.channel().remoteAddress();
        Mono<Connection> client = connectionContext.getTcpClientConnection(getIp(socketAddress));
        String connectionId = connectionContext.getClientConnectionId();
        connectionCache.put(connectionId, serverConn);
        client.subscribe((clientConn) -> {
            //LOG.info("Bridging connection with {}", bridge);
            bridge.bridge(serverConn, clientConn);
        });
    }

    private String getIp(SocketAddress socketAddress) {
        if (socketAddress == null) {
            throw new NullPointerException("remoteAddress is null");
        }
        String address = socketAddress.toString();
        return address.substring(2, address.indexOf(':'));
    }

    private void triggerJob() {
        Runnable runnable = () -> {
            while (true) {
                try {
                    //  LOG.info("job trigger start");
                    List<String> connectionKeys = new ArrayList<>(connectionCache.keySet());
                    for (String connectionKey : connectionKeys) {
                        Connection serverCon = connectionCache.get(connectionKey);
                        if (serverCon.isDisposed()) {
                            connectionCache.remove(connectionKey);
                        } else {
                            serverCon.disposeNow();
                            bridgeConnections(serverCon);
                        }
                    }
                    //LOG.info("job trigger end");
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
