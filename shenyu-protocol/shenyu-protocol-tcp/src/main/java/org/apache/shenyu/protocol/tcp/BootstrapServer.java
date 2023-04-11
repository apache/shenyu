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
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;


public class BootstrapServer {
    private static final Logger LOG = LoggerFactory.getLogger(BootstrapServer.class);
    public Linked link;
    private ConnectionContext connectionContext;

    private ConnectionHolder holder;

    public void init(Properties properties) {
        try {
            ClientConnectionConfigProviderFactory factory = ClientConnectionConfigProviderFactory.getInstance();
            this.link = new TcpConnectionLinked();
            this.holder = new ConnectionHolder();
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
        triggerJob();
        server.onDispose().block();
    }

    private void bridgeConnections(final Connection serverConn) {
        LOG.debug("Starting proxy client");
        Mono<Connection> client = connectionContext.getTcpClientConnection();
        String clientConnectionKey = connectionContext.getClientConnectionKey();
        holder.put(clientConnectionKey, serverConn);
        // Connect to client, and react when connection becomes available
        client.subscribe((clientConn) -> {
            LOG.debug("Bridging connection with {}", link);
            link.link(serverConn, clientConn);
        });
    }


    public void triggerJob() {
        ScheduledThreadPoolExecutor scheduledThreadPoolExecutor = new ScheduledThreadPoolExecutor(1);
       Runnable runnable =  ()->{
           try{
               LOG.info("job trigger start");
               ArrayList<Connection> testClient1 = new ArrayList<>(holder.getConnectionList("TEST_CLIENT"));
               for (Connection serverCon : testClient1) {
                   serverCon.disposeNow();
                   bridgeConnections(serverCon);
               }
               LOG.info("job trigger end");
           }catch (Throwable tx){
               LOG.error("error",tx);
           }
        };
        scheduledThreadPoolExecutor.schedule(runnable ,20, TimeUnit.SECONDS );
    }

    public static void main(String[] args) {
        BootstrapServer bootstrapServer = new BootstrapServer();
        bootstrapServer.init(new Properties());
        bootstrapServer.start();

    }


}
