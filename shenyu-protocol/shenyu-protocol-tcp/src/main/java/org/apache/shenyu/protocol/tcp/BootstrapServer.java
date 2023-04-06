package org.apache.shenyu.protocol.tcp;

import io.netty.handler.logging.LogLevel;
import io.netty.handler.logging.LoggingHandler;
import org.apache.shenyu.protocol.tcp.connection.ConnectionContext;
import org.apache.shenyu.protocol.tcp.handler.TestHandler;
import org.apache.shenyu.protocol.tcp.handler.TestOutHandler;
import reactor.core.publisher.Flux;
import reactor.netty.Connection;
import reactor.netty.DisposableServer;
import reactor.netty.channel.ChannelOperations;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.TcpServer;

import java.nio.charset.StandardCharsets;


public class BootstrapServer {

    public static void start() {
        ShenyuTcpConfig shenyuTcpConfig = new ShenyuTcpConfig();
        shenyuTcpConfig.setBossThreads(1);
        shenyuTcpConfig.setWorkerThreads(10);
        LoopResources loopResources = LoopResources.create("my-elg", shenyuTcpConfig.getBossThreads(), shenyuTcpConfig.getWorkerThreads(), true);
        Connection conn = ConnectionContext.getTcpClientConnection("127.0.0.1", 9124);
        TcpServer tcpServer = TcpServer.create()
                .doOnChannelInit((connectionObserver, channel, remoteAddress) -> {
                    channel.pipeline().addFirst(new LoggingHandler(LogLevel.DEBUG));
                    channel.pipeline().addFirst(new TestHandler());
                    channel.pipeline().addFirst(new TestOutHandler());
                })
                .port(9123)
                .handle((inbound, outbound) -> {
                    ChannelOperations inOp  = (ChannelOperations)inbound;
                    Flux<String> proxyFlux = inOp.receive().asString(StandardCharsets.UTF_8).map(s -> {
                        System.out.println("handle " + s);
                        return s;
                    });
                    conn.outbound().sendString(proxyFlux).then().subscribe();
                    Flux<String> proxyRemoteFlux = conn.inbound().receive().asString().map(s -> {
                        System.out.println(s);
                        return "boot|" + s;
                    });
                    return outbound.sendString(proxyRemoteFlux);
                }).runOn(loopResources);
        DisposableServer server = tcpServer.bindNow();
        server.onDispose().block();
    }

    public static void main(String[] args) {
        start();
    }



}
