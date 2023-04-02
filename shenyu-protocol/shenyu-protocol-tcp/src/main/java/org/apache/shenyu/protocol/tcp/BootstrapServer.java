package org.apache.shenyu.protocol.tcp;

import io.netty.buffer.ByteBuf;
import io.netty.handler.logging.LogLevel;
import io.netty.handler.logging.LoggingHandler;
import io.netty.util.ReferenceCountUtil;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.ByteBufFlux;
import reactor.netty.Connection;
import reactor.netty.DisposableServer;
import reactor.netty.NettyOutbound;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.TcpClient;
import reactor.netty.tcp.TcpServer;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.function.Function;
import java.util.stream.Collectors;

public class BootstrapServer {
    public static void start() {
        ShenyuTcpConfig shenyuTcpConfig = new ShenyuTcpConfig();
        shenyuTcpConfig.setBossThreads(1);
        shenyuTcpConfig.setWorkerThreads(10);
        LoopResources loopResources = LoopResources.create("my-elg", shenyuTcpConfig.getBossThreads(), shenyuTcpConfig.getWorkerThreads(), true);
        Connection conn = getTcpClientConnection();
        TcpServer tcpServer = TcpServer.create()
                .doOnChannelInit((connectionObserver, channel, remoteAddress) -> {
                    channel.pipeline().addFirst(new LoggingHandler(LogLevel.DEBUG));
                })
                .port(9123)
                .handle((inbound, outbound) -> {
                    Flux<String> proxyFlux = inbound.receive().asString(StandardCharsets.UTF_8);
                    conn.outbound().sendString(proxyFlux).then().subscribe();
                    Flux<String> proxyRemoteFlux = conn.inbound().receive().asString().map(s->{
                        return "boot|" + s;
                    });
                    return outbound.sendString(proxyRemoteFlux);
                }).runOn(loopResources);
        DisposableServer server = tcpServer.bindNow();
        server.onDispose().block();
        conn.outbound().sendString(Mono.just("hello"));
        Flux<String> stringFlux = conn.inbound().receive().asString();
        String s = stringFlux.blockLast();
    }

    public static void main(String[] args) {
        start();
    }

    public static Connection getTcpClientConnection() {
        return TcpClient.create()
                .host("127.0.0.1").port(9124)
                .connectNow();
    }
}
