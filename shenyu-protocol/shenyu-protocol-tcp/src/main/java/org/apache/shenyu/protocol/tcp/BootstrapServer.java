package org.apache.shenyu.protocol.tcp;

import io.netty.handler.logging.LogLevel;
import io.netty.handler.logging.LoggingHandler;
import org.apache.shenyu.protocol.tcp.handler.CustomInboundHandler;
import org.apache.shenyu.protocol.tcp.handler.CustomOutboundHandler;
import reactor.netty.DisposableServer;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.TcpServer;



public class BootstrapServer {

    public static void start() {
        ShenyuTcpConfig shenyuTcpConfig = new ShenyuTcpConfig();
        shenyuTcpConfig.setBossThreads(1);
        shenyuTcpConfig.setWorkerThreads(10);
        LoopResources loopResources = LoopResources.create("my-elg", shenyuTcpConfig.getBossThreads(), shenyuTcpConfig.getWorkerThreads(), true);
        TcpServer tcpServer = TcpServer.create()
                .doOnChannelInit((connectionObserver, channel, remoteAddress) -> {
                    channel.pipeline().addFirst(new LoggingHandler(LogLevel.DEBUG));
                    channel.pipeline().addFirst(new CustomInboundHandler());
                    channel.pipeline().addFirst(new CustomOutboundHandler());
                })
                .port(9123)

                .runOn(loopResources);
        DisposableServer server = tcpServer.bindNow();
        server.onDispose().block();


    }

    public static void main(String[] args) {
        start();
    }


}
