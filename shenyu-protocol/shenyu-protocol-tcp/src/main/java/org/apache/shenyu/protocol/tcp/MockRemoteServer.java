package org.apache.shenyu.protocol.tcp;

import io.netty.handler.logging.LoggingHandler;
import reactor.netty.DisposableServer;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.TcpServer;

public class MockRemoteServer {

    public static void start() {
        ShenyuTcpConfig shenyuTcpConfig = new ShenyuTcpConfig();
        shenyuTcpConfig.setBossThreads(1);
        shenyuTcpConfig.setWorkerThreads(10);
        LoopResources loopResources = LoopResources.create("my-elg", shenyuTcpConfig.getBossThreads(), shenyuTcpConfig.getWorkerThreads(), true);

        TcpServer tcpServer = TcpServer.create()
                .port(9123)
                .doOnChannelInit((observer, channel, remoteAddress) ->
                        channel.pipeline()
                                .addFirst(new LoggingHandler("reactor.netty.examples")));



    }
}
