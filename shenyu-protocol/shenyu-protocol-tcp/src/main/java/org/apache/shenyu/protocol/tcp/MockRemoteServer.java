package org.apache.shenyu.protocol.tcp;

import io.netty.handler.logging.LoggingHandler;
import reactor.core.publisher.Flux;
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
                .port(9124)
                .handle((in, out) -> {
                    System.out.println("handle((in, out)");
                    Flux<String> channel = in.receive()
                            .asString()
                            .map(s -> {
                                System.out.println(s);
                                return  "remote|"+ s ;
                            })
                            .log("channel");
                    return out.sendString(channel);
                })
                .runOn(loopResources);
        DisposableServer server = tcpServer.bindNow();
        server.onDispose().block();
    }


    public static void main(String[] args) {
        start();
    }
}
