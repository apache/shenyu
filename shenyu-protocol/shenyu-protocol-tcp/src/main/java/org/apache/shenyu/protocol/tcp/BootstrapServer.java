package org.apache.shenyu.protocol.tcp;

import reactor.netty.DisposableServer;
import reactor.netty.resources.LoopResources;
import reactor.netty.tcp.TcpServer;

public class BootstrapServer {


    public static void start(){
        ShenyuTcpConfig shenyuTcpConfig = new ShenyuTcpConfig();
        shenyuTcpConfig.setBossThreads(1);
        shenyuTcpConfig.setWorkerThreads(10);
        LoopResources loopResources = LoopResources.create("my-elg", shenyuTcpConfig.getBossThreads(), shenyuTcpConfig.getWorkerThreads(), true);

        TcpServer tcpServer = TcpServer.create()
                .port(9123)
                .handle((in, out) -> {
                    return out.sendString(in.receive()
                            .asString()
                            .map(s -> {
                                System.out.println(s);
                                return s + " World!";
                            })
                            .log("channel"));
                })
                .runOn(loopResources);
        DisposableServer server = tcpServer.bindNow();
        server.onDispose().block();
    }


    public static void main(String[] args) {
        start();
    }








}
