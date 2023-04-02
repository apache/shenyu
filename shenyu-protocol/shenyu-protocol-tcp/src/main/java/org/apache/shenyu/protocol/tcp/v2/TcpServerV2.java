package org.apache.shenyu.protocol.tcp.v2;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;
import reactor.netty.tcp.TcpClient;
import reactor.netty.tcp.TcpServer;

public class TcpServerV2 {
    public static void main(String[] args) {
//        TcpServer.create()
//                .handle((inbound, outbound) -> {
//                    // 连接目标服务器
//                    Mono<? extends Connection> targetConnectionMono = TcpClient.create()
//                            .host("127.0.0.1")
//                            .port(8080)
//                            .connect();
//
//                    // 处理客户端请求
//                    inbound.receive()
//                            .retain()
//                            .subscribe(
//                                    data -> {
//                                        targetConnectionMono.subscribe(targetConnection -> {
//                                            targetConnection.channel().writeAndFlush(data);
//                                            targetConnection.inbound().receive()
//                                                    .subscribe(targetData -> {
//                                                        inbound.send(targetData.retain());
//                                                    });
//                                        });
//                                    },
//                                    err -> {
//                                        err.printStackTrace();
//                                        inbound.context().close();
//                                    },
//                                    () -> inbound.context().close()
//                            );
//
//                    // 处理服务器响应
//                    outbound.send(
//                            Flux.from(inbound)
//                                    .doOnNext(data -> {
//                                        System.out.println("Server response: " + data.toString());
//                                    })
//                                    .map(ByteBuf::retain)
//                    );
//                    return Mono.never();
//                })
//                .bind()
//                .block()
//                .onDispose()
//                .block();
    }

}
