package org.apache.shenyu.protocol.tcp;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;
import reactor.netty.resources.ConnectionProvider;

import java.time.Duration;


public class TcpClient {
    public static void main(String[] args) throws InterruptedException {
        ConnectionProvider provider = ConnectionProvider.create("myPool");
        Connection connection =
                reactor.netty.tcp.TcpClient.create(provider)
                        .host("127.0.0.1")
                        .port(9123)
                        .handle((inbound, outbound) -> {
                            Flux<String> map = Flux.interval(Duration.ofSeconds(1)).map(i -> "hello");
                            inbound.receive().asString().doOnNext(System.out::println).subscribe();
                            System.out.println("client send");
                            return outbound.sendString(map);
                        }).connectNow();
        connection.onDispose().block();

    }
}
