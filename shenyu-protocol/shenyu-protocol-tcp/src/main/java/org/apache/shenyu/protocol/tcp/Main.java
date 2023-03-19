package org.apache.shenyu.protocol.tcp;
import reactor.core.publisher.Flux;
import reactor.netty.Connection;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.tcp.TcpClient;

import java.time.Duration;


public class Main {
    public static void main(String[] args) {
        ConnectionProvider provider = ConnectionProvider.create("myPool");
        Connection connection =
                TcpClient.create(provider)
                        .host("127.0.0.1")
                        .port(9123)
                        .handle((inbound, outbound) ->{
                            Flux<String> map = Flux.interval(Duration.ofSeconds(1)).map(i -> "hello");
                            inbound.receive().asString().doOnNext(System.out::println).subscribe();
                            return  outbound.sendString(map);
                        }).connectNow();
        connection.onDispose()
                .block();
    }
}
