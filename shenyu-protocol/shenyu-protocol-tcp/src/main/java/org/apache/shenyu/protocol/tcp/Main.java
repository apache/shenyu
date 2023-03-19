package org.apache.shenyu.protocol.tcp;

import io.netty.channel.ChannelOption;
import io.netty.handler.logging.LoggingHandler;
import io.netty.handler.timeout.ReadTimeoutHandler;
import reactor.netty.DisposableServer;
import reactor.netty.tcp.TcpServer;
import reactor.netty.tcp.TcpServerConfig;

import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class Main {


    public static void main(String[] args) throws Exception{

        DisposableServer server =
                TcpServer.create()
                        .doOnConnection(conn ->
                                conn.addHandlerFirst(new ReadTimeoutHandler(10, TimeUnit.SECONDS)))
                        .doOnChannelInit((observer, channel, remoteAddress) ->
                                channel.pipeline()
                                        .addFirst(new LoggingHandler("org.apache.shenyu.protocol.tcp")))
                        .bindNow();



        server.onDispose()
                .block();




    }
}
