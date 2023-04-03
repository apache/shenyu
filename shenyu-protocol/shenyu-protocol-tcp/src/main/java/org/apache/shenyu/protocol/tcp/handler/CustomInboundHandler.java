package org.apache.shenyu.protocol.tcp.handler;

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import org.apache.shenyu.protocol.tcp.connection.ConnectionContext;
import reactor.core.publisher.Flux;
import reactor.netty.Connection;
import reactor.netty.NettyInbound;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;

public class CustomInboundHandler extends ChannelInboundHandlerAdapter {

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        NettyInbound inbound = (NettyInbound) msg;
        // Get the client's IP address and port
        InetSocketAddress socketAddress = (InetSocketAddress) ctx.channel().remoteAddress();
        String clientIp = socketAddress.getAddress().getHostAddress();
        int clientPort = socketAddress.getPort();
        Flux<String> proxyFlux = inbound.receive().asString(StandardCharsets.UTF_8).doOnNext(s -> {
            System.out.println("CustomInboundHandler : " + s);
        });
        Connection conn = ConnectionContext.getTcpClientConnection(clientIp, clientPort);
        conn.outbound().sendString(proxyFlux).then().subscribe();
    }

}
