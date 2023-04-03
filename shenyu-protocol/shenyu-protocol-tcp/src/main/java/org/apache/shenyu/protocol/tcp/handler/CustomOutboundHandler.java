package org.apache.shenyu.protocol.tcp.handler;

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelOutboundHandlerAdapter;
import io.netty.channel.ChannelPromise;
import org.apache.shenyu.protocol.tcp.connection.ConnectionContext;
import reactor.core.publisher.Flux;
import reactor.netty.Connection;
import reactor.netty.NettyOutbound;

import java.net.InetSocketAddress;

public class CustomOutboundHandler extends ChannelOutboundHandlerAdapter {
    @Override
    public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
        NettyOutbound outbound = (NettyOutbound) msg;
        InetSocketAddress socketAddress = (InetSocketAddress) ctx.channel().remoteAddress();
        String clientIp = socketAddress.getAddress().getHostAddress();
        int clientPort = socketAddress.getPort();
        Connection conn = ConnectionContext.getTcpClientConnection(clientIp, clientPort);
        Flux<String> proxyRemoteFlux = conn.inbound().receive().asString().map(s -> {
            System.out.println("CustomOutboundHandler : " + s);
            return "boot|" + s;
        });
        outbound.sendString(proxyRemoteFlux);
    }


}
