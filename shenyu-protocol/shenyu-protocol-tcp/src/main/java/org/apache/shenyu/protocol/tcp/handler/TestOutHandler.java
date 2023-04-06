package org.apache.shenyu.protocol.tcp.handler;

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelOutboundHandlerAdapter;
import io.netty.channel.ChannelPromise;
import org.apache.shenyu.protocol.tcp.connection.ConnectionContext;

public class TestOutHandler extends ChannelOutboundHandlerAdapter {
    @Override
    public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
        System.out.println("TestOutHandler#write");
        System.out.println(msg);
        ctx.write(msg, promise);
    }
}
