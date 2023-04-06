package org.apache.shenyu.protocol.tcp.handler;
import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
public class TestHandler  extends ChannelInboundHandlerAdapter {

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        System.out.println("CustomInboundHandler#channelRead");
        ByteBuf byteBuf  = (ByteBuf)msg;
        System.out.println(byteBuf);
        ctx.fireChannelRead(msg);
    }
}
