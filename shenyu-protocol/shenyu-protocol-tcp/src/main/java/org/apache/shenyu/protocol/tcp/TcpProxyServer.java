package org.apache.shenyu.protocol.tcp;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.proxy.ProxyHandler;
import io.netty.handler.proxy.Socks5ProxyHandler;
import io.netty.handler.ssl.SslContext;
import io.netty.handler.ssl.SslContextBuilder;
import io.netty.handler.ssl.SslProvider;
import io.netty.handler.ssl.util.InsecureTrustManagerFactory;
import io.netty.handler.timeout.IdleStateHandler;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.tcp.TcpServer;

import javax.net.ssl.SSLException;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.concurrent.TimeUnit;

import java.net.InetSocketAddress;

public class TcpProxyServer {

    private static final String PROXY_HOST = "proxyHost";
    private static final String PROXY_PORT = "proxyPort";
    private static final String SSL_ENABLED = "sslEnabled";
    private static final int IDLE_TIMEOUT_SECONDS = 30;

    public static void main(String[] args) {
        // 从 Java 配置文件中读取代理服务器的 IP 和端口号
        String proxyHost = System.getProperty(PROXY_HOST);
        int proxyPort = Integer.parseInt(System.getProperty(PROXY_PORT));

        // 创建 SSL 上下文
        boolean sslEnabled = Boolean.parseBoolean(System.getProperty(SSL_ENABLED, "false"));
        SslContext sslContext = null;
        if (sslEnabled) {
            try {
                sslContext = SslContextBuilder.forClient()
                        .sslProvider(SslProvider.JDK)
                        .trustManager(InsecureTrustManagerFactory.INSTANCE)
                        .build();
            } catch (SSLException e) {
                e.printStackTrace();
            }
        }

        // 创建 TCP 服务器
        DisposableServer server = TcpServer.create()
                .bindAddress(() -> new InetSocketAddress("localhost", 8080))
                .doOnConnection(connection -> {
                    ChannelPipeline pipeline = connection.channel().pipeline();

                    // 添加空闲状态处理程序，以便在连接空闲时自动关闭连接
                    pipeline.addLast(new IdleStateHandler(IDLE_TIMEOUT_SECONDS, 0, 0, TimeUnit.SECONDS));

                    // 添加自定义处理程序
                    pipeline.addLast(new MyChannelDuplexHandler(proxyHost, proxyPort));
                })
                .bindNow();

        // 等待服务器关闭
        server.onDispose().block();
    }

    private static class MyChannelDuplexHandler extends ChannelDuplexHandler {

        private final String proxyHost;
        private final int proxyPort;

        private MyChannelDuplexHandler(String proxyHost, int proxyPort) {
            this.proxyHost = proxyHost;
            this.proxyPort = proxyPort;
        }

        @Override
        public void channelActive(ChannelHandlerContext ctx) {
            // 建立与远程代理的连接
            ProxyHandler proxyHandler = new Socks5ProxyHandler(new InetSocketAddress(proxyHost, proxyPort));
            ctx.channel().pipeline().addFirst(proxyHandler);
        }

        @Override
        public void channelInactive(ChannelHandlerContext ctx) {
            // 当连接关闭时，关闭与远程代理的连接
            ctx.channel().pipeline().remove(ProxyHandler.class);
        }

        @Override
        public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
            // 在发生异常时关闭连接
            ctx.close();
        }
    }

}