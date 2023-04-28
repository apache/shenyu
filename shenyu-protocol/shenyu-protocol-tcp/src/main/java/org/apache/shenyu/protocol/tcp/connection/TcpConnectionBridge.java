package org.apache.shenyu.protocol.tcp.connection;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.Disposable;
import reactor.core.Disposables;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;
import reactor.netty.NettyInbound;
import reactor.netty.NettyOutbound;

import java.util.function.UnaryOperator;

/**
 * TcpConnectionBridge.
 */
public class TcpConnectionBridge implements Bridge {

    private static final Logger LOG = LoggerFactory.getLogger(TcpConnectionBridge.class);

    @Override
    public void bridge(Connection server, Connection client) {
        //   LOG.info("start server#inbound -> client#outbound");
        Disposable requestDisposable = bridge(server.inbound(), client.outbound());
        //  LOG.info("start client#inbound -> server#outbound");
        Disposable responseDisposable = bridge(client.inbound(), server.outbound());
        // binding dispose: when server connection is disposed ,client while close too.
        server.onDispose(Disposables.composite(requestDisposable, responseDisposable, client.channel()::close));
        client.onDispose(Disposables.composite(requestDisposable, responseDisposable, server.channel()::close));
    }

    private Disposable bridge(final NettyInbound inbound, final NettyOutbound outbound) {
        Flux<byte[]> flux = inbound.receive().asByteArray();
        return flux.concatMap(next -> outbound.sendByteArray(Mono.just(next))).subscribe();
    }

}
