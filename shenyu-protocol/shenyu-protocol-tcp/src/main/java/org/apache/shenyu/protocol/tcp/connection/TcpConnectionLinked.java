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
 *
 */
public class TcpConnectionLinked implements Linked {

    private static final Logger LOG = LoggerFactory.getLogger(TcpConnectionLinked.class);


    @Override
    public void link(Connection server, Connection client) {
        LOG.info("start server#inbound -> client#outbound");
        Disposable requestDisposable = bridge(server.inbound(), client.outbound(), this::logToRequest);
        LOG.info("start client#inbound -> server#outbound");
        Disposable responseDisposable = bridge(client.inbound(), server.outbound(), this::logToResponse);
        // binding dispose: when server connection is disposed ,client while close too.
        server.onDispose(Disposables.composite(requestDisposable, responseDisposable, client.channel()::close));
        client.onDispose(Disposables.composite(requestDisposable, responseDisposable, server.channel()::close));
    }

    private Disposable bridge(final NettyInbound inbound, final NettyOutbound outbound, final UnaryOperator<Flux<byte[]>> decorator) {
        final Flux<byte[]> flux;
        final Flux<byte[]> wapperFlux;
        flux = inbound.receive().asByteArray();
        wapperFlux = decorator.apply(flux);
        return wapperFlux.concatMap(next -> outbound.sendByteArray(Mono.just(next))).subscribe();
    }

    private final Flux<byte[]> logToRequest(final Flux<byte[]> flux) {

        return flux;
    }

    private final Flux<byte[]> logToResponse(final Flux<byte[]> flux) {

        return flux;
    }
}
