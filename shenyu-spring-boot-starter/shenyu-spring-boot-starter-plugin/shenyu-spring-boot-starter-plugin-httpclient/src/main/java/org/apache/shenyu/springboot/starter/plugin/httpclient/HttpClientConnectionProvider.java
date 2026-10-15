/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import io.netty.resolver.AddressResolverGroup;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.netty.Connection;
import reactor.netty.ConnectionObserver;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.transport.TransportConfig;

import java.net.SocketAddress;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * Connect HTTP request cancellation to pending acquisitions, independently of metrics enablement.
 */
final class HttpClientConnectionProvider implements ConnectionProvider {

    private static final Object CANCELLATION = new Object();

    private final ConnectionProvider delegate;

    private final HttpClientPoolMetrics metrics;

    HttpClientConnectionProvider(final ConnectionProvider delegate, final boolean metricsEnabled) {
        this.delegate = delegate;
        metrics = metricsEnabled ? new HttpClientPoolMetrics(delegate.name()) : null;
    }

    static Mono<? extends Connection> propagateCancellation(final Mono<? extends Connection> connector) {
        return Mono.defer(() -> {
            Sinks.Empty<Void> cancellation = Sinks.empty();
            return connector.contextWrite(context -> context.put(CANCELLATION, cancellation.asMono()))
                    .doOnCancel(cancellation::tryEmitEmpty);
        });
    }

    @Override
    public Mono<? extends Connection> acquire(final TransportConfig config, final ConnectionObserver observer,
                                              final Supplier<? extends SocketAddress> remoteAddress, final AddressResolverGroup<?> resolver) {
        return Mono.deferContextual(context -> {
            Mono<? extends Connection> pending = delegate.acquire(config, observer, remoteAddress, resolver);
            if (Objects.nonNull(metrics)) {
                pending = metrics.observe(pending);
            }
            return context.hasKey(CANCELLATION) ? pending.takeUntilOther(context.<Mono<Void>>get(CANCELLATION)) : pending;
        });
    }

    @Override
    public void disposeWhen(final SocketAddress remoteAddress) {
        delegate.disposeWhen(remoteAddress);
    }

    @Override
    public void dispose() {
        delegate.dispose();
    }

    @Override
    public Mono<Void> disposeLater() {
        return delegate.disposeLater();
    }

    @Override
    public boolean isDisposed() {
        return delegate.isDisposed();
    }

    @Override
    public int maxConnections() {
        return delegate.maxConnections();
    }

    @Override
    public Map<SocketAddress, Integer> maxConnectionsPerHost() {
        return delegate.maxConnectionsPerHost();
    }

    @Override
    public Builder mutate() {
        return delegate.mutate();
    }

    @Override
    public String name() {
        return delegate.name();
    }
}
