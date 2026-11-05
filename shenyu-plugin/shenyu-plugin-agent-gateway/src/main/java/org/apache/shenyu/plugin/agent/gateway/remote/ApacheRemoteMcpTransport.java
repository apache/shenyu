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

package org.apache.shenyu.plugin.agent.gateway.remote;

import org.apache.http.conn.routing.HttpRoute;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.client.RedirectStrategy;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.concurrent.FutureCallback;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.nio.IOControl;
import org.apache.http.impl.nio.client.CloseableHttpAsyncClient;
import org.apache.http.impl.nio.client.HttpAsyncClientBuilder;
import org.apache.http.impl.nio.conn.ManagedNHttpClientConnectionFactory;
import org.apache.http.impl.nio.conn.PoolingNHttpClientConnectionManager;
import org.apache.http.impl.nio.reactor.DefaultConnectingIOReactor;
import org.apache.http.impl.nio.reactor.IOReactorConfig;
import org.apache.http.nio.conn.SchemeIOSessionStrategy;
import org.apache.http.nio.conn.ssl.SSLIOSessionStrategy;
import org.apache.http.nio.protocol.BasicAsyncRequestProducer;
import org.apache.http.nio.protocol.HttpAsyncRequestProducer;
import org.apache.http.nio.protocol.HttpAsyncResponseConsumer;
import org.apache.http.nio.reactor.ConnectingIOReactor;
import org.apache.http.nio.reactor.IOReactorException;
import org.apache.http.protocol.HttpContext;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import javax.net.ssl.SSLContext;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Objects;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

/**
 * One exclusively owned, bounded pooled transport per target and credential generation.
 * The connection manager maps the approved hostname only to its approved IPv4 address;
 * TLS and SNI continue to use the original HttpHost hostname.
 */
final class ApacheRemoteMcpTransport implements AutoCloseable {

    private static final int MAX_POOL = 8;

    private final ConnectingIOReactor reactor;

    private final PoolingNHttpClientConnectionManager connections;

    private final CloseableHttpAsyncClient client;

    private ApacheRemoteMcpTransport(final ConnectingIOReactor reactor, final PoolingNHttpClientConnectionManager connections,
                                     final CloseableHttpAsyncClient client) {
        this.reactor = reactor;
        this.connections = connections;
        this.client = client;
    }

    static ApacheRemoteMcpTransport create(final RemoteTransportRoute route, final SSLContext testContext) {
        SSLContext context;
        try {
            context = Objects.isNull(testContext) ? SSLContext.getDefault() : testContext;
        } catch (java.security.NoSuchAlgorithmException error) {
            throw new IllegalStateException("Remote MCP TLS initialization failed");
        }
        try {
            IOReactorConfig reactorConfig = IOReactorConfig.custom()
                    .setIoThreadCount(1)
                    .setConnectTimeout(2000)
                    .setTcpNoDelay(true)
                    .setSoKeepAlive(true)
                    .setSndBufSize(8192)
                    .setRcvBufSize(8192)
                    .build();
            ConnectingIOReactor reactor = new DefaultConnectingIOReactor(reactorConfig, task -> {
                Thread thread = new Thread(task, "shenyu-agent-mcp-transport");
                thread.setDaemon(true);
                return thread;
            });
            Registry<SchemeIOSessionStrategy> strategies = RegistryBuilder.<SchemeIOSessionStrategy>create()
                    .register("https", new SSLIOSessionStrategy(context))
                    .register("http", NoopIOSessionStrategy.INSTANCE)
                    .build();
            PoolingNHttpClientConnectionManager connections = new PoolingNHttpClientConnectionManager(
                    reactor, ManagedNHttpClientConnectionFactory.INSTANCE, strategies, new FixedAddressResolver(route), 30, TimeUnit.SECONDS);
            connections.setMaxTotal(MAX_POOL);
            connections.setDefaultMaxPerRoute(MAX_POOL);
            CloseableHttpAsyncClient client = HttpAsyncClientBuilder.create()
                    .setConnectionManager(connections)
                    .setMaxConnTotal(MAX_POOL)
                    .setMaxConnPerRoute(MAX_POOL)
                    .setConnectionTimeToLive(30, TimeUnit.SECONDS)
                    .setRedirectStrategy(new NeverRedirectStrategy())
                    .disableConnectionState()
                    .disableCookieManagement()
                    .disableAuthCaching()
                    .build();
            client.start();
            return new ApacheRemoteMcpTransport(reactor, connections, client);
        } catch (IOReactorException error) {
            throw new IllegalStateException("Remote MCP transport initialization failed");
        }
    }

    AsyncRequestHandle<ApacheRemoteHttpResponse> execute(final HttpHost target, final HttpRequest request,
                                                         final long deadlineMillis) {
        int timeout = deadlineMillis > Integer.MAX_VALUE ? Integer.MAX_VALUE : Math.max(1, (int) deadlineMillis);
        RequestConfig config = RequestConfig.custom()
                .setRedirectsEnabled(false)
                .setConnectionRequestTimeout(timeout)
                .setConnectTimeout(timeout)
                .setSocketTimeout(timeout)
                .build();
        HttpRequest configured = request instanceof ConfigurableRequest configurable
                ? configurable.withConfig(config)
                : request;
        HttpAsyncRequestProducer producer = new BasicAsyncRequestProducer(target, configured);
        Sinks.One<ApacheRemoteHttpResponse> completion = Sinks.one();
        Future<?> owned = client.execute(producer, new BoundedResponseConsumer(), new FutureCallback<ApacheRemoteHttpResponse>() {
            @Override
            public void completed(final ApacheRemoteHttpResponse result) {
                completion.tryEmitValue(result);
            }

            @Override
            public void failed(final Exception error) {
                completion.tryEmitError(error);
            }

            @Override
            public void cancelled() {
                completion.tryEmitError(new IllegalStateException("Remote MCP request cancelled"));
            }
        });
        return new AsyncRequestHandle<>(owned, completion);
    }

    @Override
    public void close() throws IOException {
        try {
            client.close();
        } finally {
            connections.shutdown();
            reactor.shutdown();
        }
    }

    /** Bind the approved route and port while retaining the original target hostname. */
    private static final class FixedAddressResolver implements org.apache.http.nio.pool.SocketAddressResolver<HttpRoute> {

        private final RemoteTransportRoute route;

        private FixedAddressResolver(final RemoteTransportRoute route) {
            this.route = route;
        }

        @Override
        public SocketAddress resolveLocalAddress(final HttpRoute key) {
            return null;
        }

        @Override
        public SocketAddress resolveRemoteAddress(final HttpRoute key) {
            HttpHost target = key.getTargetHost();
            if (!route.endpoint().getHost().equalsIgnoreCase(target.getHostName()) || route.endpoint().getPort() != target.getPort()) {
                throw new IllegalStateException("Unauthorized remote MCP route");
            }
            return new InetSocketAddress(route.address(), route.endpoint().getPort());
        }
    }

    /** Bridge the client future to the already subscribed Reactor signal without blocking callers. */
    static final class AsyncRequestHandle<T> implements Future<T> {

        private final Future<?> owned;

        private final Sinks.One<T> completion;

        /** Bridge a completion-stage-like future for isolated tests and production callbacks. */
        <F extends Future<T> & java.util.concurrent.CompletionStage<T>> AsyncRequestHandle(final F owned) {
            this(owned, Sinks.one());
            owned.whenComplete((value, error) -> {
                if (Objects.nonNull(error)) {
                    completion.tryEmitError(error);
                } else {
                    completion.tryEmitValue(value);
                }
            });
        }

        private AsyncRequestHandle(final Future<?> owned, final Sinks.One<T> completion) {
            this.owned = owned;
            this.completion = completion;
        }

        Mono<T> signal() {
            return completion.asMono();
        }

        @Override
        public boolean cancel(final boolean mayInterruptIfRunning) {
            return owned.cancel(mayInterruptIfRunning);
        }

        @Override
        public boolean isCancelled() {
            return owned.isCancelled();
        }

        @Override
        public boolean isDone() {
            return owned.isDone();
        }

        @Override
        public T get() {
            return block();
        }

        @Override
        public T get(final long timeout, final TimeUnit unit) throws InterruptedException, java.util.concurrent.TimeoutException {
            T value = completion.asMono().block(java.time.Duration.ofMillis(unit.toMillis(timeout)));
            if (Objects.isNull(value)) {
                throw new java.util.concurrent.TimeoutException();
            }
            return value;
        }

        private T block() {
            return completion.asMono().block();
        }
    }

    /** Redirect execution is intentionally disabled by returning no redirect request. */
    private static final class NeverRedirectStrategy implements RedirectStrategy {
        @Override
        public boolean isRedirected(final HttpRequest request, final HttpResponse response, final HttpContext context) {
            return false;
        }

        @Override
        public HttpUriRequest getRedirect(final HttpRequest request, final HttpResponse response, final HttpContext context) {
            return null;
        }
    }

    /** Exposes per-request timeouts to HttpAsyncClient's Configurable contract. */
    static final class ConfigurableRequest extends org.apache.http.message.BasicHttpEntityEnclosingRequest
            implements org.apache.http.client.methods.Configurable {

        private RequestConfig config;

        ConfigurableRequest(final String method, final String uri) {
            super(method, uri);
        }

        ConfigurableRequest withConfig(final RequestConfig value) {
            this.config = value;
            return this;
        }

        @Override
        public RequestConfig getConfig() {
            return config;
        }
    }

    /** Bounded response consumer retaining only headers and at most 8 KiB of body. */
    private static final class BoundedResponseConsumer implements HttpAsyncResponseConsumer<ApacheRemoteHttpResponse> {

        private static final int LIMIT = 8192;

        private final ByteArrayOutputStream body = new ByteArrayOutputStream();

        private volatile boolean completed;

        private volatile ApacheRemoteHttpResponse response;

        private volatile Exception failure;

        private volatile boolean cancelled;

        @Override
        public void responseReceived(final HttpResponse response) {
            this.response = ApacheRemoteHttpResponse.from(response, body);
        }

        @Override
        public void consumeContent(final org.apache.http.nio.ContentDecoder decoder, final IOControl ioControl) throws IOException {
            ByteBuffer buffer = ByteBuffer.allocate(4096);
            int count;
            while ((count = decoder.read(buffer)) > 0) {
                if (body.size() + count > LIMIT) {
                    throw new IOException("Response byte limit");
                }
                buffer.flip();
                body.write(buffer.array(), 0, count);
                buffer.clear();
            }
        }

        @Override
        public void responseCompleted(final HttpContext context) {
            completed = true;
        }

        @Override
        public void failed(final Exception exception) {
            failure = exception;
        }

        @Override
        public void close() throws IOException {
            // The owned stream is only a bounded in-memory adapter.
        }

        @Override
        public boolean cancel() {
            cancelled = true;
            return true;
        }

        @Override
        public ApacheRemoteHttpResponse getResult() {
            return completed && Objects.isNull(failure) && !cancelled ? response : null;
        }

        @Override
        public Exception getException() {
            return failure;
        }

        @Override
        public boolean isDone() {
            return completed || Objects.nonNull(failure) || cancelled;
        }

    }

    /** Plain-HTTP strategy retained only for the historical loopback fixed-IP contract. */
    private enum NoopIOSessionStrategy implements SchemeIOSessionStrategy {
        INSTANCE;

        @Override
        public org.apache.http.nio.reactor.IOSession upgrade(final org.apache.http.HttpHost host,
                                                             final org.apache.http.nio.reactor.IOSession iosession) {
            return iosession;
        }

        @Override
        public boolean isLayeringRequired() {
            return false;
        }
    }
}
