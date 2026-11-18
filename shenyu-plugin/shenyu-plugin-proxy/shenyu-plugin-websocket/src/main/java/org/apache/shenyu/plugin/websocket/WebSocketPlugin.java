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

package org.apache.shenyu.plugin.websocket;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.WebSocketRuleHandle;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.LoadbalancerUtils;
import org.apache.shenyu.plugin.websocket.handler.WebSocketPluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.lang.NonNull;
import org.springframework.util.StringUtils;
import org.springframework.web.reactive.socket.CloseStatus;
import org.springframework.web.reactive.socket.WebSocketHandler;
import org.springframework.web.reactive.socket.WebSocketMessage;
import org.springframework.web.reactive.socket.WebSocketSession;
import org.springframework.web.reactive.socket.client.WebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import java.net.URI;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

/**
 * The type Web socket plugin.
 */
public class WebSocketPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(WebSocketPlugin.class);

    private static final String SEC_WEB_SOCKET_PROTOCOL = "Sec-WebSocket-Protocol";

    private static final String REASON_HANDSHAKE_TIMEOUT = "upstream websocket handshake timed out";

    private static final String REASON_IDLE_TIMEOUT = "websocket session idle timeout";

    private static final Duration MIN_IDLE_CHECK_INTERVAL = Duration.ofMillis(1);

    private final WebSocketClient webSocketClient;

    private final WebSocketService webSocketService;

    /**
     * Instantiates a new Web socket plugin.
     *
     * @param webSocketClient  the web socket client
     * @param webSocketService the web socket service
     */
    public WebSocketPlugin(final WebSocketClient webSocketClient, final WebSocketService webSocketService) {
        this.webSocketClient = webSocketClient;
        this.webSocketService = webSocketService;
    }
    
    @Override
    protected String getRawPath(final ServerWebExchange exchange) {
        return RequestUrlUtils.getRewrittenRawPath(exchange);
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        final List<Upstream> upstreamList = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selector.getId());
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        if (CollectionUtils.isEmpty(upstreamList) || Objects.isNull(shenyuContext)) {
            LOG.error("websocket upstream configuration error：{}", rule);
            return chain.execute(exchange);
        }
        final WebSocketRuleHandle ruleHandle = buildRuleHandle(rule);
        Upstream upstream = LoadbalancerUtils.getForExchange(upstreamList, ruleHandle.getLoadBalance(), exchange);
        if (Objects.isNull(upstream)) {
            LOG.error("websocket has no upstream, error:{}", rule);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        URI wsRequestUrl = buildWsRealPath(exchange, upstream, shenyuContext);
        if (LOG.isDebugEnabled()) {
            LOG.debug("websocket upstream url: {}", toLogUrl(wsRequestUrl));
        }
        HttpHeaders headers = exchange.getRequest().getHeaders();
        // The Mono returned by the WebSocket service completes when the upgrade handshake finishes; the
        // proxied session then runs on its own subscription. The count therefore has to be attached to the
        // handler, which is where the session actually lives, not to the handshake.
        WebSocketHandler handler = new ShenyuWebSocketHandler(wsRequestUrl, this.webSocketClient,
                filterHeaders(headers), buildWsProtocols(headers),
                Timeouts.ofMillis(ruleHandle.getTimeout(), ruleHandle.getIdleTimeout()));
        final boolean selectionAlreadyReserved = LoadBalanceEnum.SHORTEST_RESPONSE.getName().equals(ruleHandle.getLoadBalance());
        if (LoadBalanceEnum.LEAST_ACTIVITY.getName().equals(ruleHandle.getLoadBalance()) || selectionAlreadyReserved) {
            InflightWebSocketHandler inflightHandler = new InflightWebSocketHandler(handler, upstream, selectionAlreadyReserved);
            handler = inflightHandler;
            Mono<Void> handshake = this.webSocketService.handleRequest(exchange, handler);
            if (selectionAlreadyReserved) {
                // Selection already reserved this attempt. The handshake Mono completes when the upgrade
                // finishes, while the session keeps running on its own subscription, so release here only
                // when that session never started. A session that does start releases itself exactly once.
                return handshake.doFinally(signalType -> inflightHandler.releaseSelectionIfSessionDidNotStart());
            }
            return handshake;
        }
        return this.webSocketService.handleRequest(exchange, handler);
    }

    private WebSocketRuleHandle buildRuleHandle(final RuleData rule) {
        return WebSocketPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
    }

    private URI buildWsRealPath(final ServerWebExchange exchange, final Upstream upstream, final ShenyuContext shenyuContext) {
        String protocol = upstream.getProtocol();
        if (!StringUtils.hasLength(protocol)) {
            protocol = "ws://";
        }
        return RequestUrlUtils.buildRequestUri(exchange, upstream.buildDomain(protocol));
    }

    /**
     * Describe the upstream url for logging without the query, fragment or user info,
     * which may carry credentials such as {@code ?token=...}.
     *
     * @param uri the upstream url
     * @return scheme, host, port and path of the url
     */
    static String toLogUrl(final URI uri) {
        StringBuilder builder = new StringBuilder();
        builder.append(uri.getScheme()).append("://").append(uri.getHost());
        if (uri.getPort() != -1) {
            builder.append(':').append(uri.getPort());
        }
        return builder.append(Objects.toString(uri.getRawPath(), "")).toString();
    }

    private List<String> buildWsProtocols(final HttpHeaders headers) {
        List<String> protocols = headers.get(SEC_WEB_SOCKET_PROTOCOL);
        if (CollectionUtils.isEmpty(protocols)) {
            return protocols;
        }
        return protocols.stream()
                .flatMap(header -> Arrays.stream(StringUtils.commaDelimitedListToStringArray(header)))
                .map(String::trim)
                .collect(Collectors.toList());
    }

    private HttpHeaders filterHeaders(final HttpHeaders headers) {
        HttpHeaders filtered = new HttpHeaders();
        headers.entrySet().stream()
                .filter(entry -> !entry.getKey().toLowerCase()
                        .startsWith("sec-websocket"))
                .forEach(header -> filtered.addAll(header.getKey(),
                        header.getValue()));
        filtered.remove(HttpHeaders.HOST);
        return filtered;
    }

    // see https://github.com/spring-cloud/spring-cloud-gateway/pull/2254
    private static CloseStatus adaptCloseStatus(final CloseStatus closeStatus) {
        int code = closeStatus.getCode();
        if (code > 2999 && code < 5000) {
            return closeStatus;
        }
        switch (code) {
            case 1000:
            case 1001:
            case 1002:
            case 1003:
            case 1007:
            case 1008:
            case 1009:
            case 1010:
            case 1011:
                return closeStatus;
            case 1004:
            // Should not be used in a close frame
            // RESERVED;
            case 1005:
            // Should not be used in a close frame
            // return CloseStatus.NO_STATUS_CODE;
            case 1006:
            // Should not be used in a close frame
            // return CloseStatus.NO_CLOSE_FRAME;
            case 1012:
            // Not in RFC6455
            // return CloseStatus.SERVICE_RESTARTED;
            case 1013:
            // Not in RFC6455
            // return CloseStatus.SERVICE_OVERLOAD;
            case 1015:
            // Should not be used in a close frame
            // return CloseStatus.TLS_HANDSHAKE_FAILURE;
            default:
                return CloseStatus.PROTOCOL_ERROR;
        }
    }

    @Override
    public String named() {
        return PluginEnum.WEB_SOCKET.getName();
    }

    /**
     * plugin is execute.
     *
     * @return default false.
     */
    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExcept(exchange, RpcTypeEnum.WEB_SOCKET);
    }

    @Override
    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noSelectorResult(pluginName, exchange);
    }

    @Override
    protected Mono<Void> handleRuleIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noRuleResult(pluginName, exchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.WEB_SOCKET.getCode();
    }

    /**
     * The rule's timeout controls, with the unit carried by the type. A {@code null}
     * value disables the corresponding deadline (#8474).
     *
     * @param handshake the upstream connection and upgrade deadline
     * @param idle the established session's idle limit
     */
    record Timeouts(Duration handshake, Duration idle) {

        /**
         * Build the timeout controls from the rule's millisecond values.
         *
         * @param handshakeMillis the rule timeout in milliseconds; a non-positive value disables the deadline
         * @param idleMillis the rule idle timeout in milliseconds; a non-positive value disables idle expiry
         * @return the timeout controls
         */
        static Timeouts ofMillis(final long handshakeMillis, final long idleMillis) {
            return new Timeouts(enabled(handshakeMillis), enabled(idleMillis));
        }

        private static Duration enabled(final long millis) {
            return millis > 0 ? Duration.ofMillis(millis) : null;
        }
    }

    /**
     * Keeps an upstream's outstanding-request count paired with the proxied session.
     *
     * <p>Least active reserves when the session starts, because that algorithm does not reserve on selection.
     * Shortest response already reserved on selection, so this wrapper only releases that reservation: when
     * the session ends, or when the handshake ends without a session. The count cannot be attached only to
     * the Mono the plugin returns, because that one completes as soon as the upgrade handshake is done while
     * the session keeps running.
     */
    private static final class InflightWebSocketHandler implements WebSocketHandler {

        private static final int IDLE = 0;

        private static final int SESSION_STARTED = 1;

        private static final int RELEASED = 2;

        private final WebSocketHandler delegate;

        private final Upstream upstream;

        private final boolean selectionAlreadyReserved;

        private final AtomicInteger reservation = new AtomicInteger(IDLE);

        InflightWebSocketHandler(final WebSocketHandler delegate, final Upstream upstream, final boolean selectionAlreadyReserved) {
            this.delegate = delegate;
            this.upstream = upstream;
            this.selectionAlreadyReserved = selectionAlreadyReserved;
        }

        @NonNull
        @Override
        public Mono<Void> handle(@NonNull final WebSocketSession session) {
            return Mono.defer(() -> {
                if (selectionAlreadyReserved) {
                    reservation.compareAndSet(IDLE, SESSION_STARTED);
                } else {
                    upstream.getInflight().incrementAndGet();
                }
                Mono<Void> proxied;
                try {
                    proxied = delegate.handle(session);
                } catch (RuntimeException e) {
                    // A failure before the session starts must not leave the upstream reserved forever.
                    releaseReservation();
                    throw e;
                }
                if (Objects.isNull(proxied)) {
                    releaseReservation();
                    return Mono.empty();
                }
                return proxied.doFinally(signalType -> releaseReservation());
            });
        }

        private void releaseSelectionIfSessionDidNotStart() {
            if (reservation.compareAndSet(IDLE, RELEASED)) {
                releaseInflight();
            }
        }

        private void releaseReservation() {
            if (!selectionAlreadyReserved) {
                upstream.getInflight().decrementAndGet();
                return;
            }
            int current;
            do {
                current = reservation.get();
                if (current == RELEASED) {
                    return;
                }
            } while (!reservation.compareAndSet(current, RELEASED));
            releaseInflight();
        }

        private void releaseInflight() {
            upstream.getInflight().updateAndGet(current -> current > 0 ? current - 1 : 0);
        }

        @NonNull
        @Override
        public List<String> getSubProtocols() {
            return delegate.getSubProtocols();
        }
    }

    static class ShenyuWebSocketHandler implements WebSocketHandler {

        private final WebSocketClient client;

        private final URI url;

        private final HttpHeaders headers;

        private final List<String> subProtocols;

        /**
         * The rule timeout controls (#8474). The handshake deadline bounds the upstream
         * connection and upgrade only and goes inert once the upgrade succeeds; the idle
         * limit applies after establishment and never fires on traffic in either direction.
         */
        private final Timeouts timeouts;

        /**
         * Instantiates a new shenyu web socket handler.
         *
         * @param url       the url
         * @param client    the client
         * @param headers   the headers
         * @param protocols the protocols
         * @param timeouts  the rule timeout controls
         */
        ShenyuWebSocketHandler(final URI url, final WebSocketClient client,
                               final HttpHeaders headers,
                               final List<String> protocols,
                               final Timeouts timeouts) {
            this.client = client;
            this.url = url;
            this.headers = headers;
            this.subProtocols = ObjectUtils.defaultIfNull(protocols, Collections.emptyList());
            this.timeouts = timeouts;
        }

        @NonNull
        @Override
        public List<String> getSubProtocols() {
            return this.subProtocols;
        }

        @NonNull
        @Override
        public Mono<Void> handle(@NonNull final WebSocketSession session) {
            // completed as soon as the upstream handler is invoked, which is the only
            // observable point at which the upstream upgrade has succeeded
            final Sinks.Empty<Void> upgraded = Sinks.empty();
            // pass headers along so custom headers can be sent through
            final Mono<Void> attempt = client.execute(url, this.headers, new WebSocketHandler() {

                @NonNull
                @Override
                public Mono<Void> handle(@NonNull final WebSocketSession proxySocketSession) {
                    upgraded.tryEmitEmpty();
                    return pipe(session, proxySocketSession);
                }

                @NonNull
                @Override
                public List<String> getSubProtocols() {
                    return ShenyuWebSocketHandler.this.subProtocols;
                }
            });
            if (Objects.isNull(this.timeouts.handshake())) {
                return attempt;
            }
            // The upgrade signal turns the deadline inert, so an established session keeps
            // running untimed; a stalled upgrade is closed before the rule timeout elapses.
            final Mono<Void> deadline = upgraded.asMono()
                    .timeout(this.timeouts.handshake())
                    .switchIfEmpty(Mono.<Void>never())
                    .onErrorResume(TimeoutException.class, error -> closeSessions(session, null,
                            CloseStatus.SERVER_ERROR.withReason(REASON_HANDSHAKE_TIMEOUT)));
            return Mono.firstWithSignal(attempt, deadline);
        }

        private Mono<Void> pipe(final WebSocketSession session, final WebSocketSession proxySocketSession) {
            final AtomicLong lastActivity = new AtomicLong(System.nanoTime());
            Mono<Void> serverClose = proxySocketSession.closeStatus().filter(it -> session.isOpen())
                .map(WebSocketPlugin::adaptCloseStatus).flatMap(session::close);
            Mono<Void> proxyClose = session.closeStatus().filter(it -> proxySocketSession.isOpen())
                .map(WebSocketPlugin::adaptCloseStatus).flatMap(proxySocketSession::close);
            // Use retain() for Reactor Netty
            Mono<Void> proxySessionSend = proxySocketSession.send(
                session.receive().doOnNext(message -> trackActivity(lastActivity, message)));
            Mono<Void> serverSessionSend = session.send(
                proxySocketSession.receive().doOnNext(message -> trackActivity(lastActivity, message)));
            // Ensure closeStatus from one propagates to the other
            Mono.when(serverClose, proxyClose).subscribe();
            final Mono<Void> piping = Mono.zip(proxySessionSend, serverSessionSend).then();
            if (Objects.isNull(this.timeouts.idle())) {
                return piping;
            }
            return Mono.firstWithSignal(piping, idleWatch(session, proxySocketSession, lastActivity));
        }

        private static void trackActivity(final AtomicLong lastActivity, final WebSocketMessage message) {
            message.retain();
            lastActivity.set(System.nanoTime());
        }

        /**
         * Polls while the session is established and closes both sides once no message moved
         * in either direction for the configured idle limit. Cancellation of the race disposes
         * the periodic task, so the watch never outlives the session.
         */
        private Mono<Void> idleWatch(final WebSocketSession session, final WebSocketSession proxySocketSession,
                                     final AtomicLong lastActivity) {
            final Duration idle = this.timeouts.idle();
            final long idleNanos = idle.toNanos();
            final Duration half = idle.dividedBy(2);
            final Duration period = half.isZero() ? MIN_IDLE_CHECK_INTERVAL : half;
            return Flux.interval(period)
                .filter(tick -> System.nanoTime() - lastActivity.get() >= idleNanos)
                .next()
                .flatMap(tick -> closeSessions(session, proxySocketSession,
                    CloseStatus.GOING_AWAY.withReason(REASON_IDLE_TIMEOUT)))
                .then();
        }

        /**
         * Closes both sides with an explicit status so a session terminated by a rule timeout
         * is not surfaced as an abrupt close without a code or reason. Closing errors are
         * logged and swallowed: the caller's race must terminate either way.
         */
        private Mono<Void> closeSessions(final WebSocketSession session, final WebSocketSession proxySocketSession,
                                         final CloseStatus status) {
            LOG.info("closing websocket session {} with {}: {}", toLogUrl(this.url), status.getCode(), status.getReason());
            final Mono<Void> sessionClose = session.isOpen() ? session.close(status) : Mono.empty();
            final Mono<Void> proxyClose = Objects.isNull(proxySocketSession) || !proxySocketSession.isOpen()
                ? Mono.empty() : proxySocketSession.close(status);
            return Mono.when(sessionClose, proxyClose).onErrorResume(error -> {
                LOG.warn("can not close websocket session {} cleanly", toLogUrl(this.url), error);
                return Mono.empty();
            });
        }
    }
}
