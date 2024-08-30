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
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
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
import reactor.core.publisher.Mono;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type Web socket plugin.
 */
public class WebSocketPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(WebSocketPlugin.class);

    private static final String SEC_WEB_SOCKET_PROTOCOL = "Sec-WebSocket-Protocol";

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
        final String ip = Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getAddress().getHostAddress();
        Upstream upstream = LoadBalancerFactory.selector(upstreamList, ruleHandle.getLoadBalance(), ip);
        if (Objects.isNull(upstream)) {
            LOG.error("websocket has no upstream, error:{}", rule);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        URI wsRequestUrl = buildWsRealPath(exchange, upstream, shenyuContext);
        LOG.info("you websocket urlPath is :{}", wsRequestUrl.toASCIIString());
        HttpHeaders headers = exchange.getRequest().getHeaders();
        return this.webSocketService.handleRequest(exchange, new ShenyuWebSocketHandler(
                wsRequestUrl, this.webSocketClient, filterHeaders(headers), buildWsProtocols(headers)));
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

    private static class ShenyuWebSocketHandler implements WebSocketHandler {

        private final WebSocketClient client;

        private final URI url;

        private final HttpHeaders headers;

        private final List<String> subProtocols;

        /**
         * Instantiates a new shenyu web socket handler.
         *
         * @param url       the url
         * @param client    the client
         * @param headers   the headers
         * @param protocols the protocols
         */
        ShenyuWebSocketHandler(final URI url, final WebSocketClient client,
                               final HttpHeaders headers,
                               final List<String> protocols) {
            this.client = client;
            this.url = url;
            this.headers = headers;
            this.subProtocols = ObjectUtils.defaultIfNull(protocols, Collections.emptyList());
        }

        @NonNull
        @Override
        public List<String> getSubProtocols() {
            return this.subProtocols;
        }

        @NonNull
        @Override
        public Mono<Void> handle(@NonNull final WebSocketSession session) {
            // pass headers along so custom headers can be sent through
            return client.execute(url, this.headers, new WebSocketHandler() {

                @NonNull
                @Override
                public Mono<Void> handle(@NonNull final WebSocketSession proxySocketSession) {
                    Mono<Void> serverClose = proxySocketSession.closeStatus().filter(it -> session.isOpen())
                        .map(WebSocketPlugin::adaptCloseStatus).flatMap(session::close);
                    Mono<Void> proxyClose = session.closeStatus().filter(it -> proxySocketSession.isOpen())
                        .map(WebSocketPlugin::adaptCloseStatus).flatMap(proxySocketSession::close);
                    // Use retain() for Reactor Netty
                    Mono<Void> proxySessionSend = proxySocketSession
                        .send(session.receive().doOnNext(WebSocketMessage::retain));
                    Mono<Void> serverSessionSend = session.send(
                        proxySocketSession.receive().doOnNext(WebSocketMessage::retain));
                    // Ensure closeStatus from one propagates to the other
                    Mono.when(serverClose, proxyClose).subscribe();
                    return Mono.zip(proxySessionSend, serverSessionSend).then();
                }

                @NonNull
                @Override
                public List<String> getSubProtocols() {
                    return ShenyuWebSocketHandler.this.subProtocols;
                }
            });
        }
    }
}
