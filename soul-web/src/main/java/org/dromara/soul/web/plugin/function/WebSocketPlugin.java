/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.function;

import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.dto.convert.rule.DivideRuleHandle;
import org.dromara.soul.common.dto.zk.RuleZkDTO;
import org.dromara.soul.common.dto.zk.SelectorZkDTO;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.web.balance.utils.LoadBalanceUtils;
import org.dromara.soul.web.cache.UpstreamCacheManager;
import org.dromara.soul.web.cache.ZookeeperCacheManager;
import org.dromara.soul.web.plugin.AbstractSoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.util.StringUtils;
import org.springframework.web.reactive.socket.WebSocketHandler;
import org.springframework.web.reactive.socket.WebSocketMessage;
import org.springframework.web.reactive.socket.WebSocketSession;
import org.springframework.web.reactive.socket.client.WebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.util.UriComponentsBuilder;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * The type Web socket plugin.
 *
 * @author xiaoyu(Myth)
 */
public class WebSocketPlugin extends AbstractSoulPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(WebSocketPlugin.class);

    private static final String SEC_WEB_SOCKET_PROTOCOL = "Sec-WebSocket-Protocol";

    private final UpstreamCacheManager upstreamCacheManager;

    private final WebSocketClient webSocketClient;

    private final WebSocketService webSocketService;

    /**
     * Instantiates a new WebSocket plugin.
     *
     * @param zookeeperCacheManager the zookeeper cache manager
     * @param upstreamCacheManager  the upstream cache manager
     * @param webSocketClient       the web socket client
     * @param webSocketService      the web socket service
     */
    public WebSocketPlugin(final ZookeeperCacheManager zookeeperCacheManager,
                           final UpstreamCacheManager upstreamCacheManager, final
                           WebSocketClient webSocketClient,
                           final WebSocketService webSocketService) {
        super(zookeeperCacheManager);
        this.upstreamCacheManager = upstreamCacheManager;
        this.webSocketClient = webSocketClient;
        this.webSocketService = webSocketService;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorZkDTO selector, final RuleZkDTO rule) {
        final List<DivideUpstream> upstreamList =
                upstreamCacheManager.findUpstreamListBySelectorId(selector.getId());
        final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
        if (CollectionUtils.isEmpty(upstreamList) || Objects.isNull(requestDTO)) {
            LogUtils.error(LOGGER, "divide upstream config error：{}", rule::toString);
            return chain.execute(exchange);
        }
        final DivideRuleHandle ruleHandle = GsonUtils.getInstance().fromJson(rule.getHandle(), DivideRuleHandle.class);

        final String ip = Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getAddress().getHostAddress();

        DivideUpstream divideUpstream =
                LoadBalanceUtils.selector(upstreamList, ruleHandle.getLoadBalance(), ip);

        if (Objects.isNull(divideUpstream)) {
            LogUtils.error(LOGGER, () -> "LoadBalance has error！");
            return chain.execute(exchange);
        }
        URI wsRequestUrl = UriComponentsBuilder
                .fromUri(URI.create(buildWsRealPath(divideUpstream, requestDTO)))
                .scheme(Optional.ofNullable(divideUpstream.getProtocol()).orElse("ws"))
                .build().toUri();

        HttpHeaders headers = exchange.getRequest().getHeaders();
        return this.webSocketService.handleRequest(exchange, new SoulWebSocketHandler(
                wsRequestUrl, this.webSocketClient, filterHeaders(headers), buildWsProtocols(headers)));
    }

    private String buildWsRealPath(final DivideUpstream divideUpstream, final RequestDTO requestDTO) {
        return divideUpstream.getProtocol() + "://" + divideUpstream.getUpstreamUrl() + requestDTO.getMethod();
    }

    private List<String> buildWsProtocols(final HttpHeaders headers) {
        List<String> protocols = headers.get(SEC_WEB_SOCKET_PROTOCOL);
        if (CollectionUtils.isNotEmpty(protocols)) {
            protocols = protocols
                    .stream().flatMap(header -> Arrays.stream(StringUtils.commaDelimitedListToStringArray(header)))
                    .map(String::trim).collect(Collectors.toList());
        }
        return protocols;
    }

    private HttpHeaders filterHeaders(final HttpHeaders headers) {
        HttpHeaders filtered = new HttpHeaders();
        headers.entrySet().stream()
                .filter(entry -> !entry.getKey().toLowerCase()
                        .startsWith("sec-websocket"))
                .forEach(header -> filtered.addAll(header.getKey(),
                        header.getValue()));
        return filtered;
    }

    @Override
    public String named() {
        return PluginEnum.DIVIDE.getName();
    }

    /**
     * plugin is execute.
     *
     * @return default false.
     */
    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final RequestDTO body = exchange.getAttribute(Constants.REQUESTDTO);
        return !Objects.equals(Objects.requireNonNull(body).getRpcType(), RpcTypeEnum.WEB_SOCKET.getName());
    }

    /**
     * return plugin type.
     *
     * @return {@linkplain PluginTypeEnum}
     */
    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.FUNCTION;
    }

    @Override
    public int getOrder() {
        return PluginEnum.WEB_SOCKET.getCode();
    }

    private static class SoulWebSocketHandler implements WebSocketHandler {

        private final WebSocketClient client;

        private final URI url;

        private final HttpHeaders headers;

        private final List<String> subProtocols;

        /**
         * Instantiates a new Soul web socket handler.
         *
         * @param url       the url
         * @param client    the client
         * @param headers   the headers
         * @param protocols the protocols
         */
        SoulWebSocketHandler(final URI url, final WebSocketClient client,
                             final HttpHeaders headers,
                             final List<String> protocols) {
            this.client = client;
            this.url = url;
            this.headers = headers;
            if (protocols != null) {
                this.subProtocols = protocols;
            } else {
                this.subProtocols = Collections.emptyList();
            }
        }

        @Override
        public List<String> getSubProtocols() {
            return this.subProtocols;
        }

        @Override
        public Mono<Void> handle(final WebSocketSession session) {
            // pass headers along so custom headers can be sent through
            return client.execute(url, this.headers, new WebSocketHandler() {
                @Override
                public Mono<Void> handle(final WebSocketSession webSocketSession) {
                    // Use retain() for Reactor Netty
                    Mono<Void> sessionSend = webSocketSession
                            .send(session.receive().doOnNext(WebSocketMessage::retain));
                    Mono<Void> serverSessionSend = session.send(
                            webSocketSession.receive().doOnNext(WebSocketMessage::retain));
                    return Mono.zip(sessionSend, serverSessionSend).then();
                }

                @Override
                public List<String> getSubProtocols() {
                    return SoulWebSocketHandler.this.subProtocols;
                }
            });
        }

    }

}
