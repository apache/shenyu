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

package org.apache.shenyu.plugin.ai.proxy.enhanced;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

/**
 * Terminal AI proxy gateway boundary backed exclusively by AiProxyEngine.
 */
public class AiProxyPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyPlugin.class);

    private static final int MAX_REQUEST_BODY_SIZE_BYTES = 5 * 1024 * 1024;

    private static final Set<String> HOP_HEADERS = Set.of("connection", "keep-alive", "proxy-authenticate", "proxy-authorization",
            "te", "trailer", "transfer-encoding", "upgrade");

    private final AiProxyConfigService aiProxyConfigService;

    private final AiProxyEngine aiProxyEngine;

    private final AiProxyPluginHandler aiProxyPluginHandler;

    public AiProxyPlugin(final AiProxyConfigService aiProxyConfigService, final AiProxyEngine aiProxyEngine, final AiProxyPluginHandler aiProxyPluginHandler) {
        this.aiProxyConfigService = aiProxyConfigService;
        this.aiProxyEngine = aiProxyEngine;
        this.aiProxyPluginHandler = aiProxyPluginHandler;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        return Mono.defer(() -> {
            AiProxyHandle handle = aiProxyPluginHandler.getSelectorCachedHandle()
                    .obtainHandle(CacheKeyUtils.INST.getKey(selector.getId(), Constants.DEFAULT_RULE));
            return DataBufferUtils.join(exchange.getRequest().getBody(), MAX_REQUEST_BODY_SIZE_BYTES)
                    .switchIfEmpty(Mono.error(new ShenyuException("AI request body must not be empty")))
                    .flatMap(buffer -> {
                        final String body;
                        try {
                            body = buffer.toString(StandardCharsets.UTF_8);
                        } finally {
                            DataBufferUtils.release(buffer);
                        }
                        AiCommonConfig config = aiProxyConfigService.resolvePrimaryConfig(handle);
                        if (Objects.nonNull(handle) && "true".equalsIgnoreCase(handle.getProxyEnabled())) {
                            String proxyKey = exchange.getRequest().getHeaders().getFirst(Constants.X_API_KEY);
                            String realKey = Objects.isNull(proxyKey) || proxyKey.isEmpty() ? null
                                    : AiProxyApiKeyCache.getInstance().getRealApiKey(selector.getId(), proxyKey);
                            if (Objects.isNull(realKey)) {
                                exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
                                return exchange.getResponse().setComplete();
                            }
                            config.setApiKey(realKey);
                        }
                        Optional<AiCommonConfig> fallback = aiProxyConfigService.resolveDynamicFallbackConfig(config, body)
                                .or(() -> aiProxyConfigService.resolveAdminFallbackConfig(config, handle));
                        return aiProxyEngine.execute(body, config, fallback, response -> writeResponse(exchange.getResponse(), response));
                    });
        }).onErrorResume(error -> handleFailure(exchange.getResponse(), error));
    }

    private Mono<Void> writeResponse(final ServerHttpResponse response, final AiUpstreamResponse upstream) {
        response.setStatusCode(HttpStatusCode.valueOf(upstream.getStatusCode()));
        Set<String> excluded = new HashSet<>(HOP_HEADERS);
        upstream.getHeaders().forEach((name, values) -> {
            if ("connection".equalsIgnoreCase(name)) {
                values.forEach(value -> {
                    for (String token : value.split(",")) {
                        excluded.add(token.trim().toLowerCase(Locale.ROOT));
                    }
                });
            }
        });
        upstream.getHeaders().forEach((name, values) -> {
            if (!excluded.contains(name.toLowerCase(Locale.ROOT))) {
                response.getHeaders().put(name, values);
            }
        });
        Flux<DataBuffer> body = upstream.getBody().map(bytes -> response.bufferFactory().wrap(bytes))
                .doOnDiscard(DataBuffer.class, DataBufferUtils::release);
        return response.writeAndFlushWith(body.map(buffer -> Mono.just(buffer).doOnDiscard(DataBuffer.class, DataBufferUtils::release)));
    }

    private Mono<Void> handleFailure(final ServerHttpResponse response, final Throwable error) {
        if (response.isCommitted()) {
            return Mono.error(error);
        }
        HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
        if (error instanceof DataBufferLimitException) {
            status = HttpStatus.PAYLOAD_TOO_LARGE;
        } else if (error instanceof ShenyuException) {
            status = HttpStatus.BAD_REQUEST;
        } else if (error instanceof AiProxyException) {
            status = ((AiProxyException) error).getKind() == AiProxyException.Kind.REQUEST ? HttpStatus.BAD_REQUEST : HttpStatus.BAD_GATEWAY;
        }
        LOG.warn("AI proxy failed before response commitment: {}", error.getClass().getSimpleName());
        response.setStatusCode(status);
        return response.setComplete();
    }

    @Override
    public int getOrder() {
        return PluginEnum.AI_PROXY.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.AI_PROXY.getName();
    }
}
