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

import com.fasterxml.jackson.databind.JsonNode;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.view.AiRequestViewOperations;
import org.apache.shenyu.plugin.ai.protocol.AiProtocolDetector;
import org.apache.shenyu.plugin.ai.protocol.AiProtocols;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiResponseCacheScope;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiResponseCacheScopeResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiObservationSupport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
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

    private static final AiProxyFailureMapper FAILURE_MAPPER = new AiProxyFailureMapper();

    private final AiProxyConfigService aiProxyConfigService;

    private final AiProxyEngine aiProxyEngine;

    private final AiProxyPluginHandler aiProxyPluginHandler;

    private final AiCallObserver observer;

    private final AiResponseCacheScopeResolver cacheScopeResolver;

    public AiProxyPlugin(final AiProxyConfigService aiProxyConfigService, final AiProxyEngine aiProxyEngine, final AiProxyPluginHandler aiProxyPluginHandler) {
        this(aiProxyConfigService, aiProxyEngine, aiProxyPluginHandler, null);
    }

    public AiProxyPlugin(final AiProxyConfigService aiProxyConfigService, final AiProxyEngine aiProxyEngine,
                         final AiProxyPluginHandler aiProxyPluginHandler, final AiCallObserver observer) {
        this(aiProxyConfigService, aiProxyEngine, aiProxyPluginHandler, observer, null);
    }

    /**
     * Create a proxy with an explicit, application-owned cache authorization adapter.
     *
     * @param aiProxyConfigService configuration resolver
     * @param aiProxyEngine exchange engine
     * @param aiProxyPluginHandler synchronized configuration
     * @param observer optional logical call observer
     * @param cacheScopeResolver optional cache authorization adapter
     */
    public AiProxyPlugin(final AiProxyConfigService aiProxyConfigService, final AiProxyEngine aiProxyEngine,
                         final AiProxyPluginHandler aiProxyPluginHandler, final AiCallObserver observer,
                         final AiResponseCacheScopeResolver cacheScopeResolver) {
        this.aiProxyConfigService = aiProxyConfigService;
        this.aiProxyEngine = aiProxyEngine;
        this.aiProxyPluginHandler = aiProxyPluginHandler;
        this.observer = observer;
        this.cacheScopeResolver = cacheScopeResolver;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        return Mono.deferContextual(view -> {
            AiRequestObservation inherited = view.getOrDefault(AiRequestObservation.REACTOR_KEY, null);
            Object callback = exchange.getAttribute(AgentRuntimeMetrics.KEY);
            AiRequestObservation observation = Objects.nonNull(inherited) ? inherited
                    : new AiRequestObservation(observer, AiObservationSupport.context(view, selector.getId(), rule.getId()),
                            callback instanceof AgentRuntimeMetrics metrics ? metrics : AgentRuntimeMetrics.NOOP);
            return executeRequest(exchange, selector, rule)
                    .doOnError(error -> observation.failure(AiObservationSupport.outcome(error)))
                    .onErrorResume(error -> handleFailure(exchange.getResponse(), error))
                    .doFinally(signal -> {
                        if (Objects.isNull(inherited)) {
                            HttpStatusCode status = exchange.getResponse().getStatusCode();
                            observation.finish(signal, Objects.isNull(status) ? null : status.value());
                        }
                    }).contextWrite(context -> context.put(AiRequestObservation.REACTOR_KEY, observation)
                            .put(AgentRequestContext.REACTOR_KEY, observation.getContext()));
        });
    }

    private Mono<Void> executeRequest(final ServerWebExchange exchange, final SelectorData selector, final RuleData rule) {
        return Mono.defer(() -> {
            AiProxyHandle handle = aiProxyPluginHandler.getSelectorCachedHandle()
                    .obtainHandle(CacheKeyUtils.INST.getKey(selector.getId(), Constants.DEFAULT_RULE));
            final String realApiKey;
            if (Objects.nonNull(handle) && "true".equalsIgnoreCase(handle.getProxyEnabled())) {
                String proxyKey = exchange.getRequest().getHeaders().getFirst(Constants.X_API_KEY);
                realApiKey = Objects.isNull(proxyKey) || proxyKey.isEmpty() ? null
                        : AiProxyApiKeyCache.getInstance().getRealApiKey(selector.getId(), proxyKey);
                if (Objects.isNull(realApiKey)) {
                    exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
                    return exchange.getResponse().setComplete();
                }
            } else {
                realApiKey = null;
            }
            AiCommonConfig config = aiProxyConfigService.resolvePrimaryConfig(handle);
            if (Objects.nonNull(realApiKey)) {
                config.setApiKey(realApiKey);
            }
            return DataBufferUtils.join(exchange.getRequest().getBody(), MAX_REQUEST_BODY_SIZE_BYTES)
                    .switchIfEmpty(Mono.error(new ShenyuException("AI request body must not be empty")))
                    .flatMap(buffer -> {
                        final String body;
                        try {
                            body = buffer.toString(StandardCharsets.UTF_8);
                        } finally {
                            DataBufferUtils.release(buffer);
                        }
                        final JsonNode payload = AiProxyConfigService.parseRequestBody(body);
                        String protocol = config.getProtocol();
                        if (Objects.isNull(protocol) || protocol.isBlank()) {
                            // a blank protocol opts into detection and must be resolved here: the engine rejects
                            // an unresolved protocol instead of guessing the body shape
                            Optional<String> resolved = AiProtocolDetector.resolve(exchange, payload);
                            resolved.ifPresent(config::setProtocol);
                            protocol = resolved.orElse(null);
                        } else {
                            warnOnDetectionMismatch(exchange, protocol);
                        }
                        // the body about to be decoded is authoritative; publishing it keeps the
                        // view generation current and makes stale consumer inspections observable
                        AiRequestViewOperations.ensurePublished(exchange, body, payload, PluginEnum.AI_PROXY.getName(),
                                Objects.isNull(protocol) ? AiProtocols.UNKNOWN : protocol);
                        Optional<AiCommonConfig> fallback = aiProxyConfigService.resolveDynamicFallbackConfig(config, handle, payload)
                                .or(() -> aiProxyConfigService.resolveAdminFallbackConfig(config, handle));
                        return cacheScope(exchange, selector, rule).flatMap(scope ->
                                aiProxyEngine.execute(body, payload, config, fallback, response -> writeResponse(exchange.getResponse(), response))
                                        .contextWrite(context -> scope.map(value -> context.put(AiResponseCacheScope.REACTOR_KEY, value))
                                                .orElseGet(() -> context.delete(AiResponseCacheScope.REACTOR_KEY))));
                    });
        });
    }

    private void warnOnDetectionMismatch(final ServerWebExchange exchange, final String configuredProtocol) {
        // the configured protocol stays authoritative: the plugins before this one can only detect,
        // so a contradiction is reported and the configured decoder still decides the request validity
        if (!AiProtocolDetector.isDetectable(configuredProtocol)) {
            return;
        }
        AiProtocolDetector.resolved(exchange)
                .filter(detected -> !detected.equals(configuredProtocol))
                .ifPresent(detected -> LOG.warn("configured AI protocol {} contradicts the detected protocol {} of path {}",
                        configuredProtocol, detected, exchange.getRequest().getPath().value()));
    }

    private Mono<Optional<AiResponseCacheScope>> cacheScope(final ServerWebExchange exchange, final SelectorData selector, final RuleData rule) {
        if (Objects.isNull(cacheScopeResolver) || !org.springframework.http.HttpMethod.POST.equals(exchange.getRequest().getMethod())
                || !exchange.getRequest().getCookies().isEmpty()
                || exchange.getRequest().getHeaders().containsKey("Cache-Control")
                || exchange.getRequest().getHeaders().containsKey("Pragma")) {
            return Mono.just(Optional.empty());
        }
        return Mono.defer(() -> cacheScopeResolver.resolve(exchange, selector.getId(), rule.getId()))
                .map(scope -> Optional.of(new AiResponseCacheScope(scope.tenant(), scope.subject(), scope.accessScope(),
                        scope.policyRevision(), scope.promptVersion(), selector.getId(), rule.getId())))
                .defaultIfEmpty(Optional.empty());
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
        AiProxyFailureResponse failureResponse = FAILURE_MAPPER.map(error);
        LOG.warn("AI proxy failed before response commitment: {}", error.getClass().getSimpleName());
        response.setStatusCode(failureResponse.getStatus());
        failureResponse.getHeaders().forEach((name, values) -> response.getHeaders().put(name, values));
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
