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

package org.apache.shenyu.plugin.agent.gateway;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.agent.gateway.handle.AgentGatewayRuleHandle;
import org.apache.shenyu.plugin.agent.gateway.handle.AgentGatewayRuleHandleParser;
import org.apache.shenyu.plugin.agent.gateway.handler.AgentGatewayPluginDataHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.List;
import java.util.UUID;

/**
 * Establishes an isolated request context, dispatching matched MCP rules locally
 * and continuing LLM traffic to the existing AI proxy chain.
 */
public class AgentGatewayPlugin extends AbstractShenyuPlugin {

    private final AgentGatewayRuleHandleParser parser = new AgentGatewayRuleHandleParser();

    private final AgentMcpHttpHandler mcpHandler;

    private final AgentMcpSecurityResolver llmSecurityResolver;

    private final AiCallObserver aiObserver;

    public AgentGatewayPlugin() {
        this(new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "shenyu-agent-gateway", AgentGatewayConstants.MCP_SERVER_VERSION),
                exchange -> Mono.empty()));
    }

    public AgentGatewayPlugin(final AgentMcpHttpHandler mcpHandler) {
        this(mcpHandler, null, null);
    }

    public AgentGatewayPlugin(final AgentMcpHttpHandler mcpHandler, final AgentMcpSecurityResolver llmSecurityResolver, final AiCallObserver aiObserver) {
        this.mcpHandler = Objects.requireNonNull(mcpHandler, "mcpHandler");
        this.llmSecurityResolver = llmSecurityResolver;
        this.aiObserver = aiObserver;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        if (Boolean.FALSE.equals(selector.getContinued())) {
            return chain.execute(exchange);
        }
        return Mono.defer(() -> {
            final AgentGatewayRuleHandle handle = resolveHandle(rule);
            if (!handle.isValid()) {
                return reject(exchange, handle.getErrorMessage());
            }
            final AgentTrafficContext context = new AgentTrafficContext(
                    UUID.randomUUID().toString(), handle.getTrafficType(), selector.getId(), rule.getId());
            final Object previousContext = exchange.getAttributes()
                    .put(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE, context);
            if (handle.isResponseRequestId()) {
                registerResponseRequestId(exchange, context.getRequestId());
            }
            return Mono.defer(() -> "mcp".equals(handle.getTrafficType())
                    ? mcpHandler.handle(exchange, handle.getMcp(), context, handle.getGeneration())
                    : handleLlm(exchange, chain, context.toRequestContext(handle.getGeneration())))
                    .contextWrite(reactorContext -> reactorContext.put(AgentGatewayConstants.REACTOR_CONTEXT_KEY, context)
                            .put(AgentRequestContext.REACTOR_KEY, context.toRequestContext(handle.getGeneration())))
                    .doFinally(signal -> restorePreviousContext(exchange, previousContext, context));
        });
    }

    private Mono<Void> handleLlm(final ServerWebExchange exchange, final ShenyuPluginChain chain, final AgentRequestContext initial) {
        return Mono.defer(() -> {
            Object callback = exchange.getAttribute(AgentRuntimeMetrics.KEY);
            AiRequestObservation observation = new AiRequestObservation(aiObserver, initial,
                    callback instanceof AgentRuntimeMetrics metrics ? metrics : AgentRuntimeMetrics.NOOP);
            Mono<AgentRequestContext> identity = Objects.isNull(llmSecurityResolver) ? Mono.just(initial)
                    : Mono.defer(() -> llmSecurityResolver.resolve(exchange))
                            .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.UNAUTHORIZED)))
                            .map(trusted -> initial.withIdentity(trusted.getTenant().orElse(null), trusted.getSubject()));
            // Only identity resolution errors are recovered here. A downstream write failure
            // must retain its existing error path and must never trigger another proxy request.
            Mono<Void> execution = Objects.isNull(llmSecurityResolver) ? Mono.defer(() -> chain.execute(exchange)) : identity.onErrorResume(error -> {
                observation.failure(error instanceof ResponseStatusException ? AiCallOutcome.REJECTED : AiCallOutcome.ERROR);
                exchange.getResponse().setStatusCode(error instanceof ResponseStatusException status
                        ? status.getStatusCode() : HttpStatus.INTERNAL_SERVER_ERROR);
                return exchange.getResponse().setComplete().then(Mono.empty());
            }).flatMap(context -> {
                observation.updateContext(context);
                return Mono.defer(() -> chain.execute(exchange))
                        .contextWrite(reactorContext -> reactorContext.put(AgentRequestContext.REACTOR_KEY, context));
            });
            return execution.contextWrite(context -> context.put(AiRequestObservation.REACTOR_KEY, observation))
                    .doOnError(observation::error)
                    .doFinally(signal -> observation.finish(signal, Objects.isNull(exchange.getResponse().getStatusCode())
                            ? null : exchange.getResponse().getStatusCode().value()));
        });
    }

    private void restorePreviousContext(final ServerWebExchange exchange, final Object previousContext,
                                        final AgentTrafficContext currentContext) {
        // DefaultServerWebExchange uses a ConcurrentHashMap for attributes. The conditional
        // Map.remove/replace operations rely on a mutable map that supports these methods;
        // they avoid clearing a context installed by another execution on this exchange.
        if (Objects.isNull(previousContext)) {
            exchange.getAttributes().remove(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE, currentContext);
        } else {
            exchange.getAttributes().replace(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE, currentContext,
                    previousContext);
        }
    }

    private AgentGatewayRuleHandle resolveHandle(final RuleData rule) {
        final String key = CacheKeyUtils.INST.getKey(rule);
        final AgentGatewayRuleHandle cached = AgentGatewayPluginDataHandler.CACHED_HANDLE.get().obtainHandle(key);
        if (Objects.nonNull(cached) && Objects.equals(cached.getRawHandle(), rule.getHandle())) {
            return cached;
        }
        final AgentGatewayRuleHandle parsed = parser.parse(rule.getHandle());
        AgentGatewayPluginDataHandler.CACHED_HANDLE.get().cachedHandle(key, parsed);
        return parsed;
    }

    private void registerResponseRequestId(final ServerWebExchange exchange, final String requestId) {
        exchange.getResponse().beforeCommit(() -> {
            final HttpHeaders headers = exchange.getResponse().getHeaders();
            headers.set(AgentGatewayConstants.REQUEST_ID_HEADER, requestId);
            return Mono.empty();
        });
    }

    private Mono<Void> reject(final ServerWebExchange exchange, final String reason) {
        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        final Object error = ShenyuResultWrap.error(exchange, HttpStatus.INTERNAL_SERVER_ERROR.value(),
                AgentGatewayConstants.CONFIG_INVALID_CODE + ": " + reason, null);
        return WebFluxResultUtils.result(exchange, error);
    }

    @Override
    public String named() {
        return PluginEnum.AGENT_GATEWAY.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.AGENT_GATEWAY.getCode();
    }
}
