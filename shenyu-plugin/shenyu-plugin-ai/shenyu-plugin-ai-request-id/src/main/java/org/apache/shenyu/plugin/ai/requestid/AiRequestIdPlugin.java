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

package org.apache.shenyu.plugin.ai.requestid;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.requestid.handle.AiRequestIdRule;
import org.apache.shenyu.plugin.ai.requestid.handle.AiRequestIdRuleParser;
import org.apache.shenyu.plugin.ai.requestid.handler.AiRequestIdPluginDataHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.util.context.ContextView;

import java.util.Objects;

/**
 * Establishes one AI logical request id before AI governance and upstream stages.
 * The plugin never reads a request or response body, never creates an
 * {@code AiRequestObservation}, and never accepts a caller-supplied identifier.
 * An existing compatible context is reused as-is so Agent Gateway's LLM path and
 * AI Proxy cannot replace an identifier initialized here. Agent Gateway MCP rules
 * keep their own entry id and are not covered by this reuse.
 */
public class AiRequestIdPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiRequestIdPlugin.class);

    private static final String CONFIG_INVALID_CODE = "AI_REQUEST_ID_CONFIG_INVALID";

    private static final String TRAFFIC_TYPE = "llm";

    private final AiRequestIdRuleParser parser = new AiRequestIdRuleParser();

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        final AiRequestIdRule requestIdRule = resolveRule(selector, rule);
        return Mono.deferContextual(view -> {
            final AgentRequestContext existing = existingContext(exchange, view);
            final boolean reusable = Objects.nonNull(existing)
                    && AgentRequestContext.isReusableRequestId(existing.getRequestId());
            if (Objects.nonNull(existing) && !reusable) {
                // Never log the rejected value; it may contain characters unsafe for log output.
                LOG.warn("Replacing an invalid existing AI request id");
            }
            final AgentRequestContext context = reusable ? existing
                    : new AgentRequestContext(AiRequestIdFactory.generate(), TRAFFIC_TYPE, selector.getId(), rule.getId());
            exchange.getAttributes().put(AgentRequestContext.EXCHANGE_ATTRIBUTE, context);
            if (!requestIdRule.isValid()) {
                // A rejected rule still carries the correlation id so its response header and
                // access log agree. The invalid handle cannot express responseHeaderEnabled,
                // so the canonical header is emitted for the rejection instead of omitted.
                registerResponseHeader(exchange, context.getRequestId());
                return reject(exchange, requestIdRule.getErrorMessage());
            }
            if (requestIdRule.isResponseHeaderEnabled()) {
                registerResponseHeader(exchange, context.getRequestId());
            }
            return chain.execute(exchange)
                    .contextWrite(reactorContext -> reactorContext.put(AgentRequestContext.REACTOR_KEY, context));
        });
    }

    private AiRequestIdRule resolveRule(final SelectorData selector, final RuleData rule) {
        // A non-continued selector has no rule handle; its data handler seeds the default rule.
        if (Boolean.FALSE.equals(selector.getContinued())) {
            final AiRequestIdRule cached = AiRequestIdPluginDataHandler.CACHED_HANDLE.get()
                    .obtainHandle(CacheKeyUtils.INST.getKey(selector.getId(), Constants.DEFAULT_RULE));
            return Objects.nonNull(cached) ? cached : AiRequestIdRule.defaultRule();
        }
        final String key = CacheKeyUtils.INST.getKey(rule);
        final AiRequestIdRule cached = AiRequestIdPluginDataHandler.CACHED_HANDLE.get().obtainHandle(key);
        if (Objects.nonNull(cached) && Objects.equals(cached.getRawHandle(), rule.getHandle())) {
            return cached;
        }
        final AiRequestIdRule parsed = parser.parse(rule.getHandle());
        AiRequestIdPluginDataHandler.CACHED_HANDLE.get().cachedHandle(key, parsed);
        return parsed;
    }

    private AgentRequestContext existingContext(final ServerWebExchange exchange, final ContextView view) {
        final Object attributed = exchange.getAttribute(AgentRequestContext.EXCHANGE_ATTRIBUTE);
        if (attributed instanceof AgentRequestContext context) {
            return context;
        }
        final Object subscribed = view.getOrDefault(AgentRequestContext.REACTOR_KEY, null);
        return subscribed instanceof AgentRequestContext context ? context : null;
    }

    private void registerResponseHeader(final ServerWebExchange exchange, final String requestId) {
        exchange.getResponse().beforeCommit(() -> {
            final HttpHeaders headers = exchange.getResponse().getHeaders();
            headers.set(Constants.X_SHENYU_AI_REQUEST_ID, requestId);
            return Mono.empty();
        });
    }

    private Mono<Void> reject(final ServerWebExchange exchange, final String reason) {
        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        final Object error = ShenyuResultWrap.error(exchange, HttpStatus.INTERNAL_SERVER_ERROR.value(),
                CONFIG_INVALID_CODE + ": " + reason, null);
        return WebFluxResultUtils.result(exchange, error);
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return Boolean.TRUE.equals(exchange.getAttribute(AgentRequestContext.HTTP_TOOL_CALL_ATTRIBUTE));
    }

    @Override
    public String named() {
        return PluginEnum.AI_REQUEST_ID.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.AI_REQUEST_ID.getCode();
    }
}
