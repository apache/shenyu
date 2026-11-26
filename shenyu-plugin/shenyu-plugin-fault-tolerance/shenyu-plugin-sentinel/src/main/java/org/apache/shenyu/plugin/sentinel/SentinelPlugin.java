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

package org.apache.shenyu.plugin.sentinel;

import com.alibaba.csp.sentinel.EntryType;
import com.alibaba.csp.sentinel.adapter.reactor.EntryConfig;
import com.alibaba.csp.sentinel.adapter.reactor.SentinelReactorTransformer;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.SentinelHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.fallback.FallbackHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.sentinel.handler.SentinelRuleHandle;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * Sentinel Plugin.
 */
public class SentinelPlugin extends AbstractShenyuPlugin {

    private static final int MAX_PARAMETER_LENGTH = 256;

    private static final String MISSING_PARAMETER_BUCKET = "missing";

    private final FallbackHandler fallbackHandler;

    public SentinelPlugin(final FallbackHandler fallbackHandler) {
        this.fallbackHandler = fallbackHandler;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext);
        String resourceName = CacheKeyUtils.INST.getKey(rule);
        SentinelHandle sentinelHandle = SentinelRuleHandle.CACHED_HANDLE.get().obtainHandle(resourceName);
        if (Objects.isNull(sentinelHandle)) {
            return chain.execute(exchange);
        }
        sentinelHandle.checkData();
        exchange.getAttributes().put(Constants.WATCHER_HTTP_STATUS, (Consumer<HttpStatus>) status -> {
            if (Objects.isNull(status) || !status.is2xxSuccessful()) {
                throw new SentinelFallbackException(Objects.isNull(status) ? HttpStatus.INTERNAL_SERVER_ERROR : status);
            }
        });
        return protect(exchange, chain, resourceName, sentinelHandle).onErrorResume(throwable ->
                fallbackHandler.fallback(exchange, UriUtils.createUri(sentinelHandle.getFallbackUri()), throwable)).doFinally(monoV -> {
                    final Consumer<HttpStatusCode> consumer = exchange.getAttribute(Constants.METRICS_SENTINEL);
                    Optional.ofNullable(consumer).ifPresent(c -> c.accept(exchange.getResponse().getStatusCode()));
                }
        );
    }

    private Mono<Void> protect(final ServerWebExchange exchange, final ShenyuPluginChain chain, final String resourceName, final SentinelHandle handle) {
        if (!handle.isParamFlowEnabled() || handle.getFlowRuleEnable() != Constants.FLOW_RULE_ENABLE_ON) {
            return chain.execute(exchange).transform(new SentinelReactorTransformer<>(resourceName));
        }
        final EntryConfig entry = new EntryConfig(resourceName, EntryType.OUT, 1, parameterArguments(exchange, handle));
        // Start the downstream chain only after the Sentinel subscriber has granted demand.
        return Mono.just(exchange).hide().flatMap(chain::execute).transform(new SentinelReactorTransformer<>(entry));
    }

    private Object[] parameterArguments(final ServerWebExchange exchange, final SentinelHandle handle) {
        Object value = SentinelHandle.PARAM_FLOW_HEADER.equals(handle.getParamFlowSource())
                ? singleHeader(exchange, handle.getParamFlowKey()) : exchange.getAttribute(handle.getParamFlowKey());
        if (value instanceof String parameter && !parameter.isBlank() && parameter.length() <= MAX_PARAMETER_LENGTH) {
            return new Object[] {"value:" + parameter};
        }
        return handle.skipMissingParamFlowKey() ? new Object[0] : new Object[] {MISSING_PARAMETER_BUCKET};
    }

    private String singleHeader(final ServerWebExchange exchange, final String key) {
        List<String> values = exchange.getRequest().getHeaders().get(key);
        return Objects.nonNull(values) && values.size() == 1 ? values.get(0) : null;
    }

    @Override
    public String named() {
        return PluginEnum.SENTINEL.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.SENTINEL.getCode();
    }

    public static class SentinelFallbackException extends HttpStatusCodeException {

        public SentinelFallbackException(final HttpStatus statusCode) {
            super(statusCode);
        }
    }
}
