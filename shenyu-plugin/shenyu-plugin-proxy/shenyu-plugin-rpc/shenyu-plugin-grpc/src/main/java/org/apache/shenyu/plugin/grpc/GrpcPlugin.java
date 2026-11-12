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

package org.apache.shenyu.plugin.grpc;

import io.grpc.CallOptions;
import io.grpc.Context;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.ResultEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.grpc.cache.GrpcClientCache;
import org.apache.shenyu.plugin.grpc.cache.GrpcExtInfo;
import org.apache.shenyu.plugin.grpc.client.ShenyuGrpcClient;
import org.apache.shenyu.plugin.grpc.context.GrpcConstants;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * The type grpc plugin.
 */
public class GrpcPlugin extends AbstractShenyuPlugin {

    public static final Context.Key<Map<String, String>> RPC_CONTEXT_KEY = Context.key("shenyuRpcContext");

    private static final Logger LOG = LoggerFactory.getLogger(GrpcPlugin.class);
    
    @Override
    protected String getRawPath(final ServerWebExchange exchange) {
        return RequestUrlUtils.getRewrittenRawPath(exchange);
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        final String param = exchange.getAttribute(Constants.PARAM_TRANSFORM);
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext);
        MetaData metaData = exchange.getAttribute(Constants.META_DATA);

        if (!checkMetaData(metaData)) {
            LOG.error(" path is :{}, meta data have error.... {}", shenyuContext.getPath(), metaData);
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.META_DATA_ERROR);
            return WebFluxResultUtils.result(exchange, error);
        }
        Objects.requireNonNull(metaData);
        if (StringUtils.isNoneBlank(metaData.getParameterTypes()) && StringUtils.isBlank(param)) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.GRPC_HAVE_BODY_PARAM);
            return WebFluxResultUtils.result(exchange, error);
        }

        return Mono.defer(() -> invoke(exchange, chain, selector, rule, metaData));
    }

    private Mono<Void> invoke(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector,
                              final RuleData rule, final MetaData metaData) {
        ShenyuGrpcClient client = GrpcClientCache.getGrpcClient(selector.getId());
        if (Objects.isNull(client)) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.GRPC_CLIENT_NULL);
            return WebFluxResultUtils.result(exchange, error);
        }

        GrpcExtInfo extInfo = ApplicationConfigCache.getInstance().obtainGrpcExtInfo(metaData);
        CallOptions callOptions = CallOptions.DEFAULT.withDeadlineAfter(extInfo.getTimeout(), TimeUnit.MILLISECONDS)
                .withOption(GrpcConstants.GRPC_SELECTOR_ID_OPTION, selector.getId())
                .withOption(GrpcConstants.GRPC_RULE_ID_OPTION, rule.getId())
                .withOption(GrpcConstants.GRPC_REMOTE_ADDRESS_OPTION,
                        Optional.ofNullable(exchange.getRequest().getRemoteAddress())
                                .map(InetSocketAddress::getAddress)
                                .map(InetAddress::getHostAddress)
                                .orElse(StringUtils.EMPTY));
        Context context = invocationContext(exchange);
        Context previous = context.attach();
        CompletableFuture<ShenyuGrpcResponse> result;
        try {
            result = client.call(metaData, callOptions, exchange.getAttribute(Constants.PARAM_TRANSFORM), extInfo.getMethodType());
        } finally {
            context.detach(previous);
        }

        return Mono.fromFuture(result)
                .doOnNext(response -> {
                    exchange.getAttributes().put(Constants.RPC_RESULT, response.getResults());
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
                })
                .onErrorMap(ShenyuException::new)
                .then(Mono.defer(() -> chain.execute(exchange)));
    }

    private Context invocationContext(final ServerWebExchange exchange) {
        Context context = Context.current();
        Map<String, Map<String, String>> rpcContext = exchange.getAttribute(Constants.GENERAL_CONTEXT);
        return Optional.ofNullable(rpcContext).map(values -> values.get(PluginEnum.GRPC.getName()))
                .map(values -> context.withValue(RPC_CONTEXT_KEY, values)).orElse(context);
    }

    /**
     * acquire plugin name.
     *
     * @return plugin name.
     */
    @Override
    public String named() {
        return PluginEnum.GRPC.getName();
    }

    /**
     * plugin is execute.
     *
     * @param exchange the current server exchange
     * @return default false.
     */
    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExcept(exchange, RpcTypeEnum.GRPC);
    }

    @Override
    public int getOrder() {
        return PluginEnum.GRPC.getCode();
    }

    private boolean checkMetaData(final MetaData metaData) {
        return Objects.nonNull(metaData)
                && !StringUtils.isBlank(metaData.getMethodName())
                && !StringUtils.isBlank(metaData.getServiceName());
    }

    @Override
    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noSelectorResult(pluginName, exchange);
    }

    @Override
    protected Mono<Void> handleRuleIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noRuleResult(pluginName, exchange);
    }
}
