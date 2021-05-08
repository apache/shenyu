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

package org.apache.shenyu.plugin.alibaba.dubbo;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.alibaba.dubbo.proxy.AlibabaDubboProxyService;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.ResultEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.SoulPluginChain;
import org.apache.shenyu.plugin.api.context.SoulContext;
import org.apache.shenyu.plugin.api.result.SoulResultEnum;
import org.apache.shenyu.plugin.api.result.SoulResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractSoulPlugin;
import org.apache.shenyu.plugin.base.utils.FallbackUtils;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Alibaba dubbo plugin.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class AlibabaDubboPlugin extends AbstractSoulPlugin {

    private final AlibabaDubboProxyService alibabaDubboProxyService;

    /**
     * Instantiates a new Dubbo plugin.
     *
     * @param alibabaDubboProxyService the dubbo proxy service
     */
    public AlibabaDubboPlugin(final AlibabaDubboProxyService alibabaDubboProxyService) {
        this.alibabaDubboProxyService = alibabaDubboProxyService;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        String param = exchange.getAttribute(Constants.PARAM_TRANSFORM);
        SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        MetaData metaData = exchange.getAttribute(Constants.META_DATA);
        if (!checkMetaData(metaData)) {
            assert metaData != null;
            log.error(" path is :{}, meta data have error.... {}", soulContext.getPath(), metaData.toString());
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = SoulResultWrap.error(SoulResultEnum.META_DATA_ERROR.getCode(), SoulResultEnum.META_DATA_ERROR.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        if (StringUtils.isNoneBlank(metaData.getParameterTypes()) && StringUtils.isBlank(param)) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = SoulResultWrap.error(SoulResultEnum.DUBBO_HAVE_BODY_PARAM.getCode(), SoulResultEnum.DUBBO_HAVE_BODY_PARAM.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        Object result = alibabaDubboProxyService.genericInvoker(param, metaData);
        if (Objects.nonNull(result)) {
            exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT, result);
        } else {
            exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT, Constants.DUBBO_RPC_RESULT_EMPTY);
        }
        exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
        return chain.execute(exchange);
    }

    /**
     * acquire plugin name.
     *
     * @return plugin name.
     */
    @Override
    public String named() {
        return PluginEnum.DUBBO.getName();
    }

    /**
     * plugin is execute.
     *
     * @param exchange the current server exchange
     * @return default false.
     */
    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        return !Objects.equals(soulContext.getRpcType(), RpcTypeEnum.DUBBO.getName());
    }

    @Override
    public int getOrder() {
        return PluginEnum.DUBBO.getCode();
    }

    @Override
    protected Mono<Void> handleSelectorIsNull(final String pluginName, final ServerWebExchange exchange, final SoulPluginChain chain) {
        return FallbackUtils.getNoSelectorResult(pluginName, exchange);
    }

    @Override
    protected Mono<Void> handleRuleIsNull(final String pluginName, final ServerWebExchange exchange, final SoulPluginChain chain) {
        return FallbackUtils.getNoRuleResult(pluginName, exchange);
    }

    private boolean checkMetaData(final MetaData metaData) {
        return null != metaData && !StringUtils.isBlank(metaData.getMethodName()) && !StringUtils.isBlank(metaData.getServiceName());
    }
}
