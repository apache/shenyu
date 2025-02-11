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

package org.apache.shenyu.plugin.motan;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.motan.proxy.MotanProxyService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Motan plugin.
 */
public class MotanPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(MotanPlugin.class);

    private final MotanProxyService motanProxyService;

    /**
     * Instantiates a new motan plugin.
     *
     * @param motanProxyService the motan proxy service
     */
    public MotanPlugin(final MotanProxyService motanProxyService) {
        this.motanProxyService = motanProxyService;
    }
    
    @Override
    protected String getRawPath(final ServerWebExchange exchange) {
        return RequestUrlUtils.getRewrittenRawPath(exchange);
    }

    @Override
    @SuppressWarnings("all")
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        String param = exchange.getAttribute(Constants.PARAM_TRANSFORM);
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert Objects.nonNull(shenyuContext);
        MetaData metaData = exchange.getAttribute(Constants.META_DATA);
        if (!checkMetaData(metaData)) {
            assert Objects.nonNull(metaData);
            LOG.error("path is :{}, meta data have error.... {}", shenyuContext.getPath(), metaData);
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.META_DATA_ERROR);
            return WebFluxResultUtils.result(exchange, error);
        }
        if (StringUtils.isNoneBlank(metaData.getParameterTypes()) && StringUtils.isBlank(param)) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.MOTAN_HAVE_BODY_PARAM);
            return WebFluxResultUtils.result(exchange, error);
        }
        final Mono<Object> result = motanProxyService.genericInvoker(param, metaData, exchange);
        return result.then(chain.execute(exchange));
    }

    /**
     * acquire plugin name.
     *
     * @return plugin name.
     */
    @Override
    public String named() {
        return PluginEnum.MOTAN.getName();
    }

    /**
     * plugin is execute.
     *
     * @param exchange the current server exchange
     * @return default false.
     */
    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExcept(exchange, RpcTypeEnum.MOTAN);
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
        return PluginEnum.MOTAN.getCode();
    }

    private boolean checkMetaData(final MetaData metaData) {
        return Objects.nonNull(metaData) && !StringUtils.isBlank(metaData.getMethodName()) && !StringUtils.isBlank(metaData.getServiceName());
    }
}
