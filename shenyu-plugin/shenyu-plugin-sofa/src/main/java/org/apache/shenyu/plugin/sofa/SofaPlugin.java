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

package org.apache.shenyu.plugin.sofa;

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
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.sofa.proxy.SofaProxyService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * The sofa plugin.
 */
public class SofaPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(SofaPlugin.class);

    private final SofaProxyService sofaProxyService;

    /**
     * Instantiates a new Sofa plugin.
     *
     * @param sofaProxyService the sofa proxy service
     */
    public SofaPlugin(final SofaProxyService sofaProxyService) {
        this.sofaProxyService = sofaProxyService;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        String param = exchange.getAttribute(Constants.PARAM_TRANSFORM);
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        MetaData metaData = exchange.getAttribute(Constants.META_DATA);
        if (!checkMetaData(metaData)) {
            assert metaData != null;
            LOG.error(" path is :{}, meta data have error.... {}", shenyuContext.getPath(), metaData);
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.META_DATA_ERROR.getCode(), ShenyuResultEnum.META_DATA_ERROR.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        if (StringUtils.isNoneBlank(metaData.getParameterTypes()) && StringUtils.isBlank(param)) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.SOFA_HAVE_BODY_PARAM.getCode(), ShenyuResultEnum.SOFA_HAVE_BODY_PARAM.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        final Mono<Object> result = sofaProxyService.genericInvoker(param, metaData, exchange);
        return result.then(chain.execute(exchange));
    }

    /**
     * acquire plugin name.
     *
     * @return plugin name.
     */
    @Override
    public String named() {
        return PluginEnum.SOFA.getName();
    }

    /**
     * plugin is execute.
     *
     * @param exchange the current server exchange
     * @return default false.
     */
    @Override
    public boolean skip(final ServerWebExchange exchange) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        return !Objects.equals(shenyuContext.getRpcType(), RpcTypeEnum.SOFA.getName());
    }

    @Override
    public int getOrder() {
        return PluginEnum.SOFA.getCode();
    }

    private boolean checkMetaData(final MetaData metaData) {
        return null != metaData && !StringUtils.isBlank(metaData.getMethodName()) && !StringUtils.isBlank(metaData.getServiceName());
    }

}
