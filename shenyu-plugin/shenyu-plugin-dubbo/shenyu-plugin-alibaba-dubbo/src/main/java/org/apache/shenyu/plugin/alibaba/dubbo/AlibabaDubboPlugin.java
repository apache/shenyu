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

import com.alibaba.dubbo.remoting.exchange.ResponseCallback;
import com.alibaba.dubbo.remoting.exchange.ResponseFuture;
import com.alibaba.dubbo.rpc.Result;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ResultEnum;
import org.apache.shenyu.plugin.alibaba.dubbo.proxy.AlibabaDubboProxyService;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.dubbo.common.AbstractDubboPlugin;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Alibaba dubbo plugin.
 */
public class AlibabaDubboPlugin extends AbstractDubboPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AlibabaDubboPlugin.class);

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
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.DUBBO_HAVE_BODY_PARAM.getCode(), ShenyuResultEnum.DUBBO_HAVE_BODY_PARAM.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }

        return Mono.create(monoSink -> {
            ResponseFuture future = alibabaDubboProxyService.genericInvoker(param, metaData);
            future.setCallback(new ResponseCallback() {

                @Override
                public void done(final Object resultObj) {
                    assert resultObj instanceof Result;
                    Result result = (Result) resultObj;
                    if (result.hasException()) {
                        this.caught(result.getException());
                        return;
                    }
                    monoSink.success(result.getValue());
                }

                @Override
                public void caught(final Throwable ex) {
                    LOG.error("dubbo failed using async genericInvoker() metaData={} param={}", metaData, param, ex);
                    monoSink.error(ex);
                }
            });
        }).flatMap(response -> {
            exchange.getAttributes().put(Constants.RPC_RESULT, Objects.nonNull(response) ? response : Constants.DUBBO_RPC_RESULT_EMPTY);
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
            return chain.execute(exchange);
        });
    }

    @Override
    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noSelectorResult(pluginName, exchange);
    }

    @Override
    protected Mono<Void> handleRuleIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noRuleResult(pluginName, exchange);
    }

    private boolean checkMetaData(final MetaData metaData) {
        return null != metaData && !StringUtils.isBlank(metaData.getMethodName()) && !StringUtils.isBlank(metaData.getServiceName());
    }
}
