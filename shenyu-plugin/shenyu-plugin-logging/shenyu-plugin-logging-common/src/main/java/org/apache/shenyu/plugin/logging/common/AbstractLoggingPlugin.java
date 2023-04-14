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

package org.apache.shenyu.plugin.logging.common;

import com.google.common.collect.Sets;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.apache.shenyu.plugin.logging.common.body.LoggingServerHttpRequest;
import org.apache.shenyu.plugin.logging.common.body.LoggingServerHttpResponse;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.entity.CommonLoggingRuleHandle;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectUtils;
import org.apache.shenyu.plugin.logging.desensitize.api.enums.DataDesensitizeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Collections;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;

/**
 * abstract logging plugin.
 */
public abstract class AbstractLoggingPlugin<L extends ShenyuRequestLog> extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractLoggingPlugin.class);

    private static String dataDesensitizeAlg;

    /**
     * LogCollector.
     *
     * @return LogCollector
     */
    protected abstract LogCollector<L> logCollector();

    /**
     * pluginEnum.
     *
     * @return PluginEnum
     */
    protected abstract PluginEnum pluginEnum();

    /**
     * collect log based on ShenyuRequestLog.
     *
     * @param exchange exchange
     * @param selector selector
     * @param rule rule
     * @return based on ShenyuRequestLog
     */
    protected abstract L doLogExecute(ServerWebExchange exchange, SelectorData selector, RuleData rule);

    @Override
    public Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                final SelectorData selector, final RuleData rule) {
        CommonLoggingRuleHandle commonLoggingRuleHandle = AbstractLogPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        boolean desensitized = false;
        Set<String> keywordSets = Sets.newHashSet();
        if (Objects.nonNull(commonLoggingRuleHandle)) {
            String keywords = commonLoggingRuleHandle.getKeyword();
            desensitized = StringUtils.isNotBlank(keywords) && commonLoggingRuleHandle.getMaskStatus();
            if (desensitized) {
                Collections.addAll(keywordSets, keywords.split(";"));
                dataDesensitizeAlg = Optional.ofNullable(commonLoggingRuleHandle.getMaskType()).orElse(DataDesensitizeEnum.MD5_ENCRYPT.getDataDesensitizeAlg());
                LOG.info("current plugin:{}, keyword:{}, dataDesensitizeAlg:{}", pluginEnum().getName(), keywords, dataDesensitizeAlg);
            }
        }
        ServerHttpRequest request = exchange.getRequest();
        // control sampling
        if (!LogCollectConfigUtils.isSampled(exchange.getRequest())) {
            return chain.execute(exchange);
        }
        L requestInfo = this.doLogExecute(exchange, selector, rule);
        requestInfo.setRequestUri(request.getURI().toString());
        requestInfo.setMethod(request.getMethodValue());
        requestInfo.setRequestHeader(LogCollectUtils.getHeaders(request.getHeaders()));
        requestInfo.setQueryParams(request.getURI().getQuery());
        requestInfo.setClientIp(HostAddressUtils.acquireIp(exchange));
        requestInfo.setUserAgent(request.getHeaders().getFirst(GenericLoggingConstant.USER_AGENT));
        requestInfo.setHost(request.getHeaders().getFirst(GenericLoggingConstant.HOST));
        requestInfo.setPath(request.getURI().getPath());
        LoggingServerHttpRequest<L> loggingServerHttpRequest = new LoggingServerHttpRequest<>(request, requestInfo);
        LoggingServerHttpResponse<L> loggingServerHttpResponse = new LoggingServerHttpResponse<>(exchange.getResponse(),
                requestInfo, this.logCollector(), desensitized, keywordSets, dataDesensitizeAlg);
        ServerWebExchange webExchange = exchange.mutate().request(loggingServerHttpRequest)
                .response(loggingServerHttpResponse).build();
        loggingServerHttpResponse.setExchange(webExchange);
        return chain.execute(webExchange).doOnError(loggingServerHttpResponse::logError);
    }

    /**
     * get plugin order.
     *
     * @return order
     */
    @Override
    public int getOrder() {
        return pluginEnum().getCode();
    }

    /**
     * get plugin name.
     *
     * @return plugin name
     */
    @Override
    public String named() {
        return pluginEnum().getName();
    }
}
