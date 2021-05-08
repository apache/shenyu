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

package org.apache.shenyu.plugin.context.path;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ContextMappingHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.SoulPluginChain;
import org.apache.shenyu.plugin.api.context.SoulContext;
import org.apache.shenyu.plugin.base.AbstractSoulPlugin;
import org.apache.shenyu.plugin.context.path.cache.ContextPathRuleHandleCache;
import org.apache.shenyu.plugin.context.path.handler.ContextPathMappingPluginDataHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * ContextPathMapping Plugin.
 *
 * @author zhanglei
 */
@Slf4j
public class ContextPathMappingPlugin extends AbstractSoulPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        ContextMappingHandle contextMappingHandle = ContextPathRuleHandleCache.getInstance().obtainHandle(ContextPathMappingPluginDataHandler.getCacheKeyName(rule));
        if (Objects.isNull(contextMappingHandle) || StringUtils.isBlank(contextMappingHandle.getContextPath())) {
            log.error("context path mapping rule configuration is null ：{}", rule);
            return chain.execute(exchange);
        }
        buildContextPath(soulContext, contextMappingHandle);
        return chain.execute(exchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.CONTEXTPATH_MAPPING.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.CONTEXTPATH_MAPPING.getName();
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        SoulContext body = exchange.getAttribute(Constants.CONTEXT);
        return Objects.equals(Objects.requireNonNull(body).getRpcType(), RpcTypeEnum.DUBBO.getName());
    }

    /**
     * Build the context path and realUrl.
     *
     * @param context context
     * @param handle  handle
     */
    private void buildContextPath(final SoulContext context, final ContextMappingHandle handle) {
        String realURI = "";
        if (StringUtils.isNoneBlank(handle.getContextPath())) {
            context.setContextPath(handle.getContextPath());
            context.setModule(handle.getContextPath());
            realURI = context.getPath().substring(handle.getContextPath().length());
        } else {
            if (StringUtils.isNoneBlank(handle.getAddPrefix())) {
                realURI = handle.getAddPrefix() + context.getPath();
            }
        }
        context.setRealUrl(realURI);
        if (StringUtils.isNoneBlank(handle.getRealUrl())) {
            log.info("context path mappingPlugin replaced old :{} , real:{}", context.getRealUrl(), handle.getRealUrl());
            context.setRealUrl(handle.getRealUrl());
        }
    }
}
