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
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.context.path.cache.ContextPathRuleHandleCache;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * ContextPath Plugin.
 */
@Slf4j
public class ContextPathPlugin extends AbstractShenyuPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        ContextMappingHandle contextMappingHandle = ContextPathRuleHandleCache.getInstance().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(contextMappingHandle)) {
            log.error("context path rule configuration is null ：{}", rule);
            return chain.execute(exchange);
        }
        buildContextPath(shenyuContext, contextMappingHandle);
        return chain.execute(exchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.CONTEXT_PATH.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.CONTEXT_PATH.getName();
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        ShenyuContext body = exchange.getAttribute(Constants.CONTEXT);
        assert body != null;
        String rpcType = body.getRpcType();
        return Objects.equals(rpcType, RpcTypeEnum.DUBBO.getName())
                || Objects.equals(rpcType, RpcTypeEnum.GRPC.getName())
                || Objects.equals(rpcType, RpcTypeEnum.TARS.getName())
                || Objects.equals(rpcType, RpcTypeEnum.MOTAN.getName())
                || Objects.equals(rpcType, RpcTypeEnum.SOFA.getName());
    }

    /**
     * Build the context path and realUrl.
     *
     * @param context context
     * @param handle  handle
     */
    private void buildContextPath(final ShenyuContext context, final ContextMappingHandle handle) {
        String realURI = "";
        if (StringUtils.isNoneBlank(handle.getContextPath())) {
            context.setContextPath(handle.getContextPath());
            context.setModule(handle.getContextPath());
            realURI = context.getPath().substring(handle.getContextPath().length());
        }
        if (StringUtils.isNoneBlank(handle.getAddPrefix())) {
            if (StringUtils.isNotBlank(realURI)) {
                realURI = handle.getAddPrefix() + realURI;
            } else {
                realURI = handle.getAddPrefix() + context.getPath();
            }
        }
        context.setRealUrl(realURI);
        if (StringUtils.isNoneBlank(handle.getRealUrl())) {
            log.info("context path replaced old :{} , real:{}", context.getRealUrl(), handle.getRealUrl());
            context.setRealUrl(handle.getRealUrl());
        }
    }
}
