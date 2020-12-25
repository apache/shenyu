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

package org.dromara.soul.plugin.contextpath;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.ContextMappingHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.api.result.SoulResultEnum;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.dromara.soul.plugin.base.utils.SoulResultWrap;
import org.dromara.soul.plugin.base.utils.WebFluxResultUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;

/**
 * ContextPathMapping Plugin.
 *
 * @author zhanglei
 */
@Slf4j
public class ContextPathMappingPlugin extends AbstractSoulPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        final String handle = rule.getHandle();
        final ContextMappingHandle contextMappingHandle = GsonUtils.getInstance().fromJson(handle, ContextMappingHandle.class);
        if (Objects.isNull(contextMappingHandle) || StringUtils.isBlank(contextMappingHandle.getContextPath())) {
            log.error("context path mapping rule configuration is null ：{}", rule);
            return chain.execute(exchange);
        }
        //check the context path illegal
        if (!soulContext.getPath().startsWith(contextMappingHandle.getContextPath())) {
            Object error = SoulResultWrap.error(SoulResultEnum.CONTEXT_PATH_ERROR.getCode(), SoulResultEnum.CONTEXT_PATH_ERROR.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        this.buildContextPath(soulContext, contextMappingHandle);
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
        final SoulContext body = exchange.getAttribute(Constants.CONTEXT);
        return Objects.equals(Objects.requireNonNull(body).getRpcType(), RpcTypeEnum.DUBBO.getName());
    }

    /**
     * Build the context path and realUrl.
     *
     * @param context context
     * @param handle  handle
     */
    private void buildContextPath(final SoulContext context, final ContextMappingHandle handle) {
        context.setContextPath(handle.getContextPath());
        if (!StringUtils.isBlank(handle.getRealUrl())) {
            log.info("context path mappingPlugin replaced old :{} , real:{}", context.getRealUrl(), handle.getRealUrl());
            context.setRealUrl(handle.getRealUrl());
            return;
        }
        Optional<String> optional = Arrays.stream(context.getPath()
                .split(handle.getContextPath()))
                .reduce((first, last) -> last);
        optional.ifPresent(context::setRealUrl);
    }
}
