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

package org.dromara.soul.plugin.rewrite;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.RewriteHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.rewrite.cache.RewriteRuleHandleCache;
import org.dromara.soul.plugin.rewrite.handler.RewritePluginDataHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Rewrite Plugin.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class RewritePlugin extends AbstractSoulPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        String handle = rule.getHandle();
        final RewriteHandle rewriteHandle = RewriteRuleHandleCache.getInstance()
                .obtainHandle(RewritePluginDataHandler.getCacheKeyName(rule));
        if (Objects.isNull(rewriteHandle) || StringUtils.isBlank(rewriteHandle.getRewriteURI())) {
            log.error("uri rewrite rule can not configuration：{}", handle);
            return chain.execute(exchange);
        }
        exchange.getAttributes().put(Constants.REWRITE_URI, rewriteHandle.getRewriteURI());
        return chain.execute(exchange);
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final SoulContext body = exchange.getAttribute(Constants.CONTEXT);
        return Objects.equals(Objects.requireNonNull(body).getRpcType(), RpcTypeEnum.DUBBO.getName());
    }

    @Override
    public String named() {
        return PluginEnum.REWRITE.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.REWRITE.getCode();
    }
}
