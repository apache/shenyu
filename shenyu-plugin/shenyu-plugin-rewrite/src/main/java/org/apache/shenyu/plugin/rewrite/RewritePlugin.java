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

package org.apache.shenyu.plugin.rewrite;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.RewriteHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.rewrite.handler.RewritePluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Rewrite Plugin.
 */
public class RewritePlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(RewritePlugin.class);

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        String handle = rule.getHandle();
        final RewriteHandle rewriteHandle = RewritePluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(rewriteHandle)) {
            LOG.error("uri rewrite rule can not configuration：{}", handle);
            return chain.execute(exchange);
        }
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        String rewriteUri = exchange.getRequest().getURI().getPath();
        if (StringUtils.isNotBlank(shenyuContext.getRealUrl())) {
            rewriteUri = shenyuContext.getRealUrl();
        }
        if (StringUtils.isNoneBlank(rewriteHandle.getRegex(), rewriteHandle.getReplace())) {
            rewriteUri = rewriteUri.replaceAll(rewriteHandle.getRegex(), rewriteHandle.getReplace());
        }
        exchange.getAttributes().put(Constants.REWRITE_URI, rewriteUri);
        return chain.execute(exchange);
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        final ShenyuContext body = exchange.getAttribute(Constants.CONTEXT);
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
