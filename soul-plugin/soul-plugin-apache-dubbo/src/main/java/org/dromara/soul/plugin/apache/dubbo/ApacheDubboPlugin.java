/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.plugin.apache.dubbo;

import org.dromara.soul.cache.api.LocalCacheManager;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.dromara.soul.plugin.base.context.SoulContext;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * dubbo proxy.
 *
 * @author xiaoyu(Myth)
 */
public class ApacheDubboPlugin extends AbstractSoulPlugin {

    private final ApacheDubboProxyService dubboProxyService;

    /**
     * Instantiates a new Dubbo plugin.
     *
     * @param localCacheManager the local cache manager
     * @param dubboProxyService the dubbo proxy service
     */
    public ApacheDubboPlugin(final LocalCacheManager localCacheManager, final ApacheDubboProxyService dubboProxyService) {
        super(localCacheManager);
        this.dubboProxyService = dubboProxyService;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {

        final String body = exchange.getAttribute(Constants.DUBBO_PARAMS);

        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);

        assert soulContext != null;

        final Mono<Object> result = dubboProxyService.genericInvoker(body, soulContext.getMetaData(), exchange);
        return result.then(chain.execute(exchange));
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

}
