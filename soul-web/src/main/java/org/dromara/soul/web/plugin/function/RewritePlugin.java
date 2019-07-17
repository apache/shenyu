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

package org.dromara.soul.web.plugin.function;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.RewriteHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.web.cache.LocalCacheManager;
import org.dromara.soul.web.plugin.AbstractSoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import javax.validation.constraints.NotBlank;
import java.util.Objects;

/**
 * rewrite url.
 *
 * @author xiaoyu(Myth)
 */
public class RewritePlugin extends AbstractSoulPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(RewritePlugin.class);

    /**
     * Instantiates a new Rewrite plugin.
     *
     * @param localCacheManager the local cache manager
     */
    public RewritePlugin(final LocalCacheManager localCacheManager) {
        super(localCacheManager);
    }

    /**
     * acquire plugin name.
     *
     * @return plugin name.
     */
    @Override
    public String named() {
        return PluginEnum.REWRITE.getName();
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {

        @NotBlank final String handle = rule.getHandle();

        final RewriteHandle rewriteHandle = GsonUtils.getInstance().fromJson(handle, RewriteHandle.class);

        if (Objects.isNull(rewriteHandle) || StringUtils.isBlank(rewriteHandle.getRewriteURI())) {
            LogUtils.error(LOGGER, "uri rewrite rule can not configuration：{}", () -> handle);
            return chain.execute(exchange);
        }
        exchange.getAttributes().put(Constants.REWRITE_URI, rewriteHandle.getRewriteURI());
        return chain.execute(exchange);
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final RequestDTO body = exchange.getAttribute(Constants.REQUESTDTO);
        return Objects.equals(Objects.requireNonNull(body).getRpcType(), RpcTypeEnum.DUBBO.getName());
    }

    /**
     * return plugin type.
     *
     * @return {@linkplain PluginTypeEnum}
     */
    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.FUNCTION;
    }

    @Override
    public int getOrder() {
        return PluginEnum.REWRITE.getCode();
    }
}
