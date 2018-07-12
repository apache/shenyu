/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.function;

import com.hqyg.skyway.api.convert.RewriteHandle;
import com.hqyg.skyway.api.dto.zk.RuleZkDTO;
import com.hqyg.skyway.common.constant.Constants;
import com.hqyg.skyway.common.enums.PluginEnum;
import com.hqyg.skyway.common.enums.PluginTypeEnum;
import com.hqyg.skyway.common.enums.RpcTypeEnum;
import com.hqyg.skyway.common.utils.GSONUtils;
import com.hqyg.skyway.common.utils.LogUtils;
import com.hqyg.skyway.core.request.RequestDTO;
import com.hqyg.skyway.web.cache.DataCacheManager;
import com.hqyg.skyway.web.plugin.AbstractSkywayPlugin;
import com.hqyg.skyway.web.plugin.SkywayPluginChain;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import javax.validation.constraints.NotBlank;
import java.util.Objects;

/**
 * rewrite url.
 * @author xiaoyu(Myth)
 */
public class RewriteSkywayPlugin extends AbstractSkywayPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(RewriteSkywayPlugin.class);

    public RewriteSkywayPlugin(final DataCacheManager dataCacheManager) {
        super(dataCacheManager);
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

    /**
     * this is Template Method child has Implement your own logic.
     *
     * @param exchange exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain    chain the current chain  {@linkplain ServerWebExchange}
     * @param rule     rule    {@linkplain RuleZkDTO}
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SkywayPluginChain chain, final RuleZkDTO rule) {

        @NotBlank final String handle = rule.getHandle();

        final RewriteHandle rewriteHandle = GSONUtils.getInstance().fromJson(handle, RewriteHandle.class);

        if (Objects.isNull(rewriteHandle) || StringUtils.isBlank(rewriteHandle.getRewriteURI())) {
            LogUtils.error(LOGGER, "uri rewrite 处理规则未配置：{}", () -> handle);
            return chain.execute(exchange);
        }

        exchange.getAttributes().put(Constants.REWRITE_URI, rewriteHandle.getRewriteURI());

        return chain.execute(exchange);
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final RequestDTO body = exchange.getAttribute(Constants.REQUESTDTO);
        return Objects.equals(body.getRpcType(), RpcTypeEnum.DUBBO.getName());
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
