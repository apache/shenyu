/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.before;

import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.dromara.soul.web.result.SoulResultEnum;
import org.dromara.soul.web.result.SoulResultUtils;
import org.dromara.soul.web.result.SoulResultWarp;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * Sign Plugin.
 *
 * @author xiaoyu(Myth)
 */
public class SignPlugin implements SoulPlugin {

    private final SignService signService;

    /**
     * Instantiates a new Sign plugin.
     *
     * @param signService the sign service
     */
    public SignPlugin(final SignService signService) {
        this.signService = signService;
    }

    @Override
    public String named() {
        return PluginEnum.SIGN.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.SIGN.getCode();
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final SoulPluginChain chain) {
        final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
        assert requestDTO != null;
        Pair<Boolean, String> result = signService.signVerify(requestDTO, exchange);
        if (!result.getLeft()) {
            Object error = SoulResultWarp.error(SoulResultEnum.SIGN_IS_NOT_PASS.getCode(), result.getRight(), null);
            return SoulResultUtils.result(exchange, error);
        }
        return chain.execute(exchange);
    }

    /**
     * return plugin type.
     *
     * @return {@linkplain PluginTypeEnum}
     */
    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.BEFORE;
    }

}
