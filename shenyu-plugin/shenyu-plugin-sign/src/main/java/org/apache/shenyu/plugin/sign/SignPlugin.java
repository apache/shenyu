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

package org.apache.shenyu.plugin.sign;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.sign.api.SignService;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * Sign Plugin.
 */
public class SignPlugin extends AbstractShenyuPlugin {

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
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        Pair<Boolean, String> result = signService.signVerify(exchange);
        if (!result.getLeft()) {
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.SIGN_IS_NOT_PASS.getCode(), result.getRight(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        return chain.execute(exchange);
    }
}
