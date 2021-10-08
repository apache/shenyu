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

package org.apache.shenyu.plugin.api.utils;

import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * The type Shenyu result utils.
 */
public final class WebFluxResultUtils {

    private static final Logger LOG = LoggerFactory.getLogger(WebFluxResultUtils.class);

    /**
     * Error mono.
     *
     * @param exchange the exchange
     * @param result    the result
     * @return the mono
     */
    public static Mono<Void> result(final ServerWebExchange exchange, final Object result) {
        exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse()
                // TODO this is a risk for error charset coding with getBytes
                .bufferFactory().wrap(Objects.requireNonNull(JsonUtils.toJson(result)).getBytes())));
    }

    /**
     * get no selector result.
     *
     * @param pluginName the plugin name
     * @param exchange   the exchange
     * @return the mono
     */
    public static Mono<Void> noSelectorResult(final String pluginName, final ServerWebExchange exchange) {
        LOG.error("can not match selector data: {}", pluginName);
        Object error = ShenyuResultWrap.error(ShenyuResultEnum.SELECTOR_NOT_FOUND.getCode(), pluginName + ":" + ShenyuResultEnum.SELECTOR_NOT_FOUND.getMsg(), null);
        return WebFluxResultUtils.result(exchange, error);
    }

    /**
     * get no rule result.
     *
     * @param pluginName the plugin name
     * @param exchange   the exchange
     * @return the mono
     */
    public static Mono<Void> noRuleResult(final String pluginName, final ServerWebExchange exchange) {
        LOG.error("can not match rule data: {}", pluginName);
        Object error = ShenyuResultWrap.error(ShenyuResultEnum.RULE_NOT_FOUND.getCode(), pluginName + ":" + ShenyuResultEnum.RULE_NOT_FOUND.getMsg(), null);
        return WebFluxResultUtils.result(exchange, error);
    }
}
