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

package org.apache.shenyu.plugin.sentinel.fallback;

import com.alibaba.csp.sentinel.slots.block.BlockException;
import com.alibaba.csp.sentinel.slots.block.degrade.DegradeException;
import com.alibaba.csp.sentinel.slots.block.flow.FlowException;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.base.fallback.FallbackHandler;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.sentinel.SentinelPlugin;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * Sentinel block handler.
 */
public class SentinelFallbackHandler implements FallbackHandler {

    @Override
    public Mono<Void> withoutFallback(final ServerWebExchange exchange, final Throwable throwable) {
        Object error;
        if (throwable instanceof DegradeException) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_RESULT_ERROR);
        } else if (throwable instanceof FlowException) {
            exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS);
            error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.TOO_MANY_REQUESTS);
        } else if (throwable instanceof BlockException) {
            exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS);
            error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SENTINEL_BLOCK_ERROR);
        } else if (throwable instanceof SentinelPlugin.SentinelFallbackException) {
            return exchange.getAttribute(Constants.RESPONSE_MONO);
        } else {
            return Mono.error(throwable);
        }
        return WebFluxResultUtils.result(exchange, error);
    }
}
