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
import org.springframework.http.MediaType;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * The type Soul result utils.
 *
 * @author xiaoyu
 */
public final class WebFluxResultUtils {

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
}
