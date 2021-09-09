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

package org.apache.shenyu.plugin.request.id;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.ThreadShareDataEnum;
import org.apache.shenyu.common.utils.ThreadShareUtils;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.request.id.ShenyuRequestIdWrap;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * RequestIdPlugin.
 */
public class RequestIdPlugin implements ShenyuPlugin {

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        String requestId = ShenyuRequestIdWrap.newRequestId();
        String requestIdName = ThreadShareDataEnum.REQUEST_ID.getName();
        // put the thread share data
        ThreadShareUtils.put(requestIdName, requestId);
        ServerWebExchange modifiedExchange = exchange.mutate()
                .request(originalRequest -> originalRequest
                        .headers(httpHeaders -> httpHeaders.set(requestIdName, requestId))
                )
                .build();
        return chain.execute(modifiedExchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.REQUEST_ID.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.REQUEST_ID.getName();
    }
}
