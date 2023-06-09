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

package org.apache.shenyu.plugin.response.strategy;

import com.google.common.collect.Lists;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * The type Rpc message writer.
 */
public class RPCMessageWriter implements MessageWriter {

    @Override
    public Mono<Void> writeWith(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange).then(Mono.defer(() -> {
            Object result = exchange.getAttribute(Constants.RPC_RESULT);
            if (Objects.isNull(result)) {
                Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_RESULT_ERROR);
                return WebFluxResultUtils.result(exchange, error);
            }
            Mono<Void> responseMono = WebFluxResultUtils.result(exchange, result);
            exchange.getAttributes().put(Constants.RESPONSE_MONO, responseMono);
            // watcher httpStatus
            final Consumer<HttpStatus> consumer = exchange.getAttribute(Constants.WATCHER_HTTP_STATUS);
            Optional.ofNullable(consumer).ifPresent(c -> c.accept(exchange.getResponse().getStatusCode()));
            return responseMono;
        }));
    }
    
    @Override
    public List<String> supportTypes() {
        return Lists.newArrayList(RpcTypeEnum.DUBBO.getName(), RpcTypeEnum.SOFA.getName(), 
                RpcTypeEnum.GRPC.getName(), RpcTypeEnum.MOTAN.getName(), RpcTypeEnum.TARS.getName(), RpcTypeEnum.BRPC.getName());
    }
}
