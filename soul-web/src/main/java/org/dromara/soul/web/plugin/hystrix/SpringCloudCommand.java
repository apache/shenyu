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

package org.dromara.soul.web.plugin.hystrix;

import com.netflix.hystrix.HystrixObservableCommand;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.convert.DubboHandle;
import org.dromara.soul.common.dto.convert.SpringCloudHandle;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.result.SoulResult;
import org.dromara.soul.common.utils.JSONUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.plugin.dubbo.DubboProxyService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import rx.Observable;
import rx.RxReactiveStreams;

import java.util.Map;
import java.util.Objects;

/**
 * SpringCloudCommand.
 *
 * @author xiaoyu(Myth)
 */
public class SpringCloudCommand extends HystrixObservableCommand<Void> {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(SpringCloudCommand.class);

    private final ServerWebExchange exchange;

    private final SoulPluginChain chain;

    private final SpringCloudHandle springCloudHandle;

    public SpringCloudCommand(final Setter setter,
                              final ServerWebExchange exchange,
                              final SoulPluginChain chain,
                              final SpringCloudHandle springCloudHandle) {
        super(setter);
        this.exchange = exchange;
        this.chain = chain;
        this.springCloudHandle = springCloudHandle;

    }

    @Override
    protected Observable<Void> construct() {
        return RxReactiveStreams.toObservable(doRpcInvoke());
    }

    private Mono<Void> doRpcInvoke() {

        return chain.execute(exchange);
    }

    @Override
    protected Observable<Void> resumeWithFallback() {
        return RxReactiveStreams.toObservable(doFallback());
    }

    private Mono<Void> doFallback() {
        if (isFailedExecution()) {
            LogUtils.error(LOGGER, "spring cloud rpc have error:{}", () -> getExecutionException().getMessage());
        }
        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        final SoulResult error = SoulResult.error(Constants.DUBBO_ERROR_RESULT);
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse()
                .bufferFactory().wrap(Objects.requireNonNull(JSONUtils.toJson(error)).getBytes())));
    }
}
