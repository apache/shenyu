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

package org.dromara.soul.web.plugin.hystrix;

import cn.hutool.json.JSONUtil;
import com.hqyg.skyway.api.convert.DubboHandle;
import com.hqyg.skyway.common.constant.Constants;
import com.hqyg.skyway.common.enums.ResultEnum;
import com.hqyg.skyway.common.utils.LogUtils;
import com.hqyg.skyway.web.plugin.SkywayPluginChain;
import com.hqyg.skyway.web.plugin.dubbo.DubboServiceProxy;
import com.hqyg.skyway.web.result.SkywayResult;
import com.netflix.hystrix.HystrixObservableCommand;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import rx.Observable;
import rx.RxReactiveStreams;

import java.util.Map;

/**
 * DubboHystrixCommand.
 * @author xiaoyu(Myth)
 */
public class DubboHystrixCommand extends HystrixObservableCommand<Void> {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(DubboHystrixCommand.class);

    private final ServerWebExchange exchange;

    private final SkywayPluginChain chain;

    private final DubboServiceProxy dubboServiceProxy;

    private final Map<String, Object> paramMap;

    private final DubboHandle dubboHandle;

    public DubboHystrixCommand(final Setter setter, final Map<String, Object> paramMap,
                               final ServerWebExchange exchange,
                               final SkywayPluginChain chain,
                               final DubboServiceProxy dubboServiceProxy,
                               final DubboHandle dubboHandle) {
        super(setter);
        this.exchange = exchange;
        this.paramMap = paramMap;
        this.chain = chain;
        this.dubboServiceProxy = dubboServiceProxy;
        this.dubboHandle = dubboHandle;

    }

    @Override
    protected Observable<Void> construct() {
        return RxReactiveStreams.toObservable(doRpcInvoke());
    }

    private Mono<Void> doRpcInvoke() {
        final Object result = dubboServiceProxy.genericInvoker(paramMap, dubboHandle);
        exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT, result);
        exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
        return chain.execute(exchange);
    }

    @Override
    protected Observable<Void> resumeWithFallback() {
        return RxReactiveStreams.toObservable(doFallback());
    }

    private Mono<Void> doFallback() {
        if (isFailedExecution()) {
            LogUtils.error(LOGGER, "dubbo rpc have error:{}", () -> getExecutionException().getMessage());
        }
        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        final SkywayResult error = SkywayResult.error(Constants.DUBBO_ERROR_RESULT);
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(JSONUtil.toJsonStr(error).getBytes())));
    }
}
