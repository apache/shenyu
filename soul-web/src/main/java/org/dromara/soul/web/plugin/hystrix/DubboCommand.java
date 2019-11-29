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
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.convert.rule.DubboRuleHandle;
import org.dromara.soul.common.dto.convert.selector.DubboSelectorHandle;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.plugin.dubbo.DubboProxyService;
import org.dromara.soul.web.result.SoulResultEnum;
import org.dromara.soul.web.result.SoulResultUtils;
import org.dromara.soul.web.result.SoulResultWarp;
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
 * DubboHystrixCommand.
 *
 * @author xiaoyu(Myth)
 */
public class DubboCommand extends HystrixObservableCommand<Void> {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(DubboCommand.class);

    private final ServerWebExchange exchange;

    private final SoulPluginChain chain;

    private final DubboProxyService dubboProxyService;

    private final String body;

    private final MetaData metaData;

    private final DubboRuleHandle dubboRuleHandle;

    /**
     * Instantiates a new Dubbo command.
     *
     * @param setter              the setter
     * @param body                the body json
     * @param exchange            the exchange
     * @param chain               the chain
     * @param dubboProxyService   the dubbo proxy service
     * @param metaData            the dubbo metaData
     * @param dubboRuleHandle     the dubbo rule handle
     */
    public DubboCommand(final Setter setter, final String body,
                        final ServerWebExchange exchange,
                        final SoulPluginChain chain,
                        final DubboProxyService dubboProxyService,
                        final MetaData metaData,
                        final DubboRuleHandle dubboRuleHandle) {
        super(setter);
        this.exchange = exchange;
        this.body = body;
        this.chain = chain;
        this.dubboProxyService = dubboProxyService;
        this.metaData = metaData;
        this.dubboRuleHandle = dubboRuleHandle;
    }

    @Override
    protected Observable<Void> construct() {
        return RxReactiveStreams.toObservable(doRpcInvoke());
    }

    private Mono<Void> doRpcInvoke() {
        final Object result = dubboProxyService.genericInvoker(body, metaData, dubboRuleHandle);
        if (Objects.nonNull(result)) {
            exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT, result);
        } else {
            exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT, Constants.DUBBO_RPC_RESULT_EMPTY);
        }
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
        Object error = SoulResultWarp.error(SoulResultEnum.SERVICE_RESULT_ERROR.getCode(), SoulResultEnum.SERVICE_RESULT_ERROR.getMsg(), null);
        return SoulResultUtils.result(exchange, error);
    }
}
