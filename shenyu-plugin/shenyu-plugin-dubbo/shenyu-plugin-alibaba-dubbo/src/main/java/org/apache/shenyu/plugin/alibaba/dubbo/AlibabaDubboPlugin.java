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

package org.apache.shenyu.plugin.alibaba.dubbo;

import com.alibaba.dubbo.remoting.exchange.ResponseCallback;
import com.alibaba.dubbo.remoting.exchange.ResponseFuture;
import com.alibaba.dubbo.rpc.Result;
import com.alibaba.dubbo.rpc.RpcContext;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ResultEnum;
import org.apache.shenyu.plugin.alibaba.dubbo.proxy.AlibabaDubboProxyService;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.dubbo.common.AbstractDubboPlugin;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.Objects;

/**
 * Alibaba dubbo plugin.
 */
public class AlibabaDubboPlugin extends AbstractDubboPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AlibabaDubboPlugin.class);

    private final AlibabaDubboProxyService alibabaDubboProxyService;

    /**
     * Instantiates a new Dubbo plugin.
     *
     * @param alibabaDubboProxyService the dubbo proxy service
     */
    public AlibabaDubboPlugin(final AlibabaDubboProxyService alibabaDubboProxyService) {
        this.alibabaDubboProxyService = alibabaDubboProxyService;
    }

    /**
     * do dubbo invoker.
     *
     * @param exchange exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain    chain the current chain  {@linkplain ServerWebExchange}
     * @param selector selector    {@linkplain SelectorData}
     * @param rule     rule    {@linkplain RuleData}
     * @param metaData the medata
     * @param param    the param
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    @Override
    protected Mono<Void> doDubboInvoker(final ServerWebExchange exchange,
                                        final ShenyuPluginChain chain,
                                        final SelectorData selector,
                                        final RuleData rule,
                                        final MetaData metaData,
                                        final String param) {
        RpcContext.getContext().setAttachment(Constants.DUBBO_SELECTOR_ID, selector.getId());
        RpcContext.getContext().setAttachment(Constants.DUBBO_RULE_ID, rule.getId());
        RpcContext.getContext().setAttachment(Constants.DUBBO_REMOTE_ADDRESS, Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getAddress().getHostAddress());
        return Mono.create(monoSink -> {
            ResponseFuture future = alibabaDubboProxyService.genericInvoker(param, metaData);
            future.setCallback(new ResponseCallback() {

                @Override
                public void done(final Object resultObj) {
                    assert resultObj instanceof Result;
                    Result result = (Result) resultObj;
                    if (result.hasException()) {
                        this.caught(result.getException());
                        return;
                    }
                    monoSink.success(result.getValue());
                }

                @Override
                public void caught(final Throwable ex) {
                    LOG.error("dubbo failed using async genericInvoker() metaData={} param={}", metaData, param, ex);
                    monoSink.error(ex);
                }
            });
        }).flatMap(response -> {
            exchange.getAttributes().put(Constants.RPC_RESULT, Objects.nonNull(response) ? response : Constants.DUBBO_RPC_RESULT_EMPTY);
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
            return chain.execute(exchange);
        });
    }

    @Override
    protected void transmitRpcContext(final Map<String, String> rpcContext) {
        rpcContext.forEach(RpcContext.getContext()::setAttachment);
    }
}
