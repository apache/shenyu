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

package org.apache.shenyu.plugin.mock;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.MockHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.mock.api.MockRequest;
import org.apache.shenyu.plugin.mock.generator.GeneratorFactory;
import org.apache.shenyu.plugin.mock.handler.MockPluginHandler;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;

/**
 * MockPlugin.
 */
public class MockPlugin extends AbstractShenyuPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {

        MockHandle mockHandle = MockPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
        exchange.getResponse().setStatusCode(HttpStatus.valueOf(mockHandle.getHttpStatusCode()));

        return DataBufferUtils.join(exchange.getRequest().getBody())
                .switchIfEmpty(Mono.just(DefaultDataBufferFactory.sharedInstance.allocateBuffer(0)))
                .map(dataBuffer -> dealRule(dataBuffer, mockHandle.getResponseContent(), exchange.getRequest()))
                .flatMap(bytes -> exchange.getResponse().writeWith(Mono.just(exchange.getResponse()
                        .bufferFactory().wrap(bytes))
                        .doOnNext(data -> exchange.getResponse().getHeaders()
                                .setContentLength(data.readableByteCount()))));
    }

    @Override
    public int getOrder() {
        return PluginEnum.MOCK.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.MOCK.getName();
    }

    private byte[] dealRule(final DataBuffer requestBodyBuffer, final String response, final ServerHttpRequest serverHttpRequest) {
        byte[] originalBody = new byte[requestBodyBuffer.readableByteCount()];
        requestBodyBuffer.read(originalBody);
        DataBufferUtils.release(requestBodyBuffer);
        MockRequest mockRequest = buildMockRequest(originalBody, serverHttpRequest);
        return GeneratorFactory.dealRule(response, mockRequest).getBytes(StandardCharsets.UTF_8);
    }

    private MockRequest buildMockRequest(final byte[] originalBody, final ServerHttpRequest serverHttpRequest) {

        return MockRequest.Builder.builder()
                .headers(serverHttpRequest.getHeaders().toSingleValueMap())
                .method(serverHttpRequest.getMethodValue())
                .queries(serverHttpRequest.getQueryParams().toSingleValueMap())
                .uri(serverHttpRequest.getURI().toString())
                .body(originalBody).build();
    }
}
