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

package org.apache.shenyu.plugin.modify.response;

import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ModifyResponseRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.support.BodyInserterContext;
import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.modify.response.handler.ModifyResponsePluginDataHandler;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ReactiveHttpOutputMessage;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.reactive.function.BodyExtractors;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * ModifyResponse plugin.
 */
public class ModifyResponsePlugin extends AbstractShenyuPlugin {
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        final ModifyResponseRuleHandle modifyResponseRuleHandle = ModifyResponsePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.nonNull(modifyResponseRuleHandle)) {
            ClientResponse response = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
            HttpHeaders httpHeaders = new HttpHeaders();
            httpHeaders.addAll(Objects.requireNonNull(response).headers().asHttpHeaders());
            if (MapUtils.isNotEmpty(modifyResponseRuleHandle.getAddHeaders())) {
                Map<String, String> addHeaderMap = modifyResponseRuleHandle.getAddHeaders();
                addHeaderMap.forEach(httpHeaders::add);
            }

            if (MapUtils.isNotEmpty(modifyResponseRuleHandle.getSetHeaders())) {
                Map<String, String> setHeaderMap = modifyResponseRuleHandle.getSetHeaders();
                setHeaderMap.forEach(httpHeaders::set);
            }

            if (MapUtils.isNotEmpty(modifyResponseRuleHandle.getReplaceHeaderKeys())) {
                Map<String, String> replaceHeaderMap = modifyResponseRuleHandle.getReplaceHeaderKeys();
                replaceHeaderMap.forEach((key, value) -> httpHeaders.replace(key, Collections.singletonList(value)));
            }

            if (Objects.nonNull(modifyResponseRuleHandle.getRemoveHeaderKeys()) && !CollectionUtils.isEmpty(modifyResponseRuleHandle.getRemoveHeaderKeys())) {
                Set<String> removeHeaderList = modifyResponseRuleHandle.getRemoveHeaderKeys();
                removeHeaderList.forEach(httpHeaders::remove);
            }

            if (modifyResponseRuleHandle.getStatusCode() > 0) {
                exchange.getResponse().setStatusCode(HttpStatus.valueOf(modifyResponseRuleHandle.getStatusCode()));
            }

            ClientResponse.Builder builder = ClientResponse.create(Objects.requireNonNull(exchange.getResponse().getStatusCode()),
                    ServerCodecConfigurer.create().getReaders());
            Flux<DataBuffer> body = response.body(BodyExtractors.toDataBuffers());
            ClientResponse modifyResponse = builder
                    .headers(headers -> headers.putAll(httpHeaders))
                    .body(body)
                    .cookies(cookies -> cookies.putAll(response.cookies()))
                    .build();
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, modifyResponse);
        }

        return chain.execute(exchange.mutate()
                .response(new ModifyServerHttpResponse(exchange, modifyResponseRuleHandle)).build());
    }

    @Override
    public int getOrder() {
        return PluginEnum.MODIFY_RESPONSE.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.MODIFY_RESPONSE.getName();
    }

    static class ModifyServerHttpResponse extends ServerHttpResponseDecorator {

        private final ServerWebExchange exchange;

        private final ModifyResponseRuleHandle handle;

        ModifyServerHttpResponse(final ServerWebExchange exchange, final ModifyResponseRuleHandle handle) {
            super(exchange.getResponse());
            this.exchange = exchange;
            this.handle = handle;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            ClientResponse clientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
            if (Objects.isNull(clientResponse)) {
                clientResponse = prepareClientResponse(body, this.getHeaders());
            }
            Mono<byte[]> modifiedBody = clientResponse.bodyToMono(byte[].class)
                    .flatMap(originalBody -> Mono.just(updateResponse(originalBody)));

            BodyInserter<Mono<byte[]>, ReactiveHttpOutputMessage> bodyInserter =
                    BodyInserters.fromPublisher(modifiedBody, byte[].class);
            CachedBodyOutputMessage outputMessage = new CachedBodyOutputMessage(exchange,
                    exchange.getResponse().getHeaders());
            return bodyInserter.insert(outputMessage, new BodyInserterContext()).then(Mono.defer(() -> {
                Mono<DataBuffer> messageBody = DataBufferUtils.join(outputMessage.getBody());
                HttpHeaders headers = getDelegate().getHeaders();
                if (!headers.containsKey(HttpHeaders.TRANSFER_ENCODING)
                        || headers.containsKey(HttpHeaders.CONTENT_LENGTH)) {
                    messageBody = messageBody.doOnNext(data -> headers.setContentLength(data.readableByteCount()));
                }
                return getDelegate().writeWith(messageBody);
            }));

        }

        private byte[] updateResponse(final byte[] responseBody) {
            if (Objects.isNull(handle)) {
                return responseBody;
            }

            String bodyStr = operation(new String(responseBody, StandardCharsets.UTF_8), handle);
            return bodyStr.getBytes(StandardCharsets.UTF_8);
        }

        private String operation(final String jsonValue, final ModifyResponseRuleHandle handle) {
            DocumentContext context = JsonPath.parse(jsonValue);
            if (!CollectionUtils.isEmpty(handle.getAddBodyKeys())) {
                handle.getAddBodyKeys().forEach(info -> {
                    context.put(info.getPath(), info.getKey(), info.getValue());
                });
            }
            if (!CollectionUtils.isEmpty(handle.getReplaceBodyKeys())) {
                handle.getReplaceBodyKeys().forEach(info -> {
                    context.renameKey(info.getPath(), info.getKey(), info.getValue());
                });
            }
            if (!CollectionUtils.isEmpty(handle.getRemoveBodyKeys())) {
                handle.getRemoveBodyKeys().forEach(context::delete);
            }
            return context.jsonString();
        }

        private ClientResponse prepareClientResponse(final Publisher<? extends DataBuffer> body, final HttpHeaders httpHeaders) {
            ClientResponse.Builder builder;
            builder = ClientResponse.create(this.getDelegate().getStatusCode(), ServerCodecConfigurer.create().getReaders());
            return builder.headers(headers -> headers.putAll(httpHeaders)).body(Flux.from(body)).build();
        }
    }
}
