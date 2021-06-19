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

package org.apache.shenyu.plugin.response;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ModifyResponseRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.response.cache.ModifyResponseRuleHandleCache;
import org.apache.shenyu.plugin.response.handler.ModifyResponsePluginDataHandler;
import org.apache.shenyu.web.filter.support.BodyInserterContext;
import org.apache.shenyu.web.filter.support.CachedBodyOutputMessage;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ReactiveHttpOutputMessage;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Objects;
import java.util.Set;


/**
 * ModifyResponse plugin.
 */
@Slf4j
public class ModifyResponsePlugin extends AbstractShenyuPlugin {

    public ModifyResponsePlugin() {

    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        if (Objects.isNull(rule)) {
            return Mono.empty();
        }
        final ShenyuContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        final ModifyResponseRuleHandle modifyResponseRuleHandle = ModifyResponseRuleHandleCache.getInstance().obtainHandle(ModifyResponsePluginDataHandler.getResourceName(rule));
        if (Objects.nonNull(modifyResponseRuleHandle)) {
            ServerHttpResponse response = exchange.getResponse();
            HttpHeaders httpHeaders = response.getHeaders();
            if (Objects.nonNull(modifyResponseRuleHandle.getAddHeaders()) && MapUtils.isNotEmpty(modifyResponseRuleHandle.getAddHeaders())) {
                Map<String, String> addHeaderMap = modifyResponseRuleHandle.getAddHeaders();
                addHeaderMap.entrySet().stream().forEach(a -> httpHeaders.add(a.getKey(), a.getValue()));
            }

            if (Objects.nonNull(modifyResponseRuleHandle.getSetHeaders()) && MapUtils.isNotEmpty(modifyResponseRuleHandle.getSetHeaders())) {
                Map<String, String> setHeaderMap = modifyResponseRuleHandle.getSetHeaders();
                setHeaderMap.entrySet().stream().forEach(a -> httpHeaders.set(a.getKey(), a.getValue()));
            }

            if (Objects.nonNull(modifyResponseRuleHandle.getReplaceHeaderKeys()) && MapUtils.isNotEmpty(modifyResponseRuleHandle.getReplaceHeaderKeys())) {
                Map<String, String> replaceHeaderMap = modifyResponseRuleHandle.getReplaceHeaderKeys();
                replaceHeaderMap.entrySet().stream().forEach(a -> {
                    httpHeaders.addAll(a.getValue(), httpHeaders.get(a.getKey()));
                    httpHeaders.remove(a.getKey());
                });
            }

            if (Objects.nonNull(modifyResponseRuleHandle.getRemoveHeaderKeys()) && !CollectionUtils.isEmpty(modifyResponseRuleHandle.getRemoveHeaderKeys())) {
                Set<String> removeHeaderList = modifyResponseRuleHandle.getRemoveHeaderKeys();
                removeHeaderList.stream().forEach(a -> httpHeaders.remove(a));
            }

            ClientResponse clientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
            if (modifyResponseRuleHandle.getStatusCode() > 0) {
                response.setStatusCode(HttpStatus.valueOf(modifyResponseRuleHandle.getStatusCode()));
            } else {
                response.setStatusCode(clientResponse.statusCode());
            }
        }

        return chain.execute(exchange.mutate()
                .response(new ParamServerHttpResponse(exchange, modifyResponseRuleHandle)).build());
    }

    @Override
    public int getOrder() {
        return PluginEnum.MODIFY_RESPONSE.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.MODIFY_RESPONSE.getName();
    }

    static class ParamServerHttpResponse extends ServerHttpResponseDecorator {

        private final ServerWebExchange exchange;

        private final ModifyResponseRuleHandle handle;

        ParamServerHttpResponse(final ServerWebExchange exchange, final ModifyResponseRuleHandle handle) {
            super(exchange.getResponse());
            this.exchange = exchange;
            this.handle = handle;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            HttpHeaders httpHeaders = new HttpHeaders();
            httpHeaders.setContentType(MediaType.APPLICATION_JSON);

            ClientResponse clientResponse = prepareClientResponse(body, httpHeaders);
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
            if (Objects.nonNull(handle)) {
                return responseBody;
            }

            Map<String, String> addBodyMap = handle.getAddBodyMap();
            Map<String, Object> bodyMap = GsonUtils.getInstance().toObjectMap(new String(responseBody, StandardCharsets.UTF_8));
            if (Objects.nonNull(addBodyMap) && MapUtils.isNotEmpty(addBodyMap)) {
                addBodyMap.entrySet().stream().forEach(a -> bodyMap.put(a.getKey(), a.getValue()));
            }

            Map<String, String> setBodyMap = handle.getSetBodyMap();
            if (Objects.nonNull(setBodyMap) && MapUtils.isNotEmpty(setBodyMap)) {
                setBodyMap.entrySet().stream().forEach(a -> bodyMap.put(a.getKey(), a.getValue()));
            }

            Map<String, String> replaceBodyKeys = handle.getReplaceBodyKeys();
            if (Objects.nonNull(replaceBodyKeys) && MapUtils.isNotEmpty(replaceBodyKeys)) {
                replaceBodyKeys.entrySet().stream().forEach(a -> {
                    if (bodyMap.containsKey(a.getKey())) {
                        bodyMap.put(a.getValue(), bodyMap.get(a.getKey()));
                        bodyMap.remove(a.getKey());
                    }
                });
            }

            Set<String> removeBodySet = handle.getRemoveBodyKeys();
            if (Objects.nonNull(removeBodySet) && !CollectionUtils.isEmpty(removeBodySet)) {
                removeBodySet.stream().forEach(a -> bodyMap.remove(a));
            }

            return GsonUtils.getInstance().toJson(bodyMap).getBytes(StandardCharsets.UTF_8);
        }

        private ClientResponse prepareClientResponse(final Publisher<? extends DataBuffer> body, final HttpHeaders httpHeaders) {
            ClientResponse.Builder builder;
            builder = ClientResponse.create(exchange.getResponse().getStatusCode(), ServerCodecConfigurer.create().getReaders());
            return builder.headers(headers -> headers.putAll(httpHeaders)).body(Flux.from(body)).build();
        }
    }
}
