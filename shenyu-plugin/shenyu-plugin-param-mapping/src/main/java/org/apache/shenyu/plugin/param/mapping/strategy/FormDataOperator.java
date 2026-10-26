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

package org.apache.shenyu.plugin.param.mapping.strategy;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.jayway.jsonpath.DocumentContext;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingRuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.support.BodyInserterContext;
import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ReactiveHttpOutputMessage;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.StringUtils;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Collections;
import java.util.Objects;
import java.util.function.Function;

/**
 * ApplicationFormStrategy.
 */
public class FormDataOperator implements Operator {

    private static final Logger LOG = LoggerFactory.getLogger(FormDataOperator.class);

    // GsonUtils normalizes null strings to empty strings; form values must retain that distinction.
    private static final Gson FORM_DATA_GSON = new Gson();

    @Override
    public Mono<Void> apply(final ServerWebExchange exchange, final ShenyuPluginChain shenyuPluginChain,
            final ParamMappingRuleHandle paramMappingRuleHandle) {
        return exchange.getFormData()
                .switchIfEmpty(Mono.just(new LinkedMultiValueMap<>()))
                .flatMap(multiValueMap -> {
                    if (Objects.isNull(multiValueMap)
                            || (multiValueMap.isEmpty() && CollectionUtils.isEmpty(paramMappingRuleHandle.getAddParameterKeys()))) {
                        return shenyuPluginChain.execute(exchange);
                    }
                    String original = GsonUtils.getInstance().toJson(multiValueMap);
                    LOG.info("get from data success data:{}", original);
                    String modify = operation(original, paramMappingRuleHandle);
                    if (!StringUtils.hasLength(modify)) {
                        return shenyuPluginChain.execute(exchange);
                    }
                    HttpHeaders headers = exchange.getRequest().getHeaders();
                    HttpHeaders httpHeaders = new HttpHeaders();
                    LinkedMultiValueMap<String, String> modifyMap = toLinkedMultiValueMap(modify);
                    final BodyInserter<LinkedMultiValueMap<String, String>, ReactiveHttpOutputMessage> bodyInserter = BodyInserters
                            .fromValue(modifyMap);
                    httpHeaders.putAll(headers);
                    httpHeaders.remove(HttpHeaders.CONTENT_LENGTH);
                    CachedBodyOutputMessage cachedBodyOutputMessage = new CachedBodyOutputMessage(exchange,
                            httpHeaders);
                    return bodyInserter.insert(cachedBodyOutputMessage, new BodyInserterContext())
                            .then(Mono.<Void>defer(() -> shenyuPluginChain.execute(exchange.mutate()
                                    .request(new ModifyServerHttpRequestDecorator(httpHeaders, exchange.getRequest(),
                                            cachedBodyOutputMessage))
                                    .build())))
                            .onErrorResume(
                                    (Function<Throwable, Mono<Void>>) throwable -> release(cachedBodyOutputMessage,
                                            throwable));
                });
    }

    @Override
    public void operation(final DocumentContext context, final ParamMappingRuleHandle paramMappingRuleHandle) {
        if (!CollectionUtils.isEmpty(paramMappingRuleHandle.getAddParameterKeys())) {
            paramMappingRuleHandle.getAddParameterKeys().forEach(
                    info -> context.put(info.getPath(), info.getKey(), Collections.singletonList(info.getValue())));
        }
    }

    /**
     * To linked multiValue map.
     *
     * @param json the json
     * @return the linked multiValue map
     */
    public LinkedMultiValueMap<String, String> toLinkedMultiValueMap(final String json) {
        return FORM_DATA_GSON.fromJson(json, new TypeToken<LinkedMultiValueMap<String, String>>() {
        }.getType());
    }

    static class ModifyServerHttpRequestDecorator extends ServerHttpRequestDecorator {

        private final HttpHeaders headers;

        private final CachedBodyOutputMessage cachedBodyOutputMessage;

        ModifyServerHttpRequestDecorator(final HttpHeaders headers,
                final ServerHttpRequest delegate,
                final CachedBodyOutputMessage cachedBodyOutputMessage) {
            super(delegate);
            this.headers = headers;
            this.cachedBodyOutputMessage = cachedBodyOutputMessage;
        }

        @SuppressWarnings("NullableProblems")
        @Override
        public HttpHeaders getHeaders() {
            long contentLength = headers.getContentLength();
            if (contentLength == 0) {
                headers.set(HttpHeaders.TRANSFER_ENCODING, "chunked");
            }
            return headers;
        }

        @SuppressWarnings("NullableProblems")
        @Override
        public Flux<DataBuffer> getBody() {
            return cachedBodyOutputMessage.getBody();
        }
    }
}
