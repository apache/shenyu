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

import com.jayway.jsonpath.DocumentContext;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingRuleHandle;
import org.apache.shenyu.common.exception.ShenyuException;
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

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

/**
 * ApplicationFormStrategy.
 */
public class FormDataOperator implements Operator {

    private static final Logger LOG = LoggerFactory.getLogger(FormDataOperator.class);

    @Override
    public Mono<Void> apply(final ServerWebExchange exchange, final ShenyuPluginChain shenyuPluginChain, final ParamMappingRuleHandle paramMappingRuleHandle) {
        return exchange.getFormData()
                .switchIfEmpty(Mono.defer(() -> Mono.just(new LinkedMultiValueMap<>())))
                .flatMap(multiValueMap -> {
                    if (Objects.isNull(multiValueMap) || multiValueMap.isEmpty()) {
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
                    Charset charset = Objects.requireNonNull(headers.getContentType()).getCharset();
                    charset = charset == null ? StandardCharsets.UTF_8 : charset;
                    LinkedMultiValueMap<String, String> modifyMap = GsonUtils.getInstance().toLinkedMultiValueMap(modify);
                    List<String> list = prepareParams(modifyMap, charset.name());
                    String content = String.join("&", list);
                    byte[] bodyBytes = content.getBytes(charset);
                    int contentLength = bodyBytes.length;
                    final BodyInserter<LinkedMultiValueMap<String, String>, ReactiveHttpOutputMessage> bodyInserter = BodyInserters.fromValue(modifyMap);
                    httpHeaders.putAll(headers);
                    httpHeaders.remove(HttpHeaders.CONTENT_LENGTH);
                    httpHeaders.setContentLength(contentLength);
                    CachedBodyOutputMessage cachedBodyOutputMessage = new CachedBodyOutputMessage(exchange, httpHeaders);
                    return bodyInserter.insert(cachedBodyOutputMessage, new BodyInserterContext())
                            .then(Mono.defer(() -> shenyuPluginChain.execute(exchange.mutate()
                                    .request(new ModifyServerHttpRequestDecorator(httpHeaders, exchange.getRequest(), cachedBodyOutputMessage))
                                    .build())
                            )).onErrorResume((Function<Throwable, Mono<Void>>) throwable -> release(cachedBodyOutputMessage, throwable));
                });
    }

    @Override
    public void operation(final DocumentContext context, final ParamMappingRuleHandle paramMappingRuleHandle) {
        if (!CollectionUtils.isEmpty(paramMappingRuleHandle.getAddParameterKeys())) {
            paramMappingRuleHandle.getAddParameterKeys().forEach(info -> context.put(info.getPath(), info.getKey(), Collections.singletonList(info.getValue())));
        }
    }

    private List<String> prepareParams(final LinkedMultiValueMap<String, String> modifyMap, final String charset) {
        List<String> paramList = new ArrayList<>();
        modifyMap.forEach((K, V) -> V.forEach(value -> {
            try {
                paramList.add(String.join("=", K, URLEncoder.encode(value, charset)));
            } catch (UnsupportedEncodingException e) {
                throw new ShenyuException(e);
            }
        }));
        return paramList;
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
