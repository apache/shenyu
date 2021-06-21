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
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingHandle;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.support.BodyInserterContext;
import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
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
import java.util.List;
import java.util.Map;
import java.util.Arrays;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * ApplicationFormStrategy.
 */
@Slf4j
public class ApplicationFormOperator implements Operator {

    @Override
    public Mono<Void> apply(final ServerWebExchange exchange, final ShenyuPluginChain shenyuPluginChain, final ParamMappingHandle paramMappingHandle) {
        return exchange.getFormData()
                .switchIfEmpty(Mono.defer(() -> Mono.just(new LinkedMultiValueMap<>())))
                .flatMap(multiValueMap -> {
                    if (Objects.isNull(multiValueMap) || multiValueMap.isEmpty()) {
                        return shenyuPluginChain.execute(exchange);
                    }
                    String original = GsonUtils.getInstance().toJson(multiValueMap);
                    log.info("get from data success data:{}", original);
                    String modify = operation(original, paramMappingHandle);
                    if (StringUtils.isEmpty(modify)) {
                        return shenyuPluginChain.execute(exchange);
                    }
                    HttpHeaders headers = exchange.getRequest().getHeaders();
                    HttpHeaders httpHeaders = new HttpHeaders();
                    Charset charset = headers.getContentType().getCharset();
                    charset = charset == null ? StandardCharsets.UTF_8 : charset;
                    LinkedMultiValueMap<String, String> modifyMap = GsonUtils.getInstance().toLinkedMultiValueMap(modify);
                    List<String> list = new ArrayList<>();
                    for (Map.Entry<String, List<String>> entry : modifyMap.entrySet()) {
                        for (String value : entry.getValue()) {
                            try {
                                list.add(entry.getKey() + "=" + URLEncoder.encode(value, charset.name()));
                            } catch (UnsupportedEncodingException e) {
                                return Mono.error(new ShenyuException(e));
                            }
                        }
                    }
                    String content = list.stream().collect(Collectors.joining("&"));
                    byte[] bodyBytes = content.getBytes(charset);
                    int contentLength = bodyBytes.length;
                    final BodyInserter bodyInserter = BodyInserters.fromValue(modifyMap);
                    httpHeaders.putAll(exchange.getRequest().getHeaders());
                    httpHeaders.remove(HttpHeaders.CONTENT_LENGTH);
                    httpHeaders.setContentLength(contentLength);
                    CachedBodyOutputMessage cachedBodyOutputMessage = new CachedBodyOutputMessage(exchange, httpHeaders);
                    return bodyInserter.insert(cachedBodyOutputMessage, new BodyInserterContext())
                            .then(Mono.defer(() -> {
                                ServerHttpRequestDecorator decorator = new ServerHttpRequestDecorator(exchange.getRequest()) {
                                    @Override
                                    public HttpHeaders getHeaders() {
                                        long contentLength = httpHeaders.getContentLength();
                                        if (contentLength == 0) {
                                            httpHeaders.set(HttpHeaders.TRANSFER_ENCODING, "chunked");
                                        }
                                        return httpHeaders;
                                    }

                                    @Override
                                    public Flux<DataBuffer> getBody() {
                                        return cachedBodyOutputMessage.getBody();
                                    }
                                };
                                return shenyuPluginChain.execute(exchange.mutate().request(decorator).build());
                            })).onErrorResume((Function<Throwable, Mono<Void>>) throwable -> release(cachedBodyOutputMessage, throwable));
                });
    }

    @Override
    public void operation(final DocumentContext context, final ParamMappingHandle paramMappingHandle) {
        if (!CollectionUtils.isEmpty(paramMappingHandle.getAddParameterKeys())) {
            paramMappingHandle.getAddParameterKeys().forEach(info -> {
                context.put(info.getPath(), info.getKey(), Arrays.asList(info.getValue()));
            });
        }
    }
}
