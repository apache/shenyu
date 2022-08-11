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

package org.apache.shenyu.web.filter;

import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.http.HttpHeaders.CONTENT_TYPE;
import static org.springframework.http.MediaType.MULTIPART_FORM_DATA;

/**
 * test case for FileSizeFilter.
 */
public final class FileSizeFilterTest {

    @BeforeEach
    public void setup() {
        GenericApplicationContext context = new GenericApplicationContext();
        SpringBeanUtils.getInstance().setApplicationContext(context);
        context.getBeanFactory().registerSingleton("shenyuResult", mock(ShenyuResult.class));
        context.refresh();
    }

    @Test
    public void testFilter() {
        ServerWebExchange webExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080")
                        .contentType(MediaType.TEXT_PLAIN)
                        .contentLength(4)
                        .body("test"));

        ServerHttpRequest mutatedRequest = webExchange.getRequest().mutate().header(CONTENT_TYPE,
                String.valueOf(MULTIPART_FORM_DATA)).build();
        webExchange = webExchange.mutate().request(mutatedRequest).build();

        WebFilterChain webFilterChain = mock(WebFilterChain.class);
        when(webFilterChain.filter(any())).thenReturn(Mono.empty());

        FileSizeFilter fileSizeFilter = new FileSizeFilter(10);
        Mono<Void> voidMono = fileSizeFilter.filter(webExchange, webFilterChain);
        StepVerifier.create(voidMono).expectSubscription().verifyComplete();

        final ServerWebExchange webExchangeTextPlain =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080")
                        .contentType(MediaType.TEXT_PLAIN)
                        .contentLength(4)
                        .body("test"));
        FileSizeFilter fileSizeFilterTextPlain = new FileSizeFilter(10);
        Mono<Void> voidMonoTextPlain = fileSizeFilterTextPlain.filter(webExchangeTextPlain, webFilterChain);
        StepVerifier.create(voidMonoTextPlain).expectSubscription().verifyComplete();

        fileSizeFilter = new FileSizeFilter(1);
        voidMono = fileSizeFilter.filter(webExchange, webFilterChain);
        StepVerifier.create(voidMono).expectSubscription().verifyComplete();

        final ServerWebExchange webExchangeError =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8081")
                        .contentType(MediaType.MULTIPART_FORM_DATA)
                        .body("test"));
        // hit `size.capacity() > BYTES_PER_MB * fileMaxSize`
        FileSizeFilter fileSizeFilterError = new FileSizeFilter(-1);
        Mono<Void> voidMonoError = fileSizeFilterError.filter(webExchangeError, webFilterChain);
        StepVerifier.create(voidMonoError).expectSubscription().verifyComplete();
    }

    @Test
    public void testDecorate() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {

        ServerWebExchange webExchangeTextPlain =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080")
                        .contentType(MediaType.TEXT_PLAIN)
                        .contentLength(4)
                        .body("test"));
        FileSizeFilter fileSizeFilterError = new FileSizeFilter(1);
        Method declaredMethod = FileSizeFilter.class.getDeclaredMethod("decorate", ServerWebExchange.class, CachedBodyOutputMessage.class);
        CachedBodyOutputMessage cachedBodyOutputMessage = mock(CachedBodyOutputMessage.class);
        declaredMethod.setAccessible(true);
        ServerHttpRequestDecorator decorator = (ServerHttpRequestDecorator) declaredMethod.invoke(fileSizeFilterError, webExchangeTextPlain, cachedBodyOutputMessage);
        Assertions.assertEquals(decorator.getBody(), cachedBodyOutputMessage.getBody());
    }
}
