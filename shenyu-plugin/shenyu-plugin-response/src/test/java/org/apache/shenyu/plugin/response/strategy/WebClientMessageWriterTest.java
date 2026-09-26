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

package org.apache.shenyu.plugin.response.strategy;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.HeaderUniqueStrategyEnum;
import org.apache.shenyu.common.enums.UniqueHeaderEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.http.codec.support.DefaultServerCodecConfigurer;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.when;

/**
 * The test case for {@link WebClientMessageWriter}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class WebClientMessageWriterTest {

    private ShenyuPluginChain chain;

    private WebClientMessageWriter webClientMessageWriter;

    @BeforeEach
    public void setup() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
        when(context.getBean(ServerCodecConfigurer.class)).thenReturn(new DefaultServerCodecConfigurer());
        chain = mock(ShenyuPluginChain.class);
        webClientMessageWriter = new WebClientMessageWriter();
    }

    @Test
    public void testWriteWith() {
        ServerWebExchange exchangeNormal = generateServerWebExchange(true);
        exchangeNormal.getResponse().setStatusCode(HttpStatus.OK);
        reset(chain);
        when(chain.execute(exchangeNormal)).thenReturn(Mono.empty());
        Mono<Void> monoSuccess = webClientMessageWriter.writeWith(exchangeNormal, chain);
        StepVerifier.create(monoSuccess).expectSubscription().verifyComplete();

        ServerWebExchange exchangeNullResponse = generateServerWebExchange(false);
        reset(chain);
        when(chain.execute(exchangeNullResponse)).thenReturn(Mono.empty());
        Mono<Void> monoNullResponse = webClientMessageWriter.writeWith(exchangeNullResponse, chain);
        StepVerifier.create(monoNullResponse).expectSubscription().verifyComplete();

        ServerWebExchange exchangeInternalServerError = generateServerWebExchange(true);
        exchangeInternalServerError.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        reset(chain);
        when(chain.execute(exchangeInternalServerError)).thenReturn(Mono.empty());
        Mono<Void> monoInternalServerError = webClientMessageWriter.writeWith(exchangeInternalServerError, chain);
        StepVerifier.create(monoInternalServerError).expectSubscription().verifyComplete();

        ServerWebExchange exchangeBadGateway = generateServerWebExchange(true);
        exchangeBadGateway.getResponse().setStatusCode(HttpStatus.BAD_GATEWAY);
        reset(chain);
        when(chain.execute(exchangeBadGateway)).thenReturn(Mono.empty());
        Mono<Void> monoBadGateway = webClientMessageWriter.writeWith(exchangeBadGateway, chain);
        StepVerifier.create(monoBadGateway).expectSubscription().verifyComplete();

        ServerWebExchange exchangeGatewayTimeout = generateServerWebExchange(true);
        exchangeGatewayTimeout.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
        reset(chain);
        when(chain.execute(exchangeGatewayTimeout)).thenReturn(Mono.empty());
        Mono<Void> monoGatewayTimeout = webClientMessageWriter.writeWith(exchangeGatewayTimeout, chain);
        StepVerifier.create(monoGatewayTimeout).expectSubscription().verifyComplete();
    }

    @ParameterizedTest
    @EnumSource(HeaderUniqueStrategyEnum.class)
    public void testFinalResponseRetainsDeduplication(final HeaderUniqueStrategyEnum strategy) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/headers").build());
        exchange.getResponse().getHeaders().set("X-Duplicate", "already-deduplicated");
        exchange.getResponse().getHeaders().setAccessControlAllowOrigin("https://allowed.example");
        HttpHeaders upstream = new HttpHeaders();
        upstream.put("X-Duplicate", List.of("first", "last", "last"));
        upstream.put("X-Unconfigured", List.of("one", "two"));
        upstream.setAccessControlAllowOrigin("*");
        ResponseEntity<Flux<DataBuffer>> response = new ResponseEntity<>(Flux.empty(), upstream, HttpStatus.OK);
        exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, response);
        exchange.getAttributes().put(UniqueHeaderEnum.RESP_UNIQUE_HEADER.getName(), "X-Duplicate;X-Missing");
        exchange.getAttributes().put(UniqueHeaderEnum.RESP_UNIQUE_HEADER.getStrategy(), strategy);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(webClientMessageWriter.writeWith(exchange, chain)).verifyComplete();
        List<String> expected = strategy == HeaderUniqueStrategyEnum.RETAIN_UNIQUE
                ? List.of("first", "last") : List.of(strategy == HeaderUniqueStrategyEnum.RETAIN_FIRST ? "first" : "last");
        assertEquals(expected, exchange.getResponse().getHeaders().get("X-Duplicate"));
        assertEquals(List.of("one", "two"), exchange.getResponse().getHeaders().get("X-Unconfigured"));
        assertEquals("https://allowed.example", exchange.getResponse().getHeaders().getAccessControlAllowOrigin());
        assertEquals(List.of("first", "last", "last"), response.getHeaders().get("X-Duplicate"));
    }

    private ServerWebExchange generateServerWebExchange(final boolean haveResponse) {
        ResponseEntity mockResponse = mock(ResponseEntity.class);
        when(mockResponse.getHeaders()).thenReturn(mock(HttpHeaders.class));
        when(mockResponse.getBody()).thenReturn(Mono.empty());

        ServerWebExchange exchange = MockServerWebExchange
                .from(MockServerHttpRequest.get("/test").build());

        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.HTTP_URI, "/test");
        if (haveResponse) {
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, mockResponse);
        }
        return exchange;
    }
}
