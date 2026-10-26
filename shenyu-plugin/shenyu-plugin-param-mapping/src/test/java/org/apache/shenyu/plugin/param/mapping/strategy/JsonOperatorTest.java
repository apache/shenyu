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
import com.jayway.jsonpath.JsonPath;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingRuleHandle;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.NullSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link JsonOperator}.
 */
@ExtendWith(MockitoExtension.class)
public class JsonOperatorTest {

    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private JsonOperator jsonOperator;

    private ParamMappingRuleHandle paramMappingRuleHandle;

    @BeforeEach
    public void setUp() {
        Set<String> remove = new HashSet<>();
        remove.add("$.age");
        ParamMappingRuleHandle.ParamMapInfo add = new ParamMappingRuleHandle.ParamMapInfo();
        add.setPath("$");
        add.setKey("webName");
        add.setValue("SHENYU");
        ParamMappingRuleHandle.ParamMapInfo replace = new ParamMappingRuleHandle.ParamMapInfo();
        replace.setPath("$");
        replace.setKey("name");
        replace.setValue("realName");
        this.paramMappingRuleHandle = new ParamMappingRuleHandle();
        this.paramMappingRuleHandle.setRemoveParameterKeys(remove);
        this.paramMappingRuleHandle.setAddParameterKeys(Collections.singletonList(add));
        this.paramMappingRuleHandle.setReplaceParameterKeys(Collections.singletonList(replace));
        this.jsonOperator = new JsonOperator(HandlerStrategies.builder().build().messageReaders());
        final String body = "{\"name\":\"shenyu\",\"age\":\"18\"}";
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.POST, "localhost")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE).body(body));
    }

    @Test
    public void testApply() {
        DocumentContext body = JsonPath.parse(applyAndReadBody(this.exchange, paramMappingRuleHandle));

        assertEquals("SHENYU", body.read("$.webName"));
        assertEquals("shenyu", body.read("$.realName"));
        assertEquals(2, body.<Map<String, Object>>read("$").size());
    }

    @ParameterizedTest
    @NullSource
    @EmptySource
    public void testEmptyBodyWithoutAdditionsPassesThrough(final List<ParamMappingRuleHandle.ParamMapInfo> additions) {
        ServerWebExchange jsonExchange = MockServerWebExchange.from(MockServerHttpRequest.post("/json")
                .contentType(MediaType.APPLICATION_JSON).build());
        paramMappingRuleHandle.setAddParameterKeys(additions);
        when(this.chain.execute(any())).thenReturn(Mono.empty());

        StepVerifier.create(jsonOperator.apply(jsonExchange, this.chain, paramMappingRuleHandle)).verifyComplete();

        verify(this.chain).execute(jsonExchange);
    }

    @Test
    public void testEmptyStringBodyWithoutAdditionsPassesThrough() {
        ServerWebExchange jsonExchange = MockServerWebExchange.from(MockServerHttpRequest.post("/json")
                .contentType(MediaType.APPLICATION_JSON).body(""));
        paramMappingRuleHandle.setAddParameterKeys(Collections.emptyList());

        assertEquals("", applyAndReadBody(jsonExchange, paramMappingRuleHandle));
        verify(this.chain).execute(jsonExchange);
    }

    @Test
    public void testAddParameterToEmptyBody() {
        ServerWebExchange jsonExchange = MockServerWebExchange.from(MockServerHttpRequest.post("/json")
                .contentType(MediaType.APPLICATION_JSON).build());
        ParamMappingRuleHandle rule = new ParamMappingRuleHandle();
        rule.setAddParameterKeys(paramMappingRuleHandle.getAddParameterKeys());

        assertEquals("{\"webName\":\"SHENYU\"}", applyAndReadBody(jsonExchange, rule));
    }

    private String applyAndReadBody(final ServerWebExchange jsonExchange, final ParamMappingRuleHandle rule) {
        AtomicReference<String> body = new AtomicReference<>("");
        when(this.chain.execute(any())).thenAnswer(invocation -> {
            ServerWebExchange downstream = invocation.getArgument(0);
            return DataBufferUtils.join(downstream.getRequest().getBody()).doOnNext(buffer -> {
                try {
                    body.set(buffer.toString(StandardCharsets.UTF_8));
                } finally {
                    DataBufferUtils.release(buffer);
                }
            }).then();
        });
        StepVerifier.create(jsonOperator.apply(jsonExchange, this.chain, rule)).verifyComplete();
        return body.get();
    }
}

