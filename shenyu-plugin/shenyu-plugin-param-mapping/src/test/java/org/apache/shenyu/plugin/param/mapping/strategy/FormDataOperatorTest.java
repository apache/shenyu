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

import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingRuleHandle;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URLEncoder;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


/**
 * Test case for {@link FormDataOperator}.
 */
@ExtendWith(MockitoExtension.class)
public class FormDataOperatorTest {

    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private FormDataOperator formDataOperator;

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
        this.formDataOperator = new FormDataOperator();
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.POST, "localhost")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_FORM_URLENCODED_VALUE).body("name=shenyu&age=18"));
    }

    @Test
    public void testApply() {
        String body = applyAndReadBody(this.exchange, paramMappingRuleHandle);
        assertTrue(body.contains("webName=SHENYU"));
        assertTrue(body.contains("realName=shenyu"));
        assertFalse(body.contains("age="));
    }

    @ParameterizedTest
    @ValueSource(strings = {"yes", "中文 &+"})
    public void testAddParameterToEmptyForm(final String value) {
        ServerWebExchange formExchange = MockServerWebExchange.from(MockServerHttpRequest.post("/form")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED).body(""));

        String body = applyAndReadBody(formExchange, additionRule(value));
        assertEquals("injected=" + URLEncoder.encode(value, StandardCharsets.UTF_8), body);
        ArgumentCaptor<ServerWebExchange> downstream = ArgumentCaptor.forClass(ServerWebExchange.class);
        verify(this.chain).execute(downstream.capture());
        assertEquals(body.getBytes(StandardCharsets.UTF_8).length, downstream.getValue().getRequest().getHeaders().getContentLength());
    }

    @Test
    public void testAddParameterToNonEmptyForm() {
        ServerWebExchange formExchange = MockServerWebExchange.from(MockServerHttpRequest.post("/form")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED).body("a=1"));

        assertEquals("a=1&injected=yes", applyAndReadBody(formExchange, additionRule("yes")));
    }

    @ParameterizedTest
    @NullSource
    @EmptySource
    public void testEmptyFormWithoutAdditionsPassesThrough(final List<ParamMappingRuleHandle.ParamMapInfo> additions) {
        ServerWebExchange formExchange = MockServerWebExchange.from(MockServerHttpRequest.post("/form")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED).body(""));
        paramMappingRuleHandle.setAddParameterKeys(additions);
        when(this.chain.execute(any())).thenReturn(Mono.empty());

        StepVerifier.create(formDataOperator.apply(formExchange, this.chain, paramMappingRuleHandle)).verifyComplete();

        verify(this.chain).execute(formExchange);
    }

    @ParameterizedTest
    @CsvSource({
        "flag,flag&injected=yes",
        "flag&name=x,flag&name=x&injected=yes",
        "flag=,flag=&injected=yes",
        "flag&flag=&flag=value,flag&flag=&flag=value&injected=yes",
        "encoded%20key&name=hello+world,encoded+key&name=hello+world&injected=yes"
    })
    public void testPreserveValuelessAndEmptyFormParameters(final String originalBody, final String expectedBody) {
        ServerWebExchange formExchange = MockServerWebExchange.from(MockServerHttpRequest.post("/form")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED).body(originalBody));

        String body = applyAndReadBody(formExchange, additionRule("yes"));

        assertEquals(expectedBody, body);
        ArgumentCaptor<ServerWebExchange> downstream = ArgumentCaptor.forClass(ServerWebExchange.class);
        verify(this.chain).execute(downstream.capture());
        assertEquals(body.getBytes(StandardCharsets.UTF_8).length, downstream.getValue().getRequest().getHeaders().getContentLength());
    }

    @ParameterizedTest
    @ValueSource(strings = {"UTF-8", "ISO-8859-1", "UTF-16LE"})
    public void testFormEncodingAndLengthUseTheConfiguredCharset(final String charsetName) {
        Charset charset = Charset.forName(charsetName);
        String originalBody = URLEncoder.encode("clé", charset) + "=" + URLEncoder.encode("été &+", charset) + "&flag";
        ServerWebExchange formExchange = MockServerWebExchange.from(MockServerHttpRequest.post("/form")
                .contentType(new MediaType("application", "x-www-form-urlencoded", charset))
                .body(Flux.just(new DefaultDataBufferFactory().wrap(originalBody.getBytes(charset)))));

        String body = applyAndReadBody(formExchange, additionRule("yes"), charset);

        assertEquals(originalBody + "&injected=yes", body);
        ArgumentCaptor<ServerWebExchange> downstream = ArgumentCaptor.forClass(ServerWebExchange.class);
        verify(this.chain).execute(downstream.capture());
        assertEquals(charset, downstream.getValue().getRequest().getHeaders().getContentType().getCharset());
        assertEquals(body.getBytes(charset).length, downstream.getValue().getRequest().getHeaders().getContentLength());
    }

    @Test
    public void testAddValuelessParameter() {
        ServerWebExchange formExchange = MockServerWebExchange.from(MockServerHttpRequest.post("/form")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED).body("a=1"));

        assertEquals("a=1&injected", applyAndReadBody(formExchange, additionRule(null)));
    }

    private ParamMappingRuleHandle additionRule(final String value) {
        ParamMappingRuleHandle.ParamMapInfo add = new ParamMappingRuleHandle.ParamMapInfo();
        add.setPath("$");
        add.setKey("injected");
        add.setValue(value);
        ParamMappingRuleHandle rule = new ParamMappingRuleHandle();
        rule.setAddParameterKeys(Collections.singletonList(add));
        return rule;
    }

    private String applyAndReadBody(final ServerWebExchange formExchange, final ParamMappingRuleHandle rule) {
        return applyAndReadBody(formExchange, rule, StandardCharsets.UTF_8);
    }

    private String applyAndReadBody(final ServerWebExchange formExchange, final ParamMappingRuleHandle rule, final Charset charset) {
        AtomicReference<String> body = new AtomicReference<>("");
        when(this.chain.execute(any())).thenAnswer(invocation -> {
            ServerWebExchange downstream = invocation.getArgument(0);
            return DataBufferUtils.join(downstream.getRequest().getBody()).doOnNext(buffer -> {
                try {
                    body.set(buffer.toString(charset));
                } finally {
                    DataBufferUtils.release(buffer);
                }
            }).then();
        });
        StepVerifier.create(formDataOperator.apply(formExchange, this.chain, rule)).verifyComplete();
        return body.get();
    }
}
