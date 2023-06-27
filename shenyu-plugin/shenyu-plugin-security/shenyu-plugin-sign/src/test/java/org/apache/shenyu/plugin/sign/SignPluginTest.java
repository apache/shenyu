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

package org.apache.shenyu.plugin.sign;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.sign.service.SignService;
import org.apache.shenyu.plugin.sign.api.VerifyResult;
import org.apache.shenyu.plugin.sign.handler.SignPluginDataHandler;
import org.apache.shenyu.plugin.sign.handler.SignRuleHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * SignPlugin test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class SignPluginTest {
    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private SignPlugin signPlugin;

    private RuleData ruleData;

    private SignService signService;

    private SignPluginDataHandler signPluginDataHandler;

    @BeforeEach
    public void setup() {

        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test-sign");
        this.ruleData.setName("test-sign-plugin");
        this.signPluginDataHandler = new SignPluginDataHandler();
        signService = mock(SignService.class);
        this.signPlugin = new SignPlugin(HandlerStrategies.builder().build().messageReaders(), signService);

        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        assertEquals(signPlugin.getOrder(), PluginEnum.SIGN.getCode());
        assertEquals(signPlugin.named(), PluginEnum.SIGN.getName());
        assertEquals(signPluginDataHandler.pluginNamed(), PluginEnum.SIGN.getName());

        SignRuleHandler signRuleHandler = new SignRuleHandler();
        signRuleHandler.setSignRequestBody(true);
        assertTrue(signRuleHandler.toString().contains("signRequestBody"));
        assertTrue(signRuleHandler.getSignRequestBody());
    }

    @Test
    public void testSignPluginSimple() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());

        when(signService.signatureVerify(exchange)).thenReturn(VerifyResult.success());
        RuleData data = mock(RuleData.class);
        SelectorData selectorData = mock(SelectorData.class);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(signPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
    }

    @Test
    public void testSignPluginSimple2() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());

        when(signService.signatureVerify(exchange)).thenReturn(VerifyResult.fail(""));
        RuleData data = mock(RuleData.class);
        SelectorData selectorData = mock(SelectorData.class);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(signPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
    }

    @Test
    public void testSignPluginSignBody() {
        this.ruleData.setHandle("{\"signRequestBody\": true}");
        String requestBody = "{\"data\": \"3\"}";
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body(requestBody));
        when(signService.signatureVerify(exchange, requestBody)).thenReturn(VerifyResult.success());
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        SelectorData selectorData = mock(SelectorData.class);
        signPluginDataHandler.handlerRule(ruleData);
        StepVerifier.create(signPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();

    }

    @Test
    public void testSignPluginSignBody2() {
        this.ruleData.setHandle("{\"signRequestBody\": true}");
        String requestBody = "{\"data\": \"4\"}";

        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test?data2=3")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body(requestBody));

        when(signService.signatureVerify(exchange, requestBody)).thenReturn(VerifyResult.fail(""));
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        SelectorData selectorData = mock(SelectorData.class);
        signPluginDataHandler.handlerRule(ruleData);
        StepVerifier.create(signPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();

    }

    @AfterEach
    public void clean() throws IOException {
        signPluginDataHandler.removeRule(this.ruleData);
    }

}
