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

package org.apache.shenyu.plugin.jwt;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.JwtRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.jwt.handle.JwtPluginDataHandler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link JwtPlugin}.
 */
public final class JwtPluginTest {

    private JwtPlugin jwtPluginUnderTest;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    private RuleData ruleData;

    private JwtRuleHandle jwtRuleHandle;

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setApplicationContext(context);
        PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"secretKey\":\"shenyu-test-shenyu-test-shenyu-test\"}", "0", false);
        JwtPluginDataHandler jwtPluginDataHandler = new JwtPluginDataHandler();
        jwtPluginDataHandler.handlerPlugin(pluginData);
        selectorData = mock(SelectorData.class);
        ruleData = new RuleData();
        jwtPluginUnderTest = new JwtPlugin();
        // HMAC-SHA algorithms MUST have a size >= 256 bits
        final String secreteKey = "shenyu-test-shenyu-test-shenyu-test";
        Map<String, Object> map = new HashMap<>();
        map.put("userId", 1);
        Map<String, Object> multi = new HashMap<>();
        multi.put("shenyu", "1.2.3");
        map.put("web", multi);
        Date date = new Date();
        date.setTime(1636371125000L);
        String token = Jwts.builder()
                .setIssuedAt(date)
                .setExpiration(new Date())
                .setClaims(map)
                .signWith(Keys.hmacShaKeyFor(secreteKey.getBytes(StandardCharsets.UTF_8)), SignatureAlgorithm.HS256)
                .compact();
        jwtRuleHandle = new JwtRuleHandle();

        exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header("token", token)
                .build());
        chain = mock(ShenyuPluginChain.class);
    }

    @Test
    public void testSecreteKey() {
        PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"secretKey\":\"\"}", "0", false);
        JwtPluginDataHandler jwtPluginDataHandler = new JwtPluginDataHandler();
        jwtPluginDataHandler.handlerPlugin(pluginData);
        Mono<Void> mono = jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    @Test
    public void testConverter() {
        List<JwtRuleHandle.Convert> converts = new ArrayList<>();
        JwtRuleHandle.Convert convert = new JwtRuleHandle.Convert();
        convert.setJwtVal("userId");
        convert.setHeaderVal("userId");
        converts.add(convert);
        jwtRuleHandle.setConverter(converts);
        ruleData.setHandle(GsonUtils.getGson().toJson(jwtRuleHandle));
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        Mono<Void> mono = jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    @Test
    public void testMulti() {
        final List<JwtRuleHandle.Convert> converts = new ArrayList<>();
        JwtRuleHandle.Convert webConvert = new JwtRuleHandle.Convert();
        webConvert.setJwtVal("web.shenyu");
        webConvert.setHeaderVal("shenyu");
        JwtRuleHandle.Convert userConvert = new JwtRuleHandle.Convert();
        userConvert.setJwtVal("userId");
        userConvert.setHeaderVal("userId");
        converts.add(userConvert);
        converts.add(webConvert);
        jwtRuleHandle.setConverter(converts);
        ruleData.setHandle(GsonUtils.getGson().toJson(jwtRuleHandle));
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        Mono<Void> mono = jwtPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    @Test
    public void testNamed() {
        final String result = jwtPluginUnderTest.named();
        Assertions.assertEquals(PluginEnum.JWT.getName(), result);
    }

    @Test
    public void testGetOrder() {
        final int result = jwtPluginUnderTest.getOrder();
        Assertions.assertEquals(PluginEnum.JWT.getCode(), result);
    }

}
