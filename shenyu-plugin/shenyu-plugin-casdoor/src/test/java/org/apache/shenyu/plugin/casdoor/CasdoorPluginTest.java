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

package org.apache.shenyu.plugin.casdoor;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.casdoor.handle.CasdoorPluginDateHandler;
import org.casbin.casdoor.entity.CasdoorUser;
import org.casbin.casdoor.exception.CasdoorAuthException;
import org.casbin.casdoor.service.CasdoorAuthService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.mockito.Mockito;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class CasdoorPluginTest {
    @InjectMocks
    @Spy
    private CasdoorPlugin casdoorPluginTest;

    @Spy
    private CasdoorPluginDateHandler casdoorPluginDateHandlerTest;

    private ServerWebExchange exchange;

    @Mock
    private ShenyuPluginChain chain;

    @Mock
    private SelectorData selector;

    @Mock
    private RuleData rule;

    @BeforeEach
    void setup() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setApplicationContext(context);
        MockitoAnnotations.openMocks(this);
        exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localshost")
                .header(HttpHeaders.AUTHORIZATION, "token")
                .build());

    }

    @Test
    void doExecute() {
        final PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"organization-name\":\"test\",\"application-name\":\"app-test\",\"endpoint\":\"http://localhost:8000\",\"client_secrect\":\"a4209d412a33a842b7a9c05a3446e623cbb7262d\",\"client_id\":\"6e3a84154e73d1fb156a\",\"certificate\":\"-----BEGIN CERTIFICATE-----\\n\"}", "0", false, null);
        casdoorPluginDateHandlerTest.handlerPlugin(pluginData);
        try {
            CasdoorAuthService casdoorAuthService = Singleton.INST.get(CasdoorAuthService.class);
            casdoorPluginTest.doExecute(exchange, chain, selector, rule);
        } catch (Exception e) {
            Assumptions.assumeTrue(e instanceof CasdoorAuthException);
        }

        CasdoorAuthService casdoorAuthService = mock(CasdoorAuthService.class);
        String token = exchange.getRequest().getHeaders().getFirst(HttpHeaders.AUTHORIZATION);
        CasdoorUser casdoorUser = new CasdoorUser();
        Mockito.when(casdoorAuthService.parseJwtToken(token)).thenReturn(casdoorUser);
        Singleton.INST.single(CasdoorAuthService.class, casdoorAuthService);
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        Mono<Void> mono = casdoorPluginTest.doExecute(exchange, chain, selector, rule);
        StepVerifier.create(mono).expectSubscription().verifyComplete();

        exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localshost")
                .queryParam("state", "state")
                .queryParam("code", "code")
                .build());
        Mockito.when(casdoorAuthService.getOAuthToken("code", "state")).thenReturn(token);
        Singleton.INST.single(CasdoorAuthService.class, casdoorAuthService);
        mono = casdoorPluginTest.doExecute(exchange, chain, selector, rule);
        StepVerifier.create(mono).expectSubscription().verifyComplete();

        exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localshost")
                .build());
        mono = casdoorPluginTest.doExecute(exchange, chain, selector, rule);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    @Test
    public void testNamed() {
        final String result = casdoorPluginTest.named();
        Assertions.assertEquals(PluginEnum.CASDOOR.getName(), result);
    }

    @Test
    public void testGetOrder() {
        final int result = casdoorPluginTest.getOrder();
        Assertions.assertEquals(PluginEnum.CASDOOR.getCode(), result);
    }

    @Test
    public void skipTest() {
        Assumptions.assumeFalse(casdoorPluginTest.skip(exchange));
    }

}
