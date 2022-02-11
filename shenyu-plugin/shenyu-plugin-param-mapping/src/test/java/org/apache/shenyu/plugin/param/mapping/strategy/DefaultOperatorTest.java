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
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;


/**
 * Test case for {@link DefaultOperator}.
 */
@ExtendWith(MockitoExtension.class)
public class DefaultOperatorTest {

    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private DefaultOperator defaultOperator;

    @BeforeEach
    public void setUp() {
        this.defaultOperator = new DefaultOperator();
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .build());
    }

    @Test
    public void testApply() {
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        StepVerifier.create(defaultOperator.apply(this.exchange, this.chain, new ParamMappingRuleHandle())).expectSubscription().verifyComplete();
    }
}
