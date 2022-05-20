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

package org.apache.shenyu.plugin.motan;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.motan.proxy.MotanProxyService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.web.server.ServerWebExchange;

import static org.mockito.Mockito.mock;
import reactor.test.StepVerifier;

/**
 * The Test Case For MotanPlugin.
 */
public class MotanPluginTest {

    private MotanPlugin motanPlugin;

    private RuleData ruleData;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    private ServerWebExchange exchange;

    @Mock
    private MotanProxyService motanProxyService;

    @BeforeEach
    public void setUp() {
        this.motanPlugin = new MotanPlugin(motanProxyService);
        this.ruleData = mock(RuleData.class);
        this.chain = mock(ShenyuPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        //this.exchange =
    }

    @Test
    public void testDoExecute() {
        //StepVerifier.create(motanPlugin.doExecute(exchange, chain, selectorData, ruleData)).expectSubscription().verifyComplete();
    }

    @Test
    public void testSkip() {

    }

    @Test
    public void testGetOrder() {

    }

    @Test
    public void testCheckMetaData() {

    }
}
