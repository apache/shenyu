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

package org.apache.shenyu.plugin.ai.common.strategy.openai;

import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

class OpenAITest {

    private OpenAI openAI;

    private AiCommonConfig aiCommonConfig;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private List<HttpMessageReader<?>> messageReaders;

    @BeforeEach
    void setUp() {
        openAI = new OpenAI();
        aiCommonConfig = new AiCommonConfig();
        aiCommonConfig.setApiKey("test-api-key");
        aiCommonConfig.setModel("test-model");
        aiCommonConfig.setStream(true);

        exchange = mock(ServerWebExchange.class);
        chain = mock(ShenyuPluginChain.class);
        messageReaders = mock(List.class);
    }

    @Test
    void testGetCompletionTokensValidResponse() {

        String responseBody = "{\"usage\":{\"completion_tokens\":42}}";
        Long tokens = openAI.getCompletionTokens(responseBody);
        assertEquals(42L, tokens);
    }

    @Test
    void testGetCompletionTokensInvalidResponse() {

        String responseBody = "{\"invalid\":\"data\"}";
        Long tokens = openAI.getCompletionTokens(responseBody);
        assertEquals(0L, tokens);
    }

    @Test
    void testGetCompletionTokensEmptyResponse() {

        String responseBody = "";
        Long tokens = openAI.getCompletionTokens(responseBody);
        assertEquals(0L, tokens);
    }

}
