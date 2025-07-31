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

package org.apache.shenyu.plugin.ai.transformer.request;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.AiRequestTransformerConfig;
import org.apache.shenyu.common.dto.convert.rule.AiRequestTransformerHandle;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.request.cache.ChatClientCache;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class AiRequestTransformerPluginTest {

    private AiModelFactoryRegistry aiModelFactoryRegistry;

    private ChatClientCache chatClientCache;

    private ShenyuPluginChain chain;

    private AiRequestTransformerPlugin plugin;

    @BeforeEach
    void setUp() {

        aiModelFactoryRegistry = mock(AiModelFactoryRegistry.class);
        chatClientCache = mock(ChatClientCache.class);
        chain = mock(ShenyuPluginChain.class);
        plugin = new AiRequestTransformerPlugin(Collections.emptyList(), aiModelFactoryRegistry);
    }

    @Test
    void testDoExecuteWithMissingConfigurations() {

        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").build());
        SelectorData selector = new SelectorData();
        RuleData rule = new RuleData();

        when(chain.execute(exchange)).thenReturn(Mono.empty());

        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule))
                .verifyComplete();

        verify(chain).execute(exchange);
    }

    @Test
    void testDoExecuteWithValidConfigurations() {

        AiRequestTransformerConfig config = new AiRequestTransformerConfig();
        config.setBaseUrl("http://test.com");
        config.setApiKey("test-api-key");
        config.setProvider("TEST_PROVIDER");
        config.setModel("test-model");

        AiRequestTransformerHandle handle = new AiRequestTransformerHandle();
        handle.setProvider("TEST_PROVIDER");
        handle.setBaseUrl("http://test.com");
        handle.setApiKey("test-api-key");
        handle.setModel("test-model");

        ChatClient mockClient = mock(ChatClient.class);
        ChatModel mockModel = mock(ChatModel.class);
        AiModelFactory mockFactory = mock(AiModelFactory.class);

        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"key\":\"value\"}"));
        when(aiModelFactoryRegistry.getFactory(AiModelProviderEnum.getByName("TEST_PROVIDER"))).thenReturn(mockFactory);
        when(mockFactory.createAiModel(any())).thenReturn(mockModel);
        when(chatClientCache.getClient("default")).thenReturn(mockClient);
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        SelectorData selector = new SelectorData();
        RuleData rule = new RuleData();
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule))
                .verifyComplete();

        verify(chain).execute(exchange);
    }

    @Test
    void testConvertBodyJson() {

        String aiResponse = "HTTP/1.1 200 OK\nContent-Type: application/json\n\n{\"key\":\"value\"}";
        String result = AiRequestTransformerPlugin.convertBodyJson(aiResponse);
        assertEquals("{\"key\":\"value\"}", result);
    }

    @Test
    void testExtractHeadersFromAiResponse() {

        String aiResponse = "HTTP/1.1 200 OK\nContent-Type: application/json\nAuthorization: Bearer token\n\n{\"key\":\"value\"}";
        HttpHeaders headers = AiRequestTransformerPlugin.extractHeadersFromAiResponse(aiResponse);
        assertEquals("application/json", headers.getFirst(HttpHeaders.CONTENT_TYPE));
        assertEquals("Bearer token", headers.getFirst(HttpHeaders.AUTHORIZATION));
    }
}
