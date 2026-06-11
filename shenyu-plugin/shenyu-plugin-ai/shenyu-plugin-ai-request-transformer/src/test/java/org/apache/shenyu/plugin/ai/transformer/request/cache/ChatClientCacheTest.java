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

package org.apache.shenyu.plugin.ai.transformer.request.cache;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.ai.openai.OpenAiChatModel;
import org.springframework.ai.openai.api.OpenAiApi;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

class ChatClientCacheTest {

    private ChatClientCache chatClientCache;

    @BeforeEach
    void setUp() {

        chatClientCache = ChatClientCache.getInstance();
    }

    @Test
    void testGetInstance() {

        ChatClientCache instance1 = ChatClientCache.getInstance();
        ChatClientCache instance2 = ChatClientCache.getInstance();
        assertNotNull(instance1, "Instance should not be null");
        assertSame(instance1, instance2, "Instances should be the same (singleton)");
    }

    @Test
    void testInitAndGetClient() {

        ChatModel chatModel = OpenAiChatModel
                .builder()
                .openAiApi(
                        OpenAiApi.builder()
                                .apiKey("test-ak")
                                .build()
                ).build();

        String ruleId = "testRuleId";
        ChatClient chatClient = chatClientCache.init(ruleId, chatModel);
        assertNotNull(chatClient, "ChatClient should not be null");

        ChatClient retrievedClient = chatClientCache.getClient(ruleId);
        assertNotNull(retrievedClient, "Retrieved ChatClient should not be null");
    }

    @Test
    void testDestroyClient() {

        String ruleId = "testRuleId";
        ChatModel chatModel = mock(ChatModel.class);

        chatClientCache.init(ruleId, chatModel);
        assertNotNull(chatClientCache.getClient(ruleId), "ChatClient should exist before destruction");

        chatClientCache.destroyClient(ruleId);
        assertNull(chatClientCache.getClient(ruleId), "ChatClient should be null after destruction");
    }

}
