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

package org.apache.shenyu.plugin.ai.prompt;

import org.apache.shenyu.common.dto.convert.plugin.AiPromptConfig;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.codec.HttpMessageReader;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

class AiPromptPluginTest {

    private AiPromptPlugin plugin;

    private List<HttpMessageReader<?>> messageReaders;

    @BeforeEach
    void setUp() {
        messageReaders = List.of(mock(HttpMessageReader.class));
        plugin = new AiPromptPlugin(messageReaders);
    }

    @Test
    void testNamed() {

        assertEquals(PluginEnum.AI_PROMPT.getName(), plugin.named());
    }

    @Test
    void testGetOrder() {

        assertEquals(PluginEnum.AI_PROMPT.getCode(), plugin.getOrder());
    }

    @Test
    void testDecorateBodyReturnsOriginalWhenNoMessages() throws Exception {
        AiPromptConfig config = new AiPromptConfig();
        String body = "{\"model\":\"gpt-4\"}";
        String result = invokeDecorateBody(body, config);
        assertEquals(body, result);
    }

    @Test
    void testDecorateBodyReturnsOriginalWhenMessagesEmpty() throws Exception {
        AiPromptConfig config = new AiPromptConfig();
        String body = "{\"messages\":[]}";
        String result = invokeDecorateBody(body, config);
        assertEquals(body, result);
    }

    @Test
    void testDecorateBodyWithPrependAndAppend() throws Exception {
        AiPromptConfig config = new AiPromptConfig();
        config.setPrepend("system instruction");
        config.setPreRole("system");
        config.setAppend("post instruction");
        config.setPostRole("assistant");

        String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";
        String result = invokeDecorateBody(body, config);

        Map<String, Object> resultMap = GsonUtils.getInstance().convertToMap(result);
        List<Map<String, Object>> messages = (List<Map<String, Object>>) resultMap.get("messages");

        assertEquals(3, messages.size());
        assertEquals("system instruction", messages.get(0).get("content"));
        assertEquals("system", messages.get(0).get("role"));
        assertEquals("hello", messages.get(1).get("content"));
        assertEquals("user", messages.get(1).get("role"));
        assertEquals("post instruction", messages.get(2).get("content"));
        assertEquals("assistant", messages.get(2).get("role"));
    }

    @Test
    void testDecorateBodyWithOnlyPrepend() throws Exception {
        AiPromptConfig config = new AiPromptConfig();
        config.setPrepend("system instruction");
        config.setPreRole("system");

        String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";
        String result = invokeDecorateBody(body, config);

        Map<String, Object> resultMap = GsonUtils.getInstance().convertToMap(result);
        List<Map<String, Object>> messages = (List<Map<String, Object>>) resultMap.get("messages");

        assertEquals(2, messages.size());
        assertEquals("system instruction", messages.get(0).get("content"));
        assertEquals("hello", messages.get(1).get("content"));
    }

    @Test
    void testDecorateBodyPreservesAllOriginalMessages() throws Exception {
        AiPromptConfig config = new AiPromptConfig();
        config.setPrepend("system prompt");
        config.setPreRole("system");
        config.setAppend("footer");
        config.setPostRole("assistant");

        String body = "{\"messages\":["
                + "{\"role\":\"system\",\"content\":\"sys\"},"
                + "{\"role\":\"user\",\"content\":\"hi\"},"
                + "{\"role\":\"assistant\",\"content\":\"hello\"},"
                + "{\"role\":\"user\",\"content\":\"how are you\"}"
                + "]}";
        String result = invokeDecorateBody(body, config);

        Map<String, Object> resultMap = GsonUtils.getInstance().convertToMap(result);
        List<Map<String, Object>> messages = (List<Map<String, Object>>) resultMap.get("messages");

        assertEquals(6, messages.size());
        assertEquals("system prompt", messages.get(0).get("content"));
        assertEquals("sys", messages.get(1).get("content"));
        assertEquals("hi", messages.get(2).get("content"));
        assertEquals("hello", messages.get(3).get("content"));
        assertEquals("how are you", messages.get(4).get("content"));
        assertEquals("footer", messages.get(5).get("content"));
    }

    @Test
    void testDecorateBodyNoPrependOrAppend() throws Exception {
        AiPromptConfig config = new AiPromptConfig();

        String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";
        String result = invokeDecorateBody(body, config);

        Map<String, Object> resultMap = GsonUtils.getInstance().convertToMap(result);
        List<Map<String, Object>> messages = (List<Map<String, Object>>) resultMap.get("messages");

        assertEquals(1, messages.size());
        assertEquals("hello", messages.get(0).get("content"));
    }

    @Test
    void testDecorateBodyPrependWithoutRoleIgnored() throws Exception {
        AiPromptConfig config = new AiPromptConfig();
        config.setPrepend("system instruction");

        String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";
        String result = invokeDecorateBody(body, config);

        Map<String, Object> resultMap = GsonUtils.getInstance().convertToMap(result);
        List<Map<String, Object>> messages = (List<Map<String, Object>>) resultMap.get("messages");

        assertEquals(1, messages.size());
        assertEquals("hello", messages.get(0).get("content"));
    }

    @Test
    void testDecorateBodyPreservesOtherFields() throws Exception {
        AiPromptConfig config = new AiPromptConfig();
        config.setPrepend("prefix");
        config.setPreRole("system");

        String body = "{\"model\":\"gpt-4\",\"temperature\":0.7,\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}";
        String result = invokeDecorateBody(body, config);

        Map<String, Object> resultMap = GsonUtils.getInstance().convertToMap(result);
        assertEquals("gpt-4", resultMap.get("model"));
        assertEquals(0.7, ((Number) resultMap.get("temperature")).doubleValue(), 0.001);

        List<Map<String, Object>> messages = (List<Map<String, Object>>) resultMap.get("messages");
        assertEquals(2, messages.size());
    }

    @SuppressWarnings("unchecked")
    private String invokeDecorateBody(final String body, final AiPromptConfig config) throws Exception {
        Method method = AiPromptPlugin.class.getDeclaredMethod("decorateBody", String.class, AiPromptConfig.class);
        method.setAccessible(true);
        return (String) method.invoke(plugin, body, config);
    }
}
