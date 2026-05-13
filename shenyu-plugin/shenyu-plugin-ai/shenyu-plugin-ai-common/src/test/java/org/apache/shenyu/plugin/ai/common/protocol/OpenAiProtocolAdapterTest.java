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

package org.apache.shenyu.plugin.ai.common.protocol;

import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.junit.jupiter.api.Test;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionRequest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class OpenAiProtocolAdapterTest {

    private static final String BASE_BODY = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";

    @Test
    void testNullRequestBody() {
        assertThrows(IllegalArgumentException.class,
                () -> OpenAiProtocolAdapter.toChatCompletionRequest(null, false));
    }

    @Test
    void testEmptyRequestBody() {
        assertThrows(IllegalArgumentException.class,
                () -> OpenAiProtocolAdapter.toChatCompletionRequest("", false));
    }

    @Test
    void testStreamFlagSet() {
        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, true);
        assertNotNull(req);
        assertTrue(req.stream());
    }

    @Test
    void testResolveStreamClientTrueOverridesFallbackFalse() {
        final String body = "{\"model\":\"m\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"stream\":true}";
        assertTrue(OpenAiProtocolAdapter.resolveStream(body, false));
    }

    @Test
    void testResolveStreamClientFalseOverridesFallbackTrue() {
        final String body = "{\"model\":\"m\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"stream\":false}";
        assertFalse(OpenAiProtocolAdapter.resolveStream(body, true));
    }

    @Test
    void testResolveStreamFallbackWhenClientMissing() {
        assertTrue(OpenAiProtocolAdapter.resolveStream(BASE_BODY, true));
        assertFalse(OpenAiProtocolAdapter.resolveStream(BASE_BODY, false));
    }

    @Test
    void testResolveStreamFallbackWhenClientMissingAndConfigNull() {
        assertFalse(OpenAiProtocolAdapter.resolveStream(BASE_BODY, null));
    }

    @Test
    void testResolveStreamFallbackWhenBodyEmpty() {
        assertTrue(OpenAiProtocolAdapter.resolveStream("", true));
        assertFalse(OpenAiProtocolAdapter.resolveStream("", false));
    }

    @Test
    void testStreamFlagUnset() {
        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, false);
        assertNotNull(req);
        assertFalse(req.stream());
    }

    @Test
    void testMaxCompletionTokensConvertedToMaxTokens() {
        final String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_completion_tokens\":100}";
        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(body, false);
        assertNotNull(req);
        assertEquals(100, req.maxTokens());
    }

    @Test
    void testClientModelTakesPriority() {
        final String body = "{\"model\":\"client-model\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}";
        final AiCommonConfig config = new AiCommonConfig();
        config.setModel("admin-model");

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(body, false, config);
        assertEquals("client-model", req.model());
    }

    @Test
    void testFallbackModelWhenClientMissing() {
        final AiCommonConfig config = new AiCommonConfig();
        config.setModel("admin-model");

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, false, config);
        assertEquals("admin-model", req.model());
    }

    @Test
    void testNoFallbackModelWhenConfigIsNull() {
        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, false, null);
        assertNotNull(req);
    }

    @Test
    void testNoFallbackWhenConfigModelIsNull() {
        final AiCommonConfig config = new AiCommonConfig();
        config.setModel(null);

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, false, config);
        assertNotNull(req);
    }

    @Test
    void testNoFallbackWhenConfigModelIsEmpty() {
        final AiCommonConfig config = new AiCommonConfig();
        config.setModel("");

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, false, config);
        assertNotNull(req);
    }

    @Test
    void testClientTemperatureTakesPriority() {
        final String body = "{\"model\":\"m\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"temperature\":0.5}";
        final AiCommonConfig config = new AiCommonConfig();
        config.setTemperature(0.9);

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(body, false, config);
        assertEquals(0.5, req.temperature(), 0.001);
    }

    @Test
    void testFallbackTemperatureWhenClientMissing() {
        final AiCommonConfig config = new AiCommonConfig();
        config.setTemperature(0.7);

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, false, config);
        assertEquals(0.7, req.temperature(), 0.001);
    }

    @Test
    void testNoFallbackTemperatureWhenConfigNull() {
        final AiCommonConfig config = new AiCommonConfig();
        config.setTemperature(null);

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, false, config);
        assertNotNull(req);
    }

    @Test
    void testClientMaxTokensTakesPriority() {
        final String body = "{\"model\":\"m\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_tokens\":200}";
        final AiCommonConfig config = new AiCommonConfig();
        config.setMaxTokens(500);

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(body, false, config);
        assertEquals(200, req.maxTokens());
    }

    @Test
    void testFallbackMaxTokensWhenClientMissing() {
        final AiCommonConfig config = new AiCommonConfig();
        config.setMaxTokens(500);

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, false, config);
        assertEquals(500, req.maxTokens());
    }

    @Test
    void testNoFallbackMaxTokensWhenConfigNull() {
        final AiCommonConfig config = new AiCommonConfig();
        config.setMaxTokens(null);

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, false, config);
        assertNotNull(req);
    }

    @Test
    void testAllFallbackFieldsAppliedWhenClientMissingAll() {
        final AiCommonConfig config = new AiCommonConfig();
        config.setModel("admin-model");
        config.setTemperature(0.3);
        config.setMaxTokens(1024);

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(BASE_BODY, true, config);
        assertEquals("admin-model", req.model());
        assertEquals(0.3, req.temperature(), 0.001);
        assertEquals(1024, req.maxTokens());
        assertTrue(req.stream());
    }

    @Test
    void testAllClientFieldsTakePriority() {
        final String body = "{\"model\":\"client-model\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],"
                + "\"temperature\":0.1,\"max_tokens\":50}";
        final AiCommonConfig config = new AiCommonConfig();
        config.setModel("admin-model");
        config.setTemperature(0.9);
        config.setMaxTokens(9999);

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(body, false, config);
        assertEquals("client-model", req.model());
        assertEquals(0.1, req.temperature(), 0.001);
        assertEquals(50, req.maxTokens());
    }

    @Test
    void testPartialClientFieldsWithPartialFallback() {
        final String body = "{\"model\":\"client-model\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"temperature\":0.1}";
        final AiCommonConfig config = new AiCommonConfig();
        config.setModel("admin-model");
        config.setTemperature(0.9);
        config.setMaxTokens(2048);

        final ChatCompletionRequest req = OpenAiProtocolAdapter.toChatCompletionRequest(body, false, config);
        assertEquals("client-model", req.model());
        assertEquals(0.1, req.temperature(), 0.001);
        assertEquals(2048, req.maxTokens());
    }
}
