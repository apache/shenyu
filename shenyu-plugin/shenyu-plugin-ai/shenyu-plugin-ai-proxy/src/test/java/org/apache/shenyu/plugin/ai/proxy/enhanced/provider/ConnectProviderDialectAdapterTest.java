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

package org.apache.shenyu.plugin.ai.proxy.enhanced.provider;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.github.agenticai.connect.ProviderProfile;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests provider-specific Chat dialect adaptation independently of protocol conversion.
 */
class ConnectProviderDialectAdapterTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void appliesProviderProfileExactlyOnceWithoutMutatingSource() throws Exception {
        AtomicInteger calls = new AtomicInteger();
        ProviderProfile profile = (payload, adaptation) -> {
            calls.incrementAndGet();
            assertEquals("routed", payload.get("model").getAsString());
            payload.addProperty("custom", true);
            adaptation.warning("custom_profile", "custom", "Test profile");
        };
        ObjectNode input = object("{\"model\":\"alias\",\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}");
        ObjectNode before = input.deepCopy();

        ObjectNode result = new ConnectProviderDialectAdapter("routed", profile).adapt(input);

        assertEquals(1, calls.get());
        assertEquals(before, input);
        assertEquals("routed", result.path("model").asText());
        assertTrue(result.path("custom").asBoolean());
    }

    @Test
    void keepsGenericChatPayloadUnchanged() throws Exception {
        ObjectNode input = object("{\"messages\":[{\"role\":\"user\"}],\"vendor\":true,\"content\":null}");

        assertEquals(input, new ConnectProviderDialectAdapter(null, null).adapt(input));
    }

    @Test
    void adaptsDeepSeekTokensAndReasoning() throws Exception {
        ObjectNode input = object("{\"model\":\"deepseek-model\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],"
                + "\"max_completion_tokens\":20,\"max_tokens\":10,\"reasoning_effort\":\"medium\"}");
        ObjectNode result = new ConnectProviderDialectAdapter(null, ProviderProfile.deepSeekChat()).adapt(input);

        assertEquals(20, result.path("max_tokens").asInt());
        assertFalse(result.has("max_completion_tokens"));
        assertEquals("high", result.path("reasoning_effort").asText());
        assertEquals("enabled", result.at("/thinking/type").asText());

        input.put("tool_choice", "required");
        assertThrows(AiProxyException.class,
                () -> new ConnectProviderDialectAdapter(null, ProviderProfile.deepSeekChat()).adapt(input));
    }

    private ObjectNode object(final String json) throws Exception {
        return (ObjectNode) MAPPER.readTree(json);
    }
}
