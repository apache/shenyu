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

package org.apache.shenyu.plugin.ai.proxy.enhanced.protocol;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiProviderOptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AnthropicMessagesTest {

    private final ObjectMapper json = new ObjectMapper();

    private final AnthropicMessages protocol = new AnthropicMessages();

    @Test
    void nativeProfileReusesHeadersEndpointAndDoesNotAddOpenAiUsageOptions() throws Exception {
        ObjectNode body = (ObjectNode) json.readTree("{\"model\":\"claude-fixture\",\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}],\"max_tokens\":20,\"stream\":true}");
        ObjectNode prepared = protocol.prepareOutgoingRequest(body, new AiProtocolRequestOverrides(null, null, false));
        assertFalse(prepared.has("stream_options"));
        AiCommonConfig config = new AiCommonConfig();
        config.setBaseUrl("https://example.com");
        config.setApiKey("fixture-secret");
        OpenAiCompatible provider = new OpenAiCompatible("anthropic", OpenAiProviderOptions.anthropicMessages());
        AiUpstreamRequest request = provider.buildRequest(protocol.decodeRequest(prepared.toString(), prepared), config);
        assertEquals("https://example.com/v1/messages", request.getUri().toString());
        assertEquals(List.of("fixture-secret"), request.getHeaders().get("x-api-key"));
        assertEquals(List.of("2023-06-01"), request.getHeaders().get("anthropic-version"));
        assertFalse(request.getHeaders().containsKey("Authorization"));
        assertEquals(body, json.readTree(request.getBody()));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "{\"messages\":[]}", "{\"messages\":[{\"role\":\"system\",\"content\":\"x\"}]}",
        "{\"messages\":[{\"role\":\"user\",\"content\":null}]}",
        "{\"messages\":[{\"role\":\"user\",\"content\":\"x\"}],\"max_tokens\":0}",
        "{\"messages\":[{\"role\":\"user\",\"content\":\"x\"}],\"stream\":\"true\"}"
    })
    void malformedNativeRequestsFailBeforeTransport(final String source) throws Exception {
        ObjectNode body = (ObjectNode) json.readTree(source);
        assertThrows(RuntimeException.class, () -> protocol.decodeRequest(source, body));
    }

    @Test
    void requiredOutputBoundIsValidatedAfterGatewayOverrides() throws Exception {
        ObjectNode body = (ObjectNode) json.readTree("{\"model\":\"claude-fixture\",\"messages\":[{\"role\":\"user\",\"content\":\"x\"}]}");
        protocol.decodeRequest(body.toString(), body);
        assertThrows(RuntimeException.class, () -> protocol.prepareOutgoingRequest(body.deepCopy(), new AiProtocolRequestOverrides(null, null, false)));
        assertEquals(32, protocol.prepareOutgoingRequest(body.deepCopy(), new AiProtocolRequestOverrides(null, 32, false)).path("max_tokens").intValue());
    }

    @Test
    void ambiguousMessagesEnvelopeNeverChangesExistingChatDetection() throws Exception {
        AiProxyProtocolFactory protocols = new AiProxyProtocolFactory(List.of(protocol, new OpenAiChat()));
        ObjectNode body = (ObjectNode) json.readTree("{\"messages\":[{\"role\":\"user\",\"content\":\"x\"}],\"max_tokens\":32}");
        assertEquals("openai-chat", protocols.detect(body).name());
        assertEquals(protocol, protocols.getProtocol("anthropic-messages"));
    }
}
