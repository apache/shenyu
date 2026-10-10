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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

class OpenAiTest {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private final OpenAi provider = new OpenAi();

    private AiCommonConfig config;

    @BeforeEach
    void setUp() {
        config = new AiCommonConfig();
        config.setBaseUrl("https://api.openai.com/");
        config.setApiKey("test-key");
    }

    @Test
    void testBuildRequestPreservesExplicitStreamUsageSetting() throws Exception {
        final String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}],"
                + "\"stream\":true,\"stream_options\":{\"include_usage\":false}}";
        final ShenyuAiRequest request = decode(body);

        final AiUpstreamRequest upstreamRequest = provider.buildRequest(request, config);
        final JsonNode payload = OBJECT_MAPPER.readTree(upstreamRequest.getBody());

        assertEquals("https://api.openai.com/v1/chat/completions", upstreamRequest.getUri().toString());
        assertEquals("text/event-stream", upstreamRequest.getHeaders().get("Accept").get(0));
        assertFalse(payload.get("stream_options").get("include_usage").booleanValue());
    }

    @Test
    void testBuildRequestValidatesConfigurationAndProtocol() throws Exception {
        final ShenyuAiRequest request = decode("{\"messages\":[{\"role\":\"user\"}]}");
        config.setApiKey(null);
        assertThrows(IllegalArgumentException.class, () -> provider.buildRequest(request, config));

        config.setApiKey("test-key");
        request.setProtocol("unknown");
        assertThrows(IllegalArgumentException.class, () -> provider.buildRequest(request, config));
    }

    @Test
    void testBuildRequestRejectsInvalidStreamOptions() throws Exception {
        final String body = "{\"messages\":[{\"role\":\"user\"}],\"stream\":true,"
                + "\"stream_options\":false}";

        assertThrows(ShenyuException.class, () -> provider.buildRequest(decode(body), config));
    }

    private ShenyuAiRequest decode(final String body) throws Exception {
        return new OpenAiChat().decodeRequest(body, (ObjectNode) OBJECT_MAPPER.readTree(
                body.getBytes(StandardCharsets.UTF_8)));
    }
}
