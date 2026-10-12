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
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OpenAiChatTest {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private final OpenAiChat protocol = new OpenAiChat();

    @Test
    void testDecodePreservesRawPayloadAndExtractsNormalizedFields() throws Exception {
        final String body = "{\"model\":\"gpt-4o\",\"messages\":[{\"role\":\"user\","
                + "\"content\":\"hello\",\"custom\":true}],\"stream\":true,"
                + "\"max_completion_tokens\":128,\"unknown\":\"kept\"}";
        final ObjectNode payload = (ObjectNode) OBJECT_MAPPER.readTree(body);

        final ShenyuAiRequest request = protocol.decodeRequest(body, payload);

        assertEquals(OpenAiChat.NAME, request.getProtocol());
        assertEquals("gpt-4o", request.getModel());
        assertTrue(request.isStream());
        assertTrue(request.hasStream());
        assertEquals(128, request.getMaxTokens());
        assertEquals("hello", request.getMessages().get(0).getContent());
        assertTrue(request.getMessages().get(0).getRawPayload().get("custom").booleanValue());
        assertEquals("kept", request.getRawPayload().get("unknown").asText());
        assertEquals(body, request.getRawBody());
    }

    @Test
    void testDecodeKeepsMissingStreamDistinctFromFalse() throws Exception {
        final String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";
        final ShenyuAiRequest request = protocol.decodeRequest(
                body, (ObjectNode) OBJECT_MAPPER.readTree(body));

        assertFalse(request.isStream());
        assertFalse(request.hasStream());
    }

    @Test
    void testDecodeRejectsInvalidMessage() throws Exception {
        final String body = "{\"messages\":[{\"content\":\"hello\"}]}";
        final ObjectNode payload = (ObjectNode) OBJECT_MAPPER.readTree(body);

        assertThrows(ShenyuException.class, () -> protocol.decodeRequest(body, payload));
    }
}
