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

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Responses shape detection and gateway envelope validation.
 */
class OpenAiResponsesTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private final OpenAiResponses protocol = new OpenAiResponses();

    @Test
    void detectsResponsesWithoutAmbiguousChatRequests() throws Exception {
        assertEquals(OpenAiResponses.NAME, protocol.name());
        assertFalse(protocol.matches(null));
        assertFalse(protocol.matches(MAPPER.readTree("[]")));
        assertFalse(protocol.matches(MAPPER.readTree("{}")));
        assertFalse(protocol.matches(MAPPER.readTree("{\"input\":\"hi\",\"messages\":[]}")));
        assertTrue(protocol.matches(MAPPER.readTree("{\"input\":\"hi\"}")));
        assertEquals(protocol, new AiProxyProtocolFactory(List.of(new OpenAiChat(), protocol)).detect(MAPPER.readTree("{\"input\":\"hi\"}")));
    }

    @Test
    void retainsTheOriginalPayloadAndOptionalFields() throws Exception {
        String body = "{\"input\":[],\"model\":\"alias\",\"stream\":true,\"max_output_tokens\":100,\"unknown\":true}";
        ObjectNode payload = (ObjectNode) MAPPER.readTree(body);
        ShenyuAiRequest request = protocol.decodeRequest(body, payload);
        payload.remove("unknown");
        assertTrue(request.getRawPayload().has("unknown"));
        assertEquals(body, request.getRawBody());
        assertEquals("alias", request.getModel());
        assertEquals(100, request.getMaxTokens());
        assertTrue(request.isStream());
        assertTrue(request.hasStream());
        for (String minimal : List.of("{\"input\":\"hi\"}", "{\"input\":\"hi\",\"model\":null,\"stream\":null,\"max_output_tokens\":null}")) {
            ShenyuAiRequest optional = protocol.decodeRequest(minimal, (ObjectNode) MAPPER.readTree(minimal));
            assertNull(optional.getModel());
            assertNull(optional.getMaxTokens());
            assertFalse(optional.hasStream());
        }
    }

    @Test
    void rejectsInvalidEnvelopeFields() {
        for (String body : List.of("{}", "{\"input\":null}", "{\"input\":1}", "{\"input\":{}}", "{\"input\":\"hi\",\"stream\":\"yes\"}",
                "{\"input\":\"hi\",\"model\":42}", "{\"input\":\"hi\",\"max_output_tokens\":0}", "{\"input\":\"hi\",\"max_output_tokens\":1.5}",
                "{\"input\":\"hi\",\"max_output_tokens\":99999999999}")) {
            assertThrows(ShenyuException.class, () -> protocol.decodeRequest(body, (ObjectNode) MAPPER.readTree(body)));
        }
    }
}
