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

package org.apache.shenyu.plugin.ai.common.view;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.ai.AiNormalizedRequestView;
import org.apache.shenyu.plugin.ai.common.protocol.AiClientProtocolShape;
import org.junit.jupiter.api.Test;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Anthropic Messages and Gemini shapes of the bounded request view (issue #8458).
 */
class AiRequestViewParserTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void anthropicBodySurfacesSystemAsLeadingMessageAndTextBlocksAsText() {
        AiNormalizedRequestView view = parse("{\"model\":\"claude\",\"system\":\"be careful\","
                + "\"messages\":[{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"hello there\"}]}]}");
        assertEquals(AiClientProtocolShape.ANTHROPIC_MESSAGES, view.protocol());
        assertEquals("MESSAGES", view.operation());
        assertEquals("claude", view.model());
        assertEquals(2, view.messages().size());
        assertEquals("system", view.messages().get(0).role());
        assertEquals(AiNormalizedRequestView.ContentKind.TEXT, view.messages().get(0).contentKind());
        assertEquals("be careful", view.messages().get(0).textPrefix());
        assertEquals("user", view.messages().get(1).role());
        assertEquals(AiNormalizedRequestView.ContentKind.TEXT, view.messages().get(1).contentKind());
        assertEquals("hello there", view.messages().get(1).textPrefix());
        assertFalse(view.unsupported());
    }

    @Test
    void anthropicToolUseBlocksSurfaceAsToolCalls() {
        AiNormalizedRequestView view = parse("{\"model\":\"claude\",\"system\":\"be careful\","
                + "\"messages\":[{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"t1\","
                + "\"name\":\"lookup\",\"input\":{\"city\":\"paris\"}}]}]}");
        assertTrue(view.toolCallsPresent());
        assertEquals(AiNormalizedRequestView.ContentKind.STRUCTURED, view.messages().get(1).contentKind());
    }

    @Test
    void geminiBodySurfacesSystemInstructionAndPartText() {
        AiNormalizedRequestView view = parse("{\"systemInstruction\":{\"parts\":[{\"text\":\"be careful\"}]},"
                + "\"contents\":[{\"role\":\"user\",\"parts\":[{\"text\":\"hello there\"},{\"text\":\" again\"}]}],"
                + "\"tools\":[{\"function_declarations\":[{\"name\":\"f\"}]}]}");
        assertEquals(AiClientProtocolShape.GEMINI_GENERATE_CONTENT, view.protocol());
        assertEquals("GENERATE_CONTENT", view.operation());
        assertNull(view.model());
        assertEquals(2, view.messages().size());
        assertEquals("system", view.messages().get(0).role());
        assertEquals("be careful", view.messages().get(0).textPrefix());
        assertEquals("user", view.messages().get(1).role());
        assertEquals("hello there again", view.messages().get(1).textPrefix());
        assertTrue(view.toolsPresent());
        assertFalse(view.unsupported());
    }

    @Test
    void geminiFunctionCallPartsSurfaceAsToolCalls() {
        AiNormalizedRequestView view = parse("{\"contents\":[{\"role\":\"model\","
                + "\"parts\":[{\"functionCall\":{\"name\":\"f\",\"args\":{\"a\":1}}}]}]}");
        assertTrue(view.toolCallsPresent());
    }

    @Test
    void anthropicBodyWithoutSystemClassifiesAsChatShape() {
        AiNormalizedRequestView view = parse("{\"model\":\"claude\","
                + "\"messages\":[{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}]}]}");
        assertEquals("openai-chat", view.protocol());
        // the openai-chat readers still surface the block text gracefully
        assertEquals(AiNormalizedRequestView.ContentKind.STRUCTURED, view.messages().get(0).contentKind());
    }

    @Test
    void detectorClassifiesTheFourShapesAndUnknown() throws Exception {
        assertEquals(AiClientProtocolShape.Shape.ANTHROPIC_MESSAGES, detect("{\"system\":\"s\",\"messages\":[]}"));
        assertEquals(AiClientProtocolShape.Shape.OPENAI_CHAT, detect("{\"messages\":[]}"));
        assertEquals(AiClientProtocolShape.Shape.OPENAI_RESPONSES, detect("{\"input\":\"hi\"}"));
        assertEquals(AiClientProtocolShape.Shape.GEMINI_GENERATE_CONTENT, detect("{\"contents\":[]}"));
        assertEquals(AiClientProtocolShape.Shape.UNKNOWN, detect("{\"prompt\":\"hi\"}"));
        assertEquals(AiClientProtocolShape.Shape.UNKNOWN, detect(null));
    }

    private AiClientProtocolShape.Shape detect(final String body) throws Exception {
        return AiClientProtocolShape.detect(Objects.isNull(body) ? null : MAPPER.readTree(body));
    }

    private AiNormalizedRequestView parse(final String body) {
        return AiRequestViewParser.parse(body, AiNormalizedRequestView.ViewKind.WIRE, null, 1);
    }
}
