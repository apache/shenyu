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

package org.apache.shenyu.plugin.ai.pii.profile;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Anthropic Messages and Gemini profiles of the PII extraction (issue #8458):
 * selection order, slot coverage (system, text blocks, tool inputs) and
 * in-place redaction that preserves the rest of the tree.
 */
public final class PiiMultiProtocolProfilesTest {

    @Test
    public void testAnthropicSelectionOrderBeatsGenericMessagesShape() {
        assertEquals(AnthropicMessagesProfile.ID,
                PiiProtocolProfiles.select(parse("{\"system\":\"s\",\"messages\":[]}")).id());
        assertEquals(OpenAiChatProfile.ID,
                PiiProtocolProfiles.select(parse("{\"messages\":[]}")).id());
        // anthropic response: no choices, block content at the root
        assertEquals(AnthropicMessagesProfile.ID,
                PiiProtocolProfiles.selectResponse(parse("{\"content\":[{\"type\":\"text\",\"text\":\"hi\"}],\"stop_reason\":\"end_turn\"}")).id());
    }

    @Test
    public void testGeminiSelection() {
        assertEquals(GeminiGenerateContentProfile.ID,
                PiiProtocolProfiles.select(parse("{\"contents\":[]}")).id());
        assertEquals(GeminiGenerateContentProfile.ID,
                PiiProtocolProfiles.selectResponse(parse("{\"candidates\":[{\"content\":{\"parts\":[]}}]}")).id());
    }

    @Test
    public void testAnthropicRequestSlotsCoverSystemBlocksAndToolInput() {
        JsonObject root = parse("{\"system\":\"my id is 13800138000\","
                + "\"messages\":["
                + "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"mail me a@b.com\"}]},"
                + "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"t1\",\"name\":\"lookup\","
                + "\"input\":{\"query\":\"token 13800138000\",\"nested\":{\"note\":\"secret 555\"},\"count\":3}}]}"
                + "]}");
        List<PiiTextSlot> slots = new AnthropicMessagesProfile().requestSlots(root);
        assertEquals(4, slots.size());
        slots.get(0).replace("my id is <PHONE_CN>");
        slots.get(1).replace("mail me <EMAIL>");
        slots.get(2).replace("token <PHONE_CN>");
        slots.get(3).replace("secret <ID>");
        assertEquals("my id is <PHONE_CN>", root.get("system").getAsString());
        assertEquals("mail me <EMAIL>",
                root.get("messages").getAsJsonArray().get(0).getAsJsonObject()
                        .get("content").getAsJsonArray().get(0).getAsJsonObject().get("text").getAsString());
        JsonObject input = root.get("messages").getAsJsonArray().get(1).getAsJsonObject()
                .get("content").getAsJsonArray().get(0).getAsJsonObject().get("input").getAsJsonObject();
        assertEquals("token <PHONE_CN>", input.get("query").getAsString());
        assertEquals("secret <ID>", input.get("nested").getAsJsonObject().get("note").getAsString());
        assertEquals(3, input.get("count").getAsInt());
        // the redacted tree is still a valid JSON object
        JsonElement reparsed = JsonParser.parseString(root.toString());
        assertEquals(root, reparsed);
    }

    @Test
    public void testAnthropicResponseSlots() {
        JsonObject root = parse("{\"content\":[{\"type\":\"text\",\"text\":\"dial 13800138000\"},"
                + "{\"type\":\"tool_use\",\"input\":{\"q\":\"secret 555\"}}],\"stop_reason\":\"tool_use\"}");
        List<PiiTextSlot> slots = new AnthropicMessagesProfile().responseSlots(root);
        assertEquals(2, slots.size());
        assertEquals("dial 13800138000", slots.get(0).text());
        assertEquals("secret 555", slots.get(1).text());
        slots.get(1).replace("secret <ID>");
        assertEquals("secret <ID>", root.get("content").getAsJsonArray().get(1).getAsJsonObject()
                .get("input").getAsJsonObject().get("q").getAsString());
    }

    @Test
    public void testGeminiRequestSlotsCoverSystemInstructionAndPartText() {
        JsonObject root = parse("{\"systemInstruction\":{\"parts\":[{\"text\":\"call 13800138000\"}]},"
                + "\"contents\":[{\"role\":\"user\",\"parts\":[{\"text\":\"mail a@b.com\"},"
                + "{\"inlineData\":{\"mimeType\":\"image/png\"}}]}]}");
        List<PiiTextSlot> slots = new GeminiGenerateContentProfile().requestSlots(root);
        assertEquals(2, slots.size());
        slots.get(0).replace("call <PHONE_CN>");
        slots.get(1).replace("mail <EMAIL>");
        assertEquals("call <PHONE_CN>",
                root.get("systemInstruction").getAsJsonObject().get("parts").getAsJsonArray().get(0).getAsJsonObject().get("text").getAsString());
        assertEquals("mail <EMAIL>",
                root.get("contents").getAsJsonArray().get(0).getAsJsonObject().get("parts").getAsJsonArray().get(0).getAsJsonObject().get("text").getAsString());
        assertNull(root.get("contents").getAsJsonArray().get(0).getAsJsonObject().get("parts").getAsJsonArray()
                .get(1).getAsJsonObject().get("text"));
    }

    @Test
    public void testGeminiResponseSlots() {
        JsonObject root = parse("{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"dial 13800138000\"}]}}]}");
        List<PiiTextSlot> slots = new GeminiGenerateContentProfile().responseSlots(root);
        assertEquals(1, slots.size());
        assertEquals("dial 13800138000", slots.get(0).text());
    }

    /**
     * A Gemini tool-call part carries structured {@code functionCall.args} instead of
     * text: its string leaves must be redacted like Anthropic tool_use.input instead of
     * passing through unmasked while the scan reports covered (#8458 review).
     */
    @Test
    public void testGeminiFunctionCallArgsAreSlots() {
        JsonObject root = parse("{\"contents\":[{\"role\":\"user\",\"parts\":["
                + "{\"functionCall\":{\"name\":\"addContact\",\"args\":{\"phone\":\"13800138000\","
                + "\"filters\":[{\"email\":\"a@b.com\"}],\"count\":2}}}]}]}");
        assertNotNull(PiiProtocolProfiles.select(root), "a functionCall part is a valid gemini shape");
        List<PiiTextSlot> slots = new GeminiGenerateContentProfile().requestSlots(root);
        assertEquals(2, slots.size());
        slots.get(0).replace("<PHONE_CN>");
        slots.get(1).replace("<EMAIL>");
        JsonObject args = root.get("contents").getAsJsonArray().get(0).getAsJsonObject()
                .get("parts").getAsJsonArray().get(0).getAsJsonObject()
                .get("functionCall").getAsJsonObject().get("args").getAsJsonObject();
        assertEquals("<PHONE_CN>", args.get("phone").getAsString());
        assertEquals("<EMAIL>", args.get("filters").getAsJsonArray().get(0).getAsJsonObject().get("email").getAsString());
        assertEquals(2, args.get("count").getAsInt());
        assertEquals("addContact", root.get("contents").getAsJsonArray().get(0).getAsJsonObject()
                .get("parts").getAsJsonArray().get(0).getAsJsonObject()
                .get("functionCall").getAsJsonObject().get("name").getAsString());
    }

    /**
     * tool_use.input holding an array of objects — the shape the old walker skipped —
     * must surface its string leaves as slots (#8458 review).
     */
    @Test
    public void testAnthropicToolUseArrayInputIsWalked() {
        JsonObject root = parse("{\"messages\":[{\"role\":\"assistant\",\"content\":["
                + "{\"type\":\"tool_use\",\"id\":\"t1\",\"name\":\"search\","
                + "\"input\":{\"filters\":[{\"email\":\"a@b.com\"}],\"tags\":[\"token 13800138000\"]}}]}] }");
        List<PiiTextSlot> slots = new AnthropicMessagesProfile().requestSlots(root);
        assertEquals(2, slots.size());
        slots.get(0).replace("<EMAIL>");
        slots.get(1).replace("token <PHONE_CN>");
        JsonObject input = root.get("messages").getAsJsonArray().get(0).getAsJsonObject()
                .get("content").getAsJsonArray().get(0).getAsJsonObject().get("input").getAsJsonObject();
        assertEquals("<EMAIL>", input.get("filters").getAsJsonArray().get(0).getAsJsonObject().get("email").getAsString());
        assertEquals("token <PHONE_CN>", input.get("tags").getAsJsonArray().get(0).getAsString());
    }

    /**
     * Extended-thinking prose is model- and user-authored content: a thinking block's
     * text member must be a slot on both shapes (#8458 review).
     */
    @Test
    public void testAnthropicThinkingBlockIsASlot() {
        JsonObject root = parse("{\"content\":[{\"type\":\"thinking\",\"thinking\":\"user mail is a@b.com\"},"
                + "{\"type\":\"text\",\"text\":\"done\"}]}");
        List<PiiTextSlot> slots = new AnthropicMessagesProfile().responseSlots(root);
        assertEquals(2, slots.size());
        slots.get(0).replace("user mail is <EMAIL>");
        assertEquals("user mail is <EMAIL>", root.get("content").getAsJsonArray().get(0).getAsJsonObject()
                .get("thinking").getAsString());
    }

    private JsonObject parse(final String body) {
        return JsonParser.parseString(body).getAsJsonObject();
    }
}
