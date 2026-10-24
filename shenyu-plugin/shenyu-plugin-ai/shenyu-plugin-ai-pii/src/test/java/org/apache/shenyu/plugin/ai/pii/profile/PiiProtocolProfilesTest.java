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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link PiiProtocolProfiles}, {@link OpenAiChatProfile} and
 * {@link OpenAiResponsesProfile}.
 */
public final class PiiProtocolProfilesTest {

    @Test
    public void testSelectByShape() {
        assertEquals(OpenAiChatProfile.ID,
                PiiProtocolProfiles.select(parse("{\"messages\":[]}")).id());
        assertEquals(OpenAiResponsesProfile.ID,
                PiiProtocolProfiles.select(parse("{\"input\":\"hi\"}")).id());
        assertNull(PiiProtocolProfiles.select(parse("{\"tools\":[]}")));
        assertNull(PiiProtocolProfiles.select(null));
    }

    @Test
    public void testChatRequestSlotsAndRedactionPreserveTree() {
        JsonObject root = parse("{\"model\":\"gpt-4\",\"messages\":["
                + "{\"role\":\"user\",\"content\":\"my number is 13800138000\"},"
                + "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"also a@b.com\"}]},"
                + "{\"role\":\"assistant\",\"content\":\"ok\"}],"
                + "\"tools\":[{\"function\":{\"description\":\"untouched\"}}]}");
        List<PiiTextSlot> slots = new OpenAiChatProfile().requestSlots(root);
        assertEquals(3, slots.size());
        slots.get(0).replace("my number is <PHONE_CN>");
        slots.get(1).replace("also <EMAIL>");
        // every field but the two message texts is untouched, including escaped content
        assertEquals("gpt-4", root.get("model").getAsString());
        assertEquals("untouched",
                root.get("tools").getAsJsonArray().get(0).getAsJsonObject()
                        .get("function").getAsJsonObject().get("description").getAsString());
        assertEquals("my number is <PHONE_CN>",
                root.get("messages").getAsJsonArray().get(0).getAsJsonObject().get("content").getAsString());
        assertEquals("also <EMAIL>",
                root.get("messages").getAsJsonArray().get(1).getAsJsonObject()
                        .get("content").getAsJsonArray().get(0).getAsJsonObject().get("text").getAsString());
        // the redacted tree is still a valid JSON object
        JsonElement reparsed = JsonParser.parseString(root.toString());
        assertEquals(root, reparsed);
    }

    @Test
    public void testChatResponseSlots() {
        JsonObject root = parse("{\"id\":\"x\",\"choices\":[{\"message\":"
                + "{\"role\":\"assistant\",\"content\":\"dial 13800138000\"},\"finish_reason\":\"stop\"}]}");
        List<PiiTextSlot> slots = new OpenAiChatProfile().responseSlots(root);
        assertEquals(1, slots.size());
        assertEquals("dial 13800138000", slots.get(0).text());
    }

    @Test
    public void testResponsesRequestSlots() {
        JsonObject root = parse("{\"model\":\"gpt-4\",\"input\":["
                + "{\"type\":\"message\",\"role\":\"user\",\"content\":\"reach me at a@b.com\"},"
                + "{\"type\":\"message\",\"role\":\"user\",\"content\":"
                + "[{\"type\":\"input_text\",\"text\":\"or 13800138000\"}]}"
                + "],\"metadata\":\"untouched\"}");
        List<PiiTextSlot> slots = new OpenAiResponsesProfile().requestSlots(root);
        assertEquals(2, slots.size());
        slots.get(0).replace("reach me at <EMAIL>");
        slots.get(1).replace("or <PHONE_CN>");
        assertEquals("untouched", root.get("metadata").getAsString());
    }

    @Test
    public void testResponsesPlainStringInput() {
        JsonObject root = parse("{\"input\":\"my number is 13800138000\"}");
        List<PiiTextSlot> slots = new OpenAiResponsesProfile().requestSlots(root);
        assertEquals(1, slots.size());
        slots.get(0).replace("my number is <PHONE_CN>");
        assertEquals("my number is <PHONE_CN>", root.get("input").getAsString());
    }

    @Test
    public void testResponsesResponseSlots() {
        JsonObject root = parse("{\"output\":[{\"type\":\"message\",\"content\":"
                + "[{\"type\":\"output_text\",\"text\":\"card 4111111111111111\"}]}]}");
        List<PiiTextSlot> slots = new OpenAiResponsesProfile().responseSlots(root);
        assertEquals(1, slots.size());
        assertEquals("card 4111111111111111", slots.get(0).text());
    }

    @Test
    public void testEscapedTextIsScannedAfterParse() {
        // the wire form escapes the quotes, the slot sees the parsed text
        JsonObject root = parse("{\"messages\":[{\"content\":\"line1\\nline2 13800138000\"}]}");
        List<PiiTextSlot> slots = new OpenAiChatProfile().requestSlots(root);
        assertEquals(1, slots.size());
        assertTrue(slots.get(0).text().contains("\n"));
    }

    private JsonObject parse(final String json) {
        return JsonParser.parseString(json).getAsJsonObject();
    }
}
