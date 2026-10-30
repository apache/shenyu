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

import java.nio.charset.StandardCharsets;
import java.util.Base64;
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

    @Test
    public void testMalformedRequestExtractionShapesAreUnsupported() {
        String[] payloads = {
                "{\"messages\":\"bad\"}", "{\"messages\":null}", "{\"messages\":[42]}",
                "{\"messages\":[{}]}", "{\"messages\":[{\"content\":123}]}",
                "{\"messages\":[{\"content\":[\"bad\"]}]}",
                "{\"messages\":[{\"content\":[{\"type\":\"text\"}]}]}",
                "{\"messages\":[{\"content\":[{\"text\":false}]}]}",
                "{\"input\":123}", "{\"input\":null}", "{\"input\":[{}]}",
                "{\"input\":[{\"type\":\"message\",\"content\":{}}]}",
        };
        for (String payload : payloads) {
            assertNull(PiiProtocolProfiles.select(parse(payload)), payload);
        }
    }

    @Test
    public void testMalformedResponseExtractionShapesAreUnsupported() {
        String[] payloads = {
                "{\"choices\":{}}", "{\"choices\":null}", "{\"choices\":[{}]}",
                "{\"choices\":[{\"message\":\"bad\"}]}",
                "{\"choices\":[{\"message\":{\"content\":{}}}]}",
                "{\"output\":\"bad\"}", "{\"output\":[false]}",
                "{\"output\":[{\"type\":\"message\",\"content\":[{\"text\":23}]}]}",
        };
        for (String payload : payloads) {
            assertNull(PiiProtocolProfiles.selectResponse(parse(payload)), payload);
        }
    }

    @Test
    public void testValidTextlessToolCallsRemainSupported() {
        // a tool call without arguments carries no text and stays a supported, textless message
        JsonObject chat = parse("{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,"
                + "\"tool_calls\":[{\"type\":\"function\",\"function\":{\"name\":\"lookup\"}}]}}]}");
        assertEquals(OpenAiChatProfile.ID, PiiProtocolProfiles.selectResponse(chat).id());
        assertTrue(new OpenAiChatProfile().responseSlots(chat).isEmpty());
        assertEquals(OpenAiResponsesProfile.ID,
                PiiProtocolProfiles.select(parse("{\"input\":[{\"type\":\"function_call_output\",\"output\":\"ok\"}]}")).id());
        assertEquals(OpenAiChatProfile.ID, PiiProtocolProfiles.select(parse("{\"messages\":[{\"content\":[]}]}")).id());
    }

    @Test
    public void testChatToolCallArgumentsAreScanned() {
        JsonObject root = parse("{\"messages\":[{\"role\":\"assistant\",\"content\":null,"
                + "\"tool_calls\":[{\"type\":\"function\",\"function\":{\"name\":\"lookup\","
                + "\"arguments\":\"{\\\"phone\\\":\\\"13800138000\\\"}\"}}]}]}");
        assertEquals(OpenAiChatProfile.ID, PiiProtocolProfiles.select(root).id());
        List<PiiTextSlot> slots = new OpenAiChatProfile().requestSlots(root);
        assertEquals(1, slots.size());
        assertEquals("{\"phone\":\"13800138000\"}", slots.get(0).text());
        slots.get(0).replace("{\"phone\":\"<PHONE_CN>\"}");
        assertEquals("{\"phone\":\"<PHONE_CN>\"}",
                root.get("messages").getAsJsonArray().get(0).getAsJsonObject()
                        .get("tool_calls").getAsJsonArray().get(0).getAsJsonObject()
                        .get("function").getAsJsonObject().get("arguments").getAsString());
    }

    @Test
    public void testChatDocumentSourceTextIsScanned() {
        JsonObject root = parse("{\"messages\":[{\"role\":\"user\",\"content\":["
                + "{\"type\":\"document\",\"source\":{\"type\":\"text\",\"media_type\":\"text/plain\","
                + "\"data\":\"My SSN is 13800138000\"}}]}]}");
        assertEquals(OpenAiChatProfile.ID, PiiProtocolProfiles.select(root).id());
        List<PiiTextSlot> slots = new OpenAiChatProfile().requestSlots(root);
        assertEquals(1, slots.size());
        assertEquals("My SSN is 13800138000", slots.get(0).text());
        slots.get(0).replace("My SSN is <PHONE_CN>");
        assertEquals("My SSN is <PHONE_CN>",
                root.get("messages").getAsJsonArray().get(0).getAsJsonObject()
                        .get("content").getAsJsonArray().get(0).getAsJsonObject()
                        .get("source").getAsJsonObject().get("data").getAsString());
    }

    @Test
    public void testBinaryDocumentSourceIsOutOfScope() {
        JsonObject root = parse("{\"messages\":[{\"role\":\"user\",\"content\":["
                + "{\"type\":\"document\",\"source\":{\"type\":\"base64\",\"media_type\":\"application/pdf\","
                + "\"data\":\"JVBERi0xLjQK\"}}]}]}");
        assertTrue(new OpenAiChatProfile().requestSlots(root).isEmpty());
    }

    @Test
    public void testChatFileTextualDataIsScannedAndReencoded() {
        String text = "reach me at 13800138000";
        String encoded = Base64.getEncoder().encodeToString(text.getBytes(StandardCharsets.UTF_8));
        JsonObject root = parse("{\"messages\":[{\"role\":\"user\",\"content\":["
                + "{\"type\":\"file\",\"file\":{\"filename\":\"note.txt\","
                + "\"file_data\":\"data:text/plain;base64," + encoded + "\"}}]}]}");
        List<PiiTextSlot> slots = new OpenAiChatProfile().requestSlots(root);
        assertEquals(1, slots.size());
        assertEquals(text, slots.get(0).text());
        slots.get(0).replace("reach me at <PHONE_CN>");
        String updated = root.get("messages").getAsJsonArray().get(0).getAsJsonObject()
                .get("content").getAsJsonArray().get(0).getAsJsonObject()
                .get("file").getAsJsonObject().get("file_data").getAsString();
        assertTrue(updated.startsWith("data:text/plain;base64,"));
        assertEquals("reach me at <PHONE_CN>", new String(
                Base64.getDecoder().decode(updated.substring(updated.indexOf(',') + 1)), StandardCharsets.UTF_8));
    }

    @Test
    public void testBinaryFileDataIsOutOfScope() {
        JsonObject root = parse("{\"messages\":[{\"role\":\"user\",\"content\":["
                + "{\"type\":\"file\",\"file\":{\"filename\":\"a.png\","
                + "\"file_data\":\"data:image/png;base64,iVBORw0KGgo=\"}}]}]}");
        assertTrue(new OpenAiChatProfile().requestSlots(root).isEmpty());
    }

    @Test
    public void testResponsesFunctionCallArgumentsAreScanned() {
        JsonObject root = parse("{\"output\":[{\"type\":\"function_call\",\"name\":\"lookup\","
                + "\"arguments\":\"{\\\"email\\\":\\\"a@b.com\\\"}\"}]}");
        assertEquals(OpenAiResponsesProfile.ID, PiiProtocolProfiles.selectResponse(root).id());
        List<PiiTextSlot> slots = new OpenAiResponsesProfile().responseSlots(root);
        assertEquals(1, slots.size());
        assertEquals("{\"email\":\"a@b.com\"}", slots.get(0).text());
    }

    @Test
    public void testResponsesDocumentAndFilePartsAreScanned() {
        String encoded = Base64.getEncoder().encodeToString("card 4111111111111111".getBytes(StandardCharsets.UTF_8));
        JsonObject root = parse("{\"input\":[{\"type\":\"message\",\"role\":\"user\",\"content\":["
                + "{\"type\":\"input_text\",\"text\":\"hi 13800138000\"},"
                + "{\"type\":\"document\",\"source\":{\"type\":\"text\",\"data\":\"doc a@b.com\"}},"
                + "{\"type\":\"file\",\"file\":{\"file_data\":\"data:text/plain;base64," + encoded + "\"}}"
                + "]}]}");
        List<PiiTextSlot> slots = new OpenAiResponsesProfile().requestSlots(root);
        assertEquals(3, slots.size());
        assertEquals("hi 13800138000", slots.get(0).text());
        assertEquals("doc a@b.com", slots.get(1).text());
        assertEquals("card 4111111111111111", slots.get(2).text());
    }

    @Test
    public void testResponsesNonFunctionCallItemArgumentsAreNotScanned() {
        // a stray arguments member on another item type is not mistaken for message text
        JsonObject root = parse("{\"output\":[{\"type\":\"message\",\"role\":\"assistant\","
                + "\"content\":[{\"type\":\"text\",\"text\":\"hi\"}],"
                + "\"arguments\":\"{\\\"email\\\":\\\"a@b.com\\\"}\"}]}");
        assertEquals(OpenAiResponsesProfile.ID, PiiProtocolProfiles.selectResponse(root).id());
        List<PiiTextSlot> slots = new OpenAiResponsesProfile().responseSlots(root);
        assertEquals(1, slots.size());
        assertEquals("hi", slots.get(0).text());
    }

    @Test
    public void testMimeWrappedFileDataIsScanned() {
        // a long payload forces the MIME encoder to insert CRLF line breaks every 76 characters
        String text = "notes for the agent: my primary phone number for the escalation path is 13800138000";
        String wrapped = Base64.getMimeEncoder().encodeToString(text.getBytes(StandardCharsets.UTF_8));
        assertTrue(wrapped.contains("\r\n"));
        JsonObject root = parse("{\"messages\":[{\"role\":\"user\",\"content\":["
                + "{\"type\":\"file\",\"file\":{\"file_data\":\""
                + "data:text/plain;base64," + wrapped.replace("\r", "\\r").replace("\n", "\\n")
                + "\"}}]}]}");
        assertEquals(OpenAiChatProfile.ID, PiiProtocolProfiles.select(root).id());
        List<PiiTextSlot> slots = new OpenAiChatProfile().requestSlots(root);
        assertEquals(1, slots.size());
        assertEquals(text, slots.get(0).text());
    }

    @Test
    public void testUrlSafeFileDataIsScanned() {
        // the URL safe alphabet encodes byte sequences the standard alphabet cannot express with + and /
        byte[] bytes = {(byte) 0xfb, (byte) 0xef, (byte) 0xbe, (byte) 0x6d, (byte) 0x79, (byte) 0x20,
                (byte) 0x69, (byte) 0x64, (byte) 0x20, (byte) 0x69, (byte) 0x73, (byte) 0x20,
                (byte) 0x31, (byte) 0x33, (byte) 0x38, (byte) 0x30, (byte) 0x30, (byte) 0x31,
                (byte) 0x33, (byte) 0x38, (byte) 0x30, (byte) 0x30, (byte) 0x30};
        String urlSafe = Base64.getUrlEncoder().encodeToString(bytes);
        assertTrue(urlSafe.contains("-") || urlSafe.contains("_"));
        JsonObject root = parse("{\"messages\":[{\"role\":\"user\",\"content\":["
                + "{\"type\":\"file\",\"file\":{\"file_data\":\"data:text/plain;base64," + urlSafe + "\"}}]}]}");
        assertEquals(OpenAiChatProfile.ID, PiiProtocolProfiles.select(root).id());
        List<PiiTextSlot> slots = new OpenAiChatProfile().requestSlots(root);
        assertEquals(1, slots.size());
        assertEquals(new String(bytes, StandardCharsets.UTF_8), slots.get(0).text());
    }

    @Test
    public void testUndecodableTextualFileDataIsMalformed() {
        // a corrupt payload under a textual media type is malformed, not silently skipped,
        // so a fail closed rule rejects it instead of leaking it
        String corrupt = "data:text/plain;base64,!!!!not-base64!!!!";
        assertNull(PiiProtocolProfiles.select(parse("{\"messages\":[{\"role\":\"user\",\"content\":["
                + "{\"type\":\"file\",\"file\":{\"file_data\":\"" + corrupt + "\"}}]}]}")));
        assertNull(PiiProtocolProfiles.select(parse("{\"input\":[{\"type\":\"message\",\"role\":\"user\","
                + "\"content\":[{\"type\":\"file\",\"file\":{\"file_data\":\"" + corrupt + "\"}}]}]}")));
        assertNull(PiiProtocolProfiles.selectResponse(parse("{\"choices\":[{\"message\":{\"role\":\"user\","
                + "\"content\":[{\"type\":\"file\",\"file\":{\"file_data\":\"" + corrupt + "\"}}]}}]}")));
        // an undecodable binary media type stays out of scope and therefore valid
        assertEquals(OpenAiChatProfile.ID, PiiProtocolProfiles.select(parse("{\"messages\":[{\"role\":\"user\","
                + "\"content\":[{\"type\":\"file\",\"file\":{\"file_data\":\"data:image/png;base64,!!!!\"}}]}]}"))
                .id());
    }

    private JsonObject parse(final String json) {
        return JsonParser.parseString(json).getAsJsonObject();
    }
}
