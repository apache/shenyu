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

package org.apache.shenyu.plugin.agent.gateway.resource;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AgentResourceContentTest {

    @Test
    void shouldEncodeTextAndBinaryWithoutToolContentWrapper() {
        AgentResourceContent text = AgentResourceContent.text("test://text", "text/plain", "汉字");
        assertEquals("汉字", text.toJson().get("text").getAsString());
        assertFalse(text.toJson().has("type"));
        assertFalse(text.toJson().has("resource"));
        assertEquals("AA==", AgentResourceContent.blob("test://binary", "application/octet-stream", "AA==").toJson().get("blob").getAsString());
    }

    @Test
    void shouldAcceptEmptyContentsAndKeepSnapshotsIndependent() {
        AgentResourceContent empty = AgentResourceContent.blob("test://empty", null, "");
        assertEquals("", empty.toJson().get("blob").getAsString());
        assertFalse(empty.toJson().has("mimeType"));
        empty.toJson().addProperty("uri", "changed");
        assertEquals("test://empty", empty.toJson().get("uri").getAsString());
        assertEquals("", AgentResourceContent.text("test://empty", null, "").toJson().get("text").getAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = {"relative/path", "https://user:secret@host/path", "test://bad uri", "test://bad\nuri"})
    void shouldRejectUnsafeUriMetadata(final String uri) {
        assertThrows(IllegalArgumentException.class, () -> AgentResourceContent.text(uri, null, ""));
    }

    @ParameterizedTest
    @ValueSource(strings = {"AA", "AB==", "%%%", "AA==\n"})
    void shouldRejectNoncanonicalBinaryData(final String blob) {
        assertThrows(IllegalArgumentException.class, () -> AgentResourceContent.blob("test://binary", null, blob));
    }

    @Test
    void shouldRejectInvalidMimeAndNullContent() {
        assertThrows(IllegalArgumentException.class, () -> AgentResourceContent.text("test://text", "text/plain\r\nX: value", ""));
        assertThrows(NullPointerException.class, () -> AgentResourceContent.text("test://text", null, null));
        assertThrows(NullPointerException.class, () -> AgentResourceContent.blob("test://binary", null, null));
    }
}
