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

package org.apache.shenyu.common.ai;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Contract tests for the bounded normalized request view.
 */
class AiNormalizedRequestViewTest {

    @Test
    void messageOverviewIsCopiedDefensively() {
        List<AiNormalizedRequestView.BoundedMessage> mutable = new ArrayList<>();
        mutable.add(new AiNormalizedRequestView.BoundedMessage("user", AiNormalizedRequestView.ContentKind.TEXT, 5, false, "hello"));

        AiNormalizedRequestView view = view(mutable);
        mutable.clear();

        assertEquals(1, view.messages().size());
        assertThrows(UnsupportedOperationException.class, () -> view.messages().add(
                new AiNormalizedRequestView.BoundedMessage("x", AiNormalizedRequestView.ContentKind.OPAQUE, -1, false, null)));
    }

    @Test
    void unsupportedViewsStayVisible() {
        AiNormalizedRequestView view = new AiNormalizedRequestView(null, null, null, false, List.of(), false, false,
                AiNormalizedRequestView.ViewKind.WIRE, null, 1L, true, "no known AI protocol fields");

        assertFalse(view.supported());
        assertEquals("no known AI protocol fields", view.unsupportedReason());
        assertEquals(1L, view.generation());
        assertNull(view.mutator());
    }

    @Test
    void supportedViewsExposeTheirKind() {
        AiNormalizedRequestView wire = view(new ArrayList<>());
        AiNormalizedRequestView effective = new AiNormalizedRequestView("openai-chat", "CHAT_COMPLETION", "gpt-4", false,
                List.of(), false, false, AiNormalizedRequestView.ViewKind.EFFECTIVE, "aiPrompt", 2L, false, null);

        assertTrue(wire.supported());
        assertTrue(effective.supported());
        assertEquals(AiNormalizedRequestView.ViewKind.WIRE, wire.viewKind());
        assertEquals("aiPrompt", effective.mutator());
        assertNotSame(wire, effective);
    }

    private AiNormalizedRequestView view(final List<AiNormalizedRequestView.BoundedMessage> messages) {
        return new AiNormalizedRequestView("openai-chat", "CHAT_COMPLETION", "gpt-4", false, messages, false, false,
                AiNormalizedRequestView.ViewKind.WIRE, null, 1L, false, null);
    }
}
