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

package org.apache.shenyu.plugin.ai.requestid;

import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiRequestIdFactoryTest {

    @Test
    void shouldGenerateOpaqueUuid() {
        String requestId = AiRequestIdFactory.generate();

        assertTrue(AiRequestIdFactory.reusable(requestId));
        assertEquals(36, requestId.length());
    }

    @Test
    void shouldGenerateDistinctIds() {
        Set<String> requestIds = new HashSet<>();
        for (int index = 0; index < 100; index++) {
            requestIds.add(AiRequestIdFactory.generate());
        }

        assertEquals(100, requestIds.size());
    }

    @Test
    void shouldAcceptBoundedIds() {
        assertTrue(AiRequestIdFactory.reusable("gateway-id"));
        assertTrue(AiRequestIdFactory.reusable("abc.DEF_123:456-789"));
        assertTrue(AiRequestIdFactory.reusable("a".repeat(128)));
    }

    @Test
    void shouldRejectMissingOrUnsafeIds() {
        assertFalse(AiRequestIdFactory.reusable(null));
        assertFalse(AiRequestIdFactory.reusable(""));
        assertFalse(AiRequestIdFactory.reusable("   "));
        assertFalse(AiRequestIdFactory.reusable("a".repeat(129)));
        assertFalse(AiRequestIdFactory.reusable("bad id"));
        assertFalse(AiRequestIdFactory.reusable("bad\nid"));
        assertFalse(AiRequestIdFactory.reusable("id/with/slash"));
    }
}
