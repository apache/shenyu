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

import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AiProviderCapabilitiesTest {

    @Test
    void allowsEmptyCapabilities() {
        AiProviderCapabilities empty = new AiProviderCapabilities(Map.of(), Set.of(), Set.of());

        assertEquals(Set.of(), empty.protocols());
    }

    @Test
    void rejectsNullBindingsWithClearMessage() {
        Map<String, AiProviderProtocolBinding> bindings = new java.util.HashMap<>();
        bindings.put("custom", null);

        IllegalArgumentException error = assertThrows(IllegalArgumentException.class,
                () -> new AiProviderCapabilities(bindings, Set.of(), Set.of()));

        assertEquals("AI provider protocol binding must not be null: custom", error.getMessage());
    }
}
