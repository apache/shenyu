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

package org.apache.shenyu.web.diagnostics;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.never;
import static org.mockito.ArgumentMatchers.any;

class PluginInventoryCollectorTest {

    @Test
    void legacyPluginReportsUnknownWithoutInvokingRequestEffects() {
        ShenyuPlugin plugin = mock(ShenyuPlugin.class);
        when(plugin.named()).thenReturn("external-plugin");
        when(plugin.getOrder()).thenReturn(7);
        ShenyuWebHandler handler = mock(ShenyuWebHandler.class);
        when(handler.getPlugins()).thenReturn(Collections.singletonList(plugin));
        DiagnosticSection section = new PluginInventoryCollector(handler).collect(new DiagnosticBudget(1000, 8192));
        assertEquals(SectionStatus.OK, section.getStatus());
        JsonObject json = json(section);
        assertEquals("UNKNOWN", json.getAsJsonArray("plugins").get(0).getAsJsonObject().get("declarationStatus").getAsString());
        assertEquals("UNKNOWN", json.get("requestSelection").getAsString());
        verify(plugin, never()).execute(any(), any());
        verify(plugin, never()).skip(any());
        verify(plugin, never()).before(any());
        verify(plugin, never()).after(any());
    }

    @Test
    void recordAndByteBudgetsBoundCollectionBeforeFullInventoryMaterialization() {
        ShenyuPlugin plugin = mock(ShenyuPlugin.class);
        when(plugin.named()).thenReturn("bounded");
        ShenyuWebHandler handler = mock(ShenyuWebHandler.class);
        when(handler.getPlugins()).thenReturn(Collections.nCopies(10000, plugin));
        DiagnosticSection section = new PluginInventoryCollector(handler).collect(new DiagnosticBudget(1000, 1024));
        assertEquals(SectionStatus.TRUNCATED, section.getStatus());
        assertTrue(section.getContent().length <= 1024);
        assertTrue(json(section).get("collected").getAsInt() < 64);
        assertEquals(10000, json(section).get("count").getAsInt());
    }

    private JsonObject json(final DiagnosticSection section) {
        return GsonUtils.getInstance().fromJson(new String(section.getContent(), StandardCharsets.UTF_8), JsonObject.class);
    }
}
