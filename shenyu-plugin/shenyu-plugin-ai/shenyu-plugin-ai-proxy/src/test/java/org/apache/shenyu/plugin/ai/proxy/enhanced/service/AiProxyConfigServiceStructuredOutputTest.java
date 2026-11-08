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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import com.google.gson.Gson;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.dto.convert.rule.AiStructuredOutputConfig;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The structured output policy rides the existing selector handle to the runtime
 * config: Gson round-trip, handle mapping and config copy must preserve it, and a
 * handle without the block stays disabled.
 */
class AiProxyConfigServiceStructuredOutputTest {

    private static final String HANDLE_JSON = "{\"provider\":\"OpenAI\",\"baseUrl\":\"https://main.example\","
            + "\"apiKey\":\"main-key\",\"model\":\"main-model\","
            + "\"structuredOutput\":{\"schema\":\"{\\\"type\\\":\\\"object\\\"}\",\"repairEnabled\":true,"
            + "\"repairPrompt\":\"Fix the output\",\"repairMaxAttempts\":2,\"repairMaxTokens\":256}}";

    @Test
    void gsonRoundTripAndConfigMappingPreserveThePolicy() {
        AiProxyHandle handle = new Gson().fromJson(HANDLE_JSON, AiProxyHandle.class);
        AiStructuredOutputConfig policy = handle.getStructuredOutput();
        assertEquals("{\"type\":\"object\"}", policy.getSchema());
        assertTrue(policy.getRepairEnabled());
        assertEquals("Fix the output", policy.getRepairPrompt());
        assertEquals(2, policy.getRepairMaxAttempts());
        assertEquals(256, policy.getRepairMaxTokens());

        AiCommonConfig resolved = service().resolvePrimaryConfig(handle);
        assertSame(policy, resolved.getStructuredOutput());

        AiCommonConfig copy = new AiCommonConfig(resolved);
        assertSame(policy, copy.getStructuredOutput());
        assertTrue(AiStructuredOutputPolicy.of(copy.getStructuredOutput()).isActive());
        assertTrue(AiStructuredOutputPolicy.of(copy.getStructuredOutput()).canRepair(
                org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiContentCompliance.SCHEMA_MISMATCH));
    }

    @Test
    void handleWithoutTheBlockStaysDisabled() {
        AiCommonConfig resolved = service().resolvePrimaryConfig(new AiProxyHandle());
        assertNull(resolved.getStructuredOutput());
        assertFalse(AiStructuredOutputPolicy.of(resolved.getStructuredOutput()).isActive());
    }

    private AiProxyConfigService service() {
        return new AiProxyConfigService(new AiProxyPluginHandler());
    }
}
