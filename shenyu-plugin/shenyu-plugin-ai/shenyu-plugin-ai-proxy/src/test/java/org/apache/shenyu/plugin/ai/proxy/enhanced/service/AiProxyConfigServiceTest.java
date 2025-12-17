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

import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(MockitoExtension.class)
public class AiProxyConfigServiceTest {

    private AiProxyConfigService configService;

    @BeforeEach
    void setUp() {
        configService = new AiProxyConfigService();
        // Note: Singleton has no public clear method, state is managed per test by re-singling.
    }

    @Test
    void testResolvePrimaryConfigGlobalOnly() {
        AiCommonConfig globalConfig = new AiCommonConfig();
        globalConfig.setModel("global-model");
        Singleton.INST.single(AiCommonConfig.class, globalConfig);

        AiProxyHandle handle = new AiProxyHandle();
        AiCommonConfig result = configService.resolvePrimaryConfig(handle);

        assertNotNull(result);
        assertEquals("global-model", result.getModel());
    }

    @Test
    void testResolvePrimaryConfigHandleOnly() {
        AiProxyHandle handle = new AiProxyHandle();
        handle.setModel("handle-model");

        AiCommonConfig result = configService.resolvePrimaryConfig(handle);

        assertNotNull(result);
        assertEquals("handle-model", result.getModel());
    }

    @Test
    void testResolvePrimaryConfigMerge() {
        AiCommonConfig globalConfig = new AiCommonConfig();
        globalConfig.setProvider("global-provider");
        globalConfig.setModel("global-model");
        Singleton.INST.single(AiCommonConfig.class, globalConfig);

        AiProxyHandle handle = new AiProxyHandle();
        handle.setModel("handle-model");
        handle.setApiKey("handle-key");

        AiCommonConfig result = configService.resolvePrimaryConfig(handle);

        assertNotNull(result);
        assertEquals("global-provider", result.getProvider());
        assertEquals("handle-model", result.getModel());
        assertEquals("handle-key", result.getApiKey());
    }

    @Test
    void testResolveDynamicFallbackConfigPresent() {
        String requestBody = "{\"fallbackConfig\": {\"model\": \"dynamic-fallback-model\"}}";
        AiCommonConfig primaryConfig = new AiCommonConfig();
        primaryConfig.setProvider("primary-provider");

        Optional<AiCommonConfig> result = configService.resolveDynamicFallbackConfig(primaryConfig, requestBody);

        assertTrue(result.isPresent());
        assertEquals("dynamic-fallback-model", result.get().getModel());
        assertEquals("primary-provider", result.get().getProvider());
    }

    @Test
    void testResolveDynamicFallbackConfigNotPresent() {
        String requestBody = "{\"messages\": [{\"role\": \"user\"}]}";
        AiCommonConfig primaryConfig = new AiCommonConfig();

        Optional<AiCommonConfig> result = configService.resolveDynamicFallbackConfig(primaryConfig, requestBody);

        assertFalse(result.isPresent());
    }
    
    @Test
    void testResolveDynamicFallbackConfigMalformedJson() {
        String requestBody = "{\"fallbackConfig\": {\"model\": \"dynamic-fallback-model\"";
        AiCommonConfig primaryConfig = new AiCommonConfig();

        Optional<AiCommonConfig> result = configService.resolveDynamicFallbackConfig(primaryConfig, requestBody);

        assertFalse(result.isPresent());
    }

    @Test
    void testResolveAdminFallbackConfigPresent() {
        AiProxyHandle.FallbackConfig fallback = new AiProxyHandle.FallbackConfig();
        fallback.setModel("admin-fallback-model");
        AiProxyHandle handle = new AiProxyHandle();
        handle.setFallbackConfig(fallback);
        AiCommonConfig primaryConfig = new AiCommonConfig();
        primaryConfig.setProvider("primary-provider");

        Optional<AiCommonConfig> result = configService.resolveAdminFallbackConfig(primaryConfig, handle);

        assertTrue(result.isPresent());
        assertEquals("admin-fallback-model", result.get().getModel());
        assertEquals("primary-provider", result.get().getProvider());
    }

    @Test
    void testResolveAdminFallbackConfigNotPresent() {
        AiProxyHandle handle = new AiProxyHandle();
        AiCommonConfig primaryConfig = new AiCommonConfig();

        Optional<AiCommonConfig> result = configService.resolveAdminFallbackConfig(primaryConfig, handle);

        assertFalse(result.isPresent());
    }

    @Test
    void testExtractPromptNullInput() {
        String result = configService.extractPrompt(null);
        assertEquals(null, result);
    }

    @Test
    void testExtractPromptEmptyInput() {
        String result = configService.extractPrompt("");
        assertEquals("", result);
    }

    @Test
    void testExtractPromptWithPromptField() {
        String requestBody = "{\"prompt\": \"test prompt content\"}";
        String result = configService.extractPrompt(requestBody);
        assertEquals("test prompt content", result);
    }

    @Test
    void testExtractPromptWithContentField() {
        String requestBody = "{\"content\": \"test content\"}";
        String result = configService.extractPrompt(requestBody);
        assertEquals("test content", result);
    }

    @Test
    void testExtractPromptPromptTakesPrecedenceOverContent() {
        String requestBody = "{\"prompt\": \"prompt value\", \"content\": \"content value\"}";
        String result = configService.extractPrompt(requestBody);
        assertEquals("prompt value", result);
    }

    @Test
    void testExtractPromptWithFallbackConfigAndPrompt() {
        String requestBody = "{\"fallbackConfig\": {\"model\": \"test-model\"}, \"prompt\": \"test prompt\"}";
        String result = configService.extractPrompt(requestBody);
        assertEquals("test prompt", result);
    }

    @Test
    void testExtractPromptWithFallbackConfigAndContent() {
        String requestBody = "{\"fallbackConfig\": {\"model\": \"test-model\"}, \"content\": \"test content\"}";
        String result = configService.extractPrompt(requestBody);
        assertEquals("test content", result);
    }

    @Test
    void testExtractPromptWithFallbackConfigButNoPromptOrContent() {
        String requestBody = "{\"fallbackConfig\": {\"model\": \"test-model\"}, \"messages\": [{\"role\": \"user\"}]}";
        String result = configService.extractPrompt(requestBody);
        assertEquals(requestBody, result);
    }

    @Test
    void testExtractPromptWithoutPromptOrContent() {
        String requestBody = "{\"messages\": [{\"role\": \"user\", \"content\": \"hello\"}]}";
        String result = configService.extractPrompt(requestBody);
        assertEquals(requestBody, result);
    }

    @Test
    void testExtractPromptMalformedJson() {
        String requestBody = "{\"prompt\": \"test\"";
        String result = configService.extractPrompt(requestBody);
        assertEquals(requestBody, result);
    }

    @Test
    void testExtractPromptEmptyJson() {
        String requestBody = "{}";
        String result = configService.extractPrompt(requestBody);
        assertEquals(requestBody, result);
    }
}
