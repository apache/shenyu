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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(MockitoExtension.class)
public class AiProxyConfigServiceTest {

    private AiProxyConfigService configService;

    private AiProxyPluginHandler pluginHandler;

    @BeforeEach
    void setUp() {
        pluginHandler = new AiProxyPluginHandler();
        configService = new AiProxyConfigService(pluginHandler);
    }

    @Test
    void testResolvePrimaryConfigGlobalOnly() {
        AiCommonConfig globalConfig = new AiCommonConfig();
        globalConfig.setModel("global-model");
        PluginData pluginData = new PluginData();
        pluginData.setConfig(GsonUtils.getInstance().toJson(globalConfig));
        pluginHandler.handlerPlugin(pluginData);

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
    void testResolvePrimaryConfigCopiesProtocolRequestBodyOverrides() {
        AiProxyHandle handle = new AiProxyHandle();
        handle.setRequestBodyOverrides(Map.of("openai-chat", Map.of("reasoning_effort", "high")));
        handle.setRequestBodyForceOverride(true);

        AiCommonConfig result = configService.resolvePrimaryConfig(handle);

        assertEquals("high", result.getRequestBodyOverrides().get("openai-chat").get("reasoning_effort"));
        assertTrue(result.getRequestBodyForceOverride());
    }

    @Test
    void testResolvePrimaryConfigMerge() {
        AiCommonConfig globalConfig = new AiCommonConfig();
        globalConfig.setProvider("global-provider");
        globalConfig.setModel("global-model");
        PluginData pluginData = new PluginData();
        pluginData.setConfig(GsonUtils.getInstance().toJson(globalConfig));
        pluginHandler.handlerPlugin(pluginData);

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
        AiProxyHandle.FallbackConfig target = new AiProxyHandle.FallbackConfig();
        target.setModel("dynamic-fallback-model");
        AiCommonConfig primary = primary();
        Optional<AiCommonConfig> result = configService.resolveDynamicFallbackConfig(primary, managed(target), "{\"fallbackConfig\":{\"target\":\"backup\"}}");
        assertTrue(result.isPresent());
        assertEquals("dynamic-fallback-model", result.get().getModel());
        assertEquals("OpenAI", result.get().getProvider());
        assertEquals("primary-key", result.get().getApiKey());
        assertNull(target.getApiKey());
        assertEquals("primary", primary.getModel());
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

        assertThrows(ShenyuException.class, () -> configService.resolveDynamicFallbackConfig(primaryConfig, requestBody));
    }

    @Test
    void testResolveAdminFallbackConfigPresent() {
        AiProxyHandle.FallbackConfig fallback = new AiProxyHandle.FallbackConfig();
        fallback.setModel("admin-fallback-model");
        AiProxyHandle handle = new AiProxyHandle();
        handle.setFallbackConfig(fallback);
        AiCommonConfig primaryConfig = new AiCommonConfig();
        primaryConfig.setProvider("primary-provider");
        primaryConfig.setBaseUrl("https://primary.example");
        primaryConfig.setApiKey("primary-key");

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
        // fallbackConfig should be stripped before forwarding to upstream
        assertFalse(result.contains("fallbackConfig"));
        assertTrue(result.contains("messages"));
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

    @Test
    void testConfigUpdatesReplaceDefaultsWithoutMutatingResolvedRequests() {
        PluginData pluginData = new PluginData();
        pluginData.setConfig("{\"provider\":\"OpenAI\",\"apiKey\":\"first-key\",\"model\":\"first-model\"}");
        pluginHandler.handlerPlugin(pluginData);
        AiCommonConfig first = configService.resolvePrimaryConfig(null);
        pluginData.setConfig("{\"provider\":\"OpenAI\",\"model\":\"second-model\"}");
        pluginHandler.handlerPlugin(pluginData);
        AiCommonConfig second = configService.resolvePrimaryConfig(null);
        assertEquals("first-model", first.getModel());
        assertEquals("first-key", first.getApiKey());
        assertEquals("second-model", second.getModel());
        assertNull(second.getApiKey());
        pluginHandler.removePlugin(pluginData);
        assertNull(configService.resolvePrimaryConfig(null).getProvider());
        assertNull(configService.resolvePrimaryConfig(null).getModel());
    }

    @Test
    void testRequestCredentialOverridesDoNotChangePluginDefaults() {
        PluginData pluginData = new PluginData();
        pluginData.setConfig("{\"apiKey\":\"global-key\"}");
        pluginHandler.handlerPlugin(pluginData);
        AiCommonConfig resolved = configService.resolvePrimaryConfig(null);
        resolved.setApiKey("per-request-key");
        assertEquals("global-key", configService.resolvePrimaryConfig(null).getApiKey());
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "[]", "{}", "{\"target\":null}", "{\"target\":2}", "{\"target\":\"\"}",
        "{\"target\":\"../backup\"}", "{\"target\":\"backup\",\"apiKey\":\"caller-key\"}"})
    void rejectsMalformedOrInlineSelections(final String selection) {
        AiProxyHandle handle = managed(new AiProxyHandle.FallbackConfig());
        assertThrows(ShenyuException.class, () -> configService.resolveDynamicFallbackConfig(primary(), handle, "{\"fallbackConfig\":" + selection + "}"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"https://other.example", "http://primary.example", "https://primary.example:444", "https://primary.example/another-account"})
    void changedEndpointCannotInheritThePrimaryCredential(final String endpoint) {
        AiProxyHandle.FallbackConfig target = new AiProxyHandle.FallbackConfig();
        target.setBaseUrl(endpoint);
        assertThrows(ShenyuException.class, () -> configService.resolveDynamicFallbackConfig(primary(), managed(target), "{\"fallbackConfig\":{\"target\":\"backup\"}}"));
        assertNull(target.getApiKey());
    }

    @Test
    void changedProviderCannotInheritThePrimaryCredential() {
        AiProxyHandle.FallbackConfig target = new AiProxyHandle.FallbackConfig();
        target.setProvider("OtherProvider");
        assertThrows(ShenyuException.class, () -> configService.resolveDynamicFallbackConfig(primary(), managed(target), "{\"fallbackConfig\":{\"target\":\"backup\"}}"));
    }

    @Test
    void changedAdminEndpointAlsoRequiresItsOwnCredential() {
        AiProxyHandle.FallbackConfig target = new AiProxyHandle.FallbackConfig();
        target.setBaseUrl("https://other.example");
        AiProxyHandle handle = new AiProxyHandle();
        handle.setFallbackConfig(target);
        assertThrows(ShenyuException.class, () -> configService.resolveAdminFallbackConfig(primary(), handle));
        target.setApiKey("admin-key");
        assertEquals("admin-key", configService.resolveAdminFallbackConfig(primary(), handle).orElseThrow().getApiKey());
    }

    @ParameterizedTest
    @ValueSource(strings = {"file:///tmp/model", "https://user:password@example.test", "https://example.test?token=secret", "https://example.test#fragment", "https://example.test:0", "https://example.test:99999", "not an endpoint"})
    void rejectsAmbiguousOrNonHttpManagedEndpoints(final String endpoint) {
        AiProxyHandle.FallbackConfig target = new AiProxyHandle.FallbackConfig();
        target.setBaseUrl(endpoint);
        target.setApiKey("managed-key");
        assertThrows(ShenyuException.class, () -> configService.resolveDynamicFallbackConfig(primary(), managed(target), "{\"fallbackConfig\":{\"target\":\"backup\"}}"));
    }

    @Test
    void explicitTargetCredentialDoesNotMutateThePrimaryOrOtherTargets() {
        AiCommonConfig primary = primary();
        AiProxyHandle.FallbackConfig target = new AiProxyHandle.FallbackConfig();
        target.setBaseUrl("https://other.example");
        target.setApiKey("managed-key");
        AiCommonConfig resolved = configService.resolveDynamicFallbackConfig(primary, managed(target), "{\"fallbackConfig\":{\"target\":\"backup\"}}").orElseThrow();
        assertEquals("managed-key", resolved.getApiKey());
        resolved.setApiKey("request-local-change");
        assertEquals("primary-key", primary.getApiKey());
        assertEquals("managed-key", target.getApiKey());
    }

    @Test
    void selectorsCannotUseOtherSelectorsTargetsAndUpdatesTakeEffect() {
        AiProxyHandle.FallbackConfig target = new AiProxyHandle.FallbackConfig();
        AiProxyHandle allowed = managed(target);
        AiProxyHandle denied = new AiProxyHandle();
        denied.setDynamicFallbackEnabled(true);
        String request = "{\"fallbackConfig\":{\"target\":\"backup\"}}";
        assertTrue(configService.resolveDynamicFallbackConfig(primary(), allowed, request).isPresent());
        assertThrows(ShenyuException.class, () -> configService.resolveDynamicFallbackConfig(primary(), denied, request));
        allowed.setFallbackTargets(Map.of());
        assertThrows(ShenyuException.class, () -> configService.resolveDynamicFallbackConfig(primary(), allowed, request));
        allowed.setFallbackTargets(Map.of("backup", target));
        allowed.setDynamicFallbackEnabled(null);
        assertThrows(ShenyuException.class, () -> configService.resolveDynamicFallbackConfig(primary(), allowed, request));
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"fallbackConfig\":{\"target\":\"backup\"}} {}", "{\"fallbackConfig\":{},\"fallbackConfig\":{\"target\":\"backup\"}}"})
    void rejectsTrailingOrDuplicateRequestFields(final String request) {
        assertThrows(ShenyuException.class, () -> configService.resolveDynamicFallbackConfig(primary(), managed(new AiProxyHandle.FallbackConfig()), request));
    }

    private AiProxyHandle managed(final AiProxyHandle.FallbackConfig target) {
        AiProxyHandle handle = new AiProxyHandle();
        handle.setDynamicFallbackEnabled(true);
        handle.setFallbackTargets(Map.of("backup", target));
        return handle;
    }

    private AiCommonConfig primary() {
        AiCommonConfig config = new AiCommonConfig();
        config.setProvider("OpenAI");
        config.setBaseUrl("https://primary.example");
        config.setApiKey("primary-key");
        config.setModel("primary");
        return config;
    }

}
