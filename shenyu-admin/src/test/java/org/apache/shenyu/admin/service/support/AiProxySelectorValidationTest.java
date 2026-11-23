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

package org.apache.shenyu.admin.service.support;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.common.enums.PluginEnum;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AiProxySelectorValidationTest {

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"{}", "{\"mode\":\"converted\"}", "{\"model\":\"legacy\"}",
        "{\"mode\":\"passthrough\",\"baseUrl\":\"https://vendor.example:8443/common/v1\",\"stripPrefix\":\"/ai/native-v1\"}",
        "{\"mode\":\"passthrough\",\"baseUrl\":\"http://localhost:8080\",\"stripPrefix\":\"\"}"})
    void acceptsExistingSelectorsAndNativeRoutesWithoutAStoredKey(final String handle) {
        // 验证旧选择器和未配置厂商密钥的透传选择器均可通过校验。
        assertDoesNotThrow(() -> AiProxySelectorValidation.validateHandle(PluginEnum.AI_PROXY.getName(), handle));
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"mode\":\"unknown\"}", "{\"mode\":\"\"}", "{\"mode\":\"passthrough\"}", "[]", "null"})
    void rejectsUnusableModes(final String handle) {
        assertThrows(ShenyuAdminException.class, () -> AiProxySelectorValidation.validateHandle(PluginEnum.AI_PROXY.getName(), handle));
    }

    @ParameterizedTest
    @ValueSource(strings = {"ftp://vendor.example", "//vendor.example", "https:///v1", "https://user:secret@vendor.example",
        "https://vendor.example/path?key=secret", "https://vendor.example/path#secret", "https://vendor.example:65536"})
    void rejectsInvalidDestinationsWithoutReportingCredentials(final String url) {
        String handle = "{\"mode\":\"passthrough\",\"baseUrl\":\"" + url + "\"}";
        ShenyuAdminException error = assertThrows(ShenyuAdminException.class, () -> AiProxySelectorValidation.validateHandle(PluginEnum.AI_PROXY.getName(), handle));
        // 检查错误消息中未包含原始 URL 或其中的凭证。
        assertFalse(error.getMessage().contains(url));
        assertFalse(error.getMessage().contains("secret"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"claude", "/", "/vendor/", "/vendor//v1", "/vendor/..", "/./vendor", "/vendor%2Fv1", "/vendor/*", "/a?x", "/a#x"})
    void rejectsNonliteralPathPrefixes(final String prefix) {
        // 验证含编码斜杠、点段、通配符或空段的前缀被拒绝。
        String handle = "{\"mode\":\"passthrough\",\"baseUrl\":\"https://vendor.example\",\"stripPrefix\":\"" + prefix + "\"}";
        assertThrows(ShenyuAdminException.class, () -> AiProxySelectorValidation.validateHandle(PluginEnum.AI_PROXY.getName(), handle));
    }

    @Test
    void malformedNumericFieldsAreNotIncludedInValidationErrors() {
        String handle = "{\"mode\":\"converted\",\"temperature\":\"secret\"}";
        ShenyuAdminException error = assertThrows(ShenyuAdminException.class, () -> AiProxySelectorValidation.validateHandle(PluginEnum.AI_PROXY.getName(), handle));
        // 检查校验错误消息未包含数值字段的原始输入。
        assertFalse(error.getMessage().contains("secret"));
    }

    @Test
    void otherPluginsKeepTheirOwnHandleFormats() {
        assertDoesNotThrow(() -> AiProxySelectorValidation.validateHandle(PluginEnum.DIVIDE.getName(), "[]"));
    }
}
