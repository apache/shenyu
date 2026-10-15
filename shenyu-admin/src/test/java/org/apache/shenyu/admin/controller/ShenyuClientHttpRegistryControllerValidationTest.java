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

package org.apache.shenyu.admin.controller;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests validation declarations on the HTTP registration API.
 */
public final class ShenyuClientHttpRegistryControllerValidationTest {

    @Test
    public void shouldValidateAllMutatingRequestBodies() {
        List<String> methods = Arrays.asList("registerMetadata", "registerURI", "registerApiDoc",
                "registerDiscoveryConfig", "registerMcpTools", "offline");
        for (String methodName : methods) {
            Method method = Arrays.stream(ShenyuClientHttpRegistryController.class.getDeclaredMethods())
                    .filter(candidate -> candidate.getName().equals(methodName)).findFirst().orElseThrow();
            assertTrue(Arrays.stream(method.getParameterAnnotations()[0])
                    .anyMatch(annotation -> annotation.annotationType().equals(Valid.class)), methodName);
        }
    }

    @Test
    public void shouldDeclareRequiredRegistrationFields() throws NoSuchFieldException {
        assertNotBlank(MetaDataRegisterDTO.class, "appName", "contextPath", "path", "rpcType", "ruleName", "host");
        assertNotBlank(URIRegisterDTO.class, "appName", "contextPath", "rpcType", "host");
        assertNotBlank(ApiDocRegisterDTO.class, "contextPath", "apiPath", "rpcType");
        assertNotNull(ApiDocRegisterDTO.class, "httpMethod");
        assertNotBlank(DiscoveryConfigRegisterDTO.class, "selectorName", "name", "discoveryType",
                "serverList", "pluginName");
        assertNotNull(McpToolsRegisterDTO.class, "metaDataRegisterDTO");
        assertNotBlank(McpToolsRegisterDTO.class, "mcpConfig");
    }

    private void assertNotBlank(final Class<?> type, final String... names) throws NoSuchFieldException {
        for (String name : names) {
            Field field = type.getDeclaredField(name);
            assertTrue(field.isAnnotationPresent(NotBlank.class), type.getSimpleName() + "." + name);
        }
    }

    private void assertNotNull(final Class<?> type, final String... names) throws NoSuchFieldException {
        for (String name : names) {
            Field field = type.getDeclaredField(name);
            assertTrue(field.isAnnotationPresent(NotNull.class), type.getSimpleName() + "." + name);
        }
    }
}
