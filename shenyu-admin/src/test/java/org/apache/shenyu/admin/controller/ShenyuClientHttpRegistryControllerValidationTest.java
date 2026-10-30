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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.register.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.protocol.RegistrationProtocol;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.support.StaticMessageSource;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;

import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.ArgumentCaptor.forClass;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Exercises Admin-side registration validation with the production global exception advice.
 */
@ExtendWith(MockitoExtension.class)
public final class ShenyuClientHttpRegistryControllerValidationTest {

    private final ObjectMapper mapper = new ObjectMapper();

    @Mock
    private ShenyuClientServerRegisterPublisher publisher;

    @InjectMocks
    private ShenyuClientHttpRegistryController controller;

    private MockMvc mvc;

    @BeforeEach
    void setUp() {
        mvc = MockMvcBuilders.standaloneSetup(controller)
                .setControllerAdvice(new ExceptionHandlers(new StaticMessageSource())).build();
    }

    @ParameterizedTest
    @MethodSource("validRequests")
    void acceptsValidRegistrations(final String fixture, final String path) throws Exception {
        mvc.perform(post(path).contentType(MediaType.APPLICATION_JSON).content(mapper.writeValueAsBytes(fixture(fixture))))
                .andExpect(status().isOk()).andExpect(content().string(RegistrationProtocol.SUCCESS));
        verify(publisher).publish(any(DataTypeParent.class));
        verifyNoMoreInteractions(publisher);
    }

    @ParameterizedTest
    @MethodSource("validRequests")
    void rejectsEmptyRegistrations(final String fixture, final String path) throws Exception {
        mvc.perform(post(path).contentType(MediaType.APPLICATION_JSON).content("{}"))
                .andExpect(status().isBadRequest());
        verifyNoInteractions(publisher);
    }

    @ParameterizedTest
    @MethodSource("invalidFields")
    void rejectsEachInvalidRequiredField(final String fixture, final String path, final String field, final String mutation) throws Exception {
        ObjectNode request = fixture(fixture);
        int separator = field.indexOf('.');
        ObjectNode target = separator < 0 ? request : (ObjectNode) request.get(field.substring(0, separator));
        String name = separator < 0 ? field : field.substring(separator + 1);
        switch (mutation) {
            case "missing":
                target.remove(name);
                break;
            case "null":
                target.putNull(name);
                break;
            case "empty":
                target.put(name, "");
                break;
            case "blank":
                target.put(name, " \t\r\n");
                break;
            default:
                throw new IllegalArgumentException(mutation);
        }
        mvc.perform(post(path).contentType(MediaType.APPLICATION_JSON).content(mapper.writeValueAsBytes(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message", containsString(field + ": must not be")))
                .andExpect(result -> {
                    MethodArgumentNotValidException exception = assertInstanceOf(MethodArgumentNotValidException.class, result.getResolvedException());
                    assertEquals(List.of(field), exception.getBindingResult().getFieldErrors().stream().map(FieldError::getField).toList());
                });
        verifyNoInteractions(publisher);
    }

    @Test
    void acceptsLegacyMetadataWithoutCompatibilityFields() throws Exception {
        ObjectNode request = fixture("metadata");
        removeCompatibilityFields(request);
        request.put("contextPath", "");
        mvc.perform(post(RegistrationProtocol.META_PATH).contentType(MediaType.APPLICATION_JSON)
                        .content(mapper.writeValueAsBytes(request)))
                .andExpect(status().isOk()).andExpect(content().string(RegistrationProtocol.SUCCESS));
        ArgumentCaptor<DataTypeParent> published = forClass(DataTypeParent.class);
        verify(publisher).publish(published.capture());
        MetaDataRegisterDTO metadata = assertInstanceOf(MetaDataRegisterDTO.class, published.getValue());
        assertEquals(metadata.getPath(), metadata.getRuleName());
        verifyNoMoreInteractions(publisher);
    }

    @Test
    void acceptsLegacyUriWithoutCompatibilityFields() throws Exception {
        ObjectNode request = fixture("uri");
        removeCompatibilityFields(request);
        mvc.perform(post(RegistrationProtocol.URI_PATH).contentType(MediaType.APPLICATION_JSON)
                        .content(mapper.writeValueAsBytes(request)))
                .andExpect(status().isOk()).andExpect(content().string(RegistrationProtocol.SUCCESS));
        verify(publisher).publish(any(DataTypeParent.class));
        verifyNoMoreInteractions(publisher);
    }

    @Test
    void acceptsLegacyOfflineWithoutCompatibilityFields() throws Exception {
        ObjectNode request = fixture("offline");
        removeCompatibilityFields(request);
        request.put("contextPath", "");
        mvc.perform(post(RegistrationProtocol.OFFLINE_PATH).contentType(MediaType.APPLICATION_JSON)
                        .content(mapper.writeValueAsBytes(request)))
                .andExpect(status().isOk()).andExpect(content().string(RegistrationProtocol.SUCCESS));
        verify(publisher).publish(any(DataTypeParent.class));
        verifyNoMoreInteractions(publisher);
    }

    @Test
    void acceptsLegacyMcpMetadataWithoutCompatibilityFields() throws Exception {
        ObjectNode request = fixture("mcp");
        ObjectNode metadata = (ObjectNode) request.get("metaDataRegisterDTO");
        removeCompatibilityFields(metadata);
        metadata.put("contextPath", "");
        mvc.perform(post(RegistrationProtocol.MCP_TOOLS_PATH).contentType(MediaType.APPLICATION_JSON)
                        .content(mapper.writeValueAsBytes(request)))
                .andExpect(status().isOk()).andExpect(content().string(RegistrationProtocol.SUCCESS));
        ArgumentCaptor<DataTypeParent> published = forClass(DataTypeParent.class);
        verify(publisher).publish(published.capture());
        McpToolsRegisterDTO mcpTools = assertInstanceOf(McpToolsRegisterDTO.class, published.getValue());
        assertEquals(mcpTools.getMetaDataRegisterDTO().getPath(), mcpTools.getMetaDataRegisterDTO().getRuleName());
        verifyNoMoreInteractions(publisher);
    }

    @ParameterizedTest
    @MethodSource("invalidCompatibilityFields")
    void rejectsWhitespaceCompatibilityFields(final String fixture, final String path, final String field) throws Exception {
        ObjectNode request = fixture(fixture);
        request.put(field, " \t\r\n");
        mvc.perform(post(path).contentType(MediaType.APPLICATION_JSON).content(mapper.writeValueAsBytes(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message", containsString(field + ": must not be blank")));
        verifyNoInteractions(publisher);
    }

    private static Stream<Arguments> validRequests() {
        return Stream.of(Arguments.of("metadata", RegistrationProtocol.META_PATH), Arguments.of("uri", RegistrationProtocol.URI_PATH),
                Arguments.of("api-doc", RegistrationProtocol.API_DOC_PATH), Arguments.of("discovery-config", RegistrationProtocol.DISCOVERY_CONFIG_PATH),
                Arguments.of("mcp", RegistrationProtocol.MCP_TOOLS_PATH), Arguments.of("offline", RegistrationProtocol.OFFLINE_PATH));
    }

    private static Stream<Arguments> invalidCompatibilityFields() {
        return Stream.of(Arguments.of("metadata", RegistrationProtocol.META_PATH, "contextPath"),
                Arguments.of("metadata", RegistrationProtocol.META_PATH, "ruleName"),
                Arguments.of("metadata", RegistrationProtocol.META_PATH, "host"),
                Arguments.of("uri", RegistrationProtocol.URI_PATH, "contextPath"),
                Arguments.of("uri", RegistrationProtocol.URI_PATH, "host"),
                Arguments.of("offline", RegistrationProtocol.OFFLINE_PATH, "contextPath"),
                Arguments.of("offline", RegistrationProtocol.OFFLINE_PATH, "host"));
    }

    private static Stream<Arguments> invalidFields() {
        return Stream.of(
                new String[]{"metadata", RegistrationProtocol.META_PATH, "appName,path,rpcType"},
                new String[]{"uri", RegistrationProtocol.URI_PATH, "appName,rpcType"},
                new String[]{"offline", RegistrationProtocol.OFFLINE_PATH, "appName,rpcType"},
                new String[]{"api-doc", RegistrationProtocol.API_DOC_PATH, "contextPath,apiPath,rpcType,httpMethod,eventType"},
                new String[]{"discovery-config", RegistrationProtocol.DISCOVERY_CONFIG_PATH, "selectorName,name,discoveryType,serverList,pluginName"},
                new String[]{"mcp", RegistrationProtocol.MCP_TOOLS_PATH, "mcpConfig,metaDataRegisterDTO"},
                new String[]{"mcp", RegistrationProtocol.MCP_TOOLS_PATH, "metaDataRegisterDTO.appName,metaDataRegisterDTO.path,metaDataRegisterDTO.rpcType"})
                .flatMap(spec -> Arrays.stream(spec[2].split(",")).flatMap(field -> {
                    boolean stringField = !List.of("httpMethod", "eventType", "metaDataRegisterDTO").contains(field);
                    return (stringField ? Stream.of("missing", "null", "empty", "blank") : Stream.of("missing", "null"))
                            .map(mutation -> Arguments.of(spec[0], spec[1], field, mutation));
                }));
    }

    private static void removeCompatibilityFields(final ObjectNode node) {
        node.remove("contextPath");
        node.remove("ruleName");
        node.remove("host");
    }

    private ObjectNode fixture(final String name) throws Exception {
        try (InputStream input = Objects.requireNonNull(getClass().getResourceAsStream("/registration/v1/" + name + ".json"))) {
            return (ObjectNode) mapper.readTree(input);
        }
    }
}
