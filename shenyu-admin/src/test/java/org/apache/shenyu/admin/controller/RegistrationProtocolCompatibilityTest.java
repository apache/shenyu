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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.admin.config.RegistrationProtocolInterceptor;
import org.apache.shenyu.admin.register.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.admin.service.impl.InstanceCheckService;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
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
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Replays published wire fixtures without importing or constructing client implementation classes.
 */
@ExtendWith(MockitoExtension.class)
class RegistrationProtocolCompatibilityTest {

    private final ObjectMapper mapper = new ObjectMapper();

    @Mock
    private ShenyuClientServerRegisterPublisher publisher;

    @Mock
    private InstanceInfoService instanceInfoService;

    @Spy
    @InjectMocks
    private InstanceCheckService instanceCheckService;

    @InjectMocks
    private ShenyuClientHttpRegistryController controller;

    private MockMvc mvc;

    @BeforeEach
    void setUp() {
        mvc = MockMvcBuilders.standaloneSetup(controller, new InstanceController(instanceInfoService, instanceCheckService))
                .addInterceptors(new RegistrationProtocolInterceptor()).build();
    }

    @ParameterizedTest
    @MethodSource("acceptedRequests")
    void acceptsV1Fixtures(final String fixture, final String path, final String version, final boolean unknownFields) throws Exception {
        ObjectNode expected = (ObjectNode) mapper.readTree(fixture(fixture));
        ObjectNode body = expected.deepCopy();
        if (unknownFields) {
            body.putObject("futureOptionalField").put("enabled", true);
            if (body.has("metaDataRegisterDTO")) {
                ((ObjectNode) body.get("metaDataRegisterDTO")).put("futureNestedField", "ignored");
            }
        }
        MockHttpServletRequestBuilder request = post(path).contentType(MediaType.APPLICATION_JSON).content(mapper.writeValueAsBytes(body))
                .header(RegistrationProtocol.CLIENT_VERSION_HEADER, "independently-released-client");
        if (Objects.nonNull(version)) {
            request.header(RegistrationProtocol.VERSION_HEADER, version);
        }
        mvc.perform(request).andExpect(status().isOk()).andExpect(content().string("success"))
                .andExpect(header().string(RegistrationProtocol.VERSION_HEADER, "1"))
                .andExpect(header().string(RegistrationProtocol.CAPABILITIES_HEADER, RegistrationProtocol.V1_CAPABILITIES));
        if (RegistrationProtocol.BEAT_URI_PATH.equals(path)) {
            ArgumentCaptor<InstanceBeatInfoDTO> captured = ArgumentCaptor.forClass(InstanceBeatInfoDTO.class);
            verify(instanceCheckService).handleBeatInfo(captured.capture());
            assertFields(expected, mapper.valueToTree(captured.getValue()));
            assertNotNull(instanceCheckService.getInstanceHealthBeatInfo(captured.getValue()));
        } else {
            ArgumentCaptor<DataTypeParent> captured = ArgumentCaptor.forClass(DataTypeParent.class);
            verify(publisher).publish(captured.capture());
            if (!RegistrationProtocol.API_DOC_PATH.equals(path)) {
                expected.put("namespaceId", Constants.SYS_DEFAULT_NAMESPACE_ID);
            }
            assertFields(expected, mapper.valueToTree(captured.getValue()));
        }
    }

    @ParameterizedTest
    @MethodSource("rejectedRequests")
    void rejectsUnsupportedVersionsBeforePublishing(final String fixture, final String path, final String version) throws Exception {
        mvc.perform(post(path).contentType(MediaType.APPLICATION_JSON).content(fixture(fixture))
                        .header(RegistrationProtocol.VERSION_HEADER, version))
                .andExpect(status().isBadRequest())
                .andExpect(content().string(RegistrationProtocol.UNSUPPORTED_VERSION))
                .andExpect(header().string(RegistrationProtocol.VERSION_HEADER, "1"));
        verifyNoInteractions(publisher, instanceCheckService);
    }

    @Test
    void rejectsConflictingVersionHeaders() throws Exception {
        mvc.perform(post(RegistrationProtocol.META_PATH).contentType(MediaType.APPLICATION_JSON).content(fixture("metadata"))
                        .header(RegistrationProtocol.VERSION_HEADER, "1", "2"))
                .andExpect(status().isBadRequest());
        verifyNoInteractions(publisher);
    }

    @Test
    void preservesMissingOptionalFieldDefaults() throws Exception {
        mvc.perform(post(RegistrationProtocol.META_PATH).contentType(MediaType.APPLICATION_JSON)
                        .content("{\"appName\":\"legacy\",\"path\":\"/legacy\",\"rpcType\":\"http\"}"))
                .andExpect(status().isOk()).andExpect(content().string("success"));
        ArgumentCaptor<MetaDataRegisterDTO> captured = ArgumentCaptor.forClass(MetaDataRegisterDTO.class);
        verify(publisher).publish(captured.capture());
        assertFalse(captured.getValue().isEnabled());
        assertFalse(captured.getValue().isRegisterMetaData());
        assertFalse(captured.getValue().getAddPrefixed());
        assertEquals(0, captured.getValue().getTimeMillis());
        assertEquals(Constants.SYS_DEFAULT_NAMESPACE_ID, captured.getValue().getNamespaceId());
    }

    private static Stream<Arguments> acceptedRequests() {
        return fixtures().flatMap(fixture -> Stream.of(null, "1").flatMap(version -> Stream.of(false, true)
                .map(unknown -> Arguments.of(fixture[0], fixture[1], version, unknown))));
    }

    private static Stream<Arguments> rejectedRequests() {
        return fixtures().flatMap(fixture -> Stream.of("", "2", "v1", "1,2", "01")
                .map(version -> Arguments.of(fixture[0], fixture[1], version)));
    }

    private static Stream<String[]> fixtures() {
        return Stream.of(new String[]{"metadata", RegistrationProtocol.META_PATH}, new String[]{"uri", RegistrationProtocol.URI_PATH},
                new String[]{"uri-heartbeat", RegistrationProtocol.URI_PATH}, new String[]{"offline", RegistrationProtocol.OFFLINE_PATH},
                new String[]{"api-doc", RegistrationProtocol.API_DOC_PATH}, new String[]{"discovery-config", RegistrationProtocol.DISCOVERY_CONFIG_PATH},
                new String[]{"mcp", RegistrationProtocol.MCP_TOOLS_PATH}, new String[]{"heartbeat", RegistrationProtocol.BEAT_URI_PATH});
    }

    private String fixture(final String name) throws Exception {
        try (InputStream input = Objects.requireNonNull(getClass().getResourceAsStream("/registration/v1/" + name + ".json"))) {
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private void assertFields(final JsonNode expected, final JsonNode actual) {
        assertNotNull(actual);
        if (expected.isObject()) {
            expected.fields().forEachRemaining(field -> assertFields(field.getValue(), actual.get(field.getKey())));
        } else {
            assertEquals(expected, actual);
        }
    }
}
