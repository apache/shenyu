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
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.register.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Objects;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for {@link ShenyuClientHttpRegistryController}.
 */
@ExtendWith(MockitoExtension.class)
public final class ShenyuHttpRegistryControllerTest {

    private final ObjectMapper objectMapper = new ObjectMapper();

    private MockMvc mockMvc;

    @Mock
    private ShenyuClientServerRegisterPublisher publisher;

    @Mock
    private NamespaceService namespaceService;

    @InjectMocks
    private ShenyuClientHttpRegistryController controller;

    @BeforeEach
    public void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(controller).build();
    }

    @ParameterizedTest
    @MethodSource("registrationCases")
    void testRegistrationPublishesRequest(final String endpoint, final Class<? extends DataTypeParent> dtoType,
                                         final String body, final String namespaceId) throws Exception {
        ObjectNode request = (ObjectNode) objectMapper.readTree(body);
        if (Objects.nonNull(namespaceId)) {
            request.put("namespaceId", namespaceId);
        }
        mockMvc.perform(post("/shenyu-client/" + endpoint)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isOk())
                .andExpect(content().string(ShenyuResultMessage.SUCCESS));

        request.put("namespaceId", Objects.isNull(namespaceId) ? Constants.SYS_DEFAULT_NAMESPACE_ID : namespaceId);
        assertPublishedRequest(dtoType, request);
    }

    private static Stream<Arguments> registrationCases() {
        return Stream.of(null, "tenant-a").flatMap(namespace -> Stream.of(
                Arguments.of("register-metadata", MetaDataRegisterDTO.class,
                        "{\"appName\":\"app\",\"path\":\"/orders\",\"rpcType\":\"http\",\"enabled\":true}", namespace),
                Arguments.of("register-uri", URIRegisterDTO.class,
                        "{\"appName\":\"app\",\"host\":\"127.0.0.1\",\"port\":8080,\"rpcType\":\"http\"}", namespace),
                Arguments.of("register-discoveryConfig", DiscoveryConfigRegisterDTO.class,
                        "{\"name\":\"discovery\",\"pluginName\":\"divide\",\"discoveryType\":\"zookeeper\",\"serverList\":\"localhost:2181\"}", namespace),
                Arguments.of("register-mcp", McpToolsRegisterDTO.class,
                        "{\"mcpConfig\":\"{}\",\"metaDataRegisterDTO\":{\"appName\":\"tools\",\"path\":\"/tool\"}}", namespace),
                Arguments.of("offline", URIRegisterDTO.class,
                        "{\"appName\":\"app\",\"host\":\"127.0.0.1\",\"port\":8080,\"eventType\":\"OFFLINE\"}", namespace)));
    }

    @Test
    void testRegisterApiDoc() throws Exception {
        JsonNode request = objectMapper.readTree("{\"apiPath\":\"/orders\",\"contextPath\":\"/app\",\"httpMethod\":0,\"document\":\"orders api\"}");
        mockMvc.perform(post("/shenyu-client/register-apiDoc")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isOk())
                .andExpect(content().string(ShenyuResultMessage.SUCCESS));
        assertPublishedRequest(ApiDocRegisterDTO.class, request);
    }

    @ParameterizedTest
    @ValueSource(strings = {"register-metadata", "register-uri", "register-apiDoc", "register-discoveryConfig", "register-mcp", "offline"})
    void testMissingRequestBodyIsRejected(final String endpoint) throws Exception {
        mockMvc.perform(post("/shenyu-client/" + endpoint).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest());
        verifyNoInteractions(publisher);
    }

    @ParameterizedTest
    @ValueSource(strings = {"register-metadata", "register-uri", "register-apiDoc", "register-discoveryConfig", "register-mcp", "offline"})
    void testMalformedRequestBodyIsRejected(final String endpoint) throws Exception {
        mockMvc.perform(post("/shenyu-client/" + endpoint).contentType(MediaType.APPLICATION_JSON).content("{"))
                .andExpect(status().isBadRequest());
        verifyNoInteractions(publisher);
    }

    @Test
    void testExistingNamespace() {
        when(namespaceService.findByNamespaceId("tenant-a")).thenReturn(new NamespaceVO());
        assertDoesNotThrow(() -> controller.checkClientNamespaceExist("tenant-a"));
        verify(namespaceService).findByNamespaceId("tenant-a");
    }

    @Test
    void testMissingNamespace() {
        when(namespaceService.findByNamespaceId("missing")).thenReturn(null);
        assertThrows(IllegalArgumentException.class, () -> controller.checkClientNamespaceExist("missing"));
        verify(namespaceService).findByNamespaceId("missing");
    }

    private void assertPublishedRequest(final Class<? extends DataTypeParent> dtoType, final JsonNode expected) {
        ArgumentCaptor<DataTypeParent> captor = ArgumentCaptor.forClass(DataTypeParent.class);
        verify(publisher).publish(captor.capture());
        DataTypeParent published = captor.getValue();
        assertEquals(dtoType, published.getClass());
        JsonNode actual = objectMapper.valueToTree(published);
        expected.fields().forEachRemaining(field -> assertJsonFields(field.getValue(), actual.get(field.getKey())));
        verifyNoMoreInteractions(publisher);
    }

    private void assertJsonFields(final JsonNode expected, final JsonNode actual) {
        if (expected.isObject()) {
            expected.fields().forEachRemaining(field -> assertJsonFields(field.getValue(), actual.get(field.getKey())));
        } else {
            assertEquals(expected, actual);
        }
    }
}
