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

package org.apache.shenyu.register.client.http;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.client.http.utils.RuntimeUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.io.IOException;
import java.util.Optional;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;

/**
 * Test cases for {@link HttpClientRegisterRepository}.
 */
public final class HttpClientRegisterRepositoryTest {

    private static final String FIRST_SERVER = "http://localhost:9095";

    private static final String SECOND_SERVER = "http://localhost:9096";

    private static final String TOKEN = "test-token";

    private HttpClientRegisterRepository repository;

    @BeforeEach
    public void setUp() {
        repository = new HttpClientRegisterRepository(config(FIRST_SERVER));
    }

    @Test
    public void persistUriShouldRegisterToEveryServer() {
        HttpClientRegisterRepository multiServerRepository = new HttpClientRegisterRepository(config(
                FIRST_SERVER + "," + SECOND_SERVER));
        URIRegisterDTO uriRegisterDTO = uriRegisterDTO();

        try (MockedStatic<RegisterUtils> registerUtils = mockStatic(RegisterUtils.class);
             MockedStatic<RuntimeUtils> runtimeUtils = mockStatic(RuntimeUtils.class)) {
            runtimeUtils.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            registerUtils.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of(TOKEN));

            multiServerRepository.persistURI(uriRegisterDTO);

            registerUtils.verify(() -> RegisterUtils.doRegister(anyString(),
                    eq(FIRST_SERVER + Constants.URI_PATH), eq(Constants.URI), eq(TOKEN)));
            registerUtils.verify(() -> RegisterUtils.doRegister(anyString(),
                    eq(SECOND_SERVER + Constants.URI_PATH), eq(Constants.URI), eq(TOKEN)));
        }
    }

    @Test
    public void persistUriShouldSkipRegistrationWhenPortIsUsedByAnotherProcess() {
        URIRegisterDTO uriRegisterDTO = uriRegisterDTO();

        try (MockedStatic<RegisterUtils> registerUtils = mockStatic(RegisterUtils.class);
             MockedStatic<RuntimeUtils> runtimeUtils = mockStatic(RuntimeUtils.class)) {
            runtimeUtils.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(true);

            repository.doPersistURI(uriRegisterDTO);

            registerUtils.verifyNoInteractions();
        }
    }

    @Test
    public void persistInterfaceApiDocMcpToolsAndDiscoveryShouldUseExpectedPaths() {
        MetaDataRegisterDTO metaData = metaDataRegisterDTO();
        ApiDocRegisterDTO apiDocRegisterDTO = ApiDocRegisterDTO.builder()
                .contextPath("/demo")
                .apiPath("/hello")
                .httpMethod(1)
                .rpcType("http")
                .build();
        McpToolsRegisterDTO mcpToolsRegisterDTO = new McpToolsRegisterDTO();
        mcpToolsRegisterDTO.setMetaDataRegisterDTO(metaData);
        DiscoveryConfigRegisterDTO discoveryConfigRegisterDTO = DiscoveryConfigRegisterDTO.builder().build();

        try (MockedStatic<RegisterUtils> registerUtils = mockStatic(RegisterUtils.class);
             MockedStatic<RuntimeUtils> runtimeUtils = mockStatic(RuntimeUtils.class)) {
            runtimeUtils.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            registerUtils.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of(TOKEN));

            repository.persistInterface(metaData);
            repository.persistApiDoc(apiDocRegisterDTO);
            repository.persistMcpTools(mcpToolsRegisterDTO);
            repository.doPersistDiscoveryConfig(discoveryConfigRegisterDTO);

            registerUtils.verify(() -> RegisterUtils.doRegister(anyString(),
                    eq(FIRST_SERVER + Constants.META_PATH), eq(Constants.META_TYPE), eq(TOKEN)));
            registerUtils.verify(() -> RegisterUtils.doRegister(anyString(),
                    eq(FIRST_SERVER + Constants.API_DOC_PATH), eq(Constants.API_DOC_TYPE), eq(TOKEN)));
            registerUtils.verify(() -> RegisterUtils.doRegister(anyString(),
                    eq(FIRST_SERVER + Constants.MCP_TOOLS_PATH), eq(Constants.MCP_TOOLS_TYPE), eq(TOKEN)));
            registerUtils.verify(() -> RegisterUtils.doRegister(anyString(),
                    eq(FIRST_SERVER + Constants.DISCOVERY_CONFIG_PATH), eq(Constants.DISCOVERY_CONFIG_TYPE), eq(TOKEN)));
        }
    }

    @Test
    public void heartbeatAndOfflineShouldSendExpectedRequests() {
        URIRegisterDTO heartbeatDTO = uriRegisterDTO();
        URIRegisterDTO offlineDTO = uriRegisterDTO();

        try (MockedStatic<RegisterUtils> registerUtils = mockStatic(RegisterUtils.class);
             MockedStatic<RuntimeUtils> runtimeUtils = mockStatic(RuntimeUtils.class)) {
            runtimeUtils.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            registerUtils.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of(TOKEN));

            repository.sendHeartbeat(heartbeatDTO);
            repository.offline(offlineDTO);

            registerUtils.verify(() -> RegisterUtils.doHeartBeat(anyString(),
                    eq(FIRST_SERVER + Constants.URI_PATH), eq(Constants.HEARTBEAT), eq(TOKEN)));
            registerUtils.verify(() -> RegisterUtils.doUnregister(anyString(),
                    eq(FIRST_SERVER + Constants.OFFLINE_PATH), eq(TOKEN)));
        }
    }

    @Test
    public void heartbeatShouldSkipWhenPortIsUsedByAnotherProcess() {
        URIRegisterDTO heartbeatDTO = uriRegisterDTO();

        try (MockedStatic<RegisterUtils> registerUtils = mockStatic(RegisterUtils.class);
             MockedStatic<RuntimeUtils> runtimeUtils = mockStatic(RuntimeUtils.class)) {
            runtimeUtils.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(true);

            repository.sendHeartbeat(heartbeatDTO);

            registerUtils.verifyNoInteractions();
        }
    }

    @Test
    public void closeRepositoryShouldUnregisterEveryUriAndApiDoc() {
        try (MockedStatic<RegisterUtils> registerUtils = mockStatic(RegisterUtils.class);
             MockedStatic<RuntimeUtils> runtimeUtils = mockStatic(RuntimeUtils.class)) {
            runtimeUtils.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            registerUtils.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of(TOKEN));

            URIRegisterDTO firstUri = uriRegisterDTO();
            URIRegisterDTO secondUri = URIRegisterDTO.builder().appName("demo").rpcType("http")
                    .host("127.0.0.1").port(18081).build();
            ApiDocRegisterDTO firstApiDoc = ApiDocRegisterDTO.builder().apiPath("/hello").build();
            ApiDocRegisterDTO secondApiDoc = ApiDocRegisterDTO.builder().apiPath("/goodbye").build();
            repository.persistURI(firstUri);
            repository.persistURI(secondUri);
            repository.persistApiDoc(firstApiDoc);
            repository.persistApiDoc(secondApiDoc);
            repository.closeRepository();

            registerUtils.verify(() -> RegisterUtils.doRegister(anyString(),
                    eq(FIRST_SERVER + Constants.URI_PATH), eq(Constants.URI), eq(TOKEN)), times(4));
            registerUtils.verify(() -> RegisterUtils.doRegister(anyString(),
                    eq(FIRST_SERVER + Constants.API_DOC_PATH), eq(Constants.API_DOC_TYPE), eq(TOKEN)), times(4));
        }
    }

    @Test
    public void repositoriesShouldTrackRegistrationsIndependently() {
        HttpClientRegisterRepository secondRepository = new HttpClientRegisterRepository(config(FIRST_SERVER));
        URIRegisterDTO firstUri = uriRegisterDTO();
        URIRegisterDTO secondUri = URIRegisterDTO.builder().appName("demo").rpcType("http")
                .host("127.0.0.1").port(18081).build();

        try (MockedStatic<RegisterUtils> registerUtils = mockStatic(RegisterUtils.class);
             MockedStatic<RuntimeUtils> runtimeUtils = mockStatic(RuntimeUtils.class)) {
            runtimeUtils.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            registerUtils.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of(TOKEN));
            repository.persistURI(firstUri);
            secondRepository.persistURI(secondUri);
            registerUtils.clearInvocations();

            repository.closeRepository();

            firstUri.setEventType(EventType.DELETED);
            registerUtils.verify(() -> RegisterUtils.doRegister(eq(GsonUtils.getInstance().toJson(firstUri)),
                    eq(FIRST_SERVER + Constants.URI_PATH), eq(Constants.URI), eq(TOKEN)));
            registerUtils.verify(() -> RegisterUtils.doRegister(eq(GsonUtils.getInstance().toJson(secondUri)),
                    eq(FIRST_SERVER + Constants.URI_PATH), eq(Constants.URI), eq(TOKEN)), never());
        }
    }

    @Test
    public void doPersistUriShouldThrowWhenEveryServerFails() throws IOException {
        try (MockedStatic<RegisterUtils> registerUtils = mockStatic(RegisterUtils.class);
             MockedStatic<RuntimeUtils> runtimeUtils = mockStatic(RuntimeUtils.class)) {
            runtimeUtils.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            registerUtils.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of(TOKEN));
            registerUtils.when(() -> RegisterUtils.doRegister(anyString(), anyString(), anyString(), anyString()))
                    .thenThrow(new IOException("register failed"));

            RuntimeException exception =
                    assertThrows(RuntimeException.class, () -> repository.doPersistURI(uriRegisterDTO()));

            assertTrue(exception.getCause() instanceof IOException);
            assertEquals("register failed", exception.getCause().getMessage());
        }
    }

    @Test
    public void loginFailureShouldSkipRegistration() {
        URIRegisterDTO uriRegisterDTO = uriRegisterDTO();

        try (MockedStatic<RegisterUtils> registerUtils = mockStatic(RegisterUtils.class);
             MockedStatic<RuntimeUtils> runtimeUtils = mockStatic(RuntimeUtils.class)) {
            runtimeUtils.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            registerUtils.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.empty());

            assertThrows(RuntimeException.class, () -> repository.doPersistURI(uriRegisterDTO));
            registerUtils.verify(() -> RegisterUtils.doRegister(anyString(),
                    eq(FIRST_SERVER + Constants.URI_PATH), eq(Constants.URI), eq(TOKEN)), never());
        }
    }

    private ShenyuRegisterCenterConfig config(final String serverLists) {
        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        return new ShenyuRegisterCenterConfig("http", serverLists, props);
    }

    private URIRegisterDTO uriRegisterDTO() {
        return URIRegisterDTO.builder()
                .appName("demo")
                .rpcType("http")
                .host("127.0.0.1")
                .port(18080)
                .build();
    }

    private MetaDataRegisterDTO metaDataRegisterDTO() {
        MetaDataRegisterDTO metaDataRegisterDTO = new MetaDataRegisterDTO();
        metaDataRegisterDTO.setAppName("demo");
        metaDataRegisterDTO.setRpcType("http");
        metaDataRegisterDTO.setHost("127.0.0.1");
        metaDataRegisterDTO.setPort(18080);
        metaDataRegisterDTO.setPath("/demo");
        return metaDataRegisterDTO;
    }

}
