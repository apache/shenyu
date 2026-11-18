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

package org.apache.shenyu.client.core.register.registrar;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ClientRegisterConfigImpl;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.springframework.mock.env.MockEnvironment;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/** URI instance publication and retry ownership. */
class UriApiRegistrarTest {

    private final ClientRegisterConfig config = mock(ClientRegisterConfig.class);

    private final ShenyuClientRegisterEventPublisher publisher = mock(ShenyuClientRegisterEventPublisher.class);

    private UriApiRegistrarImplImpl registrar;

    @BeforeEach
    void setUp() {
        when(config.getNamespace()).thenReturn(List.of("one", "two"));
        when(config.getHost()).thenReturn("127.0.0.1");
        when(config.getPort()).thenReturn(8080);
        when(config.getAppName()).thenReturn("app");
        when(config.getContextPath()).thenReturn("/app");
        registrar = new UriApiRegistrarImplImpl(config);
        ReflectionTestUtils.setField(registrar, "publisher", publisher);
    }

    @Test
    void testBeanAndApisShareIdentityEvenAfterHeartbeatMutation() throws Exception {
        registrar.register(bean());
        ArgumentCaptor<URIRegisterDTO> events = ArgumentCaptor.forClass(URIRegisterDTO.class);
        verify(publisher, times(2)).publishEvent(events.capture());
        events.getAllValues().forEach(uri -> uri.setEventType(EventType.HEARTBEAT));
        registrar.register(bean());
        verify(publisher, times(2)).publishEvent(any());
        assertEquals(URIRegisterDTO.class, registrar.registerDataType());
    }

    @Test
    void testEnqueueFailureCanRetryWithoutRepeatingPublishedNamespaces() throws Exception {
        doThrow(new IllegalStateException("queue unavailable")).doNothing().when(publisher).publishEvent(any());
        ApiBean bean = bean();
        bean.setStatus(ApiBean.Status.REGISTRABLE_BEAN);
        registrar.register(bean);
        verify(publisher, times(2)).publishEvent(any());
        registrar.register(bean);
        ArgumentCaptor<URIRegisterDTO> events = ArgumentCaptor.forClass(URIRegisterDTO.class);
        verify(publisher, times(3)).publishEvent(events.capture());
        assertEquals(List.of("one", "two", "one"), events.getAllValues().stream().map(URIRegisterDTO::getNamespaceId).toList());
    }

    @Test
    void testChangedInstanceIdentityIsNotSuppressed() throws Exception {
        registrar.register(bean());
        when(config.getAppName()).thenReturn("other-app");
        registrar.register(bean());
        when(config.getContextPath()).thenReturn("/other");
        registrar.register(bean());
        when(config.getPort()).thenReturn(8081);
        registrar.register(bean());
        when(config.getHost()).thenReturn("127.0.0.2");
        registrar.register(bean());
        verify(publisher, times(10)).publishEvent(any());
    }

    @Test
    void testRpcTypesHaveIndependentIdentities() throws Exception {
        registrar.register(bean());
        ApiBean other = bean();
        other.addProperties("rpcType", "grpc");
        other.getApiDefinitions().forEach(api -> api.addProperties("rpcType", "grpc"));
        registrar.register(other);
        verify(publisher, times(4)).publishEvent(any());
    }

    @Test
    void testInvalidPortIsNotPublished() {
        for (int port : List.of(-2, 65536)) {
            when(config.getPort()).thenReturn(port);
            assertThrows(IllegalArgumentException.class, () -> registrar.register(bean()));
        }
        verifyNoInteractions(publisher);
    }

    @Test
    void testPendingPortCanPublishOnRediscovery() throws Exception {
        for (int port : List.of(0, -1)) {
            when(config.getPort()).thenReturn(port);
            registrar.register(bean());
        }
        verifyNoInteractions(publisher);
        when(config.getPort()).thenReturn(8080);
        registrar.register(bean());
        verify(publisher, times(2)).publishEvent(any());
    }

    @ParameterizedTest
    @ValueSource(strings = {"ws://", "wss://"})
    void testWebSocketProtocolIsPublished(final String protocol) throws Exception {
        ShenyuClientConfig.ClientPropertiesConfig websocket = new ShenyuClientConfig.ClientPropertiesConfig();
        websocket.getProps().setProperty("host", "127.0.0.1");
        websocket.getProps().setProperty("port", "8080");
        if ("wss://".equals(protocol)) {
            websocket.getProps().setProperty("protocol", protocol);
        }
        ShenyuClientConfig client = new ShenyuClientConfig();
        client.getClient().put(RpcTypeEnum.WEB_SOCKET.getName(), websocket);
        UriApiRegistrarImplImpl websocketRegistrar = new UriApiRegistrarImplImpl(
                new ClientRegisterConfigImpl(client, RpcTypeEnum.WEB_SOCKET, null, new MockEnvironment()));
        ReflectionTestUtils.setField(websocketRegistrar, "publisher", publisher);
        ApiBean bean = bean();
        bean.addProperties("rpcType", RpcTypeEnum.WEB_SOCKET.getName());
        bean.setStatus(ApiBean.Status.REGISTRABLE_BEAN);
        websocketRegistrar.register(bean);
        ArgumentCaptor<URIRegisterDTO> event = ArgumentCaptor.forClass(URIRegisterDTO.class);
        verify(publisher).publishEvent(event.capture());
        assertEquals(protocol, event.getValue().getProtocol());
    }

    private ApiBean bean() throws Exception {
        ApiBean bean = new ApiBean("http", "endpoints", this, "/api");
        bean.addApiDefinition(Object.class.getMethod("toString"), "/one");
        bean.addApiDefinition(Object.class.getMethod("hashCode"), "/two");
        bean.setStatus(ApiBean.Status.REGISTRABLE);
        bean.getApiDefinitions().forEach(api -> api.setStatus(ApiBean.Status.REGISTRABLE));
        return bean;
    }
}
