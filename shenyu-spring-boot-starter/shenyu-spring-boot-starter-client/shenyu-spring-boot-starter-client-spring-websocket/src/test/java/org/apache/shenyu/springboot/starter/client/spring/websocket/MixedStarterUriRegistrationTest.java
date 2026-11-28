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

package org.apache.shenyu.springboot.starter.client.spring.websocket;

import org.apache.shenyu.client.auto.config.ClientRegisterConfiguration;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.core.register.extractor.RpcApiBeansExtractor;
import org.apache.shenyu.client.core.register.registrar.UriApiRegistrarImplImpl;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.springboot.starter.client.springmvc.ShenyuSpringMvcClientConfiguration;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/** URI publication with both the MVC and WebSocket starters in one context. */
class MixedStarterUriRegistrationTest {

    @Test
    void testLocalUriRegistrationUsesEachStarterConfiguration() {
        ShenyuClientRegisterEventPublisher publisher = mock(ShenyuClientRegisterEventPublisher.class);
        try (MockedStatic<ShenyuClientRegisterEventPublisher> singleton = mockStatic(ShenyuClientRegisterEventPublisher.class);
                MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class)) {
            singleton.when(ShenyuClientRegisterEventPublisher::getInstance).thenReturn(publisher);
            factory.when(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any())).thenReturn(mock(ShenyuClientRegisterRepository.class));
            new ApplicationContextRunner().withUserConfiguration(ClientRegisterConfiguration.class)
                    .withConfiguration(AutoConfigurations.of(ConfigurationPropertiesAutoConfiguration.class,
                            ShenyuSpringWebSocketClientConfiguration.class, ShenyuSpringMvcClientConfiguration.class))
                    .withBean(RpcApiBeansExtractor.class, () -> mock(RpcApiBeansExtractor.class))
                    .withPropertyValues("shenyu.register.registerType=http", "shenyu.register.serverLists=http://localhost:9095",
                            "shenyu.client.websocket.props[contextPath]=/websocket", "shenyu.client.websocket.props[appName]=websocket",
                            "shenyu.client.websocket.props[port]=8001",
                            "shenyu.client.http.props[appName]=http-fixture", "shenyu.client.http.props[contextPath]=/http",
                            "shenyu.client.http.props[host]=127.0.0.1", "shenyu.client.http.props[port]=8080", "shenyu.client.http.props[protocol]=https://",
                            "shenyu.client.websocket.props[host]=127.0.0.2", "shenyu.client.websocket.props[protocol]=wss://")
                    .run(context -> {
                        assertThat(context).hasNotFailed();
                        assertEquals(RpcTypeEnum.HTTP, context.getBean(ClientRegisterConfig.class).getRpcTypeEnum());
                        UriApiRegistrarImplImpl registrar = context.getBean(UriApiRegistrarImplImpl.class);
                        registrar.register(registrableBean(RpcTypeEnum.HTTP));
                        registrar.register(registrableBean(RpcTypeEnum.WEB_SOCKET));

                        ArgumentCaptor<URIRegisterDTO> uri = ArgumentCaptor.forClass(URIRegisterDTO.class);
                        verify(publisher, times(2)).publishEvent(uri.capture());
                        URIRegisterDTO http = uri.getAllValues().get(0);
                        assertEquals("http", http.getRpcType());
                        assertEquals("https://", http.getProtocol());
                        assertEquals("/http", http.getContextPath());
                        assertEquals("http-fixture", http.getAppName());
                        assertEquals("127.0.0.1", http.getHost());
                        assertEquals(8080, http.getPort());
                        URIRegisterDTO websocket = uri.getAllValues().get(1);
                        assertEquals("websocket", websocket.getRpcType());
                        assertEquals("wss://", websocket.getProtocol());
                        assertEquals("/websocket", websocket.getContextPath());
                        assertEquals("websocket", websocket.getAppName());
                        assertEquals("127.0.0.2", websocket.getHost());
                        assertEquals(8001, websocket.getPort());
                    });
        }
    }

    private static ApiBean registrableBean(final RpcTypeEnum rpcType) {
        ApiBean bean = new ApiBean(rpcType.getName(), rpcType.getName(), new Object());
        bean.setStatus(ApiBean.Status.REGISTRABLE_BEAN);
        return bean;
    }
}
