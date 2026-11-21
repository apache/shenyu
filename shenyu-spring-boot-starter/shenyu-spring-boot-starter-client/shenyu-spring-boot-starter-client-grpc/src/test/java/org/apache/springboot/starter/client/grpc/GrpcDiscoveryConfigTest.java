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

package org.apache.springboot.starter.client.grpc;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.grpc.server.GrpcServerBuilder;
import org.apache.shenyu.client.grpc.server.GrpcServerRunner;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.spi.ExtensionLoader;
import org.apache.shenyu.springboot.starter.client.springmvc.ShenyuSpringMvcClientConfiguration;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class GrpcDiscoveryConfigTest {

    @Test
    void grpcOnlyRegistersItsOwnSelectorAndAddress() {
        assertGrpcDiscoveryRegistration();
    }

    @Test
    void grpcWithSpringMvcRegistersItsOwnSelectorAndAddress() {
        assertGrpcDiscoveryRegistration(ShenyuSpringMvcClientConfiguration.class);
    }

    private void assertGrpcDiscoveryRegistration(final Class<?>... additionalConfigurations) {
        HttpClientRegisterRepository adminRepository = mock(HttpClientRegisterRepository.class);
        ShenyuInstanceRegisterRepository instanceRepository = mock(ShenyuInstanceRegisterRepository.class);
        ExtensionLoader<ShenyuInstanceRegisterRepository> instanceLoader = mock();
        when(instanceLoader.getJoin("nacos")).thenReturn(instanceRepository);
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> repositoryFactory = mockStatic(ShenyuClientRegisterRepositoryFactory.class);
                MockedStatic<ShenyuClientRegisterEventPublisher> publisherFactory = mockStatic(ShenyuClientRegisterEventPublisher.class);
                MockedStatic<ExtensionLoader> extensionLoader = mockStatic(ExtensionLoader.class);
                MockedConstruction<GrpcServerRunner> serverRunner = mockConstruction(GrpcServerRunner.class)) {
            repositoryFactory.when(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any())).thenReturn(adminRepository);
            publisherFactory.when(ShenyuClientRegisterEventPublisher::getInstance).thenReturn(mock(ShenyuClientRegisterEventPublisher.class));
            extensionLoader.when(() -> ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class)).thenReturn(instanceLoader);
            new ApplicationContextRunner()
                    .withBean(HttpClientRegisterRepository.class, () -> adminRepository, definition -> definition.setPrimary(true))
                    .withBean(ShenyuDiscoveryConfig.class, GrpcDiscoveryConfigTest::discoveryConfig)
                    .withBean(GrpcServerBuilder.class, () -> mock(GrpcServerBuilder.class))
                    .withConfiguration(AutoConfigurations.of(ConfigurationPropertiesAutoConfiguration.class,
                            ShenyuGrpcClientConfiguration.class, ShenyuGrpcDiscoveryConfiguration.class))
                    .withUserConfiguration(additionalConfigurations)
                    .withPropertyValues(
                            "spring.application.name=discovery-test",
                            "shenyu.namespace=grpc-discovery-test",
                            "shenyu.discovery.register=true",
                            "shenyu.discovery.serverList=127.0.0.1:8848",
                            "shenyu.client.http.props[host]=127.0.0.1",
                            "shenyu.client.http.props[port]=8080",
                            "shenyu.client.http.props[contextPath]=/api",
                            "shenyu.client.grpc.props[host]=127.0.0.2",
                            "shenyu.client.grpc.props[port]=50051",
                            "shenyu.client.grpc.props[ipAndPort]=127.0.0.2:50051",
                            "shenyu.client.grpc.props[contextPath]=/grpc")
                    .run(context -> {
                        assertNull(context.getStartupFailure());
                        ArgumentCaptor<DiscoveryConfigRegisterDTO> discovery = ArgumentCaptor.forClass(DiscoveryConfigRegisterDTO.class);
                        ArgumentCaptor<InstanceEntity> instance = ArgumentCaptor.forClass(InstanceEntity.class);
                        verify(adminRepository).doPersistDiscoveryConfig(discovery.capture());
                        verify(instanceRepository).persistInstance(instance.capture());
                        assertAll(
                                () -> assertEquals(PluginEnum.GRPC.getName(), discovery.getValue().getPluginName()),
                                () -> assertEquals("grpc-discovery-test", discovery.getValue().getNamespaceId()),
                                () -> assertEquals("/grpc", discovery.getValue().getSelectorName()),
                                () -> assertEquals("127.0.0.2", instance.getValue().getHost()),
                                () -> assertEquals(50051, instance.getValue().getPort()));
                    });
        }
    }

    private static ShenyuDiscoveryConfig discoveryConfig() {
        ShenyuDiscoveryConfig config = new ShenyuDiscoveryConfig();
        config.setServerList("127.0.0.1:8848");
        config.setType("nacos");
        config.setRegisterPath("/discovery-test");
        return config;
    }
}
