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

package org.apache.shenyu.springboot.starter.client.mcp;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

/** MCP starter configuration validation before resource allocation. */
class ShenyuMcpClientConfigurationTest {

    private final ApplicationContextRunner applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuMcpClientConfiguration.class))
            .withUserConfiguration(Binding.class)
            .withPropertyValues("shenyu.register.register-type=http", "shenyu.register.server-lists=http://localhost:9095",
                    "shenyu.client.mcp.props.appName=fixture", "shenyu.client.mcp.props.contextPath=/mcp", "shenyu.client.mcp.props.port=8181");

    @Test
    void testValidConfigurationRetainsListener() {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class);
                MockedStatic<ShenyuClientRegisterEventPublisher> publishers = mockStatic(ShenyuClientRegisterEventPublisher.class)) {
            factory.when(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any())).thenReturn(mock(ShenyuClientRegisterRepository.class));
            publishers.when(ShenyuClientRegisterEventPublisher::getInstance).thenReturn(mock(ShenyuClientRegisterEventPublisher.class));
            applicationContextRunner.run(context -> assertThat(context).hasNotFailed().hasBean("mcpServiceEventListener"));
        }
    }

    @Test
    void testInvalidConfigurationPrecedesRepositoryCreation() {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class)) {
            factory.when(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any())).thenThrow(new AssertionError("repository reached before validation"));
            applicationContextRunner.withPropertyValues("shenyu.client.mcp.props.port=65536").run(context -> {
                assertThat(context).hasFailed();
                assertThat(context.getStartupFailure()).hasStackTraceContaining("shenyu.client.mcp.props.port");
                factory.verifyNoInteractions();
            });
        }
    }

    @Configuration(proxyBeanMethods = false)
    @EnableConfigurationProperties
    static class Binding {
    }
}
