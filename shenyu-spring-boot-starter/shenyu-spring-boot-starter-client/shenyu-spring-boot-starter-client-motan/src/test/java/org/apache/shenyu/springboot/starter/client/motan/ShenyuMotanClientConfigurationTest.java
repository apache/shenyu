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

package org.apache.shenyu.springboot.starter.client.motan;

import com.weibo.api.motan.config.springsupport.ProtocolConfigBean;
import org.apache.shenyu.client.motan.MotanServiceEventListener;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

/**
 * Test case for {@link ShenyuMotanClientConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class ShenyuMotanClientConfigurationTest {

    @Test
    public void testMotanServiceEventListener() {
        MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.ofNullable("token"));
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuMotanClientConfiguration.class))
            .withBean(ShenyuMotanClientConfigurationTest.class)
            .withBean(ProtocolConfigBean.class)
            .withPropertyValues(
                "debug=true",
                "shenyu.register.registerType=http",
                "shenyu.register.serverLists=http://localhost:9095",
                "shenyu.register.props.username=admin",
                "shenyu.register.props.password=123456",
                "shenyu.client.motan.props[contextPath]=/motan",
                "shenyu.client.motan.props[appName]=motan",
                "shenyu.client.motan.props[host]=127.0.0.1",
                "shenyu.client.motan.props[port]=8081",
                "shenyu.client.motan.basicServiceConfig.exportPort=8002"
            )
            .run(context -> {
                MotanServiceEventListener motanServiceEventListener = context.getBean("motanServiceEventListener", MotanServiceEventListener.class);
                assertNotNull(motanServiceEventListener);
            });
        registerUtilsMockedStatic.close();
    }
}
