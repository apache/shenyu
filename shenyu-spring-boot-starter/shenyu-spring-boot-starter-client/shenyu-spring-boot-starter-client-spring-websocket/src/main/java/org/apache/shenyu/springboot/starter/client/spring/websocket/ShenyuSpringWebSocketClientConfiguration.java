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

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ClientRegisterConfigImpl;
import org.apache.shenyu.client.spring.websocket.init.SpringWebSocketClientEventListener;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.VersionUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

import java.util.Objects;
import java.util.Properties;

/**
 * The type shenyu websocket client http configuration.
 */
@Configuration
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
@ConditionalOnProperty(value = "shenyu.register.enabled", matchIfMissing = true, havingValue = "true")
public class ShenyuSpringWebSocketClientConfiguration {

    static {
        VersionUtils.checkDuplicate(ShenyuSpringWebSocketClientConfiguration.class);
    }

    /**
     * Spring web socket client event listener.
     *
     * @param clientConfig                   the client config
     * @param shenyuClientRegisterRepository the shenyu client register repository
     * @param env                            env
     * @return the spring web socket client event listener
     */
    @Bean
    public SpringWebSocketClientEventListener springWebSocketClientEventListener(
            final ShenyuClientConfig clientConfig,
            final Environment env,
            final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        ShenyuClientConfig.ClientPropertiesConfig clientPropertiesConfig = clientConfig.getClient().get(RpcTypeEnum.WEB_SOCKET.getName());
        Properties props = Objects.isNull(clientPropertiesConfig) ? null : clientPropertiesConfig.getProps();
        String discoveryMode = env.getProperty("shenyu.discovery.type", ShenyuClientConstants.DISCOVERY_LOCAL_MODE);
        if (Objects.nonNull(props)) {
            props.setProperty(ShenyuClientConstants.DISCOVERY_LOCAL_MODE_KEY, Boolean.valueOf(ShenyuClientConstants.DISCOVERY_LOCAL_MODE.equals(discoveryMode)).toString());
        }
        return new SpringWebSocketClientEventListener(clientConfig, shenyuClientRegisterRepository);
    }

    /**
     * ClientRegisterConfig Bean.
     *
     * @param shenyuClientConfig shenyuClientConfig
     * @param applicationContext applicationContext
     * @param env                env
     * @return clientRegisterConfig
     */
    @Bean("webSocketClientRegisterConfig")
    public ClientRegisterConfig clientRegisterConfig(final ShenyuClientConfig shenyuClientConfig,
                                                     final ApplicationContext applicationContext,
                                                     final Environment env) {
        return new ClientRegisterConfigImpl(shenyuClientConfig, RpcTypeEnum.WEB_SOCKET, applicationContext, env);
    }

}
