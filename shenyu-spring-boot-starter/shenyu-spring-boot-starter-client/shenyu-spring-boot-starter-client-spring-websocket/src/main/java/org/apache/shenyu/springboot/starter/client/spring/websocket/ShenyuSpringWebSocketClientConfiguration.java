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
import org.apache.shenyu.client.core.register.ClientDiscoveryConfigRefreshedEventListener;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ClientRegisterConfigImpl;
import org.apache.shenyu.client.core.register.InstanceRegisterListener;
import org.apache.shenyu.client.spring.websocket.init.SpringWebSocketClientEventListener;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.VersionUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

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
        Properties props = clientPropertiesConfig == null ? null : clientPropertiesConfig.getProps();
        String discoveryMode = env.getProperty("shenyu.discovery.type", ShenyuClientConstants.DISCOVERY_LOCAL_MODE);
        if (props != null) {
            props.setProperty(ShenyuClientConstants.DISCOVERY_LOCAL_MODE_KEY, Boolean.valueOf(ShenyuClientConstants.DISCOVERY_LOCAL_MODE.equals(discoveryMode)).toString());
        }
        return new SpringWebSocketClientEventListener(clientConfig.getClient().get(RpcTypeEnum.WEB_SOCKET.getName()), shenyuClientRegisterRepository);
    }

    /**
     * InstanceRegisterListener.
     *
     * @param eventListener         eventListener
     * @param shenyuDiscoveryConfig discoveryConfig
     * @return InstanceRegisterListener
     */
    @Bean("websocketInstanceRegisterListener")
    @ConditionalOnBean(ShenyuDiscoveryConfig.class)
    public InstanceRegisterListener instanceRegisterListener(final SpringWebSocketClientEventListener eventListener, final ShenyuDiscoveryConfig shenyuDiscoveryConfig) {
        DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
        discoveryUpstreamData.setProtocol(ShenyuClientConstants.WS);
        discoveryUpstreamData.setStatus(0);
        String weight = shenyuDiscoveryConfig.getProps().getOrDefault("discoveryUpstream.weight", "10").toString();
        discoveryUpstreamData.setWeight(Integer.parseInt(weight));
        discoveryUpstreamData.setUrl(eventListener.getHost() + ":" + eventListener.getPort());
        return new InstanceRegisterListener(discoveryUpstreamData, shenyuDiscoveryConfig);
    }

    /**
     * ClientRegisterConfig Bean.
     *
     * @param shenyuClientConfig shenyuClientConfig
     * @param applicationContext applicationContext
     * @param env                env
     * @return clientRegisterConfig
     */
    @Bean
    public ClientRegisterConfig clientRegisterConfig(final ShenyuClientConfig shenyuClientConfig,
                                                     final ApplicationContext applicationContext,
                                                     final Environment env) {
        return new ClientRegisterConfigImpl(shenyuClientConfig, RpcTypeEnum.WEB_SOCKET, applicationContext, env);
    }

    /**
     * clientDiscoveryConfigRefreshedEventListener.
     *
     * @param shenyuDiscoveryConfig        shenyuDiscoveryConfig
     * @param httpClientRegisterRepository httpClientRegisterRepository
     * @param clientRegisterConfig         clientRegisterConfig
     * @return ClientDiscoveryConfigRefreshedEventListener
     */
    @Bean("WebSocketClientDiscoveryConfigRefreshedEventListener")
    @ConditionalOnProperty(prefix = "shenyu.discovery", name = "serverList", matchIfMissing = false)
    @ConditionalOnBean(ShenyuDiscoveryConfig.class)
    public ClientDiscoveryConfigRefreshedEventListener clientDiscoveryConfigRefreshedEventListener(final ShenyuDiscoveryConfig shenyuDiscoveryConfig,
                                                                                                   final HttpClientRegisterRepository httpClientRegisterRepository,
                                                                                                   final ClientRegisterConfig clientRegisterConfig) {
        return new ClientDiscoveryConfigRefreshedEventListener(shenyuDiscoveryConfig, httpClientRegisterRepository, clientRegisterConfig, PluginEnum.WEB_SOCKET);
    }

}
