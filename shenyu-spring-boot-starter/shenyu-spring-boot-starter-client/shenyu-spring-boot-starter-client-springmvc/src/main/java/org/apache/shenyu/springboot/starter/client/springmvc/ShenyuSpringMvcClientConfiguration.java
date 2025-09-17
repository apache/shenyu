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

package org.apache.shenyu.springboot.starter.client.springmvc;

import java.util.Objects;
import java.util.Optional;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.auto.config.ClientRegisterConfiguration;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ClientRegisterConfigImpl;
import org.apache.shenyu.client.springmvc.init.SpringMvcClientEventListener;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.VersionUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuClientConfig.ClientPropertiesConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.core.env.Environment;

import java.util.Properties;

/**
 * The type shenyu spring mvc client configuration.
 */
@Configuration
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
@ConditionalOnProperty(value = "shenyu.register.enabled", matchIfMissing = true, havingValue = "true")
public class ShenyuSpringMvcClientConfiguration {

    static {
        VersionUtils.checkDuplicate(ShenyuSpringMvcClientConfiguration.class);
    }

    /**
     * Spring mvc client event listener.
     *
     * @param clientConfig                   the client config
     * @param shenyuClientRegisterRepository the shenyu client register repository
     * @param env                            the env
     * @return the spring mvc client event listener
     */
    @Bean
    @ConditionalOnMissingBean(ClientRegisterConfiguration.class)
    public SpringMvcClientEventListener springHttpClientEventListener(final ShenyuClientConfig clientConfig,
                                                                      final ShenyuClientRegisterRepository shenyuClientRegisterRepository,
                                                                      final Environment env) {
        ClientPropertiesConfig clientPropertiesConfig = clientConfig.getClient().get(RpcTypeEnum.HTTP.getName());
        Properties props = Optional.ofNullable(clientPropertiesConfig).map(ClientPropertiesConfig::getProps).orElse(null);
        String applicationName = env.getProperty("spring.application.name");
        String discoveryMode = env.getProperty("shenyu.discovery.type", ShenyuClientConstants.DISCOVERY_LOCAL_MODE);
        if (Objects.nonNull(props)) {
            String appName = props.getProperty(ShenyuClientConstants.APP_NAME);
            if (StringUtils.isBlank(appName) && StringUtils.isBlank(applicationName)) {
                throw new IllegalArgumentException("spring.application.name or shenyu.client.http.props.appName must not be empty");
            }
            if (StringUtils.isBlank(appName)) {
                props.setProperty(ShenyuClientConstants.APP_NAME, applicationName);
            }
            String contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
            if (StringUtils.isBlank(contextPath)) {
                props.setProperty(ShenyuClientConstants.CONTEXT_PATH, String.format("/%s", applicationName));
            }
            props.setProperty(ShenyuClientConstants.DISCOVERY_LOCAL_MODE_KEY, Boolean.valueOf(ShenyuClientConstants.DISCOVERY_LOCAL_MODE.equals(discoveryMode)).toString());
        }
        return new SpringMvcClientEventListener(clientConfig, shenyuClientRegisterRepository, env);
    }

    /**
     * ClientRegisterConfig Bean.
     *
     * @param shenyuClientConfig shenyuClientConfig
     * @param applicationContext applicationContext
     * @param env                env
     * @return clientRegisterConfig
     */
    @Bean("springMvcClientRegisterConfig")
    @Primary
    public ClientRegisterConfig clientRegisterConfig(final ShenyuClientConfig shenyuClientConfig,
                                                     final ApplicationContext applicationContext,
                                                     final Environment env) {
        return new ClientRegisterConfigImpl(shenyuClientConfig, RpcTypeEnum.HTTP, applicationContext, env);
    }

}
