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

package org.apache.shenyu.springboot.starter.client.common.config;

import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type shenyu client common bean configuration.
 */
@Configuration
@ConditionalOnProperty(value = "shenyu.register.enabled", matchIfMissing = true, havingValue = "true")
public class ShenyuClientCommonBeanConfiguration {
    
    /**
     * Register the register repository for http client bean post processor.
     *
     * @param config the config
     * @return the client register repository
     */
    @Bean
    public ShenyuClientRegisterRepository shenyuClientRegisterRepository(final ShenyuRegisterCenterConfig config) {
        return ShenyuClientRegisterRepositoryFactory.newInstance(config);
    }
    
    /**
     * Shenyu Register Center Config.
     *
     * @return the Register Center Config
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.register")
    public ShenyuRegisterCenterConfig shenyuRegisterCenterConfig() {
        return new ShenyuRegisterCenterConfig();
    }
    
    /**
     * Shenyu client config.
     *
     * @return the shenyu client config
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu")
    public ShenyuClientConfig shenyuClientConfig() {
        return new ShenyuClientConfig();
    }
}
