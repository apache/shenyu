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

package org.dromara.soul.springboot.starter.client.springcloud;

import org.dromara.soul.client.springcloud.config.SoulSpringCloudConfig;
import org.dromara.soul.client.springcloud.init.ContextRegisterListener;
import org.dromara.soul.client.springcloud.init.SpringCloudClientBeanPostProcessor;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

/**
 * The type Soul spring cloud client configuration.
 *
 * @author xiaoyu
 */
@Configuration
public class SoulSpringCloudClientConfiguration {
    
    /**
     * Spring cloud client bean post processor.
     *
     * @param soulSpringCloudConfig the soul spring cloud config
     * @param env                   the env
     * @return the spring cloud client bean post processor
     */
    @Bean
    public SpringCloudClientBeanPostProcessor springCloudClientBeanPostProcessor(final SoulSpringCloudConfig soulSpringCloudConfig, final Environment env) {
        return new SpringCloudClientBeanPostProcessor(soulSpringCloudConfig, env);
    }

    /**
     * Context register listener.
     *
     * @param soulSpringCloudConfig the soul spring cloud config
     * @param env                   the env
     * @return the context register listener
     */
    @Bean
    public ContextRegisterListener contextRegisterListener(final SoulSpringCloudConfig soulSpringCloudConfig, final Environment env) {
        return new ContextRegisterListener(soulSpringCloudConfig, env);
    }
    
    /**
     * Soul spring cloud config.
     *
     * @return the soul spring cloud config
     */
    @Bean
    @ConfigurationProperties(prefix = "soul.springcloud")
    public SoulSpringCloudConfig soulSpringCloudConfig() {
        return new SoulSpringCloudConfig();
    }
}
