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

package org.dromara.soul.springboot.starter.client.springmvc;

import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.dromara.soul.client.springmvc.init.ContextRegisterListener;
import org.dromara.soul.client.springmvc.init.SpringMvcClientBeanPostProcessor;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Soul client http configuration.
 *
 * @author xiaoyu
 */
@Configuration
public class SoulSpringMvcClientConfiguration {
    
    /**
     * Spring http client bean post processor spring http client bean post processor.
     *
     * @param soulSpringMvcConfig the soul http config
     * @return the spring http client bean post processor
     */
    @Bean
    public SpringMvcClientBeanPostProcessor springHttpClientBeanPostProcessor(final SoulSpringMvcConfig soulSpringMvcConfig) {
        return new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }
    
    /**
     * Context register listener context register listener.
     *
     * @param soulSpringMvcConfig the soul spring mvc config
     * @return the context register listener
     */
    @Bean
    public ContextRegisterListener contextRegisterListener(final SoulSpringMvcConfig soulSpringMvcConfig) {
        return new ContextRegisterListener(soulSpringMvcConfig);
    }
    
    /**
     * Soul http config soul http config.
     *
     * @return the soul http config
     */
    @Bean
    @ConfigurationProperties(prefix = "soul.http")
    public SoulSpringMvcConfig soulHttpConfig() {
        return new SoulSpringMvcConfig();
    }
}
