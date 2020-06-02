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

package org.dromara.soul.springboot.starter.client.alibaba.dubbo;

import org.dromara.soul.client.alibaba.dubbo.AlibabaDubboServiceBeanPostProcessor;
import org.dromara.soul.client.dubbo.common.config.DubboConfig;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Soul alibaba dubbo client configuration.
 *
 * @author xiaoyu
 */
@Configuration
public class SoulAlibabaDubboClientConfiguration {
    
    /**
     * Alibaba dubbo service bean post processor alibaba dubbo service bean post processor.
     *
     * @param dubboConfig the dubbo config
     * @return the alibaba dubbo service bean post processor
     */
    @Bean
    public AlibabaDubboServiceBeanPostProcessor alibabaDubboServiceBeanPostProcessor(final DubboConfig dubboConfig) {
        return new AlibabaDubboServiceBeanPostProcessor(dubboConfig);
    }
    
    /**
     * Dubbo config dubbo config.
     *
     * @return the dubbo config
     */
    @Bean
    @ConfigurationProperties(prefix = "soul.dubbo")
    public DubboConfig dubboConfig() {
        return new DubboConfig();
    }
}
