/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.client.springcloud.configuration;

import org.dromara.soul.client.springcloud.config.SoulSpringCloudConfig;
import org.dromara.soul.client.springcloud.spring.SoulSpringCloudClientBeanPostProcessor;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

/**
 * The type Soul spring mvc auto configuration.
 *
 * @author xiaoyu
 */
@Configuration
@EnableConfigurationProperties({SoulSpringCloudConfig.class})
public class SoulSpringCloudAutoConfiguration {

    private final Environment env;

    private final SoulSpringCloudConfig soulSpringCloudConfig;

    /**
     * Instantiates a new Soul spring mvc auto configuration.
     *
     * @param env                   the env
     * @param soulSpringCloudConfig the soul springCloud config
     */
    public SoulSpringCloudAutoConfiguration(final Environment env, final SoulSpringCloudConfig soulSpringCloudConfig) {
        this.env = env;
        this.soulSpringCloudConfig = soulSpringCloudConfig;
    }

    /**
     * Soul spring cloud client bean post processor soul spring cloud client bean post processor.
     *
     * @return the soul spring cloud client bean post processor
     */
    @Bean
    public SoulSpringCloudClientBeanPostProcessor soulSpringCloudClientBeanPostProcessor() {
        return new SoulSpringCloudClientBeanPostProcessor(env, soulSpringCloudConfig);
    }

}
