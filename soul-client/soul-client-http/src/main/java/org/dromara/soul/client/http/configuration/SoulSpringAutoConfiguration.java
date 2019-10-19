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

package org.dromara.soul.client.http.configuration;

import org.dromara.soul.client.http.spring.SoulClientBeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

/**
 * The type Soul spring auto configuration.
 *
 * @author xiaoyu
 */
@Configuration
public class SoulSpringAutoConfiguration {

    private final Environment env;

    /**
     * Instantiates a new Soul spring auto configuration.
     *
     * @param env the env
     */
    public SoulSpringAutoConfiguration(Environment env) {
        this.env = env;
    }

    /**
     * Soul client bean post processor soul client bean post processor.
     *
     * @return the soul client bean post processor
     */
    @Bean
    public SoulClientBeanPostProcessor soulClientBeanPostProcessor() {
        return new SoulClientBeanPostProcessor(env);
    }
}
