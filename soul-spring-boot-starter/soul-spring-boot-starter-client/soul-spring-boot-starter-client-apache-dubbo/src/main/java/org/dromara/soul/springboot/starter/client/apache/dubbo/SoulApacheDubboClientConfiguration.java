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

package org.dromara.soul.springboot.starter.client.apache.dubbo;

import org.dromara.soul.client.apache.dubbo.ApacheDubboServiceBeanPostProcessor;
import org.dromara.soul.register.client.api.SoulClientRegisterRepository;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.dromara.soul.springboot.starter.client.common.config.SoulClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Soul apache dubbo client configuration.
 *
 * @author xiaoyu
 */
@Configuration
@ImportAutoConfiguration(SoulClientCommonBeanConfiguration.class)
public class SoulApacheDubboClientConfiguration {
    
    /**
     * Apache dubbo service bean post processor alibaba dubbo service bean post processor.
     *
     * @param soulRegisterCenterConfig soulRegisterCenterConfig
     * @param soulClientRegisterRepository the soulClientRegisterRepository
     * @return the alibaba dubbo service bean post processor
     */
    @Bean
    public ApacheDubboServiceBeanPostProcessor apacheDubboServiceBeanPostProcessor(final SoulRegisterCenterConfig soulRegisterCenterConfig,
                                                                                   final SoulClientRegisterRepository soulClientRegisterRepository) {
        return new ApacheDubboServiceBeanPostProcessor(soulRegisterCenterConfig, soulClientRegisterRepository);
    }

}
