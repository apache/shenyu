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

package org.apache.shenyu.springboot.starter.client.springcloud;

import org.apache.shenyu.client.springcloud.init.ContextRegisterListener;
import org.apache.shenyu.client.springcloud.init.SpringCloudClientBeanPostProcessor;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

/**
 * The type shenyu spring cloud client configuration.
 */
@Configuration
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
public class ShenyuSpringCloudClientConfiguration {
    
    /**
     * Spring cloud client bean post processor spring cloud client bean post processor.
     *
     * @param env the env
     * @param shenyuClientRegisterRepository the shenyu client register repository
     * @param clientConfig the client config
     * @return the spring cloud client bean post processor
     */
    @Bean
    public SpringCloudClientBeanPostProcessor springCloudClientBeanPostProcessor(final Environment env,
                                                                                 final ShenyuClientRegisterRepository shenyuClientRegisterRepository,
                                                                                 final ShenyuClientConfig clientConfig) {
        return new SpringCloudClientBeanPostProcessor(env, shenyuClientRegisterRepository, clientConfig.getClient().get(RpcTypeEnum.SPRING_CLOUD.getName()));
    }
    
    /**
     * Context register listener context register listener.
     *
     * @param clientConfig the client config
     * @param env the env
     * @return the context register listener
     */
    @Bean
    public ContextRegisterListener contextRegisterListener(final ShenyuClientConfig clientConfig, final Environment env) {
        return new ContextRegisterListener(clientConfig.getClient().get(RpcTypeEnum.SPRING_CLOUD.getName()), env);
    }
}
