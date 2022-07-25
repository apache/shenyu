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

import org.apache.shenyu.client.springmvc.init.ContextRegisterListener;
import org.apache.shenyu.client.springmvc.init.SpringMvcClientEventListener;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type shenyu spring mvc client configuration.
 */
@Configuration
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
@ConditionalOnProperty(value = "shenyu.register.enabled", matchIfMissing = true, havingValue = "true")
public class ShenyuSpringMvcClientConfiguration {

    /**
     * Spring mvc client bean post processor.
     *
     * @param clientConfig                   the client config
     * @param shenyuClientRegisterRepository the shenyu client register repository
     * @return the spring mvc client bean post processor
     */
    @Bean
    public SpringMvcClientEventListener springHttpClientEventListener(final ShenyuClientConfig clientConfig,
                                                                          final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        return new SpringMvcClientEventListener(clientConfig.getClient().get(RpcTypeEnum.HTTP.getName()), shenyuClientRegisterRepository);
    }

    /**
     * Context register listener.
     *
     * @param clientConfig the client config
     * @return the context register listener
     */
    @Bean
    public ContextRegisterListener contextRegisterListener(final ShenyuClientConfig clientConfig) {
        return new ContextRegisterListener(clientConfig.getClient().get(RpcTypeEnum.HTTP.getName()));
    }
}
