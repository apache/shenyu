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

package org.apache.shenyu.springboot.starter.client.sofa;

import org.apache.shenyu.client.sofa.SofaServiceEventListener;
import org.apache.shenyu.common.utils.VersionUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Sofa type client event listener.
 */
@Configuration
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
@ConditionalOnProperty(value = "shenyu.register.enabled", matchIfMissing = true, havingValue = "true")
public class ShenyuSofaClientConfiguration {

    static {
        VersionUtils.checkDuplicate(ShenyuSofaClientConfiguration.class);
    }

    /**
     * Sofa service event listener.
     *
     * @param clientConfig the client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     * @return the sofa service event listener
     */
    @Bean
    public SofaServiceEventListener sofaServiceEventListener(final ShenyuClientConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        return new SofaServiceEventListener(clientConfig, shenyuClientRegisterRepository);
    }
}
