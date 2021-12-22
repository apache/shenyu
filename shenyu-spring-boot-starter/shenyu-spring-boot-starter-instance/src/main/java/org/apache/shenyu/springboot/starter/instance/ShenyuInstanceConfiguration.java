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

package org.apache.shenyu.springboot.starter.instance;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Shenyu instance configuration.
 */
@Configuration
public class ShenyuInstanceConfiguration {
    
    /**
     * Instance register listener.
     *
     * @param config the config
     * @return the instance register listener
     */
    @Bean
    @ConditionalOnProperty(name = "shenyu.instance.enabled", havingValue = "true")
    public InstanceRegisterListener instanceRegisterListener(final ShenyuConfig config) {
        return new InstanceRegisterListener(config.getInstance());
    }
}
