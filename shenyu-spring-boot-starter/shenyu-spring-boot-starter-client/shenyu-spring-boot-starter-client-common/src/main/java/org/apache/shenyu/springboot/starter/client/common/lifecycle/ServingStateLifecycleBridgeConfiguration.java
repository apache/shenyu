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

package org.apache.shenyu.springboot.starter.client.common.lifecycle;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;

/**
 * Opt-in lifecycle bridge from Spring readiness and graceful shutdown to the registered
 * business instance's serving state (#8429). Active only when a client register
 * repository exists and the operator sets
 * {@code shenyu.register.lifecycle-bridge.enabled=true}; shipped behavior changes
 * nothing by default.
 */
@AutoConfiguration
@ConditionalOnBean(ShenyuClientRegisterRepository.class)
@ConditionalOnProperty(name = "shenyu.register.lifecycle-bridge.enabled", havingValue = "true")
public class ServingStateLifecycleBridgeConfiguration {

    /**
     * Lifecycle bridge bean.
     *
     * @param repository the client register repository
     * @return the serving state lifecycle bridge
     */
    @Bean
    public ServingStateLifecycleBridge servingStateLifecycleBridge(final ShenyuClientRegisterRepository repository) {
        return new ServingStateLifecycleBridge(repository);
    }
}
