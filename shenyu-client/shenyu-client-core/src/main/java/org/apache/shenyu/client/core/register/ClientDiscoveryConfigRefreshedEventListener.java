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

package org.apache.shenyu.client.core.register;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

public final class ClientDiscoveryConfigRefreshedEventListener implements ApplicationListener<ContextRefreshedEvent> {

    private final ShenyuDiscoveryConfig clientRegisterConfig;

    private final HttpClientRegisterRepository httpClientRegisterRepository;

    public ClientDiscoveryConfigRefreshedEventListener(final ShenyuDiscoveryConfig shenyuDiscoveryConfig, final HttpClientRegisterRepository httpClientRegisterRepository) {
        this.clientRegisterConfig = shenyuDiscoveryConfig;
        this.httpClientRegisterRepository = httpClientRegisterRepository;
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {

    }

    protected DiscoveryConfigRegisterDTO buildDiscoveryConfigRegisterDTO(final ShenyuDiscoveryConfig shenyuDiscoveryConfig) {
        try {
            return DiscoveryConfigRegisterDTO.builder()
                    .name(shenyuDiscoveryConfig.getName())
                    .serverList(shenyuDiscoveryConfig.getServerList())
                    .props(shenyuDiscoveryConfig.getProps())
                    .type(shenyuDiscoveryConfig.getType())
                    .build();
        } catch (ShenyuException e) {
            throw new ShenyuException(e.getMessage() + "please config ${shenyu.discovery} in xml/yml !");
        }
    }
}
