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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.Ordered;

public final class ClientDiscoveryConfigRefreshedEventListener implements ApplicationListener<ContextRefreshedEvent>, Ordered {

    private static final Logger LOG = LoggerFactory.getLogger(ClientDiscoveryConfigRefreshedEventListener.class);

    private final ShenyuDiscoveryConfig shenyuDiscoveryConfig;

    private final HttpClientRegisterRepository httpClientRegisterRepository;

    private final ClientRegisterConfig clientRegisterConfig;

    private final PluginEnum plugin;

    public ClientDiscoveryConfigRefreshedEventListener(final ShenyuDiscoveryConfig shenyuDiscoveryConfig,
                                                       final HttpClientRegisterRepository httpClientRegisterRepository,
                                                       final ClientRegisterConfig clientRegisterConfig,
                                                       final PluginEnum plugin) {
        this.shenyuDiscoveryConfig = shenyuDiscoveryConfig;
        this.httpClientRegisterRepository = httpClientRegisterRepository;
        this.clientRegisterConfig = clientRegisterConfig;
        this.plugin = plugin;
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {
        httpClientRegisterRepository.doPersistDiscoveryConfig(buildDiscoveryConfigRegisterDTO(shenyuDiscoveryConfig));
    }

    protected DiscoveryConfigRegisterDTO buildDiscoveryConfigRegisterDTO(final ShenyuDiscoveryConfig shenyuDiscoveryConfig) {
        if (StringUtils.isEmpty(shenyuDiscoveryConfig.getServerList())) {
            LOG.error("If using service discovery. The configuration shenyu.discovery.name in xml/yml cannot be null");
            throw new ShenyuException("The configuration shenyu.discovery.serverList in xml/yml cannot be null");
        }
        if (StringUtils.isEmpty(shenyuDiscoveryConfig.getType())) {
            LOG.error("If using service discovery. The configuration shenyu.discovery.name in xml/yml cannot be null");
            throw new ShenyuException("The configuration shenyu.discovery.type in xml/yml cannot be null");
        }
        return DiscoveryConfigRegisterDTO.builder()
                .name(discoveryName())
                .selectorName(clientRegisterConfig.getContextPath())
                .handler("{}")
                .listenerNode(shenyuDiscoveryConfig.getRegisterPath())
                .serverList(shenyuDiscoveryConfig.getServerList())
                .props(shenyuDiscoveryConfig.getProps())
                .discoveryType(shenyuDiscoveryConfig.getType())
                .pluginName(plugin.getName())
                .build();
    }

    private String discoveryName() {
        return "default_" + shenyuDiscoveryConfig.getType();
    }

    @Override
    public int getOrder() {
        return Ordered.LOWEST_PRECEDENCE;
    }
}
