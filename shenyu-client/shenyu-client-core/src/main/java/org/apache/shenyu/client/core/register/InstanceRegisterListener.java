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
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.Ordered;

import java.net.URI;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;

/**
 * InstanceRegisterListener.
 * <p>
 * instance register into discovery.
 * </p>
 */
public class InstanceRegisterListener implements ApplicationListener<ContextRefreshedEvent>, Ordered {

    private static final Logger LOGGER = LoggerFactory.getLogger(InstanceRegisterListener.class);

    private final DiscoveryUpstreamData currentInstanceUpstream;

    private final RegisterConfig discoveryConfig;

    private ShenyuInstanceRegisterRepository discoveryService;

    private final String path;

    public InstanceRegisterListener(final DiscoveryUpstreamData discoveryUpstream, final ShenyuDiscoveryConfig shenyuDiscoveryConfig) {
        this.currentInstanceUpstream = discoveryUpstream;
        this.currentInstanceUpstream.setProps("{\"warmupTime\":\"10\"}");
        this.discoveryConfig = new RegisterConfig();
        this.discoveryConfig.setServerLists(shenyuDiscoveryConfig.getServerList());
        this.discoveryConfig.setRegisterType(shenyuDiscoveryConfig.getType());
        this.discoveryConfig.setProps(Optional.ofNullable(shenyuDiscoveryConfig.getProps()).orElse(new Properties()));
        this.path = shenyuDiscoveryConfig.getRegisterPath();
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            LOGGER.info("unregister upstream server by jvm runtime hook");
            if (Objects.nonNull(discoveryService)) {
                discoveryService.close();
            }
        }));
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {
        try {
            if (StringUtils.isBlank(discoveryConfig.getRegisterType()) || StringUtils.equalsIgnoreCase(discoveryConfig.getRegisterType(), "local")) {
                return;
            }
            this.discoveryService = ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class).getJoin(discoveryConfig.getRegisterType());
            discoveryConfig.getProps().put("watchPath", path);
            discoveryService.init(discoveryConfig);
            InstanceEntity instance = new InstanceEntity();
            instance.setStatus(currentInstanceUpstream.getStatus());
            instance.setWeight(currentInstanceUpstream.getWeight());
            final URI uri = URI.create(currentInstanceUpstream.getProtocol() + currentInstanceUpstream.getUrl());
            instance.setPort(uri.getPort());
            instance.setHost(uri.getHost());
            instance.setAppName(discoveryConfig.getProps().getProperty("name"));
            discoveryService.persistInstance(instance);
            LOGGER.info("shenyu register into ShenyuDiscoveryService {} success", discoveryConfig.getRegisterType());
        } catch (Exception e) {
            LOGGER.error("shenyu register into ShenyuDiscoveryService  {} type find error", discoveryConfig.getRegisterType(), e);
            throw new ShenyuException(String.format("shenyu register into ShenyuDiscoveryService %s type find error", discoveryConfig.getRegisterType()));
        }
    }

    @Override
    public int getOrder() {
        return HIGHEST_PRECEDENCE;
    }
}
