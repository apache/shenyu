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

import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

/**
 * InstanceRegisterListener.
 * <p>
 * instance register into discovery.
 * </p>
 */
public class InstanceRegisterListener implements ApplicationListener<ContextRefreshedEvent> {

    private static final Logger LOGGER = LoggerFactory.getLogger(InstanceRegisterListener.class);

    private final DiscoveryUpstreamData currentInstanceUpstream;

    private final DiscoveryConfig discoveryConfig;

    private final String path;

    private static final String SEQ_PRE = "seq_";

    public InstanceRegisterListener(final DiscoveryUpstreamData discoveryUpstream, final ShenyuDiscoveryConfig shenyuDiscoveryConfig) {
        this.currentInstanceUpstream = discoveryUpstream;
        this.discoveryConfig = new DiscoveryConfig();
        this.discoveryConfig.setServerList(shenyuDiscoveryConfig.getServerList());
        this.discoveryConfig.setType(shenyuDiscoveryConfig.getType());
        this.discoveryConfig.setProps(shenyuDiscoveryConfig.getProps());
        this.discoveryConfig.setName(shenyuDiscoveryConfig.getName());
        this.path = shenyuDiscoveryConfig.getRegisterPath();
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {
        try {
            ShenyuDiscoveryService discoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin("zookeeper");
            discoveryService.init(discoveryConfig);
            discoveryService.register(buildSeqPath(), GsonUtils.getInstance().toJson(currentInstanceUpstream));
        } catch (Exception e) {
            LOGGER.error("shenyu register into ShenyuDiscoveryService  {} type find error", discoveryConfig.getType(), e);
            throw new ShenyuException(String.format("shenyu register into ShenyuDiscoveryService %s type find error", discoveryConfig.getType()));
        }
    }

    private String buildSeqPath() {
        return path + "/" + SEQ_PRE;
    }

}
