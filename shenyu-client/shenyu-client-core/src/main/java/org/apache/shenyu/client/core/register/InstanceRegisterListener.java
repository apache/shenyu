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
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.spi.ExtensionLoader;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

public class InstanceRegisterListener implements ApplicationListener<ContextRefreshedEvent> {
    private final DiscoveryUpstreamData currentInstanceUpstream;

    private final DiscoveryConfig discoveryConfig;

    private final String path;

    public InstanceRegisterListener(final ClientRegisterConfig clientRegisterConfig, final DiscoveryConfig discoveryConfig,
                            final String path) {
        DiscoveryUpstreamData discoveryUpstream = new DiscoveryUpstreamData();
        RpcTypeEnum rpcTypeEnum = clientRegisterConfig.getRpcTypeEnum();
        discoveryUpstream.setProtocol(rpcTypeEnum.getName());
        discoveryUpstream.setUrl(clientRegisterConfig.getIpAndPort());
        discoveryUpstream.setStatus(0);
        this.currentInstanceUpstream = discoveryUpstream;
        this.discoveryConfig = discoveryConfig;
        this.path = path;
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {
        ShenyuDiscoveryService discoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin("zookeeper");
        discoveryService.init(discoveryConfig);
        discoveryService.register(path, GsonUtils.getInstance().toJson(currentInstanceUpstream));
    }

}
