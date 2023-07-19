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

package org.apache.shenyu.plugin.tcp.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.apache.shenyu.protocol.tcp.BootstrapServer;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;

/**
 * upstreamList data change.
 */
public class TcpUpstreamDataHandler implements DiscoveryUpstreamDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(TcpUpstreamDataHandler.class);

    @Override
    public String pluginName() {
        return PluginEnum.TCP.getName();
    }

    @Override
    public void handlerDiscoveryUpstreamData(final DiscoverySyncData discoverySyncData) {
        List<DiscoveryUpstreamData> removed = UpstreamProvider.getSingleton().refreshCache(discoverySyncData.getSelectorName(), discoverySyncData.getUpstreamDataList());
        BootstrapServer bootstrapServer = TcpBootstrapFactory.getSingleton().getCache(discoverySyncData.getSelectorName());
        if (Objects.nonNull(bootstrapServer)) {
            bootstrapServer.removeCommonUpstream(removed);
            LOG.info("shenyu update TcpBootstrapServer [{}] success upstream is {}", discoverySyncData.getSelectorName(), discoverySyncData.getUpstreamDataList());
        } else {
            LOG.warn("shenyu update TcpBootstrapServer don't find name is {}", discoverySyncData.getSelectorName());
        }
    }

}
