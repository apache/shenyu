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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.DiscoveryUpstream;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.protocol.tcp.BootstrapServer;
import org.apache.shenyu.protocol.tcp.TcpServerConfiguration;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

public class TcpPluginDataHandler implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(TcpPluginDataHandler.class);

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        TcpPluginConfig tcpPluginConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), TcpPluginConfig.class);
        BootstrapServer bootstrapServer = new BootstrapServer();
        if (pluginData.getEnabled()) {
            LOG.info("shenyu TcpPluginDataHandler start TcpProxy");
            bootstrapServer.start(tcpPluginConfig.getDiscoveryConfig(), tcpPluginConfig.getTcpServerConfiguration());
        } else {
            LOG.info("shenyu TcpPluginDataHandler shutdown TcpProxy");
            bootstrapServer.shutdown();
        }
    }


    @Override
    public String pluginNamed() {
        return PluginEnum.TCP.getName();
    }

}
