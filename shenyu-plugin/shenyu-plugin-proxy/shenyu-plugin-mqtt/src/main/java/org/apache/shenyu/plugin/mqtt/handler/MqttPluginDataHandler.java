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

package org.apache.shenyu.plugin.mqtt.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.protocol.mqtt.MqttBootstrapServer;
import org.apache.shenyu.protocol.mqtt.MqttServerConfiguration;

import java.util.Objects;

/**
 * The type Mqtt plugin data handler.
 */
public class MqttPluginDataHandler implements PluginDataHandler {

    private final MqttBootstrapServer server = new MqttBootstrapServer();

    private MqttServerConfiguration runningConfiguration;

    @Override
    public synchronized void handlerPlugin(final PluginData pluginData) {
        if (!pluginData.getEnabled()) {
            server.shutdown();
            runningConfiguration = null;
            return;
        }
        MqttServerConfiguration configuration = GsonUtils.getInstance().fromJson(pluginData.getConfig(), MqttServerConfiguration.class);
        if (configuration.equals(runningConfiguration) && server.isRunning()) {
            return;
        }
        if (Objects.nonNull(runningConfiguration)) {
            server.shutdown();
            runningConfiguration = null;
        }
        configuration.afterPropertiesSet();
        server.init();
        server.start();
        runningConfiguration = configuration;
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.MQTT.getName();
    }
}
