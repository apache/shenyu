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
import org.apache.shenyu.protocol.mqtt.BootstrapServer;
import org.apache.shenyu.protocol.mqtt.MqttBootstrapServer;
import org.apache.shenyu.protocol.mqtt.MqttServerConfiguration;

import java.net.InetAddress;
import java.net.Socket;

/**
 * The type Mqtt plugin data handler.
 */
public class MqttPluginDataHandler implements PluginDataHandler {

    private final BootstrapServer server = new MqttBootstrapServer();

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        MqttServerConfiguration configuration = GsonUtils.getInstance().fromJson(pluginData.getConfig(), MqttServerConfiguration.class);
        configuration.afterPropertiesSet();
        if (pluginData.getEnabled()) {
            server.init();
            server.start();
        } else {
            if (isPortUsing(configuration.getPort())) {
                server.shutdown();
            }
        }
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.MQTT.getName();
    }

    /**
     * Ture is use else false.
     *
     * @param port server port
     * @return boolean
     */
    private boolean isPortUsing(final int port) {
        boolean flag = false;
        try {
            InetAddress address = InetAddress.getByName("127.0.0.1");
            new Socket(address, port);
            flag = true;
        } catch (Exception ignored) {

        }
        return flag;
    }
}
