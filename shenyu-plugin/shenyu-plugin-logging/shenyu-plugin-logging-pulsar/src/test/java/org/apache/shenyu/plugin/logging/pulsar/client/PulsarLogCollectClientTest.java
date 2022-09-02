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

package org.apache.shenyu.plugin.logging.pulsar.client;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.pulsar.config.PulsarLogCollectConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

public class PulsarLogCollectClientTest {

    private final PluginData pluginData = new PluginData();

    private final List<ShenyuRequestLog> logs = new ArrayList<>();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    private PulsarLogCollectConfig.PulsarLogConfig pulsarLogConfig;

    private PulsarLogCollectClient pulsarLogCollectClient;

    @BeforeEach
    public void setup() {
        this.pulsarLogCollectClient = new PulsarLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"topic\":\"test\", \"serviceUrl\":\"test\"}");
        pulsarLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), PulsarLogCollectConfig.PulsarLogConfig.class);
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        logs.add(shenyuRequestLog);
    }

    @Test
    public void testConsume() {
        String msg = "";
        PulsarLogCollectConfig.INSTANCE.setPulsarLogConfig(pulsarLogConfig);
        pulsarLogCollectClient.initClient(pulsarLogConfig);
        try {
            pulsarLogCollectClient.consume(logs);
        } catch (Exception e) {
            msg = "false";
        }
        Assertions.assertEquals(msg, "false");
        pulsarLogCollectClient.close();
    }
}

