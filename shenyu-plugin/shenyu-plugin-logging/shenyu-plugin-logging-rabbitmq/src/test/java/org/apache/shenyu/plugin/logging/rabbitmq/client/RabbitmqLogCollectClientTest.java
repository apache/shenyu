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

package org.apache.shenyu.plugin.logging.rabbitmq.client;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.rabbitmq.config.RabbitmqLogCollectConfig;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * test cases for RabbitmqLogCollectClient.
 */
public class RabbitmqLogCollectClientTest {

    private final PluginData pluginData = new PluginData();

    private final List<ShenyuRequestLog> logs = new ArrayList<>();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    private final String config
            = "{\"host\":\"127.0.0.1\",\"port\":5672,\"password\":\"admin\",\"username\":\"admin\",\"exchangeName\":\"exchange.logging.plugin\",\"queueName\":\"queue.logging.plugin\"}";

    private RabbitmqLogCollectConfig.RabbitmqLogConfig rabbitmqLogConfig;

    private RabbitmqLogCollectClient rabbitmqLogCollectClient;

    @Before
    public void setUp() {
        this.rabbitmqLogCollectClient = new RabbitmqLogCollectClient();
        pluginData.setEnabled(Boolean.TRUE);
        pluginData.setConfig(config);
        rabbitmqLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), RabbitmqLogCollectConfig.RabbitmqLogConfig.class);
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        logs.add(shenyuRequestLog);
    }

    @Test
    public void testConsume() {
        String logMassage = "Hello Shenyu";
        RabbitmqLogCollectConfig.INSTANCE.setRabbitmqLogConfig(rabbitmqLogConfig);
        rabbitmqLogCollectClient.initClient(rabbitmqLogConfig);
        try {
            rabbitmqLogCollectClient.consume(logs);
        } catch (Exception e) {
            logMassage = "consume failed";
            Assert.assertEquals(logMassage, "consume failed");
        }
        Assert.assertEquals(logMassage, "Hello Shenyu");
        rabbitmqLogCollectClient.close();
    }

}
