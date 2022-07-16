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

package org.apache.shenyu.plugin.logging.rocketmq.client;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.rocketmq.config.RocketMQLogCollectConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * test cases for RocketMQLogCollect.
 */
public class RocketMQLogCollectClientTest {

    private final PluginData pluginData = new PluginData();

    private final Properties properties = new Properties();

    private final List<ShenyuRequestLog> logs = new ArrayList<>();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();
    
    private RocketMQLogCollectConfig.RocketMQLogConfig rocketMQLogConfig;
    
    private RocketMQLogCollectClient rocketMQLogCollectClient;

    @BeforeEach
    public void setUp() {
        this.rocketMQLogCollectClient = new RocketMQLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"topic\":\"test\", \"namesrvAddr\":\"test\", \"producerGroup\":\"test\"}");
        rocketMQLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), RocketMQLogCollectConfig.RocketMQLogConfig.class);
        properties.setProperty(GenericLoggingConstant.TOPIC, rocketMQLogConfig.getTopic());
        properties.setProperty(GenericLoggingConstant.NAMESERVER_ADDRESS, rocketMQLogConfig.getNamesrvAddr());
        properties.setProperty(GenericLoggingConstant.PRODUCER_GROUP, rocketMQLogConfig.getProducerGroup());
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        logs.add(shenyuRequestLog);
    }

    @Test
    public void testConsume() {
        String msg = "";
        RocketMQLogCollectConfig.INSTANCE.setRocketMQLogConfig(rocketMQLogConfig);
        rocketMQLogCollectClient.initProducer(properties);
        try {
            rocketMQLogCollectClient.consume(logs);
        } catch (Exception e) {
            msg = "false";
        }
        Assertions.assertEquals(msg, "");
        rocketMQLogCollectClient.close();
    }
}
