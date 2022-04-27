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

package org.apache.shenyu.plugin.logging.rocketmq.rocketmq;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.rocketmq.constant.LoggingConstant;
import org.apache.shenyu.plugin.logging.rocketmq.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.rocketmq.config.LogCollectConfig.GlobalLogConfig;
import org.apache.shenyu.plugin.logging.rocketmq.utils.LogCollectConfigUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * The Test Case For RocketMQLogCollectClient.
 */
public class RocketMQLogCollectClientTest {

    private RocketMQLogCollectClient rocketMQLogCollectClient;

    private Properties props = new Properties();

    private PluginData pluginData = new PluginData();

    private GlobalLogConfig globalLogConfig;

    private List<ShenyuRequestLog> logs = new ArrayList<>();

    private ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @BeforeEach
    public void setUp() {
        this.rocketMQLogCollectClient = new RocketMQLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"topic\":\"shenyu-access-logging\", \"namesrvAddr\":\"localhost:9876\", \"producerGroup\":\"shenyu-plugin-logging-rocketmq\"}");
        globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(),
                GlobalLogConfig.class);
        globalLogConfig.setCompressAlg("LZ4");
        props.setProperty(LoggingConstant.TOPIC, globalLogConfig.getTopic());
        props.setProperty(LoggingConstant.NAMESERVER_ADDRESS, globalLogConfig.getNamesrvAddr());
        props.setProperty(LoggingConstant.PRODUCER_GROUP, globalLogConfig.getProducerGroup());
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        logs.add(shenyuRequestLog);
    }

    @Test
    public void testInitProducer() throws NoSuchFieldException, IllegalAccessException {
        rocketMQLogCollectClient.initProducer(props);
        Field field = rocketMQLogCollectClient.getClass().getDeclaredField("topic");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(rocketMQLogCollectClient), "shenyu-access-logging");
        rocketMQLogCollectClient.close();
    }

    @Test
    public void testConsume() {
        String msg = "";
        LogCollectConfigUtils.setGlobalConfig(globalLogConfig);
        rocketMQLogCollectClient.initProducer(props);
        try {
            rocketMQLogCollectClient.consume(logs);
        } catch (Exception e) {
            msg = "false";
        }
        Assertions.assertEquals(msg, "");
        rocketMQLogCollectClient.close();
    }
}
