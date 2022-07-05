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

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.rocketmq.config.LogCollectConfig;
import org.apache.shenyu.plugin.logging.rocketmq.constant.LoggingConstant;
import org.apache.shenyu.plugin.logging.rocketmq.handler.LoggingRocketMQPluginDataHandler;
import org.apache.shenyu.plugin.logging.rocketmq.utils.RocketLogCollectConfigUtils;
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

    private RocketMQLogCollectClient rocketMQLogCollectClient;

    private SelectorData selectorData = new SelectorData();

    private ConditionData conditionData = new ConditionData();

    private PluginData pluginData = new PluginData();

    private Properties properties = new Properties();

    private LogCollectConfig.GlobalLogConfig globalLogConfig;

    private List<ShenyuRequestLog> logs = new ArrayList<>();

    private ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @BeforeEach
    public void setUp() {
        this.rocketMQLogCollectClient = new RocketMQLogCollectClient();
        selectorData.setId("1");
        selectorData.setType(1);
        selectorData.setHandle("{\"topic\":\"test\", \"sampleRate\":\"1\"}");
        conditionData.setParamName("id");
        conditionData.setParamType("uri");
        conditionData.setParamValue("11");
        conditionData.setOperator("=");
        List<ConditionData> list = new ArrayList<>();
        list.add(conditionData);
        selectorData.setConditionList(list);
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"topic\":\"test\", \"namesrvAddr\":\"test\", \"producerGroup\":\"test\"}");

        globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), LogCollectConfig.GlobalLogConfig.class);
        properties.setProperty(LoggingConstant.TOPIC, globalLogConfig.getTopic());
        properties.setProperty(LoggingConstant.NAMESERVER_ADDRESS, globalLogConfig.getNamesrvAddr());
        properties.setProperty(LoggingConstant.PRODUCER_GROUP, globalLogConfig.getProducerGroup());

        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        logs.add(shenyuRequestLog);
    }

    @Test
    public void testConsume() {
        String msg = "";
        RocketLogCollectConfigUtils.setGlobalConfig(globalLogConfig);
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
