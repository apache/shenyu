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

package org.apache.shenyu.plulgin.logging.rabbitmq.handler;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.logging.rabbitmq.client.RabbitmqLogCollectClient;
import org.apache.shenyu.plugin.logging.rabbitmq.handler.LoggingRabbitmqPluginDataHandler;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The Test Case For LoggingRabbitmqPluginDataHandler.
 */
public class LoggingRabbitmqPluginDataHandlerTest {

    private final SelectorData selectorData = new SelectorData();

    private final ConditionData conditionData = new ConditionData();

    private final PluginData pluginData = new PluginData();

    private final String config
            = "{\"host\":\"127.0.0.1\",\"port\":5672,\"password\":\"admin\",\"username\":\"admin\",\"exchangeName\":\"exchange.logging.plugin\",\"queueName\":\"queue.logging.plugin\"}";

    private LoggingRabbitmqPluginDataHandler loggingRabbitmqPluginDataHandler;

    @Before
    public void setUp() {
        this.loggingRabbitmqPluginDataHandler = new LoggingRabbitmqPluginDataHandler();
        selectorData.setId("1");
        selectorData.setType(SelectorTypeEnum.CUSTOM_FLOW.getCode());
        selectorData.setHandle("{\"routingKey\":\"routingKey.logging.rabbitmq\", \"sampleRate\":\"1\"}");

        conditionData.setParamName("id");
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setParamValue("https://shenyu.apache.org/");
        conditionData.setOperator("=");

        List<ConditionData> list = new ArrayList<>();
        list.add(conditionData);
        selectorData.setConditionList(list);
        pluginData.setEnabled(true);
        pluginData.setId(UUID.randomUUID().toString().replace("-", ""));
        pluginData.setConfig(config);
    }

    @Test
    public void testHandlerPlugin() throws Exception {
        loggingRabbitmqPluginDataHandler.handlerPlugin(pluginData);
        Field field = loggingRabbitmqPluginDataHandler.getClass().getDeclaredField("RABBITMQ_LOG_COLLECT_CLIENT");
        field.setAccessible(Boolean.TRUE);
        Assert.assertEquals(field.get(loggingRabbitmqPluginDataHandler).getClass(), RabbitmqLogCollectClient.class);
        pluginData.setEnabled(Boolean.FALSE);
        loggingRabbitmqPluginDataHandler.handlerPlugin(pluginData);
    }

    @Test
    public void testPluginNamed() {
        Assert.assertEquals(loggingRabbitmqPluginDataHandler.pluginNamed(), PluginEnum.LOGGING_RABBITMQ.getName());
    }

    @Test
    public void testGetRabbitmqLogCollectClient() {
        Assert.assertEquals(LoggingRabbitmqPluginDataHandler.getRabbitmqLogCollectClient().getClass(), RabbitmqLogCollectClient.class);
    }

    @Test
    public void testGetSelectApiConfigMap() {
        Assert.assertEquals(LoggingRabbitmqPluginDataHandler.getSelectApiConfigMap().getClass(), ConcurrentHashMap.class);
    }

}
