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

package org.apache.shenyu.plugin.logging.rocketmq.handler;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.rocketmq.client.RocketMQLogCollectClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The Test Case For LoggingRocketMQPluginDataHandler.
 */
public class LoggingRocketMQPluginDataHandlerTest {

    private final SelectorData selectorData = new SelectorData();

    private final ConditionData conditionData = new ConditionData();

    private final PluginData pluginData = new PluginData();

    private LoggingRocketMQPluginDataHandler loggingRocketMQPluginDataHandler;

    @BeforeEach
    public void setUp() {
        this.loggingRocketMQPluginDataHandler = new LoggingRocketMQPluginDataHandler();
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
    }

    @Test
    public void testHandlerPlugin() throws NoSuchFieldException, IllegalAccessException {
        loggingRocketMQPluginDataHandler.handlerPlugin(pluginData);
        Field field = loggingRocketMQPluginDataHandler.getClass().getDeclaredField("ROCKET_MQ_LOG_COLLECT_CLIENT");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(loggingRocketMQPluginDataHandler).getClass(), RocketMQLogCollectClient.class);
        pluginData.setEnabled(false);
        loggingRocketMQPluginDataHandler.handlerPlugin(pluginData);
    }

    @Test
    public void testHandlerSelector() {
        loggingRocketMQPluginDataHandler.handlerSelector(selectorData);
        Assertions.assertEquals(AbstractLogPluginDataHandler.getSelectIdUriListMap().toString(), "{1=[11]}");
        Assertions.assertNotEquals(AbstractLogPluginDataHandler.getSelectApiConfigMap().toString(), "{}");
    }

    @Test
    public void testRemoveSelector() {
        loggingRocketMQPluginDataHandler.handlerSelector(selectorData);
        Assertions.assertEquals(AbstractLogPluginDataHandler.getSelectIdUriListMap().toString(), "{1=[11]}");
        Assertions.assertNotEquals(AbstractLogPluginDataHandler.getSelectApiConfigMap().toString(), "{}");
        loggingRocketMQPluginDataHandler.removeSelector(selectorData);
        Assertions.assertEquals(AbstractLogPluginDataHandler.getSelectIdUriListMap().toString(), "{}");
        Assertions.assertEquals(AbstractLogPluginDataHandler.getSelectApiConfigMap().toString(), "{}");
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(loggingRocketMQPluginDataHandler.pluginNamed(), "loggingRocketMQ");
    }

    @Test
    public void testGetRocketMqLogCollectClient() {
        Assertions.assertEquals(LoggingRocketMQPluginDataHandler.getRocketMqLogCollectClient().getClass(), RocketMQLogCollectClient.class);
    }

    @Test
    public void testGetSelectIdUriListMap() {
        Assertions.assertEquals(LoggingRocketMQPluginDataHandler.getSelectIdUriListMap().getClass(), ConcurrentHashMap.class);
    }

    @Test
    public void testGetSelectApiConfigMap() {
        Assertions.assertEquals(LoggingRocketMQPluginDataHandler.getSelectApiConfigMap().getClass(), ConcurrentHashMap.class);
    }
}
