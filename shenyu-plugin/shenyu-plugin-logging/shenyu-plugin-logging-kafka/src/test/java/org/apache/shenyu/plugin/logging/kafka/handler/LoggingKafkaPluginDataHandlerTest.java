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

package org.apache.shenyu.plugin.logging.kafka.handler;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The Test Case For LoggingKafkaPluginDataHandler.
 */
public class LoggingKafkaPluginDataHandlerTest {

    private final SelectorData selectorData = new SelectorData();

    private final ConditionData conditionData = new ConditionData();

    private final PluginData pluginData = new PluginData();

    private LoggingKafkaPluginDataHandler loggingKafkaPluginDataHandler;

    @BeforeEach
    public void setUp() {
        this.loggingKafkaPluginDataHandler = new LoggingKafkaPluginDataHandler();
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
        pluginData.setEnabled(false);
        pluginData.setConfig("{\"topic\":\"test\", \"namesrvAddr\":\"localhost:8082\"}");
    }

    @Test
    public void testHandlerPlugin() throws NoSuchFieldException, IllegalAccessException {
        loggingKafkaPluginDataHandler.handlerPlugin(pluginData);
        Field field = loggingKafkaPluginDataHandler.getClass().getDeclaredField("KAFKA_LOG_COLLECT_CLIENT");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(loggingKafkaPluginDataHandler).getClass(), KafkaLogCollectClient.class);
    }

    @Test
    public void testHandlerSelector() {
        loggingKafkaPluginDataHandler.handlerSelector(selectorData);
        Assertions.assertEquals(AbstractLogPluginDataHandler.getSelectIdUriListMap().toString(), "{1=[11]}");
        Assertions.assertNotEquals(AbstractLogPluginDataHandler.getSelectApiConfigMap().toString(), "{}");
    }

    @Test
    public void testRemoveSelector() {
        loggingKafkaPluginDataHandler.handlerSelector(selectorData);
        Assertions.assertEquals(AbstractLogPluginDataHandler.getSelectIdUriListMap().toString(), "{1=[11]}");
        Assertions.assertNotEquals(AbstractLogPluginDataHandler.getSelectApiConfigMap().toString(), "{}");
        loggingKafkaPluginDataHandler.removeSelector(selectorData);
        Assertions.assertEquals(AbstractLogPluginDataHandler.getSelectIdUriListMap().toString(), "{}");
        Assertions.assertEquals(AbstractLogPluginDataHandler.getSelectApiConfigMap().toString(), "{}");
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(loggingKafkaPluginDataHandler.pluginNamed(), "loggingKafka");
    }

    @Test
    public void testGetKafkaLogCollectClient() {
        Assertions.assertEquals(LoggingKafkaPluginDataHandler.getKafkaLogCollectClient().getClass(), KafkaLogCollectClient.class);
    }

    @Test
    public void testGetSelectIdUriListMap() {
        Assertions.assertEquals(LoggingKafkaPluginDataHandler.getSelectIdUriListMap().getClass(), ConcurrentHashMap.class);
    }

    @Test
    public void testGetSelectApiConfigMap() {
        Assertions.assertEquals(LoggingKafkaPluginDataHandler.getSelectApiConfigMap().getClass(), ConcurrentHashMap.class);
    }
}
