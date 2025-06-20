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

package org.apache.shenyu.plugin.logging.rabbitmq.handler;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.logging.rabbitmq.client.RabbitmqLogCollectClient;
import org.apache.shenyu.plugin.logging.rabbitmq.config.RabbitmqLogCollectConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * The Test Case For LoggingRabbitmqPluginDataHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class LoggingRabbitmqPluginDataHandlerTest {

    private final SelectorData selectorData = new SelectorData();

    private final ConditionData conditionData = new ConditionData();

    private final PluginData pluginData = new PluginData();

    private final String config
            = "{\"host\":\"127.0.0.1\",\"port\":5672,\"password\":\"admin\",\"username\":\"admin\",\"exchangeName\":\"exchange.logging.plugin\",\"queueName\":\"queue.logging.plugin\"}";

    private LoggingRabbitmqPluginDataHandler loggingRabbitmqPluginDataHandler;

    @BeforeEach
    public void setUp() {
        loggingRabbitmqPluginDataHandler = Mockito.spy(new LoggingRabbitmqPluginDataHandler());
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
        Assertions.assertEquals(field.get(loggingRabbitmqPluginDataHandler).getClass(), RabbitmqLogCollectClient.class);
        pluginData.setEnabled(Boolean.FALSE);
        loggingRabbitmqPluginDataHandler.handlerPlugin(pluginData);
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(loggingRabbitmqPluginDataHandler.pluginNamed(), PluginEnum.LOGGING_RABBITMQ.getName());
    }

    @Test
    public void testGetRabbitmqLogCollectClient() {
        Assertions.assertEquals(LoggingRabbitmqPluginDataHandler.getRabbitmqLogCollectClient().getClass(), RabbitmqLogCollectClient.class);
    }

    @Test
    public void testGetSelectApiConfigMap() {
        Assertions.assertEquals(LoggingRabbitmqPluginDataHandler.getSelectApiConfigMap().getClass(), ConcurrentHashMap.class);
    }

    @Test
    public void testHandlerPluginUpdateSameConfig() {
        RabbitmqLogCollectConfig.RabbitmqLogConfig existingConfig = createValidConfig();
        Singleton.INST.single(existingConfig.getClass(), existingConfig);

        PluginData pluginData = createPluginData();
        pluginData.setConfig(GsonUtils.getGson().toJson(existingConfig));

        loggingRabbitmqPluginDataHandler.handlerPlugin(pluginData);

        verify(loggingRabbitmqPluginDataHandler, never()).doRefreshConfig(any());
    }

    @Test
    public void testHandlerPluginUpdateDifferentConfig() {
        RabbitmqLogCollectConfig.RabbitmqLogConfig existingConfig = createValidConfig();
        Singleton.INST.single(existingConfig.getClass(), existingConfig);

        RabbitmqLogCollectConfig.RabbitmqLogConfig updatedConfig = createValidConfig();
        updatedConfig.setUsername("updatedUsername");
        updatedConfig.setPassword("updatedPassword");
        PluginData pluginData = createPluginData();
        pluginData.setConfig(GsonUtils.getGson().toJson(updatedConfig));

        doNothing().when(loggingRabbitmqPluginDataHandler).doRefreshConfig(any());

        loggingRabbitmqPluginDataHandler.handlerPlugin(pluginData);

        verify(loggingRabbitmqPluginDataHandler, times(1)).doRefreshConfig(any());
    }

    private PluginData createPluginData() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setId(UUID.randomUUID().toString().replace("-", ""));
        return pluginData;
    }

    private RabbitmqLogCollectConfig.RabbitmqLogConfig createValidConfig() {
        RabbitmqLogCollectConfig.RabbitmqLogConfig config = new RabbitmqLogCollectConfig.RabbitmqLogConfig();
        config.setHost("127.0.0.1");
        config.setPort(5672);
        config.setPassword("admin");
        config.setUsername("admin");
        config.setExchangeName("exchange.logging.plugin");
        config.setQueueName("queue.logging.plugin");
        config.setRoutingKey("topic.logging");
        config.setVirtualHost("/");
        config.setExchangeType("direct");
        config.setDurable(true);
        config.setExclusive(false);
        config.setAutoDelete(false);
        config.setSampleRate("1");
        return config;
    }

}
