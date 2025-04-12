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

package org.apache.shenyu.plugin.logging.elasticsearch.handler;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.logging.elasticsearch.client.ElasticSearchLogCollectClient;
import org.apache.shenyu.plugin.logging.elasticsearch.config.ElasticSearchLogCollectConfig;
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
 * The Test Case For LoggingElasticSearchPluginDataHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class LoggingElasticSearchPluginDataHandlerTest {

    private LoggingElasticSearchPluginDataHandler loggingElasticSearchPluginDataHandler;

    private final SelectorData selectorData = new SelectorData();

    private final ConditionData conditionData = new ConditionData();

    private final PluginData pluginData = new PluginData();

    @BeforeEach
    public void setUp() {
        this.loggingElasticSearchPluginDataHandler = Mockito.spy(new LoggingElasticSearchPluginDataHandler());
        selectorData.setId("1");
        selectorData.setType(1);
        selectorData.setHandle("{\"index\":\"test\", \"sampleRate\":\"1\", \"topic\":\"1\"}");
        conditionData.setParamName("id");
        conditionData.setParamType("uri");
        conditionData.setParamValue("11");
        conditionData.setOperator("=");
        List<ConditionData> list = new ArrayList<>();
        list.add(conditionData);
        selectorData.setConditionList(list);
        pluginData.setEnabled(true);
        pluginData.setId(UUID.randomUUID().toString().replace("-", ""));
        pluginData.setConfig("{\"host\":\"localhost\", \"port\":\"9200\"}");
    }

    @Test
    public void handlerPluginTest() throws IllegalAccessException, NoSuchFieldException {
        loggingElasticSearchPluginDataHandler.handlerPlugin(pluginData);
        Field field = loggingElasticSearchPluginDataHandler.getClass().getDeclaredField("ELASTICSEARCH_LOG_COLLECT_CLIENT");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(loggingElasticSearchPluginDataHandler).getClass(), ElasticSearchLogCollectClient.class);
        pluginData.setEnabled(false);
        loggingElasticSearchPluginDataHandler.handlerPlugin(pluginData);
    }

    @Test
    public void testHandlerSelector() throws NoSuchFieldException, IllegalAccessException {
        loggingElasticSearchPluginDataHandler.handlerSelector(selectorData);
        Field field2 = loggingElasticSearchPluginDataHandler.getClass().getSuperclass().getDeclaredField("SELECT_API_CONFIG_MAP");
        field2.setAccessible(true);
        Assertions.assertNotEquals(field2.get("1").toString(), "{}");
    }

    @Test
    public void testRemoveSelector() throws NoSuchFieldException, IllegalAccessException {
        loggingElasticSearchPluginDataHandler.handlerSelector(selectorData);
        Field field2 = loggingElasticSearchPluginDataHandler.getClass().getSuperclass().getDeclaredField("SELECT_API_CONFIG_MAP");
        field2.setAccessible(true);
        Assertions.assertNotEquals(field2.get("1").toString(), "{}");
        loggingElasticSearchPluginDataHandler.removeSelector(selectorData);
        Field field4 = loggingElasticSearchPluginDataHandler.getClass().getSuperclass().getDeclaredField("SELECT_API_CONFIG_MAP");
        field4.setAccessible(true);
        Assertions.assertEquals(field4.get("1").toString(), "{}");
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(loggingElasticSearchPluginDataHandler.pluginNamed(), "loggingElasticSearch");
    }

    @Test
    public void testGetRocketMqLogCollectClient() {
        Assertions.assertEquals(LoggingElasticSearchPluginDataHandler.getElasticSearchLogCollectClient().getClass(), ElasticSearchLogCollectClient.class);
    }

    @Test
    public void testGetSelectApiConfigMap() {
        Assertions.assertEquals(LoggingElasticSearchPluginDataHandler.getSelectApiConfigMap().getClass(), ConcurrentHashMap.class);
    }

    @Test
    public void testHandlerPluginUpdateSameConfig() {
        ElasticSearchLogCollectConfig.ElasticSearchLogConfig existingConfig = createValidConfig();
        Singleton.INST.single(existingConfig.getClass(), existingConfig);

        PluginData pluginData = createPluginData();
        pluginData.setConfig(GsonUtils.getGson().toJson(existingConfig));

        loggingElasticSearchPluginDataHandler.handlerPlugin(pluginData);

        verify(loggingElasticSearchPluginDataHandler, never()).doRefreshConfig(any());
    }

    @Test
    public void testHandlerPluginUpdateDifferentConfig() {
        ElasticSearchLogCollectConfig.ElasticSearchLogConfig existingConfig = createValidConfig();
        Singleton.INST.single(existingConfig.getClass(), existingConfig);

        ElasticSearchLogCollectConfig.ElasticSearchLogConfig updatedConfig = createValidConfig();
        updatedConfig.setUsername("updatedUsername");
        updatedConfig.setPassword("updatedPassword");
        updatedConfig.setAuthCache(false);
        PluginData pluginData = createPluginData();
        pluginData.setConfig(GsonUtils.getGson().toJson(updatedConfig));

        doNothing().when(loggingElasticSearchPluginDataHandler).doRefreshConfig(any());

        loggingElasticSearchPluginDataHandler.handlerPlugin(pluginData);

        verify(loggingElasticSearchPluginDataHandler, times(1)).doRefreshConfig(any());
    }

    private PluginData createPluginData() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setId(UUID.randomUUID().toString().replace("-", ""));
        return pluginData;
    }

    private ElasticSearchLogCollectConfig.ElasticSearchLogConfig createValidConfig() {
        ElasticSearchLogCollectConfig.ElasticSearchLogConfig config = new ElasticSearchLogCollectConfig.ElasticSearchLogConfig();
        config.setHost("localhost");
        config.setPort("9200");
        config.setUsername("username");
        config.setPassword("password");
        config.setAuthCache(true);
        config.setSampleRate("1");
        config.setMaxResponseBody(1024);
        config.setMaxRequestBody(1024);
        config.setCompressAlg("none");
        config.setIndexName("shenyu-access-logging");
        return config;
    }
}
