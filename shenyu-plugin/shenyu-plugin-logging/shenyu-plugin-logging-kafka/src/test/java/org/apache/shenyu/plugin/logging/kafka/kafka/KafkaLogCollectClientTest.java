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

package org.apache.shenyu.plugin.logging.kafka.kafka;

import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.common.KafkaException;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import java.lang.reflect.Field;
import java.util.Collections;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.verify;

/**
 * The Test Case For KafkaLogCollectClient.
 */
public class KafkaLogCollectClientTest {

    private final PluginData pluginData = new PluginData();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    private KafkaLogCollectClient kafkaLogCollectClient;
    
    private KafkaLogCollectConfig.KafkaLogConfig globalLogConfig;

    @BeforeEach
    public void setUp() {
        this.kafkaLogCollectClient = new KafkaLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"topic\":\"shenyu-access-logging\", \"bootstrapServer\":\"localhost:8082\"}");
        globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), KafkaLogCollectConfig.KafkaLogConfig.class);
        globalLogConfig.setCompressAlg("LZ4");
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        shenyuRequestLog.setSelectorId("test-selector-id");
    }

    @Test
    @Disabled
    public void testInitClient() throws NoSuchFieldException, IllegalAccessException {
        try (MockedConstruction<KafkaProducer> construction = mockConstruction(KafkaProducer.class)) {
            kafkaLogCollectClient.initClient(globalLogConfig);
            Field field = kafkaLogCollectClient.getClass().getDeclaredField("topic");
            field.setAccessible(true);
            Assertions.assertEquals(field.get(kafkaLogCollectClient), "shenyu-access-logging");
            kafkaLogCollectClient.close();
        }
    }

    @Test
    public void testConsume0FlushesOncePerBatch() throws NoSuchFieldException, IllegalAccessException {
        KafkaProducer<String, String> producer = mock(KafkaProducer.class);
        setProducer(producer);
        kafkaLogCollectClient.consume0(Collections.singletonList(shenyuRequestLog));
        verify(producer).flush();
    }

    @Test
    public void testConsume0HandlesFlushFailure() throws NoSuchFieldException, IllegalAccessException {
        KafkaProducer<String, String> producer = mock(KafkaProducer.class);
        doThrow(new KafkaException("flush error")).when(producer).flush();
        setProducer(producer);
        Assertions.assertDoesNotThrow(() -> kafkaLogCollectClient.consume0(Collections.singletonList(shenyuRequestLog)));
        verify(producer).flush();
    }

    private void setProducer(final KafkaProducer<String, String> producer) throws NoSuchFieldException, IllegalAccessException {
        Field field = KafkaLogCollectClient.class.getDeclaredField("producer");
        field.setAccessible(true);
        field.set(kafkaLogCollectClient, producer);
    }
}
