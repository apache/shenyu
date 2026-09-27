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
import org.apache.kafka.common.errors.TimeoutException;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
    }

    @Test
    public void testInitClientDoesNotSendRecord() {
        try (MockedConstruction<KafkaProducer> construction = mockConstruction(KafkaProducer.class)) {
            Assertions.assertTrue(kafkaLogCollectClient.initClient0(globalLogConfig));
            Assertions.assertEquals(1, construction.constructed().size());
            verify(construction.constructed().get(0)).partitionsFor("shenyu-access-logging");
            verify(construction.constructed().get(0), never()).send(any());
        }
        kafkaLogCollectClient.close0();
    }

    @Test
    public void testInitClientFailsWhenTopicMetadataIsUnavailable() {
        try (MockedConstruction<KafkaProducer> construction = mockConstruction(KafkaProducer.class,
                (mock, context) -> when(mock.partitionsFor("shenyu-access-logging")).thenThrow(new TimeoutException("metadata unavailable")))) {
            Assertions.assertFalse(kafkaLogCollectClient.initClient0(globalLogConfig));
            verify(construction.constructed().get(0)).close();
            verify(construction.constructed().get(0), never()).send(any());
        }
    }
}
