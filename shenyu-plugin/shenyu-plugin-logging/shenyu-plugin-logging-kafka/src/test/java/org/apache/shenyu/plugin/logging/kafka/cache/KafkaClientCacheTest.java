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

package org.apache.shenyu.plugin.logging.kafka.cache;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.Assert.assertNotNull;

/**
 * KafkaClientCacheTest.
 */
public class KafkaClientCacheTest {

    private KafkaClientCache cache;

    private SelectorData selectorData;

    @BeforeEach
    void setup() {
        cache = KafkaClientCache.getInstance();
        selectorData = SelectorData.builder().id("153153464562434")
                .handle("{\n"
                        + "  \"bootstrapServer\": \"127.0.0.1:5672\",\n"
                        + "  \"producerGroup\": \"testGroup\",\n"
                        + "  \"compressAlg\": \"gzip\",\n"
                        + "  \"securityProtocol\": \"PLAINTEXT\",\n"
                        + "  \"saslMechanism\": \"PLAIN\",\n"
                        + "  \"userName\": \"guest\",\n"
                        + "  \"passWord\": \"guest\"\n"
                        + "}")
                .build();
        cache.invalidateAll();
    }

    @Test
    public void testInitRabbitmqClient() {
        KafkaLogCollectConfig.LogApiConfig logApiConfig = GsonUtils.getInstance()
                .fromJson(selectorData.getHandle(), KafkaLogCollectConfig.LogApiConfig.class);
        cache.initKafkaClient(selectorData.getId(), logApiConfig);
        Map<String, KafkaLogCollectClient> clientCache = cache.getClientCache();
        KafkaLogCollectClient kafkaLogCollectClient = clientCache.get(selectorData.getId());
        assertNotNull(kafkaLogCollectClient);
    }

}
