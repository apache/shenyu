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

package org.apache.shenyu.plugin.logging.rabbitmq.cache;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.rabbitmq.client.RabbitmqLogCollectClient;
import org.apache.shenyu.plugin.logging.rabbitmq.config.RabbitmqLogCollectConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * RabbitmqClientCacheTest.
 */
public class RabbitmqClientCacheTest {

    private RabbitmqClientCache cache;

    private SelectorData selectorData;

    @BeforeEach
    void setup() {
        cache = RabbitmqClientCache.getInstance();
        selectorData = SelectorData.builder().id("153153464562434")
                .handle("{\n"
                        + "  \"queueName\": \"shenyu-logs-queue\",\n"
                        + "  \"exchangeName\": \"shenyu-logs-exchange\",\n"
                        + "  \"host\": \"127.0.0.1\",\n"
                        + "  \"port\": 5672,\n"
                        + "  \"username\": \"admin\",\n"
                        + "  \"password\": \"123456\",\n"
                        + "  \"routingKey\": \"shenyu.log.key\",\n"
                        + "  \"exchangeType\": \"topic\",\n"
                        + "  \"virtualHost\": \"/\",\n"
                        + "  \"durable\": true,\n"
                        + "  \"exclusive\": false,\n"
                        + "  \"autoDelete\": false,\n"
                        + "  \"args\": {\n"
                        + "    \"x-message-ttl\": 60000,\n"
                        + "    \"x-max-length\": 1000\n"
                        + "  }\n"
                        + "}")
                .build();
        cache.invalidateAll();
    }

    @Test
    public void testInitRabbitmqClient() {
        RabbitmqLogCollectConfig.LogApiConfig logApiConfig = GsonUtils.getInstance()
                .fromJson(selectorData.getHandle(), RabbitmqLogCollectConfig.LogApiConfig.class);
        cache.initRabbitmqClient(selectorData.getId(), logApiConfig);
        Map<String, RabbitmqLogCollectClient> clientCache = cache.getClientCache();
        RabbitmqLogCollectClient rabbitmqLogCollectClient = clientCache.get(selectorData.getId());
        assertNotNull(rabbitmqLogCollectClient);
    }

}
