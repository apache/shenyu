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

package org.apache.shenyu.e2e.testcase.logging.rocketmq;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.model.data.PluginData;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.apache.shenyu.e2e.constant.Constants.SYS_DEFAULT_NAMESPACE_NAMESPACE_ID;

class LoggingRuleSyncTest {

    @Test
    void waitsForEnabledConfigurationInTheCurrentNamespace() throws Exception {
        AtomicInteger reads = new AtomicInteger();
        ObjectMapper mapper = new ObjectMapper();
        String config = "{\"topic\":\"shenyu-access-logging\",\"namesrvAddr\":\"broker:9876\"}";
        GatewayClient gateway = new GatewayClient("sync-test", "gateway", "http://localhost", new Properties()) {
            @Override
            public Map<String, PluginData> getPluginCache() {
                int attempt = reads.incrementAndGet();
                String namespace = attempt == 1 ? "other-namespace" : SYS_DEFAULT_NAMESPACE_NAMESPACE_ID;
                String actual = attempt == 3 ? "old-topic" : "shenyu-access-logging";
                return Map.of("loggingRocketMQ", mapper.convertValue(Map.of("name", "loggingRocketMQ", "namespaceId", namespace,
                        "enabled", attempt != 2, "config", "{\"namesrvAddr\":\"broker:9876\",\"topic\":\"" + actual + "\"}"), PluginData.class));
            }
        };

        DividePluginTest.waitForPluginConfiguration(gateway, config);

        assertTrue(reads.get() >= 4);
    }

}
