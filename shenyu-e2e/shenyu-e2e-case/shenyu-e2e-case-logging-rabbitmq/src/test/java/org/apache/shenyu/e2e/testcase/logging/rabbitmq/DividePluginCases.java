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

package org.apache.shenyu.e2e.testcase.logging.rabbitmq;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.GetResponse;
import io.restassured.http.Method;
import net.jpountz.lz4.LZ4Factory;
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuScenarioSpec;
import org.apache.shenyu.e2e.model.MatchMode;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.data.Condition;
import org.junit.jupiter.api.Assertions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;
import static org.awaitility.Awaitility.await;

public class DividePluginCases implements ShenYuScenarioProvider {

    private static final String QUEUE = "queue.logging.plugin";

    private static final String HEALTH_URI = "/http/order/findById?id=rabbitmq-e2e-health";

    private static final int CONNECTION_TIMEOUT_MILLIS = 5_000;

    private static final int HANDSHAKE_TIMEOUT_MILLIS = 5_000;

    private static final Duration LOG_CONSUME_TIMEOUT = Duration.ofSeconds(30);

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final Logger LOG = LoggerFactory.getLogger(DividePluginCases.class);

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(testDivideHello(), testRabbitMqLog());
    }

    private ShenYuScenarioSpec testDivideHello() {
        return ShenYuScenarioSpec.builder()
                .name("http client hello")
                .beforeEachSpec(ShenYuBeforeEachSpec.builder()
                        .checker(exists(HEALTH_URI))
                        .build())
                .caseSpec(ShenYuCaseSpec.builder()
                        .addExists(HEALTH_URI)
                        .build())
                .build();
    }

    private ShenYuScenarioSpec testRabbitMqLog() {
        final String requestUri = "/http/order/findById?id=rabbitmq-e2e-" + UUID.randomUUID();
        return ShenYuScenarioSpec.builder()
                .name("testRabbitMqLog")
                .beforeEachSpec(ShenYuBeforeEachSpec.builder()
                        .addSelectorAndRule(
                                newSelectorBuilder("selector", Plugin.LOGGINGRABBITMQ)
                                        .name("rabbitmq-e2e")
                                        .matchMode(MatchMode.OR)
                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/http"))
                                        .build(),
                                newRuleBuilder("rule")
                                        .name("rabbitmq-e2e")
                                        .matchMode(MatchMode.OR)
                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/http"))
                                        .build())
                        .checker(exists(HEALTH_URI))
                        .build())
                .caseSpec(ShenYuCaseSpec.builder()
                        .add(request -> {
                            try {
                                ConnectionFactory factory = new ConnectionFactory();
                                factory.setHost("localhost");
                                factory.setPort(5672);
                                factory.setUsername("admin");
                                factory.setPassword("admin");
                                factory.setVirtualHost("/");
                                factory.setConnectionTimeout(CONNECTION_TIMEOUT_MILLIS);
                                factory.setHandshakeTimeout(HANDSHAKE_TIMEOUT_MILLIS);
                                try (Connection connection = factory.newConnection(); Channel channel = connection.createChannel()) {
                                    request.request(Method.GET, requestUri);
                                    await().alias("RabbitMQ access log for " + requestUri)
                                            .atMost(LOG_CONSUME_TIMEOUT)
                                            .until(() -> containsRequestLog(channel, requestUri));
                                }
                            } catch (Exception e) {
                                LOG.error("Failed to consume RabbitMQ access log", e);
                                Assertions.fail("RabbitMQ access log was not received", e);
                            }
                        })
                        .build())
                .build();
    }

    private boolean containsRequestLog(final Channel channel, final String requestUri) throws Exception {
        GetResponse response = channel.basicGet(QUEUE, true);
        if (Objects.isNull(response)) {
            return false;
        }
        JsonNode compressedLog = MAPPER.readTree(response.getBody());
        int originalLength = compressedLog.path("length").asInt();
        byte[] compressedData = compressedLog.path("compressedData").binaryValue();
        byte[] logBytes = LZ4Factory.fastestInstance().safeDecompressor().decompress(compressedData, originalLength);
        String log = new String(logBytes, StandardCharsets.UTF_8);
        LOG.info("RabbitMQ access log received: {}", log);
        return log.contains(requestUri);
    }
}
