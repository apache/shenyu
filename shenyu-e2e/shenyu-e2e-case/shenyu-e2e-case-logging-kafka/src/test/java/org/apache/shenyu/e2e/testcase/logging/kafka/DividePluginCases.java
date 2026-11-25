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

package org.apache.shenyu.e2e.testcase.logging.kafka;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import io.restassured.http.Method;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuScenarioSpec;
import org.apache.shenyu.e2e.model.MatchMode;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.data.Condition;

import java.net.URI;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.UUID;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;
import static org.awaitility.Awaitility.await;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class DividePluginCases implements ShenYuScenarioProvider {

    private static final String TOPIC = "shenyu-access-logging";

    private static final String TEST = "/http/order/findById?id=123";
    
    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final Duration LOG_CONSUME_TIMEOUT = Duration.ofSeconds(60);

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testDivideHello(),
                testKafkaHello()
        );
    }

    private ShenYuScenarioSpec testDivideHello() {
        return ShenYuScenarioSpec.builder()
                .name("http client hello1")
                .beforeEachSpec(ShenYuBeforeEachSpec.builder()
                        .checker(exists(TEST))
                        .build())
                .caseSpec(ShenYuCaseSpec.builder()
                        .addExists(TEST)
                        .build())
                .build();
    }

    static Optional<JsonNode> findRequestLog(final Iterable<ConsumerRecord<String, String>> records,
                                             final URI endpoint) throws JsonProcessingException {
        for (var record : records) {
            JsonNode log = MAPPER.readTree(record.value());
            URI requestUri = URI.create(log.path("requestUri").asText());
            if (Objects.equals(endpoint.getRawPath(), requestUri.getRawPath())
                    && Objects.equals(endpoint.getRawQuery(), requestUri.getRawQuery())) {
                return Optional.of(log);
            }
        }
        return Optional.empty();
    }

    private ShenYuScenarioSpec testKafkaHello() {
        return ShenYuScenarioSpec.builder()
                .name("testKafkaHello")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.LOGGING_KAFKA)
                                                .name("2")
                                                .matchMode(MatchMode.OR)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/http"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .name("2")
                                                .matchMode(MatchMode.OR)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/http"))
                                                .build()
                                )
                                .checker(exists(TEST))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .add(request -> {
                                    String requestId = UUID.randomUUID().toString();
                                    URI endpoint = URI.create("/http/order/findById?id=23&e2e=" + requestId);
                                    try (KafkaConsumer<String, String> consumer = new KafkaConsumer<>(consumerProperties(requestId))) {
                                        consumer.subscribe(List.of(TOPIC));
                                        request.request(Method.GET, endpoint.toString()).then().statusCode(200).body("id", equalTo("23"));
                                        JsonNode log = await().alias("Kafka access log for " + endpoint)
                                                .pollInSameThread().atMost(LOG_CONSUME_TIMEOUT)
                                                .until(() -> findRequestLog(consumer.poll(Duration.ofMillis(200)), endpoint), Optional::isPresent)
                                                .orElseThrow();
                                        assertEquals(Method.GET.name(), log.path("requestMethod").asText());
                                        assertEquals(200, log.path("status").asInt());
                                    }
                                }).build()
                ).build();
    }

    private Properties consumerProperties(final String requestId) {
        Properties properties = new Properties();
        properties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
        properties.put(ConsumerConfig.GROUP_ID_CONFIG, "shenyu-logging-kafka-e2e-" + requestId);
        properties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        properties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        properties.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "false");
        properties.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
        return properties;
    }
}
