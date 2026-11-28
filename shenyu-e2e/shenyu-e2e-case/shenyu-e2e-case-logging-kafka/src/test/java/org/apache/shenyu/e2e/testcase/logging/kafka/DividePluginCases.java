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
import org.apache.kafka.clients.consumer.ConsumerRecords;
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
import org.awaitility.core.ConditionTimeoutException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;
import static org.awaitility.Awaitility.await;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

public class DividePluginCases implements ShenYuScenarioProvider {

    private static final Logger LOG = LoggerFactory.getLogger(DividePluginCases.class);

    private static final String TOPIC = "shenyu-access-logging";

    private static final String TEST = "/http/order/findById?id=123";
    
    private static final ObjectMapper MAPPER = new ObjectMapper();

    // A fresh consumer group may need to scan retained history before finding the current request.
    private static final Duration LOG_CONSUME_TIMEOUT = Duration.ofMinutes(5);

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
                                             final URI endpoint) {
        for (var record : records) {
            String value = record.value();
            if (Objects.isNull(value) || value.isBlank()) {
                continue;
            }
            try {
                JsonNode log = MAPPER.readTree(value);
                String uri = log.path("requestUri").textValue();
                if (Objects.isNull(uri) || uri.isBlank()) {
                    continue;
                }
                URI requestUri = URI.create(uri);
                if (Objects.equals(endpoint.getRawPath(), requestUri.getRawPath())
                        && Objects.equals(endpoint.getRawQuery(), requestUri.getRawQuery())) {
                    return Optional.of(log);
                }
            } catch (JsonProcessingException | IllegalArgumentException ignored) {
                LOG.debug("Skipping malformed Kafka access log in partition {} at offset {}", record.partition(), record.offset());
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
                                        JsonNode log = awaitRequestLog(consumer, endpoint);
                                        assertThat(log.path("requestMethod").asText(), equalTo(Method.GET.name()));
                                        assertThat(log.path("status").asInt(), equalTo(200));
                                    }
                                }).build()
                ).build();
    }

    private static JsonNode awaitRequestLog(final KafkaConsumer<String, String> consumer, final URI endpoint) {
        AtomicLong scanned = new AtomicLong();
        try {
            return await().alias("Kafka access log for " + endpoint).pollInSameThread().atMost(LOG_CONSUME_TIMEOUT)
                    .until(() -> {
                        ConsumerRecords<String, String> records = consumer.poll(Duration.ofMillis(200));
                        scanned.addAndGet(records.count());
                        return findRequestLog(records, endpoint);
                    }, Optional::isPresent).orElseThrow();
        } catch (ConditionTimeoutException timeout) {
            throw new AssertionError("No Kafka access log for " + endpoint + " after scanning " + scanned.get()
                    + " records in " + TOPIC + " (assigned partitions: " + consumer.assignment() + ")", timeout);
        }
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
