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

package org.apache.shenyu.e2e.testcase.http;

import com.google.common.collect.Lists;
import io.restassured.http.Method;
import org.apache.commons.collections.CollectionUtils;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.rocketmq.client.consumer.DefaultMQPushConsumer;
import org.apache.rocketmq.client.consumer.listener.ConsumeConcurrentlyStatus;
import org.apache.rocketmq.client.consumer.listener.MessageListenerConcurrently;
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

import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;

public class DividePluginCases implements ShenYuScenarioProvider {

    private static final String NAMESERVER = "http://localhost:31876";

    private static final String CONSUMERGROUP = "shenyu-plugin-logging-rocketmq";

    private static final String TOPIC = "shenyu-access-logging";

    private static final String TEST = "/http/order/findById?id=123";

    private static final Logger LOG = LoggerFactory.getLogger(DividePluginCases.class);

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testDivideHello(),
                testRocketMQHello(),
                testKafkaHello()
        );
    }

    private ShenYuScenarioSpec testDivideHello() {
        return ShenYuScenarioSpec.builder()
                .name("http client hello1")
                .beforeEachSpec(ShenYuBeforeEachSpec.builder()
                        .checker(exists("/http/order/findById?id=123"))
                        .build())
                .caseSpec(ShenYuCaseSpec.builder()
                        .addExists("/http/order/findById?id=123")
                        .build())
                .build();
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
                                    AtomicBoolean isLog = new AtomicBoolean(false);
                                    try {
                                        Thread.sleep(1000 * 30);
                                        request.request(Method.GET, "/http/order/findById?id=23");
                                        Properties props = new Properties();
                                        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG,
                                                StringDeserializer.class.getName());
                                        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG,
                                                StringDeserializer.class.getName());
                                        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "127.0.0.1:31877");
                                        KafkaConsumer<String, String> consumer = new KafkaConsumer<>(props);
                                        consumer.subscribe(Arrays.asList(TOPIC));
                                        AtomicReference<Boolean> keepCosuming = new AtomicReference<>(true);
                                        Instant start = Instant.now();
                                        while (keepCosuming.get()) {
                                            if (Duration.between(start, Instant.now()).toMillis() > 60000) {
                                                keepCosuming.set(false);
                                            }
                                            ConsumerRecords<String, String> records = consumer.poll(Duration.ofMillis(1000));
                                            records.forEach(record -> {
                                                String message = record.value();
                                                if (message.contains("/http/order/findById?id=23")) {
                                                    isLog.set(true);
                                                    keepCosuming.set(false);
                                                }
                                            });
                                        }
                                        Assertions.assertTrue(isLog.get());
                                    } catch (InterruptedException e) {
                                        LOG.info("isLog.get():{}", isLog.get());
                                        LOG.error("error", e);
                                        throw new RuntimeException(e);
                                    }
                                }).build()
                ).build();
    }

    private ShenYuScenarioSpec testRocketMQHello() {
        return ShenYuScenarioSpec.builder()
                .name("testRocketMQHello")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.LOGGING_ROCKETMQ)
                                                .name("1")
                                                .matchMode(MatchMode.OR)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/http"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .name("1")
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
                                    AtomicBoolean isLog = new AtomicBoolean(false);
                                    try {
                                        Thread.sleep(1000 * 30);
                                        request.request(Method.GET, "/http/order/findById?id=23");
                                        DefaultMQPushConsumer consumer = new DefaultMQPushConsumer(CONSUMERGROUP);
                                        consumer.setNamesrvAddr(NAMESERVER);
                                        consumer.subscribe(TOPIC, "*");
                                        consumer.registerMessageListener((MessageListenerConcurrently) (msgs, consumeConcurrentlyContext) -> {
                                            LOG.info("Msg:{}", msgs);
                                            if (CollectionUtils.isNotEmpty(msgs)) {
                                                msgs.forEach(e -> {
                                                    if (new String(e.getBody()).contains("/http/order/findById?id=23")) {
                                                        isLog.set(true);
                                                    }
                                                });
                                            }
                                            return ConsumeConcurrentlyStatus.CONSUME_SUCCESS;
                                        });
                                        LOG.info("consumer.start ; isLog.get():{}", isLog.get());
                                        consumer.start();
                                        Thread.sleep(1000 * 30);
                                        LOG.info("isLog.get():{}", isLog.get());
                                        Assertions.assertTrue(isLog.get());
                                    } catch (Exception e) {
                                        LOG.error("error", e);
                                        Assertions.assertTrue(isLog.get());
                                    }
                                })
                                .build()
                )
//                .afterEachSpec(ShenYuAfterEachSpec.builder()
//                        .deleteWaiting(notExists(TEST)).build())
                .build();
    }
}
