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

import com.google.common.collect.Lists;
import io.restassured.http.Method;
import org.apache.commons.collections.CollectionUtils;
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
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;
import static org.awaitility.Awaitility.await;

public class DividePluginCases implements ShenYuScenarioProvider {

    private static final String NAMESERVER = "http://localhost:31876";

    private static final String CONSUMERGROUP = "shenyu-plugin-logging-rocketmq";

    private static final String TOPIC = "shenyu-access-logging";

    private static final String TEST = "/http/order/findById?id=123";

    private static final Duration LOG_CONSUME_TIMEOUT = Duration.ofSeconds(30);

    private static final Logger LOG = LoggerFactory.getLogger(DividePluginCases.class);

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testDivideHello(),
                testRocketMQHello()
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
                                    String runId = UUID.randomUUID().toString();
                                    String uri = "/http/order/findById?id=rocketmq-e2e-" + runId;
                                    DefaultMQPushConsumer consumer = new DefaultMQPushConsumer(CONSUMERGROUP + "-" + runId);
                                    try (RocketMQTestSupport support = new RocketMQTestSupport(NAMESERVER, TOPIC, runId);
                                            AutoCloseable consumerShutdown = () -> consumer.shutdown()) {
                                        consumer.setNamesrvAddr(NAMESERVER);
                                        consumer.setInstanceName("rocketmq-e2e-" + runId);
                                        consumer.subscribe(TOPIC, "*");
                                        consumer.registerMessageListener((MessageListenerConcurrently) (msgs, consumeConcurrentlyContext) -> {
                                            LOG.info("Msg:{}", msgs);
                                            if (CollectionUtils.isNotEmpty(msgs)) {
                                                msgs.forEach(e -> {
                                                    if (RocketMQTestSupport.matchesAccessLog(e.getBody(), uri)) {
                                                        isLog.set(true);
                                                    }
                                                });
                                            }
                                            return ConsumeConcurrentlyStatus.CONSUME_SUCCESS;
                                        });
                                        support.prepare(consumer);
                                        try {
                                            Assertions.assertEquals(200, request.request(Method.GET, uri).statusCode(), "HTTP request failed: " + uri);
                                        } catch (Exception e) {
                                            LOG.error("HTTP request failed: {}", uri, e);
                                            Assertions.fail("HTTP request failed: " + uri, e);
                                        }
                                        try {
                                            await().alias("RocketMQ access log for " + uri).atMost(LOG_CONSUME_TIMEOUT).untilTrue(isLog);
                                        } catch (Exception e) {
                                            String diagnostic = support.diagnostics(consumer);
                                            LOG.error("Failed to consume RocketMQ access log: {}; {}", uri, diagnostic, e);
                                            Assertions.fail("Failed to consume RocketMQ access log: " + uri + "; " + diagnostic, e);
                                        }
                                        LOG.info("isLog.get():{}", isLog.get());
                                    } catch (Exception e) {
                                        LOG.error("RocketMQ case failed for {}", uri, e);
                                        Assertions.fail("RocketMQ case failed for " + uri, e);
                                    }
                                })
                                .build()
                )
//                .afterEachSpec(ShenYuAfterEachSpec.builder()
//                        .deleteWaiting(notExists(TEST)).build())
                .build();
    }
}
