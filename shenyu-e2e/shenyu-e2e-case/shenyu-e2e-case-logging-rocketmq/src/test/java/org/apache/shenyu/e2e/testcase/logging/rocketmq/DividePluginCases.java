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
import io.restassured.builder.ResponseSpecBuilder;
import org.apache.rocketmq.client.consumer.DefaultLitePullConsumer;
import org.apache.rocketmq.client.exception.MQClientException;
import org.apache.rocketmq.common.TopicConfig;
import org.apache.rocketmq.common.consumer.ConsumeFromWhere;
import org.apache.rocketmq.common.message.MessageQueue;
import org.apache.rocketmq.common.protocol.ResponseCode;
import org.apache.rocketmq.common.protocol.body.ClusterInfo;
import org.apache.rocketmq.common.protocol.route.BrokerData;
import org.apache.rocketmq.tools.admin.DefaultMQAdminExt;
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
import java.util.Collection;
import java.util.List;
import java.util.UUID;

import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;
import static org.awaitility.Awaitility.await;

public class DividePluginCases implements ShenYuScenarioProvider {

    private static final String NAMESERVER = "localhost:31876";

    private static final String CONSUMERGROUP = "shenyu-plugin-logging-rocketmq";

    private static final String TOPIC = "shenyu-access-logging";

    private static final String TEST = "/http/order/findById?id=123";

    private static final Duration LOG_CONSUME_TIMEOUT = Duration.ofSeconds(60);

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
                        .waiting(Method.GET, TEST, new ResponseSpecBuilder().expectStatusCode(200).build())
                        .build())
                .caseSpec(ShenYuCaseSpec.builder()
                        .addExists("/http/order/findById?id=123")
                        .build())
                .build();
    }

    static void initializeTopic() throws Exception {
        DefaultMQAdminExt admin = new DefaultMQAdminExt("shenyu-e2e-topic-" + UUID.randomUUID());
        admin.setNamesrvAddr(NAMESERVER);
        try {
            admin.start();
            prepareTopic(admin);
        } finally {
            admin.shutdown();
        }
    }

    static void prepareTopic(final DefaultMQAdminExt admin) throws Exception {
        ClusterInfo cluster = await().alias("RocketMQ broker registered with NameServer")
                .atMost(Duration.ofSeconds(30))
                .until(admin::examineBrokerClusterInfo, info -> !info.getBrokerAddrTable().isEmpty());
        for (BrokerData broker : cluster.getBrokerAddrTable().values()) {
            admin.createAndUpdateTopicConfig(broker.selectBrokerAddr(), new TopicConfig(TOPIC));
        }
        await().alias("RocketMQ test topic route published")
                .atMost(Duration.ofSeconds(30))
                .until(() -> {
                    try {
                        return admin.examineTopicRouteInfo(TOPIC).getQueueDatas().stream()
                                .anyMatch(queue -> queue.getReadQueueNums() > 0 && queue.getWriteQueueNums() > 0);
                    } catch (MQClientException ex) {
                        if (ex.getResponseCode() == ResponseCode.TOPIC_NOT_EXIST) {
                            return false;
                        }
                        throw ex;
                    }
                });
    }

    static void prepareConsumer(final DefaultLitePullConsumer consumer) throws MQClientException {
        Collection<MessageQueue> queues = consumer.fetchMessageQueues(TOPIC);
        Assertions.assertFalse(queues.isEmpty(), "RocketMQ test topic has no consumer queues");
        consumer.assign(queues);
        for (MessageQueue queue : queues) {
            consumer.seekToBegin(queue);
        }
    }

    static void awaitRequestLog(final DefaultLitePullConsumer consumer, final String endpoint) {
        await().alias("RocketMQ access log for " + endpoint)
                .atMost(LOG_CONSUME_TIMEOUT).pollInterval(Duration.ofMillis(200))
                .until(() -> {
                    return consumer.poll(200).stream().anyMatch(message -> {
                        String body = new String(message.getBody(), StandardCharsets.UTF_8);
                        LOG.info("RocketMQ access log: {}", body);
                        return body.contains(endpoint);
                    });
                });
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
                                .waiting(Method.GET, TEST, new ResponseSpecBuilder().expectStatusCode(200).build())
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .add(request -> {
                                    String requestId = UUID.randomUUID().toString();
                                    String endpoint = "/http/order/findById?id=23&e2e=" + requestId;
                                    DefaultLitePullConsumer consumer = new DefaultLitePullConsumer(CONSUMERGROUP + "-" + requestId);
                                    try {
                                        consumer.setNamesrvAddr(NAMESERVER);
                                        consumer.setConsumeFromWhere(ConsumeFromWhere.CONSUME_FROM_FIRST_OFFSET);
                                        consumer.setAutoCommit(false);
                                        consumer.start();
                                        LOG.info("RocketMQ consumer started");
                                        prepareConsumer(consumer);
                                        request.request(Method.GET, endpoint).then().statusCode(200);
                                        awaitRequestLog(consumer, endpoint);
                                    } catch (Exception e) {
                                        LOG.error("error", e);
                                        Assertions.fail("Failed to consume RocketMQ access log", e);
                                    } finally {
                                        consumer.shutdown();
                                    }
                                })
                                .build()
                )
//                .afterEachSpec(ShenYuAfterEachSpec.builder()
//                        .deleteWaiting(notExists(TEST)).build())
                .build();
    }
}
