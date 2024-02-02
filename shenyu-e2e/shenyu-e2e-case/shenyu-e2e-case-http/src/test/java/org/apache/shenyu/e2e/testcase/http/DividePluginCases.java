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
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuScenarioSpec;
import org.apache.shenyu.e2e.model.MatchMode;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.data.Condition;

import java.util.List;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;

public class DividePluginCases implements ShenYuScenarioProvider {


//    private final static String nameServer = "http://localhost:9876";
//
//    private final static String consumerGroup = "shenyu-plugin-logging-rocketmq";
//
//    private final static String topic = "shenyu-access-logging";

    private static final String TEST = "/http/order/findById?id=123";

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testDivideHello(),
                testRocketMQHello()
        );
    }

    private ShenYuScenarioSpec testDivideHello() {
        return ShenYuScenarioSpec.builder()
                .name("http client hello")
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
//                                    try {
//                                        request.request(Method.GET, "/http/order/findById?id=22");
//                                        DefaultMQPushConsumer consumer = new DefaultMQPushConsumer(consumerGroup);
//                                        consumer.setNamesrvAddr(nameServer);
//                                        consumer.subscribe(topic, "*");
//                                        AtomicBoolean isLog = new AtomicBoolean(false);
//                                        consumer.registerMessageListener(new MessageListenerConcurrently() {
//                                            public ConsumeConcurrentlyStatus consumeMessage(List<MessageExt> msgs, ConsumeConcurrentlyContext consumeConcurrentlyContext) {
//                                                if (CollectionUtils.isNotEmpty(msgs)) {
//                                                    msgs.forEach(e -> {
//                                                        System.out.println("Thread：{}，QueueID：{}，receive message：{}" + Thread.currentThread().getName() + e.getQueueId() + new String(e.getBody()));
//                                                        if (new String(e.getBody()).contains("/http/order/findById?id=22")) {
//                                                            isLog.set(true);
//                                                        }
//                                                    });
//
//                                                }
//                                                return ConsumeConcurrentlyStatus.CONSUME_SUCCESS;
//                                            }
//                                        });
//                                        consumer.start();
//                                        Thread.sleep(2000);
//                                        Assertions.assertEquals(true, isLog.get());
//                                    } catch (Exception e) {
//
//                                    }

                                })
                                .build()
                )
//                .afterEachSpec(ShenYuAfterEachSpec.builder()
//                        .deleteWaiting(notExists(TEST)).build())
                .build();
    }
}
