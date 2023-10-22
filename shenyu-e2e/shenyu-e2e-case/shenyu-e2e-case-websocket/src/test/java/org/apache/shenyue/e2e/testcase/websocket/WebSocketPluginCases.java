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

package org.apache.shenyue.e2e.testcase.websocket;

import com.google.common.collect.Lists;
import io.restassured.http.Method;
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.*;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.data.Condition;
import org.apache.shenyu.e2e.model.handle.WebSocketRuleHandle;
import org.apache.shenyu.e2e.model.handle.WebSocketSelectorHandles;
import org.apache.shenyu.e2e.model.handle.WebSocketSelectorHandles.WebSocketHandler;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.function.Supplier;

import static org.apache.shenyu.e2e.template.ResourceDataTemplate.*;

public class WebSocketPluginCases implements ShenYuScenarioProvider {

    private static final String WEBSOCKET_URI = "ws://localhost:9195/ws-annotation/myWs?token=Jack";

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testWithUriEquals(),
                testWithUriPathPattern(),
                testWithUriStartWith()
        );
    }

    /**
     * test with uri equal.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriEquals() {
        return ShenYuScenarioSpec.builder()
                .name("single-websocket uri =]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.WEBSOCKET)
                                                .name("/ws-annotation")
                                                .handle(
                                                        (
                                                                WebSocketSelectorHandles.builder().add(
                                                                WebSocketHandler.builder().protocol("ws://")
                                                                        .upstreamUrl("localhost:8001").status(true).timestamp(System.currentTimeMillis()).upstreamHost("localhost")
                                                                        .weight(50).warmup(600000).build()).build()
                                                        )
                                                )
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/myWs"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(WebSocketRuleHandle.builder().loadBalance("random").timeout(3000).retry(0).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/myWs"))
                                                .build()
                                )
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }

    /**
     * test with uri path pattern.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriPathPattern() {
        return ShenYuScenarioSpec.builder()
                .name("single-websocket uri path_pattern]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.WEBSOCKET)
                                                .name("/ws-annotation")
                                                .handle(
                                                        (
                                                                WebSocketSelectorHandles.builder().add(
                                                                        WebSocketHandler.builder().protocol("ws://")
                                                                                .upstreamUrl("localhost:8001").status(true).timestamp(System.currentTimeMillis()).upstreamHost("localhost")
                                                                                .weight(50).warmup(600000).build()).build()
                                                        )
                                                )
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/myWs"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(WebSocketRuleHandle.builder().loadBalance("random").timeout(3000).retry(0).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/myWs"))
                                                .build()
                                )
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }

    /**
     * test with uri start with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriStartWith() {
        return ShenYuScenarioSpec.builder()
                .name("single-websocket uri starts_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.WEBSOCKET)
                                                .name("/ws-annotation")
                                                .handle(
                                                        (
                                                                WebSocketSelectorHandles.builder().add(
                                                                        WebSocketHandler.builder().protocol("ws://")
                                                                                .upstreamUrl("localhost:8001").status(true).timestamp(System.currentTimeMillis()).upstreamHost("localhost")
                                                                                .weight(50).warmup(600000).build()).build()
                                                        )
                                                )
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/myWs"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(WebSocketRuleHandle.builder().loadBalance("random").timeout(3000).retry(0).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/myWs"))
                                                .build()
                                )
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }
//
//    /**
//     * test with uri end with.
//     *
//     * @return ShenYuScenarioSpec
//     */
//    public ShenYuScenarioSpec testWithEndWith() {
//        return ShenYuScenarioSpec.builder()
//                .name("single-websocket uri ends_with]")
//                .beforeEachSpec(
//                        ShenYuBeforeEachSpec.builder()
//                                .addSelectorAndRule(
//                                        newSelectorBuilder("selector", Plugin.WEBSOCKET)
//                                                .name("/ws-annotation")
//                                                .handle(WebSocketSelectorHandle.builder().protocol("ws://")
//                                                        .upstreamUrl("localhost:8001").status(true).timestamp(System.currentTimeMillis()).upstreamHost("localhost")
//                                                        .weight(50).warmup(600000).build())
//                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/ws-annotation/myWs"))
//                                                .build(),
//                                        newRuleBuilder("rule")
//                                                .handle(WebSocketRuleHandle.builder().loadBalance("random").timeout(3000).build())
//                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/ws-annotation/myWs"))
//                                                .build()
//                                )
//                                .checker(notExists("/ws-annotation/myWs"))
//                                .waiting(exists("/ws-annotation/myWs"))
//                                .build()
//                )
//                .caseSpec(
//                        ShenYuCaseSpec.builder()
//                                .addExists("/ws-annotation/myWs")
//                                .addNotExists("/ws-native")
//                                .addNotExists("/ws-reactive")
//                                .addNotExists("/get")
//                                .build())
//                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
//                .build();
//    }
//
//    /**
//     * test with uri method get.
//     *
//     * @return ShenYuScenarioSpec
//     */
//    public ShenYuScenarioSpec testWithMethodGet() {
//        return ShenYuScenarioSpec.builder()
//                .name("single-websocket uri method GET]")
//                .beforeEachSpec(
//                        ShenYuBeforeEachSpec.builder()
//                                .addSelectorAndRule(
//                                        newSelectorBuilder("selector", Plugin.WEBSOCKET)
//                                                .name("/ws-annotation")
//                                                .handle(WebSocketSelectorHandle.builder().protocol("ws://")
//                                                        .upstreamUrl("localhost:8001").status(true).timestamp(System.currentTimeMillis()).upstreamHost("localhost")
//                                                        .weight(50).warmup(600000).build())
//                                                .conditionList(Lists.newArrayList(
//                                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
//                                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/ws-annotation/myWs")
//                                                ))
//                                                .build(),
//                                        newRuleBuilder("rule")
//                                                .handle(WebSocketRuleHandle.builder().loadBalance("random").timeout(3000).build())
//                                                .conditionList(Lists.newArrayList(
//                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
//                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/ws-annotation/myWs")
//                                                ))
//                                                .build()
//                                )
//                                .checker(notExists("/ws-annotation/myWs"))
//                                .waiting(exists("/ws-annotation/myWs"))
//                                .build()
//                )
//                .caseSpec(
//                        ShenYuCaseSpec.builder()
//                                .addExists(Method.GET, "/ws-annotation/myWs")
//                                .addNotExists(Method.GET, "/ws-annotation")
//                                .addNotExists(Method.POST, "/ws-annotation/myWs")
//                                .addNotExists(Method.PUT, "/ws-annotation/myWs")
//                                .addNotExists(Method.DELETE, "/ws-annotation/myWs")
//                                .build())
//                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
//                .build();
//    }
//
//    /**
//     * test with uri method post.
//     *
//     * @return ShenYuScenarioSpec
//     */
//    public ShenYuScenarioSpec testWithMethodPost() {
//        return ShenYuScenarioSpec.builder()
//                .name("single-websocket uri method POST]")
//                .beforeEachSpec(
//                        ShenYuBeforeEachSpec.builder()
//                                .addSelectorAndRule(
//                                        newSelectorBuilder("selector", Plugin.WEBSOCKET)
//                                                .name("/ws-annotation")
//                                                .handle(WebSocketSelectorHandle.builder().protocol("ws://")
//                                                        .upstreamUrl("localhost:8001").status(true).timestamp(System.currentTimeMillis()).upstreamHost("localhost")
//                                                        .weight(50).warmup(600000).build())
//                                                .conditionList(Lists.newArrayList(
//                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
//                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/ws-annotation/myWs")
//                                                ))
//                                                .build(),
//                                        newRuleBuilder("rule")
//                                                .handle(WebSocketRuleHandle.builder().loadBalance("random").timeout(3000).build())
//                                                .conditionList(Lists.newArrayList(
//                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
//                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/ws-annotation/myWs")
//                                                ))
//                                                .build()
//                                )
//                                .checker(notExists("/ws-annotation/myWs"))
//                                .waiting(exists("/ws-annotation/myWs"))
//                                .build()
//                )
//                .caseSpec(
//                        ShenYuCaseSpec.builder()
//                                .addExists(Method.POST, "/ws-annotation/myWs")
//                                .addNotExists(Method.GET, "/ws-annotation/myWs")
//                                .addNotExists(Method.POST, "/ws-annotation")
//                                .addNotExists(Method.PUT, "/ws-annotation/myWs")
//                                .addNotExists(Method.DELETE, "/ws-annotation/myWs")
//                                .build())
//                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
//                .build();
//    }
//
//    /**
//     * test with uri method put.
//     *
//     * @return ShenYuScenarioSpec
//     */
//    public ShenYuScenarioSpec testWithMethodPut() {
//        return ShenYuScenarioSpec.builder()
//                .name("single-websocket uri method PUT]")
//                .beforeEachSpec(
//                        ShenYuBeforeEachSpec.builder()
//                                .addSelectorAndRule(
//                                        newSelectorBuilder("selector", Plugin.WEBSOCKET)
//                                                .name("/ws-annotation")
//                                                .handle(WebSocketSelectorHandle.builder().protocol("ws://")
//                                                        .upstreamUrl("localhost:8001").status(true).timestamp(System.currentTimeMillis()).upstreamHost("localhost")
//                                                        .weight(50).warmup(600000).build())
//                                                .conditionList(Lists.newArrayList(
//                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
//                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/ws-annotation/myWs")
//                                                ))
//                                                .build(),
//                                        newRuleBuilder("rule")
//                                                .handle(WebSocketRuleHandle.builder().loadBalance("random").timeout(3000).build())
//                                                .conditionList(Lists.newArrayList(
//                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
//                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/ws-annotation/myWs")
//                                                ))
//                                                .build()
//                                )
//                                .checker(notExists("/ws-annotation/myWs"))
//                                .waiting(exists("/ws-annotation/myWs"))
//                                .build()
//                )
//                .caseSpec(
//                        ShenYuCaseSpec.builder()
//                                .addExists(Method.PUT, "/ws-annotation/myWs")
//                                .addNotExists(Method.GET, "/ws-annotation/myWs")
//                                .addNotExists(Method.POST, "/ws-annotation/myWs")
//                                .addNotExists(Method.PUT, "/ws-annotation")
//                                .addNotExists(Method.DELETE, "/ws-annotation/myWs")
//                                .build())
//                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
//                .build();
//    }
//
//    /**
//     * test with uri method delete.
//     *
//     * @return ShenYuScenarioSpec
//     */
//    public ShenYuScenarioSpec testWithMethodDelete() {
//        return ShenYuScenarioSpec.builder()
//                .name("single-websocket uri method DELETE]")
//                .beforeEachSpec(
//                        ShenYuBeforeEachSpec.builder()
//                                .addSelectorAndRule(
//                                        newSelectorBuilder("selector", Plugin.WEBSOCKET)
//                                                .name("/ws-annotation")
//                                                .handle(WebSocketSelectorHandle.builder().protocol("ws://")
//                                                        .upstreamUrl("localhost:8001").status(true).timestamp(System.currentTimeMillis()).upstreamHost("localhost")
//                                                        .weight(50).warmup(600000).build())
//                                                .conditionList(Lists.newArrayList(
//                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
//                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/ws-annotation/myWs")
//                                                ))
//                                                .build(),
//                                        newRuleBuilder("rule")
//                                                .handle(WebSocketRuleHandle.builder().loadBalance("random").timeout(3000).build())
//                                                .conditionList(Lists.newArrayList(
//                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
//                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/ws-annotation/myWs")
//                                                ))
//                                                .build()
//                                )
//                                .checker(notExists("/ws-annotation/myWs"))
//                                .waiting(exists("/ws-annotation/myWs"))
//                                .build()
//                )
//                .caseSpec(
//                        ShenYuCaseSpec.builder()
//                                .addExists(Method.DELETE, "/ws-annotation/myWs")
//                                .addNotExists(Method.GET, "/ws-annotation/myWs")
//                                .addNotExists(Method.POST, "/ws-annotation/myWs")
//                                .addNotExists(Method.PUT, "/ws-annotation/myWs")
//                                .addNotExists(Method.DELETE, "/ws-annotation")
//                                .build())
//                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
//                .build();
//    }
}
