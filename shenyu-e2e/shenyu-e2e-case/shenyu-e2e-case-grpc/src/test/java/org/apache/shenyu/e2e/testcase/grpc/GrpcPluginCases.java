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

package org.apache.shenyu.e2e.testcase.grpc;

import com.google.common.collect.Lists;
import io.restassured.http.Method;
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuAfterEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuScenarioSpec;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.data.Condition;
import org.apache.shenyu.e2e.model.handle.GrpcSelectorHandle;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.notExists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newCondition;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;

public class GrpcPluginCases implements ShenYuScenarioProvider {

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testGrpc()
                //testWithUriEquals(),
                //testWithUriPathPattern(),
                //testWithUriStartWith(),
                //testWithEndWith(),
                //testWithMethodGet(),
                //testWithMethodPost(),
                //testWithMethodPut(),
                //testWithMethodDelete()
        );
    }

    private ShenYuScenarioSpec testGrpc() {
        Map<String, List<MessageData>> body = new ConcurrentHashMap<>();
        body.put("data", Lists.newArrayList(new MessageData("hello grpc")));
        return ShenYuScenarioSpec.builder()
                .name("test grpc invoker")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .checker(notExists("/sofa/findAll"))
                                .checker(exists(Method.POST, "/grpc/echo", body))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/echo", body)
                                .addNotExists("/grpc/fin")
                                .addNotExists("/put")
                                .addNotExists("/get")
                                .build())
                .build();
    }
    
    /**
     * test with uri equal.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriEquals() {
        Map<String, List<MessageData>> body = new ConcurrentHashMap<>();
        body.put("data", Lists.newArrayList(new MessageData("hello grpc")));
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri =]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .name("/grpc")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/echo"))
                                                .handle(GrpcSelectorHandle.builder()
                                                        .status(true)
                                                        .upstreamUrl("grpc:38080")
                                                        .weight(50).build())
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/echo"))
                                                .build()
                                )
                                .checker(notExists("/sofa/findAll"))
                                .waiting(exists(Method.POST, "/grpc/echo", body))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/echo", body)
                                .addNotExists("/grpc/fin")
                                .addNotExists("/put")
                                .addNotExists("/get")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/sofa/findAll")).build())
                .build();
    }

    /**
     * test with uri path pattern.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriPathPattern() {
        Map<String, List<MessageData>> body = new ConcurrentHashMap<>();
        body.put("data", Lists.newArrayList(new MessageData("hello grpc")));
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri path_pattern]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .name("/grpc")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/grpc/**"))
                                                .handle(GrpcSelectorHandle.builder()
                                                        .status(true)
                                                        .upstreamUrl("grpc:38080")
                                                        .weight(50).build())
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/grpc/**"))
                                                .build()
                                )
                                .checker(notExists("/grpc/findAll"))
                                .waiting(exists(Method.POST, "/grpc/echo", body))
                                .build()
                ).caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/echo", body)
                                .addExists(Method.POST, "/grpc/unaryFun", body)
                                .addExists(Method.POST, "/grpc/bidiStreamingFun", body)
                                .addExists(Method.POST, "/grpc/serverStreamingFun", body)
                                .addNotExists(Method.GET, "/grp")
                                .addNotExists(Method.POST, "/grpc/findAll")
                                .addNotExists(Method.PUT, "/grpc/findAll")
                                .addNotExists(Method.DELETE, "/grpc/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/grpc/findAll")).build())
                .build();
    }

    /**
     * test with uri start with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriStartWith() {
        Map<String, List<MessageData>> body = new ConcurrentHashMap<>();
        body.put("data", Lists.newArrayList(new MessageData("hello grpc")));
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri path_pattern]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .name("/grpc")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/grpc/"))
                                                .handle(GrpcSelectorHandle.builder()
                                                        .status(true)
                                                        .upstreamUrl("grpc:38080")
                                                        .weight(50).build())
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/grpc/"))
                                                .build()
                                )
                                .checker(notExists("/grpc/findAll"))
                                .waiting(exists(Method.POST, "/grpc/echo", body))
                                .build()
                ).caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/echo", body)
                                .addExists(Method.POST, "/grpc/unaryFun", body)
                                .addExists(Method.POST, "/grpc/bidiStreamingFun", body)
                                .addExists(Method.POST, "/grpc/serverStreamingFun", body)
                                .addNotExists(Method.POST, "/grpc/findAll")
                                .addNotExists(Method.PUT, "/grpc/findAll")
                                .addNotExists(Method.DELETE, "/grpc/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/grpc/findAll")).build())
                .build();
    }

    /**
     * test with uri end with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithEndWith() {
        Map<String, List<MessageData>> body = new ConcurrentHashMap<>();
        body.put("data", Lists.newArrayList(new MessageData("hello grpc")));
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri path_pattern]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .name("/grpc")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/echo"))
                                                .handle(GrpcSelectorHandle.builder()
                                                        .status(true)
                                                        .upstreamUrl("grpc:38080")
                                                        .weight(50).build())
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/echo"))
                                                .build()
                                )
                                .checker(notExists("/grpc/findAll"))
                                .waiting(exists(Method.POST, "/grpc/echo", body))
                                .build()
                ).caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/echo", body)
                                .addNotExists("/grp")
                                .addNotExists(Method.POST, "/grpc/findAll")
                                .addNotExists(Method.PUT, "/grpc/findAll")
                                .addNotExists(Method.DELETE, "/grpc/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/grpc/findAll")).build())
                .build();
    }

    /**
     * test with uri method get.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodGet() {
        Map<String, List<MessageData>> body = new ConcurrentHashMap<>();
        body.put("data", Lists.newArrayList(new MessageData("hello grpc")));
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri method GET]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .name("/grpc")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/grpc/")
                                                ))
                                                .handle(GrpcSelectorHandle.builder()
                                                        .status(true)
                                                        .upstreamUrl("grpc:38080")
                                                        .weight(50).build())
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/echo")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.GET, "/grpc/findAll"))
                                .waiting(notExists(Method.GET, "/grpc/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/echo", body)
                                .addNotExists(Method.GET, "/grpc/find")
                                .addNotExists(Method.POST, "/grpc/findAll")
                                .addNotExists(Method.PUT, "/grpc/findAll")
                                .addNotExists(Method.DELETE, "/grpc/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.GET, "/grpc/findAll")).build())
                .build();
    }

    /**
     * test with uri method post.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodPost() {
        Map<String, List<MessageData>> body = new ConcurrentHashMap<>();
        body.put("data", Lists.newArrayList(new MessageData("hello grpc")));
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri method POST]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .name("/grpc")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/grpc/")
                                                ))
                                                .handle(GrpcSelectorHandle.builder()
                                                        .status(true)
                                                        .upstreamUrl("grpc:38080")
                                                        .weight(50).build())
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/echo")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.POST, "/grpc/findAll"))
                                .waiting(exists(Method.POST, "/grpc/echo", body))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/echo", body)
                                .addNotExists(Method.POST, "/grpc/find")
                                .addNotExists(Method.GET, "/grpc/findAll")
                                .addNotExists(Method.PUT, "/grpc/findAll")
                                .addNotExists(Method.DELETE, "/grpc/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.POST, "/grpc/findAll")).build())
                .build();
    }

    /**
     * test with uri method put.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodPut() {
        Map<String, List<MessageData>> body = new ConcurrentHashMap<>();
        body.put("data", Lists.newArrayList(new MessageData("hello grpc")));
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri method PUT]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .name("/grpc")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/grpc/")
                                                ))
                                                .handle(GrpcSelectorHandle.builder()
                                                        .status(true)
                                                        .upstreamUrl("grpc:38080")
                                                        .weight(50).build())
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/echo")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.PUT, "/grpc/findAll"))
                                .waiting(exists(Method.POST, "/grpc/echo", body))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/echo", body)
                                .addNotExists(Method.PUT, "/grpc/find")
                                .addNotExists(Method.GET, "/grpc/findAll")
                                .addNotExists(Method.POST, "/grpc/findAll")
                                .addNotExists(Method.DELETE, "/grpc/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.PUT, "/grpc/findAll")).build())
                .build();
    }

    /**
     * test with uri method delete.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodDelete() {
        Map<String, List<MessageData>> body = new ConcurrentHashMap<>();
        body.put("data", Lists.newArrayList(new MessageData("hello grpc")));
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri method DELETE]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .name("/grpc")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/grpc/echo")
                                                ))
                                                .handle(GrpcSelectorHandle.builder()
                                                        .status(true)
                                                        .upstreamUrl("grpc:38080")
                                                        .weight(50).build())
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/echo")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.DELETE, "/grpc/findAll"))
                                .waiting(exists(Method.POST, "/grpc/echo", body))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/echo", body)
                                .addNotExists(Method.DELETE, "/grpc/find")
                                .addNotExists(Method.GET, "/grpc/findAll")
                                .addNotExists(Method.POST, "/grpc/findAll")
                                .addNotExists(Method.PUT, "/grpc/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.DELETE, "/grpc/findAll")).build())
                .build();
    }

    public static class MessageData {

        /**
         * message.
         */
        private String message;

        /**
         * default constructor.
         */
        public MessageData() {
        }

        public MessageData(final String message) {
            this.message = message;
        }

        /**
         * get message.
         *
         * @return message
         */
        public String getMessage() {
            return message;
        }

        /**
         * set message.
         *
         * @param message message
         */
        public void setMessage(final String message) {
            this.message = message;
        }

    }

}
