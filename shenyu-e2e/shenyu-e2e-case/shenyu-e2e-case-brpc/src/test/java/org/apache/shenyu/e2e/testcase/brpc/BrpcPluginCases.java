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

package org.apache.shenyu.e2e.testcase.brpc;

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

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.notExists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newCondition;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;

public class BrpcPluginCases implements ShenYuScenarioProvider {

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testWithUriEquals(),
                testWithUriPathPattern(),
                testWithUriStartWith(),
                testWithEndWith(),
                testWithMethodGet(),
                testWithMethodPost(),
                testWithMethodPut(),
                testWithMethodDelete()
        );
    }

    /**
     * test with uri equal.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriEquals() {
        ConcurrentHashMap<String, Integer> body = new ConcurrentHashMap<>();
        body.put("userId", 127);
        return ShenYuScenarioSpec.builder()
                .name("single-brpc uri =]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.BRPC)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/brpc/getUser"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/brpc/getUser"))
                                                .build()
                                )
                                .checker(notExists("/brpc/testchecker"))
                                .waiting(exists(Method.POST, "/brpc/getUser", body))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/brpc/getUser", body)
                                .addNotExists("/brpc/fin")
                                .addNotExists("/put")
                                .addNotExists("/get")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }

    /**
     * test case with uri path pattern.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriPathPattern() {
        ConcurrentHashMap<String, Integer> body = new ConcurrentHashMap<>();
        body.put("userId", 127);
        return ShenYuScenarioSpec.builder()
                .name("single-brpc uri path_pattern]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.BRPC)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/brpc/**"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/brpc/**"))
                                                .build()
                                )
                                .checker(notExists("/brpc/testchecker"))
                                .waiting(exists(Method.POST, "/brpc/getUser", body))
                                .build()
                ).caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/brpc/getUser", body)
                                .addNotExists("/brp")
                                .addNotExists(Method.PUT, "/brpc/testP")
                                .addNotExists(Method.DELETE, "/brpc/testD")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }

    /**
     * test with uri start with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriStartWith() {
        ConcurrentHashMap<String, Integer> body = new ConcurrentHashMap<>();
        body.put("userId", 127);
        return ShenYuScenarioSpec.builder()
                .name("single-brpc uri starts_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.BRPC)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/brpc/"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/brpc/"))
                                                .build()
                                )
                                .checker(notExists("/brpc/testcheck"))
                                .waiting(exists(Method.POST, "/brpc/getUser", body))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/brpc/getUser", body)
                                .addNotExists("/brpc/de")
                                .addNotExists(Method.POST, "/brpc/testpost")
                                .addNotExists(Method.PUT, "/brpc/testput")
                                .addNotExists(Method.DELETE, "/brpc/tesdelete")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }

    /**
     * test case with uri end with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithEndWith() {
        ConcurrentHashMap<String, Integer> body = new ConcurrentHashMap<>();
        body.put("userId", 127);
        return ShenYuScenarioSpec.builder()
                .name("single-brpc uri ends_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.BRPC)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/getUser"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/getUser"))
                                                .build()
                                )
                                .waiting(exists(Method.POST, "/brpc/getUser", body))
                                .checker(notExists("/brpc/testcheck"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/brpc/getUser", body)
                                .addNotExists("/brpc/find")
                                .addNotExists(Method.POST, "/brpc/testPost")
                                .addNotExists(Method.PUT, "/brpc/testPut")
                                .addNotExists(Method.DELETE, "/brpc/testDelete")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }

    /**
     * test with uri method get.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodGet() {
        ConcurrentHashMap<String, Integer> body = new ConcurrentHashMap<>();
        body.put("userId", 127);
        return ShenYuScenarioSpec.builder()
                .name("single-brpc uri method GET]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.BRPC)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/brpc/bigObject0")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/brpc/bigObject0")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.GET, "/brpc/tesget"))
                                .waiting(exists(Method.GET, "/brpc/bigObject0"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.GET, "/brpc/bigObject0")
                                .addNotExists(Method.GET, "/brpc/testget")
                                .addNotExists(Method.POST, "/brpc/testpod")
                                .addNotExists(Method.PUT, "/brpc/testput")
                                .addNotExists(Method.DELETE, "/brpc/testdelete")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }

    /**
     * test with uri method post.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodPost() {
        ConcurrentHashMap<String, Integer> body = new ConcurrentHashMap<>();
        body.put("userId", 127);
        return ShenYuScenarioSpec.builder()
                .name("single-brpc uri method POST]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.BRPC)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/brpc/userMap")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/brpc/userMap")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.POST, "/brpc/testpost"))
                                .waiting(exists(Method.POST, "/brpc/userMap", body))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/brpc/userMap", body)
                                .addNotExists(Method.POST, "/brpc/testpost")
                                .addNotExists(Method.PUT, "/brpc/testput")
                                .addNotExists(Method.DELETE, "/brpc/testdelete")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }

    /**
     * test with uri method put.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodPut() {
        return ShenYuScenarioSpec.builder()
                .name("single-brpc uri method PUT]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.BRPC)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/brpc/allName")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/brpc/allName")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.PUT, "/brpc/testput"))
                                .waiting(notExists(Method.GET, "/brpc/allName"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addNotExists(Method.GET, "/brpc/allName")
                                .addNotExists(Method.PUT, "/brpc/testput")
                                .addNotExists(Method.POST, "/brpc/testpost")
                                .addNotExists(Method.DELETE, "/brpc/testdelete")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }

    /**
     * test with uri method delete.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodDelete() {
        return ShenYuScenarioSpec.builder()
                .name("single-brpc uri method DELETE]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.BRPC)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/brpc/allName")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/brpc/allName")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.DELETE, "/brpc/testdelete"))
                                .waiting(notExists(Method.GET, "/brpc/allName"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addNotExists(Method.GET, "/brpc/allName")
                                .addNotExists(Method.DELETE, "/brpc/find")
                                .addNotExists(Method.POST, "/brpc/testpost")
                                .addNotExists(Method.PUT, "/brpc/testput")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }
}
