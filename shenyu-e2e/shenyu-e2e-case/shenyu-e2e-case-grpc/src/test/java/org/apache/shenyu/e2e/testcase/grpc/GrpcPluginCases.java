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

import java.util.List;

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
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri =]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/findAll"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/findAll"))
                                                .build()
                                )
                                .checker(notExists("/grpc/findAll"))
                                .waiting(exists("/grpc/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/grpc/findAll")
                                .addNotExists("/grpc/fin")
                                .addNotExists("/put")
                                .addNotExists("/get")
                                .build())
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
                .name("single-grpc uri path_pattern]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/grpc/**"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/grpc/**"))
                                                .build()
                                )
                                .checker(notExists("/grpc/findAll"))
                                .waiting(exists("/grpc/findAll"))
                                .build()
                ).caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/grpc/findAll")
                                .addNotExists("/grp")
                                .addExists(Method.POST, "/grpc/findAll")
                                .addExists(Method.PUT, "/grpc/findAll")
                                .addExists(Method.DELETE, "/grpc/findAll")
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
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri starts_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/grpc/"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/grpc/"))
                                                .build()
                                )
                                .checker(notExists("/grpc/findAll"))
                                .waiting(exists("/grpc/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/grpc/findAll")
                                .addNotExists("/grpc/de")
                                .addExists(Method.POST, "/grpc/findAll")
                                .addExists(Method.PUT, "/grpc/findAll")
                                .addExists(Method.DELETE, "/grpc/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }

    /**
     * test with uri end with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithEndWith() {
        return ShenYuScenarioSpec.builder()
                .name("single-alibaba-grpc uri ends_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/findAll"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/findAll"))
                                                .build()
                                )
                                .checker(notExists("/grpc/findAll"))
                                .waiting(exists("/grpc/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/grpc/findAll")
                                .addNotExists("/grpc/find")
                                .addExists(Method.POST, "/grpc/findAll")
                                .addExists(Method.PUT, "/grpc/findAll")
                                .addExists(Method.DELETE, "/grpc/findAll")
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
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri method GET]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.GET, "/grpc/findAll"))
                                .waiting(exists(Method.GET, "/grpc/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.GET, "/grpc/findAll")
                                .addNotExists(Method.GET, "/grpc/find")
                                .addNotExists(Method.POST, "/grpc/findAll")
                                .addNotExists(Method.PUT, "/grpc/findAll")
                                .addNotExists(Method.DELETE, "/grpc/findAll")
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
        return ShenYuScenarioSpec.builder()
                .name("single-grpc uri method POST]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.POST, "/grpc/findAll"))
                                .waiting(exists(Method.POST, "/grpc/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/findAll")
                                .addNotExists(Method.POST, "/grpc/find")
                                .addNotExists(Method.GET, "/grpc/findAll")
                                .addNotExists(Method.PUT, "/grpc/findAll")
                                .addNotExists(Method.DELETE, "/grpc/findAll")
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
                .name("single-grpc uri method PUT]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.PUT, "/grpc/findAll"))
                                .waiting(exists(Method.PUT, "/grpc/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.PUT, "/grpc/findAll")
                                .addNotExists(Method.PUT, "/grpc/find")
                                .addNotExists(Method.GET, "/grpc/findAll")
                                .addNotExists(Method.POST, "/grpc/findAll")
                                .addNotExists(Method.DELETE, "/grpc/findAll")
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
                .name("single-grpc uri method DELETE]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.GRPC)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/grpc/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.DELETE, "/grpc/findAll"))
                                .waiting(exists(Method.DELETE, "/grpc/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.DELETE, "/grpc/findAll")
                                .addNotExists(Method.DELETE, "/grpc/find")
                                .addNotExists(Method.GET, "/grpc/findAll")
                                .addNotExists(Method.POST, "/grpc/findAll")
                                .addNotExists(Method.PUT, "/grpc/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                .build();
    }
}
