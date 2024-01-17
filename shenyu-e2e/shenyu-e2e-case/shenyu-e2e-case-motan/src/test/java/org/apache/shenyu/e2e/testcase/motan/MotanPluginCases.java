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

package org.apache.shenyu.e2e.testcase.motan;

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
import org.apache.shenyu.e2e.model.handle.DivideRuleHandle;

import java.util.List;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.notExists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newCondition;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;

public class MotanPluginCases implements ShenYuScenarioProvider {

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
                .name("single-motan uri =]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.MOTAN)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/motan/demo/hi"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/motan/demo/hi"))
                                                .handle(DivideRuleHandle.builder().timeout(100000).retry(10).build())
                                                .build()
                                )
                                .checker(notExists("/motan/demo/hi"))
                                .waiting(exists("/motan/demo/hi"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/motan/demo/hi")
                                .addNotExists("/motan/demo/h")
                                .addNotExists("/put")
                                .addNotExists("/get")
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/motan/demo/hi")).build())
                .build();
    }

    /**
     * test with uri path pattern.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriPathPattern() {
        return ShenYuScenarioSpec.builder()
                .name("single-motan uri path_pattern]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.MOTAN)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/motan/demo/**"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/motan/demo/**"))
                                                .build()
                                )
                                .checker(notExists("/motan/demo/hi"))
                                .waiting(exists("/motan/demo/hi"))
                                .build()
                ).caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/motan/demo/hi")
                                .addNotExists("/motan/de")
                                .addExists(Method.POST, "/motan/demo/hi")
                                .addExists(Method.PUT, "/motan/demo/hi")
                                .addExists(Method.DELETE, "/motan/demo/hi")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/motan/demo/hi")).build())
                .build();
    }

    /**
     * test with uri start with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriStartWith() {
        return ShenYuScenarioSpec.builder()
                .name("single-motan uri starts_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.MOTAN)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/motan/"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/motan/"))
                                                .build()
                                )
                                .checker(notExists("/motan/demo/hi"))
                                .waiting(exists("/motan/demo/hi"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/motan/demo/hi")
                                .addNotExists("/mota/")
                                .addExists(Method.POST, "/motan/demo/hi")
                                .addExists(Method.PUT, "/motan/demo/hi")
                                .addExists(Method.DELETE, "/motan/demo/hi")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/motan/demo/hi")).build())
                .build();
    }

    /**
     * test with uri end with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithEndWith() {
        return ShenYuScenarioSpec.builder()
                .name("single-motan uri ends_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.MOTAN)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/hi"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/hi"))
                                                .build()
                                )
                                .checker(notExists("/motan/demo/hi"))
                                .waiting(exists("/motan/demo/hi"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/motan/demo/hi")
                                .addNotExists("/motan/demo/h")
                                .addExists(Method.POST, "/motan/demo/hi")
                                .addExists(Method.PUT, "/motan/demo/hi")
                                .addExists(Method.DELETE, "/motan/demo/hi")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/motan/demo/hi")).build())
                .build();
    }

    /**
     * test with uri method get.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodGet() {
        return ShenYuScenarioSpec.builder()
                .name("single-motan uri method GET]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.MOTAN)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/motan/demo/hi")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/motan/demo/hi")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.GET, "/motan/demo/hi"))
                                .waiting(exists(Method.GET, "/motan/demo/hi"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.GET, "/motan/demo/hi")
                                .addNotExists(Method.GET, "/motan/demo/h")
                                .addNotExists(Method.POST, "/motan/demo/hi")
                                .addNotExists(Method.PUT, "/motan/demo/hi")
                                .addNotExists(Method.DELETE, "/motan/demo/hi")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.GET, "/motan/demo/hi")).build())
                .build();
    }

    /**
     * test with uri method post.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodPost() {
        return ShenYuScenarioSpec.builder()
                .name("single-motan uri method POST]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.MOTAN)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/motan/demo/hi")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/motan/demo/hi")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.POST, "/motan/demo/hi"))
                                .waiting(exists(Method.POST, "/motan/demo/hi"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/motan/demo/hi")
                                .addNotExists(Method.POST, "/motan/demo/h")
                                .addNotExists(Method.GET, "/motan/demo/hi")
                                .addNotExists(Method.PUT, "/motan/demo/hi")
                                .addNotExists(Method.DELETE, "/motan/demo/hi")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.POST, "/motan/demo/hi")).build())
                .build();
    }

    /**
     * test with uri method put.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodPut() {
        return ShenYuScenarioSpec.builder()
                .name("single-motan uri method PUT]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.MOTAN)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/motan/demo/hi")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/motan/demo/hi")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.PUT, "/motan/demo/hi"))
                                .waiting(exists(Method.PUT, "/motan/demo/hi"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.PUT, "/motan/demo/hi")
                                .addNotExists(Method.PUT, "/motan/demo/h")
                                .addNotExists(Method.GET, "/motan/demo/hi")
                                .addNotExists(Method.POST, "/motan/demo/hi")
                                .addNotExists(Method.DELETE, "/motan/demo/hi")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.PUT, "/motan/demo/hi")).build())
                .build();
    }


    /**
     * test with uri method delete.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodDelete() {
        return ShenYuScenarioSpec.builder()
                .name("single-motan uri method DELETE]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.MOTAN)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/motan/demo/hi")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/motan/demo/hi")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.DELETE, "/motan/demo/hi"))
                                .waiting(exists(Method.DELETE, "/motan/demo/hi"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.DELETE, "/motan/demo/hi")
                                .addNotExists(Method.DELETE, "/motan/demo/h")
                                .addNotExists(Method.GET, "/motan/demo/hi")
                                .addNotExists(Method.POST, "/motan/demo/hi")
                                .addNotExists(Method.PUT, "/motan/demo/hi")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.DELETE, "/motan/demo/hi")).build())
                .build();
    }
}
