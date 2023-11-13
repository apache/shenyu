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

package org.apache.shenyu.e2e.testcase.sofa;

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

public class SofaPluginCases implements ShenYuScenarioProvider {

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testSofaFindAll()
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
    
    private ShenYuScenarioSpec testSofaFindAll() {
        return ShenYuScenarioSpec.builder()
                .name("sofa test")
                .beforeEachSpec(ShenYuBeforeEachSpec.builder()
                        .checker(exists("/sofa/findAll"))
                        .build())
                .caseSpec(ShenYuCaseSpec.builder()
                        .addExists("/sofa/findAll")
                        .addNotExists("/sofa/findAll/123")
                        .build())
                .build();
    }

    /**
     * test with uri equal.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriEquals() {
        return ShenYuScenarioSpec.builder()
                .name("single-sofa uri =]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SOFA)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/sofa/findAll"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/sofa/findAll"))
                                                .build()
                                )
                                .checker(notExists("/sofa/findAll"))
                                .waiting(exists("/sofa/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/sofa/findAll")
                                .addNotExists("/sofa/fin")
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
        return ShenYuScenarioSpec.builder()
                .name("single-sofa uri path_pattern]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SOFA)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/sofa/**"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/sofa/**"))
                                                .build()
                                )
                                .checker(notExists("/sofa/findAll"))
                                .waiting(exists("/sofa/findAll"))
                                .build()
                ).caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/sofa/findAll")
                                .addNotExists("/sof")
                                .addExists(Method.POST, "/sofa/findAll")
                                .addExists(Method.PUT, "/sofa/findAll")
                                .addExists(Method.DELETE, "/sofa/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/sofa/findAll")).build())
                .build();
    }

    /**
     * test with uri start with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriStartWith() {
        return ShenYuScenarioSpec.builder()
                .name("single-sofa uri starts_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SOFA)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/sofa/"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/sofa/"))
                                                .build()
                                )
                                .checker(notExists("/sofa/findAll"))
                                .waiting(exists("/sofa/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/sofa/findAll")
                                .addNotExists("/sofa/de")
                                .addExists(Method.POST, "/sofa/findAll")
                                .addExists(Method.PUT, "/sofa/findAll")
                                .addExists(Method.DELETE, "/sofa/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/sofa/findAll")).build())
                .build();
    }

    /**
     * test with uri end with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithEndWith() {
        return ShenYuScenarioSpec.builder()
                .name("single-sofa uri ends_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SOFA)
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/findAll"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/findAll"))
                                                .build()
                                )
                                .checker(notExists("/sofa/findAll"))
                                .waiting(exists("/sofa/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/sofa/findAll")
                                .addNotExists("/sofa/find")
                                .addExists(Method.POST, "/sofa/findAll")
                                .addExists(Method.PUT, "/sofa/findAll")
                                .addExists(Method.DELETE, "/sofa/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/sofa/findAll")).build())
                .build();
    }

    /**
     * test with uri method get.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodGet() {
        return ShenYuScenarioSpec.builder()
                .name("single-sofa uri method GET]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SOFA)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/sofa/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/sofa/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.GET, "/sofa/findAll"))
                                .waiting(exists(Method.GET, "/sofa/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.GET, "/sofa/findAll")
                                .addNotExists(Method.GET, "/sofa/find")
                                .addNotExists(Method.POST, "/sofa/findAll")
                                .addNotExists(Method.PUT, "/sofa/findAll")
                                .addNotExists(Method.DELETE, "/sofa/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.GET, "/sofa/findAll")).build())
                .build();
    }

    /**
     * test with uri method post.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodPost() {
        return ShenYuScenarioSpec.builder()
                .name("single-sofa uri method POST]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SOFA)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/sofa/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/sofa/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.POST, "/sofa/findAll"))
                                .waiting(exists(Method.POST, "/sofa/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/sofa/findAll")
                                .addNotExists(Method.POST, "/sofa/find")
                                .addNotExists(Method.GET, "/sofa/findAll")
                                .addNotExists(Method.PUT, "/sofa/findAll")
                                .addNotExists(Method.DELETE, "/sofa/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.POST, "/sofa/findAll")).build())
                .build();
    }

    /**
     * test with uri method put.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodPut() {
        return ShenYuScenarioSpec.builder()
                .name("single-sofa uri method PUT]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SOFA)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/sofa/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/sofa/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.PUT, "/sofa/findAll"))
                                .waiting(exists(Method.PUT, "/sofa/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.PUT, "/sofa/findAll")
                                .addNotExists(Method.PUT, "/sofa/find")
                                .addNotExists(Method.GET, "/sofa/findAll")
                                .addNotExists(Method.POST, "/sofa/findAll")
                                .addNotExists(Method.DELETE, "/sofa/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.PUT, "/sofa/findAll")).build())
                .build();
    }

    /**
     * test with uri method delete.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodDelete() {
        return ShenYuScenarioSpec.builder()
                .name("single-sofa uri method DELETE]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SOFA)
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/sofa/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/sofa/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.DELETE, "/sofa/findAll"))
                                .waiting(exists(Method.DELETE, "/sofa/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.DELETE, "/sofa/findAll")
                                .addNotExists(Method.DELETE, "/sofa/find")
                                .addNotExists(Method.GET, "/sofa/findAll")
                                .addNotExists(Method.POST, "/sofa/findAll")
                                .addNotExists(Method.PUT, "/sofa/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.DELETE, "/sofa/findAll")).build())
                .build();
    }
}
