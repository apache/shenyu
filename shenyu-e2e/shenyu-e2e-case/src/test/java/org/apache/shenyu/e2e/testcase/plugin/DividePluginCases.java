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

package org.apache.shenyu.e2e.testcase.plugin;

import com.google.common.collect.Lists;
import io.restassured.http.Method;
import org.apache.shenyu.e2e.client.admin.model.Plugin;
import org.apache.shenyu.e2e.client.admin.model.data.Condition.Operator;
import org.apache.shenyu.e2e.client.admin.model.data.Condition.ParamType;
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.testcase.common.specification.ShenYuAfterEachSpec;
import org.apache.shenyu.e2e.testcase.common.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.testcase.common.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.testcase.common.specification.ShenYuScenarioSpec;

import java.util.List;

import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newCondition;
import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newDivideRuleHandle;
import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newSelectorBuilder;
import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newUpstreamsBuilder;
import static org.apache.shenyu.e2e.testcase.common.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.testcase.common.function.HttpCheckers.notExists;
import static org.hamcrest.text.IsEmptyString.isEmptyOrNullString;

public class DividePluginCases implements ShenYuScenarioProvider {
    private static final String ANYTHING = "/anything";
    
    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                ShenYuScenarioSpec.builder()
                        .name("single-divide uri =]")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("httpbin", Plugin.DIVIDE)
                                                        .handle(newUpstreamsBuilder("httpbin:80"))
                                                        .conditionList(newConditions(ParamType.URI, Operator.EQUAL, ANYTHING))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(newDivideRuleHandle())
                                                        .conditionList(newConditions(ParamType.URI, Operator.EQUAL, ANYTHING))
                                                        .build()
                                        )
                                        .checker(notExists(ANYTHING))
                                        .waiting(exists(ANYTHING))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(ANYTHING)
                                        .addNotExists("/anythin")
                                        .addNotExists(ANYTHING + "/x")
                                        .addNotExists("/get")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-divide uri path_pattern]")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("httpbin", Plugin.DIVIDE)
                                                        .handle(newUpstreamsBuilder("httpbin:80"))
                                                        .conditionList(newConditions(ParamType.URI, Operator.PATH_PATTERN, "/anything/xx/**"))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(newDivideRuleHandle())
                                                        .conditionList(newConditions(ParamType.URI, Operator.PATH_PATTERN, "/anything/xx/**"))
                                                        .build()
                                        )
                                        .checker(notExists(ANYTHING + "/xx/yyy"))
                                        .waiting(exists(ANYTHING + "/xx/yyy"))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(ANYTHING + "/xx")
                                        .addExists(ANYTHING + "/xx/yy")
                                        .addNotExists(ANYTHING + "/x")
                                        .addNotExists(ANYTHING + "/x")
                                        .addExists(Method.POST, ANYTHING + "/xx/yy")
                                        .addExists(Method.PUT, ANYTHING + "/xx/yy")
                                        .addExists(Method.DELETE, ANYTHING + "/xx/yy")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-divide uri starts_with]")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("httpbin", Plugin.DIVIDE)
                                                        .handle(newUpstreamsBuilder("httpbin:80"))
                                                        .conditionList(newConditions(ParamType.URI, Operator.STARTS_WITH, ANYTHING + "/xx"))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(newDivideRuleHandle())
                                                        .conditionList(newConditions(ParamType.URI, Operator.STARTS_WITH, ANYTHING + "/xx"))
                                                        .build()
                                        )
                                        .checker(notExists(ANYTHING + "/xx"))
                                        .waiting(exists(ANYTHING + "/xx"))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(ANYTHING + "/xx/yy")
                                        .addExists(ANYTHING + "/xxx")
                                        .addNotExists(ANYTHING + "/x")
                                        .addExists(Method.POST, ANYTHING + "/xx/yy")
                                        .addExists(Method.PUT, ANYTHING + "/xx/yy")
                                        .addExists(Method.DELETE, ANYTHING + "/xx/yy")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-divide uri ends_with]")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("httpbin", Plugin.DIVIDE)
                                                        .handle(newUpstreamsBuilder("httpbin:80"))
                                                        .conditionList(newConditions(ParamType.URI, Operator.ENDS_WITH, "/200"))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(newDivideRuleHandle())
                                                        .conditionList(newConditions(ParamType.URI, Operator.ENDS_WITH, "/200"))
                                                        .build()
                                        )
                                        .checker(notExists(ANYTHING + "/200"))
                                        .waiting(exists(ANYTHING + "/200"))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addVerifier("/status/200", isEmptyOrNullString())
                                        .addExists("/anything/200")
                                        .addNotExists(ANYTHING)
                                        .addNotExists("/status/300")
                                        .addNotExists("/anything/300")
                                        .addVerifier(Method.PUT, "/status/200", isEmptyOrNullString())
                                        .addVerifier(Method.POST, "/status/200", isEmptyOrNullString())
                                        .addVerifier(Method.DELETE, "/status/200", isEmptyOrNullString())
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-divide method GET")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("httpbin", Plugin.DIVIDE)
                                                        .handle(newUpstreamsBuilder("httpbin:80"))
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(ParamType.METHOD, Operator.EQUAL, "GET"),
                                                                newCondition(ParamType.URI, Operator.EQUAL, ANYTHING)
                                                        ))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(newDivideRuleHandle())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(ParamType.METHOD, Operator.EQUAL, "GET"),
                                                                newCondition(ParamType.URI, Operator.EQUAL, ANYTHING)
                                                        ))
                                                        .build()
                                        )
                                        .checker(notExists(Method.GET, ANYTHING))
                                        .waiting(exists(Method.GET, ANYTHING))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(Method.GET, ANYTHING)
                                        .addNotExists(Method.POST, ANYTHING)
                                        .addNotExists(Method.PUT, ANYTHING)
                                        .addNotExists(Method.DELETE, ANYTHING)
                                        .addNotExists(Method.GET, "/anythin")
                                        .addNotExists(Method.GET, ANYTHING + "/x")
                                        .addNotExists(Method.GET, "/get")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-divide method POST")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("httpbin", Plugin.DIVIDE)
                                                        .handle(newUpstreamsBuilder("httpbin:80"))
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(ParamType.METHOD, Operator.EQUAL, "POST"),
                                                                newCondition(ParamType.URI, Operator.EQUAL, ANYTHING)
                                                        ))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(newDivideRuleHandle())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(ParamType.METHOD, Operator.EQUAL, "POST"),
                                                                newCondition(ParamType.URI, Operator.EQUAL, ANYTHING)
                                                        ))
                                                        .build()
                                        )
                                        .checker(notExists(Method.POST, ANYTHING))
                                        .waiting(exists(Method.POST, ANYTHING))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(Method.POST, ANYTHING)
                                        .addNotExists(Method.GET, ANYTHING)
                                        .addNotExists(Method.PUT, ANYTHING)
                                        .addNotExists(Method.DELETE, ANYTHING)
                                        .addNotExists(Method.POST, "/anythin")
                                        .addNotExists(Method.POST, ANYTHING + "/x")
                                        .addNotExists(Method.POST, "/get")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-divide method PUT")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("httpbin", Plugin.DIVIDE)
                                                        .handle(newUpstreamsBuilder("httpbin:80"))
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(ParamType.METHOD, Operator.EQUAL, "PUT"),
                                                                newCondition(ParamType.URI, Operator.EQUAL, ANYTHING)
                                                        ))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(newDivideRuleHandle())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(ParamType.METHOD, Operator.EQUAL, "PUT"),
                                                                newCondition(ParamType.URI, Operator.EQUAL, ANYTHING)
                                                        ))
                                                        .build()
                                        )
                                        .checker(notExists(Method.PUT, ANYTHING))
                                        .waiting(exists(Method.PUT, ANYTHING))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(Method.PUT, ANYTHING)
                                        .addNotExists(Method.GET, ANYTHING)
                                        .addNotExists(Method.POST, ANYTHING)
                                        .addNotExists(Method.DELETE, ANYTHING)
                                        .addNotExists(Method.PUT, "/anythin")
                                        .addNotExists(Method.PUT, ANYTHING + "/x")
                                        .addNotExists(Method.PUT, "/get")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-divide method DELETE")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("httpbin", Plugin.DIVIDE)
                                                        .handle(newUpstreamsBuilder("httpbin:80"))
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(ParamType.METHOD, Operator.EQUAL, "DELETE"),
                                                                newCondition(ParamType.URI, Operator.EQUAL, ANYTHING)
                                                        ))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(newDivideRuleHandle())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(ParamType.METHOD, Operator.EQUAL, "DELETE"),
                                                                newCondition(ParamType.URI, Operator.EQUAL, ANYTHING)
                                                        ))
                                                        .build()
                                        )
                                        .checker(notExists(Method.DELETE, ANYTHING))
                                        .waiting(exists(Method.DELETE, ANYTHING))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(Method.DELETE, ANYTHING)
                                        .addNotExists(Method.GET, ANYTHING)
                                        .addNotExists(Method.PUT, ANYTHING)
                                        .addNotExists(Method.POST, ANYTHING)
                                        .addNotExists(Method.DELETE, "/anythin")
                                        .addNotExists(Method.DELETE, ANYTHING + "/x")
                                        .addNotExists(Method.DELETE, "/get")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build()
        );
    }
}
