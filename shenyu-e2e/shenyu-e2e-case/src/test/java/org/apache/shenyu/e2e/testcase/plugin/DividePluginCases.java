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

import org.apache.shenyu.e2e.client.admin.model.Plugin;
import org.apache.shenyu.e2e.client.admin.model.data.Condition.Operator;
import org.apache.shenyu.e2e.client.admin.model.data.Condition.ParamType;
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.engine.service.NamingResolver;
import org.apache.shenyu.e2e.testcase.common.specification.ShenYuAfterEachSpec;
import org.apache.shenyu.e2e.testcase.common.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.testcase.common.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.testcase.common.specification.ShenYuScenarioSpec;

import java.util.List;

import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newDivideRuleHandle;
import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newSelectorBuilder;
import static org.apache.shenyu.e2e.testcase.common.ResourceDataTemplate.newUpstreamsBuilder;
import static org.apache.shenyu.e2e.testcase.common.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.testcase.common.function.HttpCheckers.notExists;

public class DividePluginCases implements ShenYuScenarioProvider {
    private static final String ANYTHING = "/anything";
    
    @Override
    public List<ScenarioSpec> get() {
        final String address = NamingResolver.INSTANCE.resolve("httpbin");
        return List.of(
                ShenYuScenarioSpec.builder()
                        .name("divide-single rule[uri=]")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("httpbin", Plugin.DIVIDE)
                                                        .handle(newUpstreamsBuilder(address + ":80"))
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
                                        .add(exists(ANYTHING))
                                        .add(notExists("/anythin"))
                                        .add(notExists(ANYTHING + "/x"))
                                        .add(notExists("/get"))
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build()
        );
    }
}
