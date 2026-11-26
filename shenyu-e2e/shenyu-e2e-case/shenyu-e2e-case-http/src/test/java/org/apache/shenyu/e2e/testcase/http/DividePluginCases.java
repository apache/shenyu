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

import io.restassured.builder.ResponseSpecBuilder;
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
import java.util.Map;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.notExists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newBindingData;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newDivideRuleHandle;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newUpstreamsBuilder;
import static org.hamcrest.Matchers.equalTo;

public class DividePluginCases implements ShenYuScenarioProvider {

    private static final String ROUTE = "/http/order/findById";

    private static final String REQUEST = ROUTE + "?id=123";

    private static final String ORDER_PATH = "/http/order/**";

    private static final String SAVE_PATH = "/http/order/save";

    private static final int SAVED_ORDER_ID = 456;

    private static final String UPSTREAM = System.getProperty("shenyu.test.http.upstream", "httpbin.org");

    @Override
    public List<ScenarioSpec> get() {
        return List.of(testDivideHello());
    }

    private ShenYuScenarioSpec testDivideHello() {
        return ShenYuScenarioSpec.builder()
                .name("proxy an HTTP order request to the fixture")
                .beforeEachSpec(ShenYuBeforeEachSpec.builder()
                        .checker(notExists(REQUEST))
                        .addSelectorAndRule(
                                newSelectorBuilder("http-fixture", Plugin.DIVIDE)
                                        .handle(newUpstreamsBuilder(UPSTREAM))
                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, ORDER_PATH))
                                        .build(),
                                newBindingData("http-fixture", Plugin.DIVIDE.getAlias(), UPSTREAM),
                                newRuleBuilder("http-fixture")
                                        .handle(newDivideRuleHandle())
                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, ORDER_PATH))
                                        .build())
                        .waiting(Method.GET, REQUEST, new ResponseSpecBuilder()
                                .expectStatusCode(200).expectBody("id", equalTo("123")).build())
                        .build())
                .caseSpec(ShenYuCaseSpec.builder()
                        .add(request -> request.request(Method.GET, REQUEST).then().statusCode(200).body("id", equalTo("123")))
                        .add(request -> request.contentType("application/json").body(Map.of("id", SAVED_ORDER_ID, "name", "test"))
                                .post(SAVE_PATH).then().statusCode(200)
                                .body("id", equalTo(SAVED_ORDER_ID), "name", equalTo("hello world save order")))
                        .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(REQUEST)).build())
                .build();
    }
}
