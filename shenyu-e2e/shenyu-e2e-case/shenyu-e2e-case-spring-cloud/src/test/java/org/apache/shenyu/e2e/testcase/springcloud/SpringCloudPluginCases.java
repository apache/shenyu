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

package org.apache.shenyu.e2e.testcase.springcloud;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import io.restassured.http.Method;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.data.Condition;
import org.apache.shenyu.e2e.model.handle.DivideUpstream;
import org.apache.shenyu.e2e.model.handle.SpringCloudRuleHandle;
import org.apache.shenyu.e2e.model.handle.SpringCloudSelectorHandle;
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuAfterEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuScenarioSpec;
import org.junit.jupiter.api.Assertions;

import java.util.List;

import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newCondition;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;
import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.notExists;

public class SpringCloudPluginCases implements ShenYuScenarioProvider {

    private static final String TEST = "/springboot/test";

    private static final ObjectMapper mapper = new ObjectMapper();

    @Override
    public List<ScenarioSpec> get() {
        List<DivideUpstream> divideUpstreams = Lists.newArrayList();
        divideUpstreams.add(DivideUpstream.builder().protocol("http://")
                .upstreamUrl("springcloud:8884").status(true).timestamp(System.currentTimeMillis())
                .weight(50).warmup(10).build());
        return Lists.newArrayList(
                ShenYuScenarioSpec.builder()
                        .name("single-spring-cloud uri =]")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                        .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                                .gray(true)
                                                                .divideUpstreams(divideUpstreams)
                                                                .build())
                                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/springcloud/test"))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/springcloud/test"))
                                                        .build()
                                        )
                                        .checker(notExists("/springcloud/test"))
                                        .waiting(exists("/springcloud/test"))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists("/springcloud/test")
                                        .addNotExists("/springcloud/te")
                                        .addNotExists( "/put")
                                        .addNotExists("/get")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-spring-cloud uri path_pattern]")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                        .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                                .gray(true)
                                                                .divideUpstreams(divideUpstreams)
                                                                .build())
                                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/springcloud/test/**"))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/springcloud/test/**"))
                                                        .build()
                                        )
                                        .checker(notExists("/springcloud/test/xx"))
                                        .waiting(exists("/springcloud/test/xx"))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists("/springcloud/test/xx")
                                        .addExists("/springcloud/test/yy")
                                        .addNotExists("/springcloud/te")
                                        .addNotExists("/springcloud/td")
                                        .addExists(Method.POST, "/springcloud/test/xx")
                                        .addExists(Method.PUT, "/springcloud/test/xx")
                                        .addExists(Method.DELETE, "/springcloud/test/xx")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-spring-cloud uri starts_with]")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                        .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                                .gray(true)
                                                                .divideUpstreams(divideUpstreams)
                                                                .build())
                                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/springcloud/test"))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/springcloud/test"))
                                                        .build()
                                        )
                                        .checker(notExists("/springcloud/test"))
                                        .waiting(exists("/springcloud/test"))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists("/springcloud/test/xx")
                                        .addNotExists("/springcloud/testt")
                                        .addNotExists("/springcloud/tes")
                                        .addExists(Method.POST, "/springcloud/test/xx")
                                        .addExists(Method.PUT, "/springcloud/test/xx")
                                        .addExists(Method.DELETE, "/springcloud/test/xx")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-spring-cloud uri ends_with]")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                        .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                                .gray(true)
                                                                .divideUpstreams(divideUpstreams)
                                                                .build())
                                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/test"))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                        .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/test"))
                                                        .build()
                                        )
                                        .checker(notExists("/springcloud/test"))
                                        .waiting(exists("/spring/test"))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists("/springcloud/test")
                                        .addNotExists("/springcloud")
                                        .addNotExists("/springcloud/tested")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-spring-cloud method GET")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                        .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                                .gray(true)
                                                                .divideUpstreams(divideUpstreams)
                                                                .build())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                                newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/springcloud/test")
                                                        ))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                                newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/springcloud/test")
                                                        ))
                                                        .build()
                                        )
                                        .checker(notExists(Method.GET, "/springcloud/test"))
                                        .waiting(exists(Method.GET, "/springcloud/test"))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(Method.GET, "/springcloud/test")
                                        .addNotExists(Method.POST, "/springcloud/test")
                                        .addNotExists(Method.PUT, "/springcloud/test")
                                        .addNotExists(Method.DELETE, "/springcloud/test")
                                        .addNotExists(Method.GET, "/springcloud/tes")
                                        .addNotExists(Method.GET, "/springcloud/tests")
                                        .addNotExists(Method.GET, "/get")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-spring-cloud method POST")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                        .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                                .gray(true)
                                                                .divideUpstreams(divideUpstreams)
                                                                .build())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                                newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/springcloud/test")
                                                        ))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                                newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/springcloud/test")
                                                        ))
                                                        .build()
                                        )
                                        .checker(notExists(Method.POST, TEST))
                                        .waiting(exists(Method.POST, TEST))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(Method.POST, "/springcloud/test")
                                        .addNotExists(Method.GET, "/springcloud/test")
                                        .addNotExists(Method.PUT, "/springcloud/test")
                                        .addNotExists(Method.DELETE, "/springcloud/test")
                                        .addNotExists(Method.POST, "/springcloud/tes")
                                        .addNotExists(Method.POST, "/springcloud/testx")
                                        .addNotExists(Method.POST, "/post")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-spring-cloud method PUT")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                        .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                                .gray(true)
                                                                .divideUpstreams(divideUpstreams)
                                                                .build())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                                newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/springcloud/test")
                                                        ))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                                newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/springcloud/test")
                                                        ))
                                                        .build()
                                        )
                                        .checker(notExists(Method.PUT, "/springcloud/test"))
                                        .waiting(exists(Method.PUT, "/springcloud/test"))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(Method.PUT, "/springcloud/test")
                                        .addNotExists(Method.GET, "/springcloud/test")
                                        .addNotExists(Method.POST, "/springcloud/test")
                                        .addNotExists(Method.DELETE, "/springcloud/test")
                                        .addNotExists(Method.PUT, "/springcloud/tes")
                                        .addNotExists(Method.PUT, "/springcloud/testt")
                                        .addNotExists(Method.PUT, "/put")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build(),
                ShenYuScenarioSpec.builder()
                        .name("single-divide method DELETE")
                        .beforeEachSpec(
                                ShenYuBeforeEachSpec.builder()
                                        .addSelectorAndRule(
                                                newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                        .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                                .gray(true)
                                                                .divideUpstreams(divideUpstreams)
                                                                .build())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                                newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/springcloud/test")
                                                        ))
                                                        .build(),
                                                newRuleBuilder("rule")
                                                        .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                        .conditionList(Lists.newArrayList(
                                                                newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                                newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/springcloud/test")
                                                        ))
                                                        .build()
                                        )
                                        .checker(notExists(Method.DELETE, "/springcloud/test"))
                                        .waiting(exists(Method.DELETE, "/springcloud/test"))
                                        .build()
                        )
                        .caseSpec(
                                ShenYuCaseSpec.builder()
                                        .addExists(Method.DELETE, "/springcloud/test")
                                        .addNotExists(Method.GET, "/springcloud/test")
                                        .addNotExists(Method.PUT, "/springcloud/test")
                                        .addNotExists(Method.POST, "/springcloud/test")
                                        .addNotExists(Method.DELETE, "/springcloud/tes")
                                        .addNotExists(Method.DELETE, "/springcloud/testt")
                                        .addNotExists(Method.DELETE, "/delete")
                                        .build()
                        )
                        .afterEachSpec(ShenYuAfterEachSpec.DEFAULT)
                        .build()
        );
    }

    public static void verifierUri(String handle) throws JsonProcessingException {
        SpringCloudSelectorHandle springCloudSelectorHandle = mapper.readValue(handle, SpringCloudSelectorHandle.class);
        Assertions.assertEquals("springCloud-test", springCloudSelectorHandle.getServiceId());
        Assertions.assertEquals(false, springCloudSelectorHandle.getGray());
        DivideUpstream divideUpstream = springCloudSelectorHandle.getDivideUpstreams().get(0);
        Assertions.assertEquals(50, divideUpstream.getWeight());
        Assertions.assertEquals(600000, divideUpstream.getWarmup());
        Assertions.assertEquals("http://", divideUpstream.getProtocol());
        Assertions.assertEquals(true, divideUpstream.isStatus());
    }
}
