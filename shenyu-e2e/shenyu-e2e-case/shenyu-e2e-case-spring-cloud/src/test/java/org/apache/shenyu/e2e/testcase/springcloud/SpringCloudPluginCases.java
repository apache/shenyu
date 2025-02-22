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
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuAfterEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuScenarioSpec;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.data.Condition;
import org.apache.shenyu.e2e.model.handle.DivideUpstream;
import org.apache.shenyu.e2e.model.handle.SpringCloudRuleHandle;
import org.apache.shenyu.e2e.model.handle.SpringCloudSelectorHandle;
import org.junit.jupiter.api.Assertions;
import org.springframework.util.ObjectUtils;

import java.util.List;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.notExists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newCondition;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;

public class SpringCloudPluginCases implements ShenYuScenarioProvider {

    private static final String TEST = "/springcloud/test";

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final List<DivideUpstream> DIVIDE_UPSTREAMS = Lists.newArrayList();

    static {
        DIVIDE_UPSTREAMS.add(DivideUpstream.builder().protocol("http://")
                .upstreamUrl("springcloud:8884").status(true).timestamp(System.currentTimeMillis()).weight(50).warmup(10).build());

    }

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testSpringCloud()
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
    
    private ShenYuScenarioSpec testSpringCloud() {
        return ShenYuScenarioSpec.builder()
                .name("sping-cloud order test")
                .beforeEachSpec(ShenYuBeforeEachSpec.builder()
                        .checker(exists("/springcloud/order/path/123/hahah"))
                        .build())
                .caseSpec(ShenYuCaseSpec.builder()
                        .addExists("/springcloud/order/path/123/hahah")
                        .build())
                .build();
    }

    private ShenYuScenarioSpec testWithUriEquals() {
        return ShenYuScenarioSpec.builder()
                .name("single-spring-cloud uri =]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                        .gray(true)
                                                        .divideUpstreams(DIVIDE_UPSTREAMS)
                                                        .build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, TEST))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, TEST))
                                                .build()
                                )
                                .checker(notExists(TEST))
                                .waiting(exists(TEST))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(TEST)
                                .addNotExists("/springcloud/te")
                                .addNotExists("/put")
                                .addNotExists("/get")
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(TEST)).build())
                .build();
    }

    private ShenYuScenarioSpec testWithUriPathPattern() {
        return ShenYuScenarioSpec.builder()
                .name("single-spring-cloud uri path_pattern]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                        .gray(true)
                                                        .divideUpstreams(DIVIDE_UPSTREAMS)
                                                        .build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, TEST + "/**"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, TEST + "/**"))
                                                .build()
                                )
                                .checker(notExists(TEST + "/xx"))
                                .waiting(exists(TEST + "/xx"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(TEST + "/xx")
                                .addExists(TEST + "/yy")
                                .addNotExists("/springcloud/te")
                                .addNotExists("/springcloud/td")
                                .addExists(Method.POST, TEST + "/xx")
                                .addExists(Method.PUT, TEST + "/xx")
                                .addExists(Method.DELETE, TEST + "/xx")
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(TEST + "/xx")).build())
                .build();
    }

    private ShenYuScenarioSpec testWithUriStartWith() {
        return ShenYuScenarioSpec.builder()
                .name("single-spring-cloud uri starts_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                        .gray(true)
                                                        .divideUpstreams(DIVIDE_UPSTREAMS)
                                                        .build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, TEST))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, TEST))
                                                .build()
                                )
                                .checker(notExists(TEST))
                                .waiting(exists(TEST))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(TEST + "/xx")
                                .addNotExists("/springcloud/testt")
                                .addNotExists("/springcloud/tes")
                                .addExists(Method.POST, TEST + "/xx")
                                .addExists(Method.PUT, TEST + "/xx")
                                .addExists(Method.DELETE, TEST + "/xx")
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(TEST)).build())
                .build();
    }

    private ShenYuScenarioSpec testWithEndWith() {
        return ShenYuScenarioSpec.builder()
                .name("single-spring-cloud uri ends_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                        .gray(true)
                                                        .divideUpstreams(DIVIDE_UPSTREAMS)
                                                        .build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/test"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/test"))
                                                .build()
                                )
                                .checker(notExists(TEST))
                                .waiting(exists(TEST))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(TEST)
                                .addNotExists("/springcloud")
                                .addNotExists("/springcloud/tested")
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(TEST)).build())
                .build();
    }

    private ShenYuScenarioSpec testWithMethodGet() {
        return ShenYuScenarioSpec.builder()
                .name("single-spring-cloud method GET")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                        .gray(true)
                                                        .divideUpstreams(DIVIDE_UPSTREAMS)
                                                        .build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, TEST)
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, TEST)
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.GET, TEST))
                                .waiting(exists(Method.GET, TEST))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.GET, TEST)
                                .addNotExists(Method.POST, TEST)
                                .addNotExists(Method.PUT, TEST)
                                .addNotExists(Method.DELETE, TEST)
                                .addNotExists(Method.GET, "/springcloud/tes")
                                .addNotExists(Method.GET, "/springcloud/tests")
                                .addNotExists(Method.GET, "/get")
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.GET, TEST)).build())
                .build();
    }

    private ShenYuScenarioSpec testWithMethodPost() {
        return ShenYuScenarioSpec.builder()
                .name("single-spring-cloud method POST")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                        .gray(true)
                                                        .divideUpstreams(DIVIDE_UPSTREAMS)
                                                        .build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, TEST)
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, TEST)
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.POST, TEST))
                                .waiting(exists(Method.POST, TEST))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, TEST)
                                .addNotExists(Method.GET, TEST)
                                .addNotExists(Method.PUT, TEST)
                                .addNotExists(Method.DELETE, TEST)
                                .addNotExists(Method.POST, "/springcloud/tes")
                                .addNotExists(Method.POST, "/springcloud/testx")
                                .addNotExists(Method.POST, "/post")
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.POST, TEST)).build())
                .build();
    }

    private ShenYuScenarioSpec testWithMethodPut() {
        return ShenYuScenarioSpec.builder()
                .name("single-spring-cloud method PUT")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                        .gray(true)
                                                        .divideUpstreams(DIVIDE_UPSTREAMS)
                                                        .build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, TEST)
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, TEST)
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.PUT, TEST))
                                .waiting(exists(Method.PUT, TEST))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.PUT, TEST)
                                .addNotExists(Method.GET, TEST)
                                .addNotExists(Method.POST, TEST)
                                .addNotExists(Method.DELETE, TEST)
                                .addNotExists(Method.PUT, "/springcloud/tes")
                                .addNotExists(Method.PUT, "/springcloud/testt")
                                .addNotExists(Method.PUT, "/put")
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.PUT, TEST)).build())
                .build();
    }

    private ShenYuScenarioSpec testWithMethodDelete() {
        return ShenYuScenarioSpec.builder()
                .name("single-divide method DELETE")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.SPRING_CLOUD)
                                                .handle(SpringCloudSelectorHandle.builder().serviceId("springCloud-test")
                                                        .gray(true)
                                                        .divideUpstreams(DIVIDE_UPSTREAMS)
                                                        .build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, TEST)
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(SpringCloudRuleHandle.builder().loadBalance("hash").timeout(3000).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, TEST)
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.DELETE, TEST))
                                .waiting(exists(Method.DELETE, TEST))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.DELETE, TEST)
                                .addNotExists(Method.GET, TEST)
                                .addNotExists(Method.PUT, TEST)
                                .addNotExists(Method.POST, TEST)
                                .addNotExists(Method.DELETE, "/springcloud/tes")
                                .addNotExists(Method.DELETE, "/springcloud/testt")
                                .addNotExists(Method.DELETE, "/delete")
                                .build()
                )
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.DELETE, TEST)).build())
                .build();
    }
    
    /**
     * verifier uri.
     * @param handle handle
     * @throws JsonProcessingException JsonProcessingException
     */
    public static void verifierUri(final String handle) throws JsonProcessingException {
        SpringCloudSelectorHandle springCloudSelectorHandle = MAPPER.readValue(handle, SpringCloudSelectorHandle.class);
        Assertions.assertEquals("springCloud-test", springCloudSelectorHandle.getServiceId());
        Assertions.assertEquals(false, springCloudSelectorHandle.getGray());
        if (ObjectUtils.isEmpty(springCloudSelectorHandle.getDivideUpstreams())) {
            return;
        }
        DivideUpstream divideUpstream = springCloudSelectorHandle.getDivideUpstreams().get(0);
        Assertions.assertEquals(50, divideUpstream.getWeight());
        Assertions.assertEquals(600000, divideUpstream.getWarmup());
        Assertions.assertEquals("http://", divideUpstream.getProtocol());
        Assertions.assertEquals(true, divideUpstream.isStatus());
    }
}
