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

package org.apache.shenyu.e2e.testcase.apachedubbo;

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
import org.apache.shenyu.e2e.model.handle.DubboHandler;
import org.apache.shenyu.e2e.model.handle.DubboRuleHandle;
import org.junit.jupiter.api.Assertions;

import java.util.List;
import java.util.Map;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.notExists;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newCondition;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;

public class ApacheDubboPluginCases implements ShenYuScenarioProvider {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testDubboFindAll()
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
    
    private ShenYuScenarioSpec testDubboFindAll() {
        return ShenYuScenarioSpec.builder()
                .name("apache dubbo findAll")
                .beforeEachSpec(ShenYuBeforeEachSpec.builder()
                        .checker(exists("/dubbo/findAll"))
                        .build())
                .caseSpec(ShenYuCaseSpec.builder()
                        .addExists("/dubbo/findAll")
                        .addNotExists("/dubbo/findAll/aaa")
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
                .name("single-apache-dubbo uri =]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.DUBBO)
                                                .handle(DubboHandler.builder().protocol("dubbo://")
                                                        .upstreamUrl("dubbo:20888").status(true).timestamp(System.currentTimeMillis()).upstreamHost("dubbo")
                                                        .weight(50).warmup(600000).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/dubbo/findAll"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(DubboRuleHandle.builder().loadBalance("random").timeout(3000).retries(0).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/dubbo/findAll"))
                                                .build()
                                )
                                .checker(notExists("/dubbo/findAll"))
                                .waiting(exists("/dubbo/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/dubbo/findAll")
                                .addNotExists("/dubbo")
                                .addNotExists("/put")
                                .addNotExists("/get")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/dubbo/findAll")).build())
                .build();
    }

    /**
     * test with uri path pattern.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriPathPattern() {
        return ShenYuScenarioSpec.builder()
                .name("single-apache-dubbo uri path_pattern]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.DUBBO)
                                                .handle(DubboHandler.builder().protocol("dubbo://")
                                                        .upstreamUrl("dubbo:20888").status(true).timestamp(System.currentTimeMillis()).upstreamHost("dubbo")
                                                        .weight(50).warmup(600000).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/dubbo/**"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(DubboRuleHandle.builder().loadBalance("random").timeout(3000).retries(0).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.PATH_PATTERN, "/dubbo/**"))
                                                .build()
                                )
                                .checker(notExists("/dubbo/findAll"))
                                .waiting(exists("/dubbo/findAll "))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/dubbo/findAll")
                                .addNotExists("/dubbo/find")
                                .addExists(Method.POST, "/dubbo/findAll")
                                .addExists(Method.PUT, "/dubbo/findAll")
                                .addExists(Method.DELETE, "/dubbo/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/dubbo/findAll")).build())
                .build();
    }

    /**
     * test with uri start with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithUriStartWith() {
        return ShenYuScenarioSpec.builder()
                .name("single-apache-dubbo uri starts_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.DUBBO)
                                                .handle(DubboHandler.builder().protocol("dubbo://")
                                                        .upstreamUrl("dubbo:20888").status(true).timestamp(System.currentTimeMillis()).upstreamHost("dubbo")
                                                        .weight(50).warmup(600000).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/dubbo/"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(DubboRuleHandle.builder().loadBalance("random").timeout(3000).retries(0).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.STARTS_WITH, "/dubbo/"))
                                                .build()
                                )
                                .checker(notExists("/dubbo/findAll"))
                                .waiting(exists("/dubbo/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/dubbo/findAll")
                                .addNotExists("/dubbo/de")
                                .addExists(Method.POST, "/dubbo/findAll")
                                .addExists(Method.PUT, "/dubbo/findAll")
                                .addExists(Method.DELETE, "/dubbo/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/dubbo/findAll")).build())
                .build();
    }

    /**
     * test with uri end with.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithEndWith() {
        return ShenYuScenarioSpec.builder()
                .name("single-apache-dubbo uri ends_with]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.DUBBO)
                                                .handle(DubboHandler.builder().protocol("dubbo://")
                                                        .upstreamUrl("dubbo:20888").status(true).timestamp(System.currentTimeMillis()).upstreamHost("dubbo")
                                                        .weight(50).warmup(600000).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/findAll"))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(DubboRuleHandle.builder().loadBalance("random").timeout(3000).retries(0).build())
                                                .conditionList(newConditions(Condition.ParamType.URI, Condition.Operator.ENDS_WITH, "/findAll"))
                                                .build()
                                )
                                .checker(notExists("/dubbo/findAll"))
                                .waiting(exists("/dubbo/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists("/dubbo/findAll")
                                .addNotExists("/dubbo/find")
                                .addExists(Method.POST, "/dubbo/findAll")
                                .addExists(Method.PUT, "/dubbo/findAll")
                                .addExists(Method.DELETE, "/dubbo/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists("/dubbo/findAll")).build())
                .build();
    }

    /**
     * test with uri method get.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodGet() {
        return ShenYuScenarioSpec.builder()
                .name("single-apache-dubbo uri method GET]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.DUBBO)
                                                .handle(DubboHandler.builder().protocol("dubbo://")
                                                        .upstreamUrl("dubbo:20888").status(true).timestamp(System.currentTimeMillis()).upstreamHost("dubbo")
                                                        .weight(50).warmup(600000).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/dubbo/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(DubboRuleHandle.builder().loadBalance("random").timeout(3000).retries(0).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "GET"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/dubbo/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.GET, "/dubbo/findAll"))
                                .waiting(exists(Method.GET, "/dubbo/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.GET, "/dubbo/findAll")
                                .addNotExists(Method.GET, "/dubbo/find")
                                .addNotExists(Method.POST, "/dubbo/findAll")
                                .addNotExists(Method.PUT, "/dubbo/findAll")
                                .addNotExists(Method.DELETE, "/dubbo/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.GET, "/dubbo/findAll")).build())
                .build();
    }

    /**
     * test with uri method post.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodPost() {
        return ShenYuScenarioSpec.builder()
                .name("single-apache-dubbo uri method POST]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.DUBBO)
                                                .handle(DubboHandler.builder().protocol("dubbo://")
                                                        .upstreamUrl("dubbo:20888").status(true).timestamp(System.currentTimeMillis()).upstreamHost("dubbo")
                                                        .weight(50).warmup(600000).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/dubbo/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(DubboRuleHandle.builder().loadBalance("random").timeout(3000).retries(0).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "POST"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/dubbo/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.POST, "/dubbo/findAll"))
                                .waiting(exists(Method.POST, "/dubbo/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/dubbo/findAll")
                                .addNotExists(Method.POST, "/dubbo/find")
                                .addNotExists(Method.GET, "/dubbo/findAll")
                                .addNotExists(Method.PUT, "/dubbo/findAll")
                                .addNotExists(Method.DELETE, "/dubbo/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.POST, "/dubbo/findAll")).build())
                .build();
    }

    /**
     * test with uri method put.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodPut() {
        return ShenYuScenarioSpec.builder()
                .name("single-apache-dubbo uri method PUT]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.DUBBO)
                                                .handle(DubboHandler.builder().protocol("dubbo://")
                                                        .upstreamUrl("dubbo:20888").status(true).timestamp(System.currentTimeMillis()).upstreamHost("dubbo")
                                                        .weight(50).warmup(600000).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/dubbo/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(DubboRuleHandle.builder().loadBalance("random").timeout(3000).retries(0).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "PUT"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/dubbo/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.PUT, "/dubbo/findAll"))
                                .waiting(exists(Method.PUT, "/dubbo/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.PUT, "/dubbo/findAll")
                                .addNotExists(Method.PUT, "/dubbo/find")
                                .addNotExists(Method.GET, "/dubbo/findAll")
                                .addNotExists(Method.POST, "/dubbo/findAll")
                                .addNotExists(Method.DELETE, "/dubbo/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.PUT, "/dubbo/findAll")).build())
                .build();
    }

    /**
     * test with uri method delete.
     *
     * @return ShenYuScenarioSpec
     */
    public ShenYuScenarioSpec testWithMethodDelete() {
        return ShenYuScenarioSpec.builder()
                .name("single-apache-dubbo uri method DELETE]")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .addSelectorAndRule(
                                        newSelectorBuilder("selector", Plugin.DUBBO)
                                                .handle(DubboHandler.builder().protocol("dubbo://")
                                                        .upstreamUrl("dubbo:20888").status(true).timestamp(System.currentTimeMillis()).upstreamHost("dubbo")
                                                        .weight(50).warmup(600000).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/dubbo/findAll")
                                                ))
                                                .build(),
                                        newRuleBuilder("rule")
                                                .handle(DubboRuleHandle.builder().loadBalance("random").timeout(3000).retries(0).build())
                                                .conditionList(Lists.newArrayList(
                                                        newCondition(Condition.ParamType.METHOD, Condition.Operator.EQUAL, "DELETE"),
                                                        newCondition(Condition.ParamType.URI, Condition.Operator.EQUAL, "/dubbo/findAll")
                                                ))
                                                .build()
                                )
                                .checker(notExists(Method.DELETE, "/dubbo/findAll"))
                                .waiting(exists(Method.DELETE, "/dubbo/findAll"))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.DELETE, "/dubbo/findAll")
                                .addNotExists(Method.DELETE, "/dubbo/find")
                                .addNotExists(Method.GET, "/dubbo/findAll")
                                .addNotExists(Method.POST, "/dubbo/findAll")
                                .addNotExists(Method.PUT, "/dubbo/findAll")
                                .build())
                .afterEachSpec(ShenYuAfterEachSpec.builder().deleteWaiting(notExists(Method.DELETE, "/dubbo/findAll")).build())
                .build();
    }

    /**
     * verifier uri.
     *
     * @param handle handle
     * @throws JsonProcessingException JsonProcessingException
     */
    public static void verifierUri(final String handle) throws JsonProcessingException {
        List<DubboHandler> dubboHandlerList = MAPPER.readValue(handle, List.class);
        Map<String, Object> map = (Map<String, Object>) dubboHandlerList.get(0);
        Assertions.assertEquals(50, map.get("weight"));
        Assertions.assertEquals(600000, map.get("warmup"));
        Assertions.assertEquals("dubbo://", map.get("protocol"));
        Assertions.assertEquals(true, map.get("status"));
    }
}
