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

package org.apache.shenyu.e2e.client.admin;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.e2e.client.admin.model.MatchMode;
import org.apache.shenyu.e2e.client.admin.model.Plugin;
import org.apache.shenyu.e2e.client.admin.model.SelectorType;
import org.apache.shenyu.e2e.client.admin.model.data.Condition;
import org.apache.shenyu.e2e.client.admin.model.data.Condition.Operator;
import org.apache.shenyu.e2e.client.admin.model.data.Condition.ParamType;
import org.apache.shenyu.e2e.client.admin.model.data.RuleData;
import org.apache.shenyu.e2e.client.admin.model.data.SelectorData;
import org.apache.shenyu.e2e.client.admin.model.handle.DivideRuleHandle;
import org.apache.shenyu.e2e.client.admin.model.handle.Upstreams;
import org.apache.shenyu.e2e.client.admin.model.handle.Upstreams.Upstream;
import org.apache.shenyu.e2e.client.admin.model.response.RuleDTO;
import org.apache.shenyu.e2e.client.admin.model.response.SelectorDTO;
import org.apache.shenyu.e2e.matcher.SelectorMatcher;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.containers.wait.strategy.HttpWaitStrategy;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.time.Duration;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

@Slf4j
@Testcontainers
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class AdminClientTest {
    static AdminClient client;
    
    static GenericContainer<?> container = new GenericContainer<>("shenyu/admin:latest")
            .withExposedPorts(9095)
            .withLogConsumer(new Slf4jLogConsumer(log));
    
    @BeforeAll
    static void setup() {
        container.start();
        container.waitingFor(new HttpWaitStrategy()
                .allowInsecure()
                .forPort(9095)
                .withMethod("GET")
                .forPath("/actuator")
                .forStatusCode(200)
                .withReadTimeout(Duration.ofMinutes(1))
                .withStartupTimeout(Duration.ofMinutes(3)));
        
        Properties properties = new Properties();
        properties.put("username", "admin");
        properties.put("password", "123456");
        
        client = new AdminClient("shenyu-e2e", "http://localhost:" + container.getMappedPort(9095), properties);
        client.login();
    }
    
    @BeforeEach
    void createResources() {
        SelectorData selectorData = SelectorData.builder()
                .name("my-plugin-divide")
                .plugin(Plugin.DIVIDE)
                .type(SelectorType.CUSTOM)
                .matchMode(MatchMode.AND)
                .logged(true)
                .enabled(true)
                .continued(true)
                .handle(Upstreams.builder().add(Upstream.builder().upstreamUrl("httpbin.org:80").build()).build())
                .conditionList(
                        Lists.newArrayList(Condition.builder().paramType(ParamType.URI).operator(Operator.MATCH).paramName("/").paramValue("/**").build())
                )
                .sort(1)
                .build();
        SelectorDTO selector = client.create(selectorData);
        
        RuleData ruleData = RuleData.builder()
                .name("my-rule")
                .enabled(true)
                .logged(true)
                .handle(DivideRuleHandle.builder()
                        .loadBalance("hash")
                        .retryStrategy("current")
                        .retry(1)
                        .timeout(3000)
                        .headerMaxSize(10240)
                        .requestMaxSize(10240)
                        .build())
                .sort(1)
                .matchMode(MatchMode.AND)
                .selectorId(selector.getId())
                .conditionList(Lists.newArrayList(Condition.builder()
                        .paramValue("/z")
                        .paramType(ParamType.URI)
                        .paramName("/")
                        .operator(Operator.EQUAL)
                        .build()))
                .build();
        client.create(ruleData);
    }
    
    @Test
    @Order(0)
    void testCreateSelector() throws JsonProcessingException {
        SelectorData selectorData = SelectorData.builder()
                .name("my-plugin-divide")
                .plugin(Plugin.DIVIDE)
                .type(SelectorType.CUSTOM)
                .matchMode(MatchMode.AND)
                .logged(true)
                .enabled(true)
                .continued(true)
                .handle(Upstreams.builder().add(Upstream.builder().upstreamUrl("httpbin.org:80").build()).build())
                .conditionList(
                        Lists.newArrayList(Condition.builder().paramType(ParamType.URI).operator(Operator.MATCH).paramName("/").paramValue("/**").build())
                )
                .sort(1)
                .build();
        SelectorDTO selector = client.create(selectorData);
        
        List<SelectorDTO> selectors = client.searchSelector(selector.getName());
        Assertions.assertThat(selectors.size()).isEqualTo(1);
        SelectorMatcher.verify(selectorData).matches(selector);
        
        RuleDTO ruleDTO = client.create(RuleData.builder()
                .name("my-rule")
                .enabled(true)
                .logged(true)
                .handle(DivideRuleHandle.builder()
                        .loadBalance("hash")
                        .retryStrategy("current")
                        .retry(1)
                        .timeout(3000)
                        .headerMaxSize(10240)
                        .requestMaxSize(10240)
                        .build())
                .sort(1)
                .matchMode(MatchMode.AND)
                .selectorId(selector.getId())
                .conditionList(Lists.newArrayList(Condition.builder()
                        .paramValue("/z")
                        .paramType(ParamType.URI)
                        .paramName("/")
                        .operator(Operator.EQUAL)
                        .build()))
                .build());
    }
    
    @Test
    void testDeleteAllSelectors() {
        client.deleteAllSelectors();
        List<SelectorDTO> selectors = client.listAllSelectors();
        Assertions.assertThat(selectors).isEmpty();
    }
    
    @Test
    void testListRules() {
        List<String> list = client.listAllRules()
                .stream()
                .map(RuleDTO::getId)
                .collect(Collectors.toList());
        client.deleteRules(list.toArray(new String[]{}));
        
        Assertions.assertThat(client.listAllRules()).isEmpty();
    }
    
}
