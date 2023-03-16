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
import org.apache.shenyu.e2e.client.admin.model.response.SearchedResources;
import org.apache.shenyu.e2e.client.admin.model.response.SelectorDTO;
import org.apache.shenyu.e2e.matcher.RuleMatcher;
import org.apache.shenyu.e2e.matcher.SelectorMatcher;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
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

@Slf4j
@Testcontainers
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class AdminClientTest {
    static AdminClient client;

    static GenericContainer<?> container = new GenericContainer<>("shenyu/admin:latest")
            .withExposedPorts(9095)
            .withLogConsumer(new Slf4jLogConsumer(log));
    
    SelectorDTO selector;
    RuleDTO rule;
    
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
                .matchRestful(false)
                .handle(Upstreams.builder().add(Upstream.builder().upstreamUrl("httpbin.org:80").build()).build())
                .conditionList(
                        Lists.newArrayList(Condition.builder().paramType(ParamType.URI).operator(Operator.MATCH).paramName("/").paramValue("/**").build())
                )
                .sort(1)
                .build();
        selector = client.create(selectorData);
    
        RuleData ruleData = RuleData.builder()
                .name("test-create-rule")
                .enabled(true)
                .logged(true)
                .matchRestful(false)
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
        rule = client.create(ruleData);
    }
    
    @Test
    void testCreateSelector() {
        SelectorData selectorData = SelectorData.builder()
                .name("test-create-selector")
                .plugin(Plugin.DIVIDE)
                .type(SelectorType.CUSTOM)
                .matchMode(MatchMode.AND)
                .logged(true)
                .enabled(true)
                .continued(true)
                .matchRestful(false)
                .handle(Upstreams.builder().add(Upstream.builder().upstreamUrl("httpbin.org:80").build()).build())
                .conditionList(
                        Lists.newArrayList(Condition.builder().paramType(ParamType.URI).operator(Operator.MATCH).paramName("/").paramValue("/**").build())
                )
                .sort(1)
                .build();
        SelectorDTO selector = client.create(selectorData);
        
        List<SelectorDTO> selectors = client.searchSelectors(selector.getName()).getList();
        Assertions.assertThat(selectors.size()).isEqualTo(1);
        SelectorMatcher.verify(selectorData).matches(selector);
    
        client.deleteAllRules(selector.getId());
    }
    
    @Test
    void testCreateRule() {
        RuleData ruleData = RuleData.builder()
                .name("test-create-rule")
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
                .matchRestful(false)
                .matchMode(MatchMode.AND)
                .selectorId(selector.getId())
                .conditionList(Lists.newArrayList(Condition.builder()
                        .paramValue("/z")
                        .paramType(ParamType.URI)
                        .paramName("/")
                        .operator(Operator.EQUAL)
                        .build()))
                .build();
        RuleDTO rule = client.create(ruleData);
        RuleMatcher.verify(ruleData).matches(rule);
    }
    
    @Test
    @Order(200)
    void testListSelectors() {
        for (int i = 0; i <= 20; i++) {
            client.create(SelectorData.builder()
                    .name("test-list-selectors")
                    .plugin(Plugin.DIVIDE)
                    .type(SelectorType.CUSTOM)
                    .matchMode(MatchMode.AND)
                    .logged(true)
                    .enabled(true)
                    .continued(true)
                    .matchRestful(false)
                    .handle(Upstreams.builder().add(Upstream.builder().upstreamUrl("httpbin.org:80").build()).build())
                    .conditionList(
                            Lists.newArrayList(Condition.builder().paramType(ParamType.URI).operator(Operator.MATCH).paramName("/").paramValue("/**").build())
                    )
                    .sort(1)
                    .build());
        }
        SearchedResources<SelectorDTO> searchedResources = client.searchSelectors(null);
        Assertions.assertThat(searchedResources.getTotal()).isGreaterThan(20);
        Assertions.assertThat(client.listAllSelectors().size()).isEqualTo(searchedResources.getTotal());
    }
    
    @Test
    @Order(201)
    // depends on {@link #testListSelectors}
    void testSearchSelectors() {
        SearchedResources<SelectorDTO> searched = client.searchSelectors("test-list-selectors");
        Assertions.assertThat(searched.getTotal()).isGreaterThanOrEqualTo(20);
        Assertions.assertThat(searched.getPages()).isGreaterThanOrEqualTo(2);
        Assertions.assertThat(searched.getPageNum()).isEqualTo(1);
        Assertions.assertThat(searched.getPageSize()).isEqualTo(10);
        
        searched = client.searchSelectors("test-list-selectors", 2, 10);
        Assertions.assertThat(searched.getTotal()).isGreaterThanOrEqualTo(20);
        Assertions.assertThat(searched.getPages()).isGreaterThanOrEqualTo(2);
        Assertions.assertThat(searched.getPageNum()).isEqualTo(2);
        Assertions.assertThat(searched.getPageSize()).isEqualTo(10);
    }
    
    @Test
    void searchRules() {
        SearchedResources<RuleDTO> searched = client.searchRules(rule.getName());
        Assertions.assertThat(searched.getPageNum()).isEqualTo(1);
        Assertions.assertThat(searched.getPageSize()).isEqualTo(10);
        Assertions.assertThat(searched.getPages()).isEqualTo(1);
        Assertions.assertThat(searched.getTotal()).isEqualTo(1);
    
        searched = client.searchRules(rule.getName(), selector.getId());
        Assertions.assertThat(searched.getPageNum()).isEqualTo(1);
        Assertions.assertThat(searched.getPageSize()).isEqualTo(10);
        Assertions.assertThat(searched.getPages()).isEqualTo(1);
        Assertions.assertThat(searched.getTotal()).isEqualTo(1);
    
        searched = client.searchRules(null, selector.getId());
        Assertions.assertThat(searched.getPageNum()).isEqualTo(1);
        Assertions.assertThat(searched.getPageSize()).isEqualTo(10);
        Assertions.assertThat(searched.getPages()).isEqualTo(1);
        Assertions.assertThat(searched.getTotal()).isEqualTo(1);
    
        searched = client.searchRules(null, "fake");
        Assertions.assertThat(searched.getPageNum()).isEqualTo(1);
        Assertions.assertThat(searched.getPageSize()).isEqualTo(10);
        Assertions.assertThat(searched.getPages()).isEqualTo(0);
        Assertions.assertThat(searched.getTotal()).isEqualTo(0);
    }
    
    @AfterEach
    void deleteResources() {
        client.deleteSelectors(selector.getId());
        Assertions.assertThat(client.getSelector(selector.getId())).isNull();
        Assertions.assertThat(client.searchSelectors(selector.getName()).getTotal()).isZero();
        Assertions.assertThat(client.searchRules(null, selector.getId()).getTotal()).isZero();
    }
    
    @AfterAll
    static void testDeleteAllSelectors() {
        client.deleteAllSelectors();
    }
    
}
