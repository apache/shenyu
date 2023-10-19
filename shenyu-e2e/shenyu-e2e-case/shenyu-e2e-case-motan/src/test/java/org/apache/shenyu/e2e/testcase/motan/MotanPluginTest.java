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
//
//import io.restassured.RestAssured;
//import io.restassured.parsing.Parser;
//import org.apache.shenyu.e2e.client.WaitDataSync;
//import org.apache.shenyu.e2e.client.admin.AdminClient;
//import org.apache.shenyu.e2e.client.gateway.GatewayClient;
//import org.apache.shenyu.e2e.engine.annotation.ShenYuScenario;
//import org.apache.shenyu.e2e.engine.annotation.ShenYuTest;
//import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure;
//import org.apache.shenyu.e2e.engine.scenario.specification.AfterEachSpec;
//import org.apache.shenyu.e2e.engine.scenario.specification.BeforeEachSpec;
//import org.apache.shenyu.e2e.engine.scenario.specification.CaseSpec;
//import org.apache.shenyu.e2e.model.ResourcesData;
//import org.apache.shenyu.e2e.model.response.SelectorDTO;
//import org.junit.jupiter.api.AfterAll;
//import org.junit.jupiter.api.AfterEach;
//import org.junit.jupiter.api.Assertions;
//import org.junit.jupiter.api.BeforeAll;
//import org.junit.jupiter.api.BeforeEach;
//import org.springframework.util.LinkedMultiValueMap;
//import org.springframework.util.MultiValueMap;
//import org.testcontainers.shaded.com.google.common.collect.Lists;
//
//import java.util.List;
//
//@ShenYuTest(
//        mode = ShenYuEngineConfigure.Mode.DOCKER,
//        services = {
//                @ShenYuTest.ServiceConfigure(
//                        serviceName = "admin",
//                        port = 9095,
//                        baseUrl = "http://{hostname:localhost}:9095",
//                        parameters = {
//                                @ShenYuTest.Parameter(key = "username", value = "admin"),
//                                @ShenYuTest.Parameter(key = "password", value = "123456"),
//                                @ShenYuTest.Parameter(key = "dataSyn", value = "admin_websocket")
//                        }
//                ),
//                @ShenYuTest.ServiceConfigure(
//                        serviceName = "gateway",
//                        port = 9195,
//                        baseUrl = "http://{hostname:localhost}:9195",
//                        type = ShenYuEngineConfigure.ServiceType.SHENYU_GATEWAY,
//                        parameters = {
//                                @ShenYuTest.Parameter(key = "dataSyn", value = "gateway_websocket")
//                        }
//                )
//        },
//        dockerComposeFile = "classpath:./docker-compose.mysql.yml"
//)
///**
// * Testing spring-cloud plugin.
// */
//public class MotanPluginTest {
//    private List<String> selectorIds = Lists.newArrayList();
//
//    @BeforeAll
//    static void setup(final AdminClient adminClient, final GatewayClient gatewayClient) throws Exception {
//        adminClient.login();
//        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllRules, gatewayClient::getRuleCache, adminClient);
//        adminClient.syncPluginAll();
//        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllSelectors, gatewayClient::getSelectorCache, adminClient);
//        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllMetaData, gatewayClient::getMetaDataCache, adminClient);
//        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllRules, gatewayClient::getRuleCache, adminClient);
//
//        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
//        formData.add("id", "17");
//        formData.add("name", "motan");
//        formData.add("enabled", "true");
//        formData.add("role", "Proxy");
//        formData.add("sort", "310");
//        formData.add("config", "{\"registerProtocol\":\"zk\", \"registerAddress\":\"zookeeper:2181\"}");
//        adminClient.changePluginStatus("17", formData);
//        adminClient.deleteAllSelectors();
//        List<SelectorDTO> selectorDTOList = adminClient.listAllSelectors();
//        Assertions.assertEquals(0, selectorDTOList.size());
//        RestAssured.registerParser("text/plain", Parser.JSON);
//    }
//
//    @BeforeEach
//    void before(final AdminClient client, final GatewayClient gateway, final BeforeEachSpec spec) {
//        spec.getChecker().check(gateway);
//
//        ResourcesData resources = spec.getResources();
//        for (ResourcesData.Resource res : resources.getResources()) {
//            SelectorDTO dto = client.create(res.getSelector());
//            selectorIds.add(dto.getId());
//
//            res.getRules().forEach(rule -> {
//                rule.setSelectorId(dto.getId());
//                client.create(rule);
//            });
//        }
//
//        spec.getWaiting().waitFor(gateway);
//    }
//
//    @ShenYuScenario(provider = MotanPluginCases.class)
//    void testMotan(final GatewayClient gateway, final CaseSpec spec) {
//        spec.getVerifiers().forEach(verifier -> verifier.verify(gateway.getHttpRequesterSupplier().get()));
//    }
//
//    @AfterEach
//    void after(final AdminClient client, final GatewayClient gateway, final AfterEachSpec spec) {
//        spec.getDeleter().delete(client, selectorIds);
//        spec.deleteWaiting().waitFor(gateway);
//        selectorIds = Lists.newArrayList();
//    }
//
//    @AfterAll
//    static void teardown(final AdminClient client) {
//        client.deleteAllSelectors();
//        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
//        formData.add("id", "17");
//        formData.add("name", "motan");
//        formData.add("enabled", "false");
//        formData.add("role", "Proxy");
//        formData.add("sort", "310");
//        client.changePluginStatus("17", formData);
//    }
//}

