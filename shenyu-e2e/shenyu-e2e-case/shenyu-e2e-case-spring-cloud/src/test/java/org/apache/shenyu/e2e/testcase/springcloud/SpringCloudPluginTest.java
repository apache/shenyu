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
import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.engine.annotation.ShenYuScenario;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure;
import org.apache.shenyu.e2e.engine.scenario.specification.AfterEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.BeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.CaseSpec;
import org.apache.shenyu.e2e.model.ResourcesData;
import org.apache.shenyu.e2e.model.data.MetaData;
import org.apache.shenyu.e2e.model.data.RuleCacheData;
import org.apache.shenyu.e2e.model.data.SelectorCacheData;
import org.apache.shenyu.e2e.model.response.MetaDataDTO;
import org.apache.shenyu.e2e.model.response.RuleDTO;
import org.apache.shenyu.e2e.model.response.SelectorDTO;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.testcontainers.shaded.com.google.common.collect.Lists;

import java.util.List;

/**
 * Testing spring-cloud plugin.
 */
@ShenYuTest(
        mode = ShenYuEngineConfigure.Mode.DOCKER,
        services = {
                @ShenYuTest.ServiceConfigure(
                        serviceName = "admin",
                        port = 9095,
                        baseUrl = "http://{hostname:localhost}:9095",
                        parameters = {
                                @ShenYuTest.Parameter(key = "username", value = "admin"),
                                @ShenYuTest.Parameter(key = "password", value = "123456"),
                                @ShenYuTest.Parameter(key = "dataSyn", value = "admin_websocket")
                        }
                ),
                @ShenYuTest.ServiceConfigure(
                        serviceName = "gateway",
                        port = 9195,
                        baseUrl = "http://{hostname:localhost}:9195", type = ShenYuEngineConfigure.ServiceType.SHENYU_GATEWAY, parameters = {@ShenYuTest.Parameter(key = "application", value = "spring.cloud.discovery.enabled:true,eureka.client.enabled:true"), @ShenYuTest.Parameter(key = "dataSyn", value = "gateway_websocket")})}, dockerComposeFile = "classpath:./docker-compose.mysql.yml")
public class SpringCloudPluginTest {
    
    private List<String> selectorIds = Lists.newArrayList();
    
    @BeforeAll
    static void setup(final AdminClient adminClient, final GatewayClient gatewayClient) throws InterruptedException, JsonProcessingException {
        adminClient.login();
        Thread.sleep(10000);
        List<SelectorDTO> selectorDTOList = adminClient.listAllSelectors();
        List<MetaDataDTO> metaDataDTOList = adminClient.listAllMetaData();
        List<RuleDTO> ruleDTOList = adminClient.listAllRules();
        Assertions.assertEquals(2, selectorDTOList.size());
        Assertions.assertEquals(13, metaDataDTOList.size());
        Assertions.assertEquals(14, ruleDTOList.size());
        
        for (SelectorDTO selectorDTO : selectorDTOList) {
            if (selectorDTO.getHandle() != null && !"".equals(selectorDTO.getHandle())) {
                SpringCloudPluginCases.verifierUri(selectorDTO.getHandle());
            }
        }
        
        List<MetaData> metaDataCacheList = gatewayClient.getMetaDataCache();
        List<SelectorCacheData> selectorCacheList = gatewayClient.getSelectorCache();
        List<RuleCacheData> ruleCacheList = gatewayClient.getRuleCache();
        Assertions.assertEquals(2, selectorCacheList.size());
        Assertions.assertEquals(13, metaDataCacheList.size());
        Assertions.assertEquals(14, ruleCacheList.size());

        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
        formData.add("id", "8");
        formData.add("name", "springCloud");
        formData.add("enabled", "true");
        formData.add("role", "Proxy");
        formData.add("sort", "200");
        adminClient.startPlugin("8", formData);
        String id = "";
        for (SelectorDTO selectorDTO : selectorDTOList) {
            if (!"".equals(selectorDTO.getHandle())) {
                id = selectorDTO.getId();
            }
        }
        adminClient.deleteSelectors(id);
        selectorDTOList = adminClient.listAllSelectors();
        Assertions.assertEquals(1, selectorDTOList.size());
    }

    @BeforeEach
    void before(final AdminClient client, final GatewayClient gateway, final BeforeEachSpec spec) {
        spec.getChecker().check(gateway);

        ResourcesData resources = spec.getResources();
        for (ResourcesData.Resource res : resources.getResources()) {
            SelectorDTO dto = client.create(res.getSelector());
            selectorIds.add(dto.getId());

            res.getRules().forEach(rule -> {
                rule.setSelectorId(dto.getId());
                client.create(rule);
            });
        }

        spec.getWaiting().waitFor(gateway);
    }

    @ShenYuScenario(provider = SpringCloudPluginCases.class)
    void testSpringCloud(final GatewayClient gateway, final CaseSpec spec) {
        spec.getVerifiers().forEach(verifier -> verifier.verify(gateway.getHttpRequesterSupplier().get()));
    }

    @AfterEach
    void after(final AdminClient client, final GatewayClient gateway, final AfterEachSpec spec) {
        spec.getDeleter().delete(client, selectorIds);
        spec.getPostChecker().check(gateway);
        selectorIds = Lists.newArrayList();
    }

    @AfterAll
    static void teardown(final AdminClient client) {
        client.deleteAllSelectors();
        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
        formData.add("id", "8");
        formData.add("name", "springCloud");
        formData.add("enabled", "false");
        formData.add("role", "Proxy");
        formData.add("sort", "200");
        client.startPlugin("8", formData);
    }
}

