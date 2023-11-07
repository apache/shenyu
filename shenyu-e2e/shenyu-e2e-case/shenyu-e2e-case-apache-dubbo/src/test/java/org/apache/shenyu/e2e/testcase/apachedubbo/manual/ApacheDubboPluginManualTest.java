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

package org.apache.shenyu.e2e.testcase.apachedubbo.manual;

import com.google.common.collect.Lists;
import org.apache.shenyu.e2e.client.WaitDataSync;
import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.engine.annotation.ShenYuScenario;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest;
import org.apache.shenyu.e2e.engine.scenario.specification.AfterEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.BeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.CaseSpec;
import org.apache.shenyu.e2e.enums.ServiceTypeEnum;
import org.apache.shenyu.e2e.model.ResourcesData;
import org.apache.shenyu.e2e.model.response.SelectorDTO;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import java.util.List;

@ShenYuTest(environments = {
        @ShenYuTest.Environment(
                serviceName = "shenyu-e2e-admin",
                service = @ShenYuTest.ServiceConfigure(moduleName = "shenyu-e2e",
                        baseUrl = "http://localhost:31095",
                        type = ServiceTypeEnum.SHENYU_ADMIN,
                        parameters = {
                                @ShenYuTest.Parameter(key = "username", value = "admin"),
                                @ShenYuTest.Parameter(key = "password", value = "123456")
                        }
                )
        ),
        @ShenYuTest.Environment(
                serviceName = "shenyu-e2e-gateway",
                service = @ShenYuTest.ServiceConfigure(moduleName = "shenyu-e2e",
                        baseUrl = "http://localhost:31195",
                        type = ServiceTypeEnum.SHENYU_GATEWAY
                )
        )
})
public class ApacheDubboPluginManualTest {
    
    private static final Logger LOG = LoggerFactory.getLogger(ApacheDubboPluginManualTest.class);
    
    private List<String> selectorIds = Lists.newArrayList();
    
    @BeforeEach
    public void setup(final AdminClient adminClient, final GatewayClient gatewayClient) throws Exception {
        adminClient.login();
        final List<SelectorDTO> selectorDTOList = adminClient.listAllSelectors();
        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllSelectors, gatewayClient::getSelectorCache, adminClient);
        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllMetaData, gatewayClient::getMetaDataCache, adminClient);
        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllRules, gatewayClient::getRuleCache, adminClient);
        for (SelectorDTO selectorDTO : selectorDTOList) {
            if (selectorDTO.getHandle() != null && !selectorDTO.getHandle().equals("")) {
                ApacheDubboPluginManualCases.verifierUri(selectorDTO.getHandle());
            }
        }
        LOG.info("start dubbo plugin");
        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
        formData.add("id", "6");
        formData.add("name", "dubbo");
        formData.add("enabled", "true");
        formData.add("role", "Proxy");
        formData.add("sort", "310");
        formData.add("config", "{\"corethreads\":\"0\",\"multiSelectorHandle\":\"1\",\"queues\":\"0\","
                + "\"threadpool\":\"shared\",\"threads\":2147483647,\"register\":\"zookeeper://shenyu-zookeeper:2181\"}");
        adminClient.changePluginStatus("6", formData);
        WaitDataSync.waitGatewayPluginUse(gatewayClient, "org.apache.shenyu.plugin.apache.dubbo.ApacheDubboPlugin");
        LOG.info("start dubbo plugin success!");
        adminClient.deleteAllSelectors();
        Assertions.assertEquals(0, adminClient.listAllSelectors().size());
    }
    
    @ShenYuScenario(provider = ApacheDubboPluginManualCases.class)
    void testSofa(final GatewayClient gateway, final CaseSpec spec) {
        spec.getVerifiers().forEach(verifier -> verifier.verify(gateway.getHttpRequesterSupplier().get()));
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
    
    @AfterEach
    void after(final AdminClient client, final GatewayClient gateway, final AfterEachSpec spec) {
        spec.getDeleter().delete(client, selectorIds);
        spec.deleteWaiting().waitFor(gateway);
        selectorIds = Lists.newArrayList();
    }
    
    @AfterAll
    static void down(final AdminClient client) {
        client.deleteAllSelectors();
    }
}

