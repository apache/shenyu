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

package org.apache.shenyu.e2e.testcase.sofa;

import com.google.common.collect.Lists;
import org.apache.shenyu.e2e.client.WaitDataSync;
import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.constant.Constants;
import org.apache.shenyu.e2e.engine.annotation.ShenYuScenario;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest;
import org.apache.shenyu.e2e.engine.scenario.specification.CaseSpec;
import org.apache.shenyu.e2e.enums.ServiceTypeEnum;
import org.junit.jupiter.api.BeforeAll;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
public class SofaPluginTest {
    private List<String> selectorIds = Lists.newArrayList();

    @BeforeAll
    static void setup(final AdminClient adminClient, final GatewayClient gatewayClient) throws Exception {
        adminClient.login();
        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllSelectors, gatewayClient::getSelectorCache, adminClient);
        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllMetaData, gatewayClient::getMetaDataCache, adminClient);
        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllRules, gatewayClient::getRuleCache, adminClient);

        Map<String, String> reqBody = new HashMap<>();
        reqBody.put("pluginId", "11");
        reqBody.put("name", "sofa");
        reqBody.put("enabled", "true");
        reqBody.put("role", "Proxy");
        reqBody.put("sort", "310");
        reqBody.put("config", "{\"protocol\":\"zookeeper\",\"register\":\"shenyu-zookeeper:2181\"}");
        reqBody.put("namespaceId", Constants.SYS_DEFAULT_NAMESPACE_NAMESPACE_ID);
        adminClient.changePluginStatus("1801816010882822147", reqBody);
        WaitDataSync.waitGatewayPluginUse(gatewayClient, "org.apache.shenyu.plugin.sofa.SofaPlugin");
    }
    
    @ShenYuScenario(provider = SofaPluginCases.class)
    void testSofa(final GatewayClient gateway, final CaseSpec spec) {
        //spec.getVerifiers().forEach(verifier -> verifier.verify(gateway.getHttpRequesterSupplier().get()));
    }
}

