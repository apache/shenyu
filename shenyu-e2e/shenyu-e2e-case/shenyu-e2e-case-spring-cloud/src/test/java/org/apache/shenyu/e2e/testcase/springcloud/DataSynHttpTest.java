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
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure;
import org.apache.shenyu.e2e.model.data.MetaData;
import org.apache.shenyu.e2e.model.data.RuleCacheData;
import org.apache.shenyu.e2e.model.data.SelectorCacheData;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;

/**
 * Testing the correctness of Http data synchronization method.
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
                                @ShenYuTest.Parameter(key = "dataSyn", value = "admin_http")
                        }
                ),
                @ShenYuTest.ServiceConfigure(
                        serviceName = "gateway",
                        port = 9195,
                        baseUrl = "http://{hostname:localhost}:9195",
                        type = ShenYuEngineConfigure.ServiceType.SHENYU_GATEWAY,
                        parameters = {
                                @ShenYuTest.Parameter(key = "application", value = "spring.cloud.discovery.enabled:true,eureka.client.enabled:true"),
                                @ShenYuTest.Parameter(key = "dataSyn", value = "gateway_http")
                        }
                )
        },
        dockerComposeFile = "classpath:./docker-compose.mysql.yml"
)
public class DataSynHttpTest {

    @Test
    void testDataSyn(final AdminClient adminClient, final GatewayClient gatewayClient) throws InterruptedException, JsonProcessingException {
        adminClient.login();
        Thread.sleep(10000);
        List<MetaData> metaDataCacheList = gatewayClient.getMetaDataCache();
        List<SelectorCacheData> selectorCacheList = gatewayClient.getSelectorCache();
        List<RuleCacheData> ruleCacheList = gatewayClient.getRuleCache();
        Assertions.assertEquals(2, selectorCacheList.size());
        Assertions.assertEquals(13, metaDataCacheList.size());
        Assertions.assertEquals(14, ruleCacheList.size());
    }
}
