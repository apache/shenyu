///*
// * Licensed to the Apache Software Foundation (ASF) under one or more
// * contributor license agreements.  See the NOTICE file distributed with
// * this work for additional information regarding copyright ownership.
// * The ASF licenses this file to You under the Apache License, Version 2.0
// * (the "License"); you may not use this file except in compliance with
// * the License.  You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package org.apache.shenyu.e2e.testcase.springcloud;
//
//import org.apache.shenyu.e2e.client.WaitDataSync;
//import org.apache.shenyu.e2e.client.admin.AdminClient;
//import org.apache.shenyu.e2e.client.gateway.GatewayClient;
//import org.apache.shenyu.e2e.enums.ServiceTypeEnum;
//import org.apache.shenyu.e2e.k8s.engine.annotation.ShenYuE2ETest;
//import org.junit.jupiter.api.Test;
//
///**
// * Testing the correctness of etcd data synchronization method.
// */
//
////@ShenYuTest(
////        mode = ShenYuEngineConfigure.Mode.DOCKER,
////        services = {
////                @ShenYuTest.ServiceConfigure(
////                        serviceName = "admin",
////                        port = 9095,
////                        baseUrl = "http://{hostname:localhost}:9095",
////                        parameters = {
////                                @ShenYuTest.Parameter(key = "username", value = "admin"),
////                                @ShenYuTest.Parameter(key = "password", value = "123456"),
////                                @ShenYuTest.Parameter(key = "dataSyn", value = "etcd")
////                        }
////                ),
////                @ShenYuTest.ServiceConfigure(
////                        serviceName = "gateway",
////                        port = 9195,
////                        baseUrl = "http://{hostname:localhost}:9195",
////                        type = ShenYuEngineConfigure.ServiceType.SHENYU_GATEWAY,
////                        parameters = {
////                                @ShenYuTest.Parameter(key = "dataSyn", value = "etcd")
////                        }
////                )
////        },
////        dockerComposeFile = "classpath:./docker-compose.mysql.yml"
////)
//@ShenYuE2ETest(environments = {
//        @ShenYuE2ETest.Environment(
//                serviceName = "shenyu-e2e-admin",
//                service = @ShenYuE2ETest.ServiceConfigure(moduleName = "shenyu-e2e",
//                        baseUrl = "http://localhost:31095",
//                        type = ServiceTypeEnum.SHENYU_ADMIN,
//                        parameters = {
//                                @ShenYuE2ETest.Parameter(key = "username", value = "admin"),
//                                @ShenYuE2ETest.Parameter(key = "password", value = "123456")
//                        }
//                )
//        ),
//        @ShenYuE2ETest.Environment(
//                serviceName = "shenyu-e2e-gateway",
//                service = @ShenYuE2ETest.ServiceConfigure(moduleName = "shenyu-e2e",
//                        baseUrl = "http://localhost:31195",
//                        type = ServiceTypeEnum.SHENYU_GATEWAY
//                )
//        )
//})
//public class DataSynEtcdTest {
//    @Test
//    void testDataSyn(final AdminClient adminClient, final GatewayClient gatewayClient) throws Exception {
//        adminClient.login();
//        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllRules, gatewayClient::getRuleCache, adminClient);
//        adminClient.syncPluginAll();
//        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllSelectors, gatewayClient::getSelectorCache, adminClient);
//        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllMetaData, gatewayClient::getMetaDataCache, adminClient);
//        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllRules, gatewayClient::getRuleCache, adminClient);
//    }
//}
