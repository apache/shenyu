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

package org.apache.shenyu.e2e.k8s.engine;

//import org.apache.shenyu.e2e.client.EnvironmentClient;
//import org.apache.shenyu.e2e.client.admin.AdminClient;
//import org.apache.shenyu.e2e.client.gateway.GatewayClient;
//import org.apache.shenyu.e2e.enums.ServiceTypeEnum;
//import org.apache.shenyu.e2e.engine.annotation.ShenYuE2ETest;
//import org.junit.jupiter.api.MethodOrderer;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.TestMethodOrder;

//@ShenYuE2ETest(environments = {
//        @ShenYuE2ETest.Environment(
//                serviceName = "shenyu-e2e-mysql",
//                service = @ShenYuE2ETest.ServiceConfigure(moduleName = "shenyu-e2e",
//                        baseUrl = "http://localhost:9095",
//                        type = ServiceTypeEnum.SHENYU_ADMIN,
//                        parameters = {
//                                @ShenYuE2ETest.Parameter(key = "username", value = "root"),
//                                @ShenYuE2ETest.Parameter(key = "password", value = "123456")
//                        }
//                )
//        ),
//        @ShenYuE2ETest.Environment(
//                serviceName = "shenyu-e2e-postgresql",
//                service = @ShenYuE2ETest.ServiceConfigure(moduleName = "shenyu-e2e",
//                        baseUrl = "http://localhost:9095",
//                        type = ServiceTypeEnum.SHENYU_GATEWAY
//                )
//        )
//})
//@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
//public class ShenYuE2eExtensionTest {
//
//    @Test
//    public void test(final AdminClient adminClient, final GatewayClient gatewayClient, final EnvironmentClient client) {
//    }
//}
