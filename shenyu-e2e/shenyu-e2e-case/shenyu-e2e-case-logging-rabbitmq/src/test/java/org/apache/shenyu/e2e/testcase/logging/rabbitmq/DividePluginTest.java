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

package org.apache.shenyu.e2e.testcase.logging.rabbitmq;

import com.google.common.collect.Lists;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import org.apache.shenyu.e2e.client.WaitDataSync;
import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.constant.Constants;
import org.apache.shenyu.e2e.engine.annotation.ShenYuScenario;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest;
import org.apache.shenyu.e2e.engine.scenario.specification.AfterEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.BeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.CaseSpec;
import org.apache.shenyu.e2e.enums.ServiceTypeEnum;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.ResourcesData;
import org.apache.shenyu.e2e.model.data.BindingData;
import org.apache.shenyu.e2e.model.response.SelectorDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.apache.shenyu.e2e.constant.Constants.SYS_DEFAULT_NAMESPACE_NAMESPACE_ID;
import static org.awaitility.Awaitility.await;

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
public class DividePluginTest {

    private static final Logger LOG = LoggerFactory.getLogger(DividePluginTest.class);

    private static final int CONNECTION_TIMEOUT_MILLIS = 5_000;

    private static final int HANDSHAKE_TIMEOUT_MILLIS = 5_000;

    private List<String> selectorIds = Lists.newArrayList();

    @BeforeEach
    void before(final AdminClient client, final GatewayClient gateway, final BeforeEachSpec spec) {
        spec.getChecker().check(gateway);

        ResourcesData resources = spec.getResources();
        for (ResourcesData.Resource resource : resources.getResources()) {
            SelectorDTO selector = client.create(resource.getSelector());
            selectorIds.add(selector.getId());
            resource.getRules().forEach(rule -> {
                rule.setSelectorId(selector.getId());
                client.create(rule);
            });
            BindingData bindingData = resource.getBindingData();
            if (Objects.nonNull(bindingData)) {
                bindingData.setSelectorId(selector.getId());
                bindingData.setNamespaceId(SYS_DEFAULT_NAMESPACE_NAMESPACE_ID);
                client.bindingData(bindingData);
            }
        }
        spec.getWaiting().waitFor(gateway);
    }

    @AfterEach
    void after(final AdminClient client, final GatewayClient gateway, final AfterEachSpec spec) {
        spec.getDeleter().delete(client, selectorIds);
        spec.deleteWaiting().waitFor(gateway);
        selectorIds = Lists.newArrayList();
    }

    @BeforeAll
    void setup(final AdminClient adminClient, final GatewayClient gatewayClient) throws Exception {
        adminClient.login();
        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllSelectors, gatewayClient::getSelectorCache, adminClient);
        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllMetaData, gatewayClient::getMetaDataCache, adminClient);
        WaitDataSync.waitAdmin2GatewayDataSyncEquals(adminClient::listAllRules, gatewayClient::getRuleCache, adminClient);

        LOG.info("Starting logging RabbitMQ plugin");
        Map<String, String> requestBody = new HashMap<>();
        requestBody.put("pluginId", "45");
        requestBody.put("name", Plugin.LOGGINGRABBITMQ.getAlias());
        requestBody.put("enabled", "true");
        requestBody.put("role", "Logging");
        requestBody.put("sort", "171");
        requestBody.put("namespaceId", Constants.SYS_DEFAULT_NAMESPACE_NAMESPACE_ID);
        requestBody.put("config", "{\"host\":\"shenyu-rabbitmq\",\"port\":5672,\"username\":\"admin\",\"password\":\"admin\","
                + "\"exchangeName\":\"exchange.logging.plugin\",\"queueName\":\"queue.logging.plugin\","
                + "\"routingKey\":\"topic.logging\",\"virtualHost\":\"/\",\"exchangeType\":\"direct\","
                + "\"durable\":true,\"exclusive\":false,\"autoDelete\":false}");
        adminClient.changePluginStatus("1801816010882822182", requestBody);
        WaitDataSync.waitGatewayPluginUse(gatewayClient, "org.apache.shenyu.plugin.logging.rabbitmq.LoggingRabbitmqPlugin");
        waitForLogQueue();
    }

    private void waitForLogQueue() {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        factory.setPort(5672);
        factory.setUsername("admin");
        factory.setPassword("admin");
        factory.setVirtualHost("/");
        factory.setConnectionTimeout(CONNECTION_TIMEOUT_MILLIS);
        factory.setHandshakeTimeout(HANDSHAKE_TIMEOUT_MILLIS);
        await().alias("RabbitMQ logging queue initialized")
                .atMost(Duration.ofSeconds(30))
                .until(() -> {
                    try (Connection connection = factory.newConnection(); Channel channel = connection.createChannel()) {
                        channel.queueDeclarePassive("queue.logging.plugin");
                        return true;
                    } catch (Exception e) {
                        LOG.debug("Waiting for RabbitMQ logging queue", e);
                        return false;
                    }
                });
    }

    @ShenYuScenario(provider = DividePluginCases.class)
    void testDivide(final GatewayClient gateway, final CaseSpec spec) {
        spec.getVerifiers().forEach(verifier -> verifier.verify(gateway.getHttpRequesterSupplier().get()));
    }
}
