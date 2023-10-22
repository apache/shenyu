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

package org.apache.shenyue.e2e.testcase.websocket;

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
import org.apache.shenyu.e2e.model.response.SelectorDTO;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.shaded.com.google.common.collect.Lists;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

@ShenYuTest(
        mode = ShenYuEngineConfigure.Mode.HOST,
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
                        baseUrl = "http://{hostname:localhost}:9195",
                        type = ShenYuEngineConfigure.ServiceType.SHENYU_GATEWAY,
                        parameters = {
                                @ShenYuTest.Parameter(key = "dataSyn", value = "gateway_websocket")
                        }
                )
        },
        dockerComposeFile = "classpath:./docker-compose.mysql.yml"
)
/**
 * Testing websocket plugin.
 */
public class WebSocketPluginTest {

    private static final String WEBSOCKET_URI = "ws://localhost:9195/myWs?token=Jack";

    private static final Logger LOG = LoggerFactory.getLogger(WebSocketPluginTest.class);

    private List<String> selectorIds = Lists.newArrayList();

    @BeforeAll
    static void setup(final AdminClient adminClient, final GatewayClient gatewayClient) throws InterruptedException, JsonProcessingException {
        adminClient.login();
        adminClient.deleteAllSelectors();
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

    @ShenYuScenario(provider = WebSocketPluginCases.class)
    void testWebSocket(final GatewayClient gateway, final CaseSpec spec) throws InterruptedException, URISyntaxException {
        final String sendMessage = "Shenyu says hello to you!";
        ArrayBlockingQueue<String> blockingQueue = new ArrayBlockingQueue<>(1);
        WebSocketClient webSocketClient = new WebSocketClient(new URI(WEBSOCKET_URI)) {
            @Override
            public void onOpen(final ServerHandshake serverHandshake) {
            }

            @Override
            public void onMessage(final String s) {
                LOG.info("websocket client received message : {}", s);
                blockingQueue.add(s);
            }

            @Override
            public void onClose(final int i, final String s, final boolean b) {
            }

            @Override
            public void onError(final Exception e) {
                e.printStackTrace();
            }
        };
        webSocketClient.connectBlocking();
        webSocketClient.send(sendMessage);
        String receivedMessage = blockingQueue.poll(10, TimeUnit.SECONDS);
        assertThat(receivedMessage, is("server send message：" + sendMessage));
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
    }
}
