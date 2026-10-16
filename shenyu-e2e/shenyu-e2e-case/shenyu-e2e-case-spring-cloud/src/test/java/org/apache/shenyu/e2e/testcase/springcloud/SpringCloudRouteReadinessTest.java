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

import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.junit.jupiter.api.Test;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertTrue;

class SpringCloudRouteReadinessTest {

    @Test
    void waitsForDiscoveredUpstreamAndExpectedResponse() throws Exception {
        AtomicInteger requests = new AtomicInteger();
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/springcloud/order/path/123/hahah", exchange -> {
            int attempt = requests.incrementAndGet();
            byte[] body = (attempt < 3 ? "{\"code\":-107,\"message\":\"upstream not ready\"}"
                    : "{\"id\":\"123\",\"name\":\"hahah\"}").getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.sendResponseHeaders(attempt == 1 ? 503 : 200, body.length);
            try (var output = exchange.getResponseBody()) {
                output.write(body);
            }
        });
        server.start();
        try {
            GatewayClient gateway = new GatewayClient("readiness-test", "gateway",
                    "http://127.0.0.1:" + server.getAddress().getPort(), new Properties());
            SpringCloudPluginTest.waitForRegisteredRoute(gateway);
            assertTrue(requests.get() >= 3);
        } finally {
            server.stop(0);
        }
    }
}
