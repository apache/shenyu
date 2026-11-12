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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.apache.shenyu.plugin.httpclient.config.UpstreamPoolGroupRegistry;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;

import java.time.Duration;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies that an explicitly metrics-enabled group publishes Reactor Netty provider
 * meters into the global Micrometer registry with a bounded group name tag.
 */
public final class HttpClientPoolGroupMetricsTest {

    @Test
    public void groupMetricsAreRegisteredWithTheGroupName() {
        SimpleMeterRegistry meterRegistry = new SimpleMeterRegistry();
        Metrics.addRegistry(meterRegistry);
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0)
                .handle((request, response) -> response.sendString(Mono.just("ok"))).bindNow();
        UpstreamPoolGroupRegistry groups = null;
        try {
            final HttpClientProperties properties = new HttpClientProperties();
            HttpClientProperties.Pool group = new HttpClientProperties.Pool();
            group.setName("bulk");
            group.setMaxConnections(1);
            group.setMetrics(true);
            properties.setPoolGroups(List.of(group));

            groups = new HttpClientFactory(properties, null, new ServerProperties()).buildGroupRegistry();
            assertEquals("ok", groups.httpClientFor("bulk").get()
                    .uri("http://127.0.0.1:" + server.port() + "/")
                    .responseContent().aggregate().asString().block(Duration.ofSeconds(5)));

            assertFalse(meterRegistry.find("reactor.netty.connection.provider.active.connections").meters().isEmpty(),
                    "metrics: true must register the group provider meters");
            boolean tagged = meterRegistry.find("reactor.netty.connection.provider.active.connections").meters().stream()
                    .anyMatch(meter -> "bulk".equals(meter.getId().getTag("id"))
                            || "bulk".equals(meter.getId().getTag("name")));
            assertTrue(tagged, "the group provider meters must carry the configured group name");
        } finally {
            if (Objects.nonNull(groups)) {
                groups.close();
            }
            server.disposeNow(Duration.ofSeconds(5));
            Metrics.removeRegistry(meterRegistry);
        }
    }
}
