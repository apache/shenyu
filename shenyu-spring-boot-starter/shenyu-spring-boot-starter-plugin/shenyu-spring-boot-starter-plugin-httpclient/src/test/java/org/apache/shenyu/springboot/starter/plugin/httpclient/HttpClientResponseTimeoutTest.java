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

import io.netty.handler.timeout.ReadTimeoutException;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class HttpClientResponseTimeoutTest {

    @Test
    void appliesDefaultAndConfiguredResponseTimeouts() {
        HttpClientProperties properties = new HttpClientProperties();
        assertEquals(Duration.ofSeconds(3), createClient(properties).configuration().responseTimeout());
        properties.setResponseTimeout(10000L);
        assertEquals(Duration.ofSeconds(10), createClient(properties).configuration().responseTimeout());
        properties.setResponseTimeout(null);
        assertEquals(Duration.ofSeconds(3), createClient(properties).configuration().responseTimeout());
    }

    @ParameterizedTest
    @ValueSource(longs = {0, -1})
    void nonPositiveTimeoutDoesNotInstallReadDeadline(final long timeout) {
        HttpClientProperties properties = new HttpClientProperties();
        properties.setResponseTimeout(timeout);
        assertNull(createClient(properties).configuration().responseTimeout());
    }

    @Test
    void timesOutAnUnresponsiveServer() {
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> Mono.never()).bindNow();
        try {
            HttpClientProperties properties = new HttpClientProperties();
            properties.setResponseTimeout(100L);
            HttpClient client = createClient(properties);
            assertThrows(ReadTimeoutException.class, () -> client.get().uri("http://127.0.0.1:" + server.port())
                    .response().block(Duration.ofSeconds(5)));
        } finally {
            server.disposeNow();
        }
    }

    private HttpClient createClient(final HttpClientProperties properties) {
        properties.getPool().setType(HttpClientProperties.Pool.PoolType.DISABLED);
        return new HttpClientFactory(properties, null, new ServerProperties()).createInstance();
    }
}
