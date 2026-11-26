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

package org.apache.shenyu.admin.scale.collector;

import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.admin.config.properties.DeploymentProperties;
import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.apache.shenyu.admin.scale.config.PrometheusProperties;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests the Prometheus query request at the HTTP boundary.
 */
class PrometheusMetricsProviderTest {

    private static final String METRIC = "query_metric";

    @Test
    void shouldSendSimpleQuery() throws IOException {
        URI uri = executeQuery("", "up");
        assertEquals("/api/v1/query", uri.getRawPath());
        assertEquals("query=up", uri.getRawQuery());
    }

    @Test
    void shouldPreservePlusOperatorInQuery() throws IOException {
        URI uri = executeQuery("", "sum(up)+sum(up)");
        assertEquals("query=sum(up)+sum(up)",
                URLDecoder.decode(uri.getRawQuery(), StandardCharsets.UTF_8));
    }

    @Test
    void shouldKeepAmpersandInsideQueryParameter() throws IOException {
        URI uri = executeQuery("", "up{namespace=\"%s\",pod=\"%s\",job=\"a&b\"}");
        assertEquals(1, uri.getRawQuery().split("&").length);
        assertEquals("query=up{namespace=\"test-ns\",pod=\"test-pod\",job=\"a&b\"}",
                URLDecoder.decode(uri.getRawQuery(), StandardCharsets.UTF_8));
    }

    @Test
    void shouldKeepPrometheusBasePath() throws IOException {
        URI uri = executeQuery("/prometheus/", "up");
        assertEquals("/prometheus/api/v1/query", uri.getRawPath());
    }

    private URI executeQuery(final String basePath, final String query) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        AtomicReference<URI> capturedUri = new AtomicReference<>();
        byte[] body = "{\"status\":\"success\",\"data\":{\"result\":[{\"value\":[1,\"2.5\"]}]}}".getBytes(StandardCharsets.UTF_8);
        server.createContext("/", exchange -> {
            capturedUri.set(exchange.getRequestURI());
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream response = exchange.getResponseBody()) {
                response.write(body);
            }
        });
        server.start();
        try {
            PrometheusProperties properties = new PrometheusProperties();
            properties.setUrl("http://127.0.0.1:" + server.getAddress().getPort() + basePath);
            properties.setQueries(Map.of(METRIC, query));
            DeploymentProperties deployment = new DeploymentProperties();
            deployment.setNamespace("test-ns");
            deployment.setName("test-pod");
            PrometheusMetricsProvider provider = new PrometheusMetricsProvider(properties, deployment);
            assertEquals(new MetricData(METRIC, 2.5D), provider.getMetricData(METRIC));
            return capturedUri.get();
        } finally {
            server.stop(0);
        }
    }
}
