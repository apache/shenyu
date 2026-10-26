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

package org.apache.shenyu.e2e.engine.function;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import io.restassured.builder.ResponseSpecBuilder;
import io.restassured.http.Method;
import org.apache.shenyu.e2e.engine.scenario.function.WaitForHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

import static io.restassured.RestAssured.given;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;

public class WaitForHelperTest {

    private final CountDownLatch delayedRequest = new CountDownLatch(1);

    private final CountDownLatch releaseResponse = new CountDownLatch(1);

    private HttpServer server;

    @BeforeEach
    void setUp() throws IOException {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/get", exchange -> writeJson(exchange, "{\"url\":\"" + exchange.getRequestURI() + "\"}"));
        server.createContext("/delay/2", exchange -> {
            delayedRequest.countDown();
            try {
                releaseResponse.await();
                writeJson(exchange, "{\"delay\":2}");
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
                exchange.close();
            }
        });
        server.start();
        // Initialize the HTTP client before measuring the helper's short timeout.
        given().baseUri(baseUri()).get("/get").then().statusCode(200);
    }

    @AfterEach
    void tearDown() {
        releaseResponse.countDown();
        server.stop(0);
    }

    @Test
    void testSuccess() throws TimeoutException {
        new WaitForHelper().waitFor(
                () -> given().baseUri(baseUri()).when(),
                Method.GET,
                "/get?foo1=bar1&foo2=bar2",
                new ResponseSpecBuilder().expectBody("url", containsString("/get")).build()
        );
    }

    @Test
    void testTimeout() throws InterruptedException {
        Assertions.assertThrows(TimeoutException.class, () ->
            new WaitForHelper(10, Duration.ofSeconds(1), Duration.ofMillis(500)).waitFor(
                    () -> given().baseUri(baseUri()).when(),
                    Method.GET,
                    "/delay/2",
                    new ResponseSpecBuilder().expectBody("delay", equalTo(2)).build()
            )
        );
        Assertions.assertTrue(delayedRequest.await(2, TimeUnit.SECONDS));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testRetryExhaustion(final boolean useChecker) {
        AtomicInteger attempts = new AtomicInteger();
        AssertionError failure = new AssertionError("configuration has not reached the gateway");
        WaitForHelper helper = new WaitForHelper(3, Duration.ZERO, Duration.ofSeconds(5));
        AssertionError thrown = Assertions.assertThrows(AssertionError.class, () -> {
            if (useChecker) {
                helper.waitFor(() -> given().baseUri(baseUri()), supplier -> {
                    attempts.incrementAndGet();
                    throw failure;
                });
            } else {
                helper.waitFor(() -> {
                    attempts.incrementAndGet();
                    throw failure;
                }, Method.GET, "/get", new ResponseSpecBuilder().build());
            }
        });
        Assertions.assertEquals(3, attempts.get());
        Assertions.assertSame(failure, useChecker ? thrown.getCause().getCause() : thrown.getCause());
    }

    @Test
    void testRetryThenSuccess() throws TimeoutException {
        AtomicInteger attempts = new AtomicInteger();
        new WaitForHelper(3, Duration.ZERO, Duration.ofSeconds(5)).waitFor(
                () -> given().baseUri(baseUri()), supplier -> {
                    if (attempts.incrementAndGet() == 1) {
                        throw new AssertionError("configuration has not reached the gateway");
                    }
                });
        Assertions.assertEquals(2, attempts.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testExecutionFailure(final boolean useChecker) {
        IllegalStateException failure = new IllegalStateException("request failed");
        AtomicInteger attempts = new AtomicInteger();
        WaitForHelper helper = new WaitForHelper(3, Duration.ZERO, Duration.ofSeconds(5));
        AssertionError thrown = Assertions.assertThrows(AssertionError.class, () -> {
            if (useChecker) {
                helper.waitFor(() -> given().baseUri(baseUri()), supplier -> {
                    attempts.incrementAndGet();
                    throw failure;
                });
            } else {
                helper.waitFor(() -> {
                    attempts.incrementAndGet();
                    throw failure;
                }, Method.GET, "/get", new ResponseSpecBuilder().build());
            }
        });
        Assertions.assertEquals(1, attempts.get());
        Assertions.assertSame(failure, thrown.getCause());
    }

    private String baseUri() {
        return "http://127.0.0.1:" + server.getAddress().getPort();
    }

    private static void writeJson(final HttpExchange exchange, final String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(200, bytes.length);
        try (OutputStream output = exchange.getResponseBody()) {
            output.write(bytes);
        } finally {
            exchange.close();
        }
    }
}
