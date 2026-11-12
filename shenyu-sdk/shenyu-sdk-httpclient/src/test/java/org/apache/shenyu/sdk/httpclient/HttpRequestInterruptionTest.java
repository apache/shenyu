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

package org.apache.shenyu.sdk.httpclient;

import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class HttpRequestInterruptionTest {

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    @Timeout(20)
    void interruptedRequestReleasesConnection(final boolean responseStarted) throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        ExecutorService serverExecutor = Executors.newCachedThreadPool();
        HttpShenyuSdkClient client = new HttpShenyuSdkClient();
        CountDownLatch requestReceived = new CountDownLatch(1);
        CountDownLatch releaseResponse = new CountDownLatch(1);
        CountDownLatch callerFinished = new CountDownLatch(1);
        AtomicReference<IOException> failure = new AtomicReference<>();
        AtomicBoolean interrupted = new AtomicBoolean();
        String url = "http://127.0.0.1:" + server.getAddress().getPort();
        Thread caller = new Thread(() -> {
            try {
                client.doRequest(request(url + "/slow"));
            } catch (IOException e) {
                failure.set(e);
                interrupted.set(Thread.currentThread().isInterrupted());
            } finally {
                callerFinished.countDown();
            }
        }, "interrupted-sdk-caller");
        try {
            server.setExecutor(serverExecutor);
            server.createContext("/slow", exchange -> {
                try {
                    if (responseStarted) {
                        exchange.sendResponseHeaders(200, 2);
                        exchange.getResponseBody().write('a');
                        exchange.getResponseBody().flush();
                    }
                    requestReceived.countDown();
                    releaseResponse.await(15, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    exchange.close();
                }
            });
            server.createContext("/fast", exchange -> {
                try {
                    exchange.sendResponseHeaders(204, -1);
                } finally {
                    exchange.close();
                }
            });
            server.start();
            Properties props = new Properties();
            props.setProperty("http.maxTotal", "1");
            props.setProperty("http.maxPerRoute", "1");
            props.setProperty("http.connectionRequestTimeOut", "1000");
            props.setProperty("http.serverRequestTimeOut", "10000");
            props.setProperty("http.serverResponseTimeOut", "5000");
            client.initClient(props);
            caller.start();
            assertTrue(requestReceived.await(5, TimeUnit.SECONDS), "The slow request must reach the server");

            caller.interrupt();

            assertTrue(callerFinished.await(5, TimeUnit.SECONDS), "The interrupted caller must stop waiting");
            assertNotNull(failure.get());
            assertTrue(failure.get().getCause() instanceof InterruptedException);
            assertTrue(interrupted.get(), "The caller's interrupt status must be preserved");
            assertEquals(1L, releaseResponse.getCount(), "The first response must still be held open");
            ShenyuResponse response = client.doRequest(request(url + "/fast"));
            assertEquals(204, response.getStatus(), "Cancellation must free the only connection for the next request");
        } finally {
            releaseResponse.countDown();
            caller.interrupt();
            try {
                client.close();
            } finally {
                server.stop(0);
                serverExecutor.shutdownNow();
                caller.join(5000);
                assertFalse(caller.isAlive(), "The caller thread must terminate");
                assertTrue(serverExecutor.awaitTermination(5, TimeUnit.SECONDS));
            }
        }
    }

    private ShenyuRequest request(final String url) {
        return ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, url, Collections.emptyMap(), null, null, null);
    }
}
