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

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.Executors;

import com.sun.net.httpserver.HttpServer;

/**
 * Benchmark upstream backend (issue #7649).
 *
 * <p>Single-file source program: {@code java BenchBackend.java [port] [threads]}.
 * Unlike the smoke-test stub this one runs a fixed thread pool large enough to
 * stay out of the way of the gateway under load: the benchmark must measure the
 * gateway, never the backend. Response payload is tiny and constant.
 */
public final class BenchBackend {

    private static final byte[] BODY =
            "{\"marker\":\"bench-backend-ok\"}".getBytes(StandardCharsets.UTF_8);

    private BenchBackend() {
    }

    /**
     * Starts the benchmark backend.
     *
     * @param args first arg listen port (default 18081), second arg pool size (default 256)
     * @throws IOException if the port cannot be bound
     */
    public static void main(final String[] args) throws IOException {
        final int port = args.length > 0 ? Integer.parseInt(args[0]) : 18081;
        final int threads = args.length > 1 ? Integer.parseInt(args[1]) : 256;
        // Deep accept backlog is load-bearing: the gateway's httpclient plugin
        // ships with shenyu.httpclient.keepAlive=false, so every proxied request
        // is a fresh TCP connection. With the OS-default backlog (~50) connect
        // bursts get aborted by the kernel and surface gateway-side as
        // PrematureCloseException / HTTP 500 rows in the errors column.
        final HttpServer server = HttpServer.create(new InetSocketAddress("0.0.0.0", port), 4096);
        server.createContext("/", exchange -> {
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, BODY.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(BODY);
            }
        });
        server.setExecutor(Executors.newFixedThreadPool(threads));
        server.start();
        System.out.println("bench backend listening on port " + port + " with " + threads + " threads");
    }
}
