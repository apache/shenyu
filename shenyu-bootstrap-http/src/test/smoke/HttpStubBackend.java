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

import com.sun.net.httpserver.HttpServer;

/**
 * Stub upstream backend for the minimal gateway smoke test.
 *
 * <p>Single-file source program: run with {@code java HttpStubBackend.java [port]}.
 * Requires only a JDK (11+), no build step and no extra runtime. Every request is
 * answered with {@code 200 {"marker":"smoke-backend-ok","path":"<request path>"}} so
 * the smoke test can tell a proxied response apart from a gateway-generated one.
 */
public final class HttpStubBackend {

    private HttpStubBackend() {
    }

    /**
     * Starts the stub backend.
     *
     * @param args first arg is the listen port, default 18080
     * @throws IOException if the port cannot be bound
     */
    public static void main(final String[] args) throws IOException {
        final int port = args.length > 0 ? Integer.parseInt(args[0]) : 18080;
        final HttpServer server = HttpServer.create(new InetSocketAddress("0.0.0.0", port), 0);
        server.createContext("/", exchange -> {
            final byte[] body = ("{\"marker\":\"smoke-backend-ok\",\"path\":\""
                    + exchange.getRequestURI().getRawPath() + "\"}").getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(body);
            }
        });
        server.start();
        System.out.println("stub backend listening on port " + port);
    }
}
