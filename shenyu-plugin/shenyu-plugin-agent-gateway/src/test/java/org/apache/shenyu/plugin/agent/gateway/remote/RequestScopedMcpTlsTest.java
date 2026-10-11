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

package org.apache.shenyu.plugin.agent.gateway.remote;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpsServer;
import com.sun.net.httpserver.HttpsConfigurator;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManagerFactory;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyStore;
import java.time.Duration;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

class RequestScopedMcpTlsTest {

    @TempDir
    private static Path certificates;

    private static SSLContext validContext;

    private static SSLContext wrongIpContext;

    @BeforeAll
    static void createPrivateTestCertificates() throws Exception {
        validContext = certificate("valid", "SAN=IP:127.0.0.1");
        wrongIpContext = certificate("wrong", "SAN=IP:127.0.0.2");
        try (Server server = new Server(validContext)) {
            HttpClient warmup = HttpClient.newBuilder().sslContext(validContext).connectTimeout(Duration.ofSeconds(30)).build();
            HttpRequest ready = HttpRequest.newBuilder(URI.create("https://127.0.0.1:" + server.server.getAddress().getPort() + "/ready"))
                    .timeout(Duration.ofSeconds(30)).GET().build();
            assertEquals(204, warmup.send(ready, HttpResponse.BodyHandlers.discarding()).statusCode());
        }
    }

    @Test
    void acceptsTrustedChainWithMatchingIpSan() throws Exception {
        try (Server server = new Server(validContext)) {
            RequestScopedMcpClient client = server.client(validContext);
            assertEquals("2025-06-18", client.initialize().block(Duration.ofSeconds(5)).protocolVersion());
            client.closeGracefully().block(Duration.ofSeconds(5));
        }
    }

    @Test
    void rejectsUntrustedCertificateWithDefaultTrust() throws Exception {
        try (Server server = new Server(validContext)) {
            RequestScopedMcpClient client = server.client(null);
            assertThrows(RuntimeException.class, () -> client.initialize().block(Duration.ofSeconds(5)));
            client.closeGracefully().block(Duration.ofSeconds(5));
        }
    }

    @Test
    void rejectsWrongIpDespiteTrustedCertificate() throws Exception {
        try (Server server = new Server(wrongIpContext)) {
            RequestScopedMcpClient client = server.client(wrongIpContext);
            assertThrows(RuntimeException.class, () -> client.initialize().block(Duration.ofSeconds(5)));
            client.closeGracefully().block(Duration.ofSeconds(5));
        }
    }

    private static SSLContext certificate(final String name, final String extension) throws Exception {
        Path file = certificates.resolve(name + ".p12");
        String executable = Path.of(System.getProperty("java.home"), "bin", System.getProperty("os.name").startsWith("Windows") ? "keytool.exe" : "keytool").toString();
        Process process = new ProcessBuilder(executable, "-genkeypair", "-alias", "fixture", "-keyalg", "RSA", "-keysize", "2048", "-validity", "2",
                "-dname", "CN=fixture", "-ext", extension, "-storetype", "PKCS12", "-keystore", file.toString(), "-storepass", "changeit", "-noprompt")
                .redirectErrorStream(true).redirectOutput(certificates.resolve(name + "-keytool.log").toFile()).start();
        assertTrue(process.waitFor(45, TimeUnit.SECONDS));
        assertEquals(0, process.exitValue());
        KeyStore store = KeyStore.getInstance("PKCS12");
        try (var input = Files.newInputStream(file)) {
            store.load(input, "changeit".toCharArray());
        }
        KeyManagerFactory keys = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        keys.init(store, "changeit".toCharArray());
        TrustManagerFactory trust = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
        trust.init(store);
        SSLContext context = SSLContext.getInstance("TLS");
        context.init(keys.getKeyManagers(), trust.getTrustManagers(), null);
        return context;
    }

    private static final class Server implements AutoCloseable {

        private final HttpsServer server;

        private Server(final SSLContext context) throws Exception {
            server = HttpsServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.setHttpsConfigurator(new HttpsConfigurator(context));
            server.createContext("/ready", exchange -> {
                exchange.sendResponseHeaders(204, -1);
                exchange.close();
            });
            server.createContext("/mcp", exchange -> {
                try {
                    if ("DELETE".equals(exchange.getRequestMethod())) {
                        exchange.sendResponseHeaders(204, -1);
                        return;
                    }
                    ObjectMapper json = new ObjectMapper();
                    var request = json.readTree(exchange.getRequestBody());
                    if ("notifications/initialized".equals(request.path("method").textValue())) {
                        exchange.sendResponseHeaders(202, -1);
                        return;
                    }
                    var result = json.createObjectNode().put("protocolVersion", "2025-06-18");
                    result.putObject("capabilities").putObject("tools");
                    result.putObject("serverInfo").put("name", "tls-fixture").put("version", "1");
                    var envelope = json.createObjectNode().put("jsonrpc", "2.0");
                    envelope.set("id", request.get("id"));
                    envelope.set("result", result);
                    byte[] bytes = envelope.toString().getBytes(StandardCharsets.UTF_8);
                    exchange.getResponseHeaders().set("Content-Type", "application/json");
                    exchange.getResponseHeaders().set("MCP-Session-Id", "tls-session");
                    exchange.sendResponseHeaders(200, bytes.length);
                    exchange.getResponseBody().write(bytes);
                } finally {
                    exchange.close();
                }
            });
            server.start();
        }

        private RequestScopedMcpClient client(final SSLContext trustedContext) {
            URI endpoint = URI.create("https://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
            RemoteServerBinding.Config config = new RemoteServerBinding.Config("orders", endpoint, "service/orders", "v1");
            RemoteServerBinding binding = RemoteServerBinding.resolve(config, Set.of(endpoint), ignored -> new RemoteServerBinding.Credential(config, "fake-tls-token"));
            return new RequestScopedMcpClient(binding, ignored -> { }, trustedContext);
        }

        @Override
        public void close() {
            server.stop(0);
        }
    }
}
