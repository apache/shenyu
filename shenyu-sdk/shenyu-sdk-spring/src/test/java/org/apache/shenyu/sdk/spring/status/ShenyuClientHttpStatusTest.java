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

package org.apache.shenyu.sdk.spring.status;

import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.httpclient.HttpShenyuSdkClient;
import org.apache.shenyu.sdk.okhttp.OkHttpShenyuSdkClient;
import org.apache.shenyu.sdk.spring.EnableShenyuClients;
import org.apache.shenyu.sdk.spring.FallbackFactory;
import org.apache.shenyu.sdk.spring.NoFallbackAvailableException;
import org.apache.shenyu.sdk.spring.ShenyuClient;
import org.apache.shenyu.sdk.spring.support.SpringMvcContract;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.MapPropertySource;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.client.HttpClientErrorException;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.function.Consumer;
import java.util.function.Supplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ShenyuClientHttpStatusTest {

    private static final int UNLISTED_SUCCESS = 299;

    private static final int ORIGIN_TIMEOUT = 524;

    private static final int FALLBACK_RESULT = -1;

    @ParameterizedTest
    @EnumSource(Transport.class)
    void decodesUnlistedSuccessfulStatus(final Transport transport) throws Exception {
        withProxy(transport, context -> assertEquals(42, context.getBean(StatusClient.class).readValue()));
    }

    @ParameterizedTest
    @EnumSource(Transport.class)
    void preservesDtoTypesInGenericJsonResponses(final Transport transport) throws Exception {
        withProxy(transport, context -> {
            final StatusClient client = context.getBean(StatusClient.class);
            assertEquals(new Item("sample"), client.readItem());
            assertEquals(new Item("sample"), client.readItems().get(0));
            assertEquals(new Item("sample"), client.readItemGroups().get("group").get(0));
        });
    }

    @ParameterizedTest
    @EnumSource(Transport.class)
    void preservesStatusInFallbackFactoryCause(final Transport transport) throws Exception {
        withProxy(transport, context -> {
            assertEquals(FALLBACK_RESULT, context.getBean(FallbackStatusClient.class).readTimeout());
            assertOriginTimeout(context.getBean(StatusFallbackFactory.class).cause);
        });
    }

    @ParameterizedTest
    @EnumSource(Transport.class)
    void preservesCauseWithoutFallback(final Transport transport) throws Exception {
        withProxy(transport, context -> {
            final StatusClient client = context.getBean(StatusClient.class);
            final NoFallbackAvailableException failure = assertThrows(NoFallbackAvailableException.class, client::readTimeout);
            assertOriginTimeout(failure.getCause());
        });
    }

    @ParameterizedTest
    @EnumSource(Transport.class)
    void returnsRawResponseForUnlistedError(final Transport transport) throws Exception {
        withProxy(transport, context -> assertEquals(ORIGIN_TIMEOUT, context.getBean(StatusClient.class).rawTimeout().getStatus()));
    }

    private static void assertOriginTimeout(final Throwable cause) {
        final HttpClientErrorException failure = assertInstanceOf(HttpClientErrorException.class, cause);
        assertEquals(ORIGIN_TIMEOUT, failure.getStatusCode().value());
    }

    private static void withProxy(final Transport transport, final Consumer<AnnotationConfigApplicationContext> test) throws Exception {
        final HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        try (ShenyuSdkClient client = transport.create(); AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext()) {
            serve(server, "/value", UNLISTED_SUCCESS);
            serveBody(server, "/item", 200, "application/json", "{\"name\":\"sample\"}");
            serveBody(server, "/items", 200, "application/json", "[{\"name\":\"sample\"}]");
            serveBody(server, "/item-groups", 200, "application/json", "{\"group\":[{\"name\":\"sample\"}]}");
            serve(server, "/timeout", ORIGIN_TIMEOUT);
            server.start();
            final String address = "127.0.0.1:" + server.getAddress().getPort();

            final RegisterConfig config = new RegisterConfig();
            config.setServerLists(address);
            config.setProps(new Properties());
            client.init(config, Collections.emptyList(), null);

            context.getEnvironment().getPropertySources().addFirst(new MapPropertySource(
                    "sdk-status-test", Collections.singletonMap("sdk.status.test.url", "http://" + address)));
            context.register(ClientConfiguration.class, SpringMvcContract.class, StatusFallbackFactory.class);
            context.getBeanFactory().registerSingleton("shenyuSdkClient", client);
            context.refresh();
            test.accept(context);
        } finally {
            server.stop(0);
        }
    }

    private static void serve(final HttpServer server, final String path, final int status) {
        serveBody(server, path, status, "application/json", "42");
    }

    private static void serveBody(final HttpServer server, final String path, final int status, final String contentType, final String text) {
        final byte[] body = text.getBytes(StandardCharsets.UTF_8);
        server.createContext(path, exchange -> {
            try {
                exchange.getResponseHeaders().set("Content-Type", contentType);
                exchange.sendResponseHeaders(status, body.length);
                exchange.getResponseBody().write(body);
            } finally {
                exchange.close();
            }
        });
    }

    private enum Transport {
        APACHE(HttpShenyuSdkClient::new),
        OKHTTP(OkHttpShenyuSdkClient::new);

        private final Supplier<ShenyuSdkClient> factory;

        Transport(final Supplier<ShenyuSdkClient> factory) {
            this.factory = factory;
        }

        private ShenyuSdkClient create() {
            return factory.get();
        }
    }

    @Configuration(proxyBeanMethods = false)
    @EnableShenyuClients(basePackageClasses = StatusClient.class)
    static class ClientConfiguration {
    }

    @ShenyuClient(name = "status-client", url = "${sdk.status.test.url}")
    public interface StatusClient {

        @GetMapping("/value")
        Integer readValue();

        @GetMapping("/item")
        Item readItem();

        @GetMapping("/items")
        List<Item> readItems();

        @GetMapping("/item-groups")
        Map<String, List<Item>> readItemGroups();

        @GetMapping("/timeout")
        Integer readTimeout();

        @GetMapping("/timeout")
        ShenyuResponse rawTimeout();
    }

    public record Item(String name) {
    }

    @ShenyuClient(name = "fallback-status-client", url = "${sdk.status.test.url}", fallbackFactory = StatusFallbackFactory.class)
    public interface FallbackStatusClient {

        @GetMapping("/timeout")
        Integer readTimeout();
    }

    public static class StatusFallbackFactory implements FallbackFactory<FallbackStatusClient> {

        private Throwable cause;

        @Override
        public FallbackStatusClient create(final Throwable cause) {
            this.cause = cause;
            return () -> FALLBACK_RESULT;
        }
    }
}
