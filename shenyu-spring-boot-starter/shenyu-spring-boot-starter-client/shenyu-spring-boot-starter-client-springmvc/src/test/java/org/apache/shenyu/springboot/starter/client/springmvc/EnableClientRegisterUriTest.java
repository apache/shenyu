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

package org.apache.shenyu.springboot.starter.client.springmvc;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.client.auto.config.EnableClientRegister;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.disruptor.subcriber.ShenyuClientURIExecutorSubscriber;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.core.register.matcher.ApiRegisterProcessor;
import org.apache.shenyu.client.core.shutdown.ShutdownHookManager;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Servlet application and real HTTP registration, with an isolated recording control-plane endpoint. */
public final class EnableClientRegisterUriTest {

    private final List<MetaDataRegisterDTO> metadata = new CopyOnWriteArrayList<>();

    private final List<URIRegisterDTO> uris = new CopyOnWriteArrayList<>();

    private final List<URIRegisterDTO> attempts = new CopyOnWriteArrayList<>();

    private final List<URIRegisterDTO> heartbeats = new CopyOnWriteArrayList<>();

    private final List<URIRegisterDTO> offline = new CopyOnWriteArrayList<>();

    private final AtomicBoolean failSecondNamespace = new AtomicBoolean();

    private final OwnedPublisher publisher = new OwnedPublisher();

    private MockedStatic<ShenyuClientRegisterEventPublisher> singleton;

    private HttpServer admin;

    private ConfigurableApplicationContext application;

    private Map<String, ShenyuClientRegisterRepository> previousRepositories;

    private Set<?> previousHooks;

    @BeforeEach
    public void startAdmin() throws Exception {
        previousRepositories = new HashMap<>(ShenyuClientRegisterRepositoryFactory.getRepositoryMap());
        previousHooks = new HashSet<>((Set<?>) Objects.requireNonNull(ReflectionTestUtils.getField(ShutdownHookManager.get(), "hooks")));
        admin = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        admin.createContext("/", this::handleRegistration);
        admin.start();
        singleton = Mockito.mockStatic(ShenyuClientRegisterEventPublisher.class);
        singleton.when(ShenyuClientRegisterEventPublisher::getInstance).thenReturn(publisher);
        ShenyuClientRegisterRepositoryFactory.getRepositoryMap().clear();
    }

    private void handleRegistration(final HttpExchange exchange) throws IOException {
        String path = exchange.getRequestURI().getPath();
        String body = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
        int status = 200;
        if (path.endsWith("register-metadata")) {
            metadata.add(GsonUtils.getGson().fromJson(body, MetaDataRegisterDTO.class));
        }
        if (path.endsWith("register-uri")) {
            URIRegisterDTO uri = GsonUtils.getGson().fromJson(body, URIRegisterDTO.class);
            if (EventType.HEARTBEAT.equals(uri.getEventType())) {
                heartbeats.add(uri);
            } else {
                attempts.add(uri);
                if ("two".equals(uri.getNamespaceId()) && failSecondNamespace.compareAndSet(true, false)) {
                    status = 503;
                } else {
                    uris.add(uri);
                }
            }
        }
        if (path.endsWith("offline")) {
            offline.add(GsonUtils.getGson().fromJson(body, URIRegisterDTO.class));
        }
        String result = "/platform/login".equals(path) ? "{\"code\":200,\"data\":{\"token\":\"fixture\"}}" : "success";
        byte[] response = (status == 200 ? result : "temporarily unavailable").getBytes(StandardCharsets.UTF_8);
        exchange.sendResponseHeaders(status, response.length);
        exchange.getResponseBody().write(response);
        exchange.close();
    }

    @ParameterizedTest
    @ValueSource(strings = {"http://", "https://"})
    public void testLocalRegistrationHasOneUriPerNamespace(final String protocol) throws Exception {
        if ("http://".equals(protocol)) {
            startApplication(null);
        } else {
            startApplication(null, "--shenyu.client.http.props.protocol=" + protocol);
        }
        await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> {
            assertEquals(4, metadata.size());
            assertEquals(2, uris.size());
        });
        assertFalse(application.containsBean("springHttpClientEventListener"));
        assertEquals(List.of("one", "one", "two", "two"), metadata.stream().map(MetaDataRegisterDTO::getNamespaceId).sorted().toList());
        assertEquals(List.of("one", "two"), uris.stream().map(URIRegisterDTO::getNamespaceId).sorted().toList());
        for (URIRegisterDTO uri : uris) {
            assertEquals("http", uri.getRpcType());
            assertEquals(protocol, uri.getProtocol());
            assertEquals("/client", uri.getContextPath());
            assertEquals("uri-fixture", uri.getAppName());
            assertEquals("127.0.0.1", uri.getHost());
            assertEquals(application.getEnvironment().getProperty("server.port", Integer.class), uri.getPort());
        }
        application.publishEvent(new ContextRefreshedEvent(application));
        await().during(Duration.ofMillis(300)).atMost(Duration.ofSeconds(2)).untilAsserted(() -> assertEquals(2, uris.size()));
    }

    @ParameterizedTest
    @ValueSource(strings = {"nacos", "LOCAL", ""})
    public void testRegistryDiscoveryKeepsMetadataWithoutLocalUris(final String mode) throws Exception {
        startApplication(mode);
        await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> assertEquals(4, metadata.size()));
        assertEquals(0, uris.size());
    }

    @ParameterizedTest
    @ValueSource(ints = {0, -1})
    public void testPendingAdvertisedPortKeepsApplicationBootable(final int port) throws Exception {
        startApplication("local", "--shenyu.client.http.props.port=" + port);
        await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> assertEquals(4, metadata.size()));
        assertEquals(0, attempts.size());
    }

    @Test
    public void testUriProcessorCanExcludeApisWithoutSuppressingMetadata() throws Exception {
        startApplication("local", "--fixture.exclude-uris=true");
        await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> assertEquals(4, metadata.size()));
        assertEquals(0, attempts.size());
    }

    @ParameterizedTest
    @ValueSource(strings = {"invalid", "65536"})
    public void testInvalidAdvertisedPortFailsStartupWithoutPublishingUri(final String port) {
        assertThrows(RuntimeException.class, () -> startApplication("local", "--shenyu.client.http.props.port=" + port));
        assertEquals(0, attempts.size());
    }

    @Test
    public void testPartialHttpFailureRetriesOnlyFailedIdentityAndKeepsLifecycleIdentity() throws Exception {
        failSecondNamespace.set(true);
        startApplication("local", "--shenyu.client.http.props.protocol=https://");
        await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> {
            assertEquals(2, attempts.size());
            assertEquals(1, uris.size());
        });
        await().atMost(Duration.ofSeconds(20)).untilAsserted(() -> {
            assertEquals(3, attempts.size());
            assertEquals(2, uris.size());
        });
        assertEquals(1, attempts.stream().filter(uri -> "one".equals(uri.getNamespaceId())).count());
        assertEquals(2, attempts.stream().filter(uri -> "two".equals(uri.getNamespaceId())).count());
        List<URIRegisterDTO> tracked = ((List<?>) Objects.requireNonNull(ReflectionTestUtils.getField(publisher.subscriber, "uris"))).stream()
                .map(URIRegisterDTO.class::cast).toList();
        assertEquals(2, tracked.size());
        for (URIRegisterDTO uri : tracked) {
            ReflectionTestUtils.invokeMethod(publisher.subscriber, "sendHeartbeat", uri);
            ReflectionTestUtils.invokeMethod(publisher.subscriber, "offlineAndShutdown", uri);
        }
        assertEquals(2, heartbeats.size());
        assertEquals(2, offline.size());
        for (int index = 0; index < uris.size(); index++) {
            assertEquals(identity(tracked.get(index)), identity(heartbeats.get(index)));
            assertEquals(identity(tracked.get(index)), identity(offline.get(index)));
            assertEquals(EventType.OFFLINE, offline.get(index).getEventType());
        }
    }

    private List<Object> identity(final URIRegisterDTO uri) {
        return List.of(uri.getNamespaceId(), uri.getRpcType(), uri.getAppName(), uri.getContextPath(), uri.getHost(), uri.getPort(), uri.getProtocol());
    }

    private void startApplication(final String discovery, final String... overrides) throws Exception {
        int port;
        try (ServerSocket reservation = new ServerSocket(0)) {
            port = reservation.getLocalPort();
        }
        SpringApplicationBuilder builder = new SpringApplicationBuilder(Fixture.class).properties(
                "server.address=127.0.0.1", "server.port=" + port,
                "spring.application.name=uri-fixture",
                "shenyu.register.registerType=http",
                "shenyu.register.serverLists=http://127.0.0.1:" + admin.getAddress().getPort(),
                "shenyu.register.props.username=fixture", "shenyu.register.props.password=fixture",
                "shenyu.client.http.props.host=127.0.0.1", "shenyu.client.http.props.port=" + port,
                "shenyu.client.http.props.appName=uri-fixture", "shenyu.client.http.props.contextPath=/client",
                "shenyu.namespace=one;two", "spring.main.banner-mode=off");
        if (Objects.nonNull(discovery)) {
            builder.properties("shenyu.discovery.type=" + discovery);
        }
        application = builder.run(overrides);
    }

    @AfterEach
    public void stop() throws InterruptedException {
        if (Objects.nonNull(application)) {
            application.close();
        }
        if (Objects.nonNull(publisher.subscriber)) {
            publisher.subscriber.shutdown();
        }
        if (Objects.nonNull(publisher.getProviderManage())) {
            publisher.getProviderManage().getProvider().shutdown();
            ExecutorService executor = (ExecutorService) Objects.requireNonNull(ReflectionTestUtils.getField(publisher.getProviderManage().getProvider(), "executor"));
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
        Set<?> hooks = (Set<?>) Objects.requireNonNull(ReflectionTestUtils.getField(ShutdownHookManager.get(), "hooks"));
        hooks.removeIf(hook -> !previousHooks.contains(hook));
        singleton.close();
        ShenyuClientRegisterRepositoryFactory.getRepositoryMap().clear();
        ShenyuClientRegisterRepositoryFactory.getRepositoryMap().putAll(previousRepositories);
        admin.stop(0);
    }

    @Configuration(proxyBeanMethods = false)
    @EnableAutoConfiguration(exclude = ErrorMvcAutoConfiguration.class)
    @EnableClientRegister
    @Import(ShenyuSpringMvcClientConfiguration.class)
    public static class Fixture {

        /**
         * Run this fixture against an isolated Admin and bootstrap using normal Spring properties.
         *
         * @param args application properties
         */
        public static void main(final String[] args) {
            SpringApplication.run(Fixture.class, args);
        }

        @Bean
        public Endpoints endpoints() {
            return new Endpoints();
        }

        @Bean
        public ApiRegisterProcessor uriFilter(final Environment environment) {
            return new ApiRegisterProcessor() {
                @Override
                public void process(final ApiBean apiBean) {
                    if (environment.getProperty("fixture.exclude-uris", Boolean.class, false)) {
                        apiBean.setStatus(ApiBean.Status.CAN_NO_BE_REGISTERED);
                    }
                }

                @Override
                public boolean match(final ApiBean apiBean) {
                    return true;
                }

                @Override
                public List<Class<?>> supportedRegisterDataType() {
                    return List.of(URIRegisterDTO.class);
                }
            };
        }
    }

    @RestController
    @RequestMapping(path = "/api")
    public static class Endpoints {

        @GetMapping(path = "/hello")
        public String hello() {
            return "hello";
        }

        @GetMapping(path = "/second")
        public String second() {
            return "second";
        }
    }

    private static final class OwnedPublisher extends ShenyuClientRegisterEventPublisher {

        private ShenyuClientURIExecutorSubscriber subscriber;

        @Override
        protected ShenyuClientURIExecutorSubscriber createUriSubscriber(final ShenyuClientRegisterRepository repository) {
            subscriber = super.createUriSubscriber(repository);
            return subscriber;
        }
    }
}
