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

package org.apache.shenyu.springboot.starter.sync.data.file;

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.utils.InitialSyncApplication;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.web.filter.LocalDispatcherFilter;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.availability.ApplicationAvailability;
import org.springframework.boot.availability.ReadinessState;
import org.springframework.boot.web.reactive.context.ReactiveWebServerApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

import java.io.IOException;
import java.net.ServerSocket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/** Real Boot startup with stock bootstrap settings and real HTTP upstream forwarding. */
class FileModeIntegrationTest {

    @TempDir
    private Path directory;

    private boolean failApplication;

    private ConfigurableApplicationContext start(final Path path, final String... additional) {
        String[] args = new String[additional.length + 7];
        args[0] = "--spring.config.location=classpath:bootstrap-defaults/application.yml";
        args[1] = "--shenyu.sync.file.enabled=true";
        args[2] = "--shenyu.sync.file.path=" + path;
        args[3] = "--server.port=0";
        args[4] = "--spring.main.banner-mode=off";
        args[5] = "--shenyu.sync.websocket.initial-sync-readiness=true";
        args[6] = "--management.endpoint.health.probes.enabled=true";
        System.arraycopy(additional, 0, args, 7, additional.length);
        ArrayList<String> arguments = new ArrayList<>(Arrays.asList(args));
        if (additional.length > 0) {
            arguments.remove("--server.port=0");
        }
        SpringApplication application = new SpringApplication(TestApplication.class);
        if (failApplication) {
            application.addInitializers(context -> context.getBeanFactory().addBeanPostProcessor(new BeanPostProcessor() {
                @Override
                public Object postProcessAfterInitialization(final Object bean, final String name) {
                    if (bean instanceof DividePluginDataHandler) {
                        return new DividePluginDataHandler() {
                            @Override
                            public void handlerRule(final RuleData rule) {
                                throw new IllegalStateException("sensitive-handler-payload");
                            }
                        };
                    }
                    return bean;
                }
            }));
        }
        return application.run(arguments.toArray(new String[0]));
    }

    @Test
    void forwardsAndRestartsWithoutAdminOrMutableLocalApis() throws IOException {
        AtomicInteger upstreamCalls = new AtomicInteger();
        LoopResources upstreamLoops = LoopResources.create("file-test-upstream", 1, true);
        DisposableServer upstream = HttpServer.create().runOn(upstreamLoops).host("127.0.0.1").port(0).handle((request, response) -> {
            upstreamCalls.incrementAndGet();
            return response.sendString(Mono.just("forwarded:" + request.uri()));
        }).bindNow();
        Path path = directory.resolve("gateway.yaml");
        ObjectNode document = FileConfigurationParserTest.document();
        ((ObjectNode) document.path("upstreams").get(0).path("targets").get(0)).put("url", "http://127.0.0.1:" + upstream.port());
        Files.writeString(path, document.toString());
        try {
            for (int restart = 0; restart < 2; restart++) {
                try (ConfigurableApplicationContext context = start(path)) {
                    assertThat(context.getBean(FileConfigurationLoader.class).isReady()).isTrue();
                    assertThat(context.getBean(ApplicationAvailability.class).getReadinessState()).isEqualTo(ReadinessState.ACCEPTING_TRAFFIC);
                    assertThat(context.getBeansOfType(SyncDataService.class)).isEmpty();
                    assertThat(context.getBean(ShenyuConfig.class).getExtPlugin().getEnabled()).isFalse();
                    assertThat(context.containsBean("heartbeatListener")).isFalse();
                    assertThat(context.containsBean("websocketSyncHealthIndicator")).isFalse();
                    assertThat(context.getBeansOfType(LocalDispatcherFilter.class)).isEmpty();
                    assertThat(context.getBean(ShenyuWebHandler.class).getPlugins()).extracting(plugin -> plugin.named())
                            .containsExactly("global", "divide", "uri", "nettyHttpClient", "response");
                    assertThat(BaseDataCache.getInstance().obtainSelectorData("divide")).hasSize(1).first().extracting(selector -> selector.getId()).isEqualTo("http-api");
                    assertThat(BaseDataCache.getInstance().obtainRuleData("http-api")).hasSize(1).first().extracting(rule -> rule.getId()).isEqualTo("http-api-route");
                    int port = ((ReactiveWebServerApplicationContext) context).getWebServer().getPort();
                    WebClient client = WebClient.create("http://127.0.0.1:" + port);
                    assertThat(client.get().uri("/api/hello?value=1").retrieve().bodyToMono(String.class).block(Duration.ofSeconds(10))).isEqualTo("forwarded:/api/hello?value=1");
                    assertThat(client.get().uri("/unmatched").exchangeToMono(response -> response.bodyToMono(String.class)).block(Duration.ofSeconds(10))).contains("-107");
                    assertThat(client.post().uri("/shenyu/plugin/rule/saveOrUpdate").bodyValue("{}")
                            .exchangeToMono(response -> response.bodyToMono(String.class)).block(Duration.ofSeconds(10))).contains("-107");
                    assertThat(upstreamCalls.get()).isEqualTo(restart + 1);
                    assertThat(BaseDataCache.getInstance().obtainRuleData("http-api")).hasSize(1);
                }
                assertThat(BaseDataCache.getInstance().obtainSelectorData("divide")).isNullOrEmpty();
                assertThat(BaseDataCache.getInstance().obtainRuleData("http-api")).isNullOrEmpty();
            }
        } finally {
            upstream.disposeNow();
            upstreamLoops.disposeLater().block(Duration.ofSeconds(10));
        }
    }

    @Test
    void invalidConfigurationCannotStartListening() throws IOException {
        Path path = directory.resolve("invalid.yaml");
        Files.writeString(path, "version: 99");
        int port;
        try (ServerSocket reservation = new ServerSocket(0)) {
            port = reservation.getLocalPort();
        }
        assertThatThrownBy(() -> start(path, "--server.port=" + port)).hasMessageContaining("version");
        try (ServerSocket available = new ServerSocket(port)) {
            assertThat(available.isBound()).isTrue();
        }
        assertThat(InitialSyncApplication.isActive()).isFalse();
    }

    @Test
    void handlerFailureAbortsBootAndRemovesPartialConfiguration() throws IOException {
        Path path = directory.resolve("gateway.yaml");
        Files.writeString(path, FileConfigurationParserTest.document().toString());
        failApplication = true;
        assertThatThrownBy(() -> start(path)).hasMessageContaining("application failed at rules").hasMessageNotContaining("sensitive-handler-payload").hasNoCause();
        assertThat(BaseDataCache.getInstance().obtainSelectorData("divide")).isNullOrEmpty();
        assertThat(BaseDataCache.getInstance().obtainRuleData("http-api")).isNullOrEmpty();
    }

    @SpringBootConfiguration
    @EnableAutoConfiguration
    static class TestApplication {
    }
}
