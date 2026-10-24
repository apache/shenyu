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

import org.apache.shenyu.common.utils.InitialSyncApplication;
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.mock.env.MockEnvironment;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Startup publication includes asynchronous subscriber completion. */
class FileConfigurationLoaderTest {

    @TempDir
    private Path directory;

    private final PluginDataSubscriber subscriber = mock(PluginDataSubscriber.class);

    private final DiscoveryUpstreamDataSubscriber upstreamSubscriber = mock(DiscoveryUpstreamDataSubscriber.class);

    private MockEnvironment environment;

    @BeforeEach
    void setUp() throws IOException {
        Path file = directory.resolve("gateway.yaml");
        Files.writeString(file, FileConfigurationParserTest.document().toString());
        environment = new MockEnvironment().withProperty("shenyu.sync.file.path", file.toString());
    }

    private FileConfigurationLoader loader() {
        PluginDataHandler handler = mock(PluginDataHandler.class);
        when(handler.pluginNamed()).thenReturn("divide");
        PluginDataHandler context = mock(PluginDataHandler.class);
        when(context.pluginNamed()).thenReturn("contextPath");
        DiscoveryUpstreamDataHandler upstreamHandler = mock(DiscoveryUpstreamDataHandler.class);
        when(upstreamHandler.pluginName()).thenReturn("divide");
        return new FileConfigurationLoader(environment, "namespace", FileConfigurationParserTest.plugins(), List.of(handler, context), List.of(upstreamHandler), subscriber, upstreamSubscriber);
    }

    @Test
    void publishesInDependencyOrderAndCleansOnClose() {
        FileConfigurationLoader loader = loader();
        assertThat(loader.isReady()).isFalse();
        loader.afterSingletonsInstantiated();
        assertThat(loader.isReady()).isTrue();
        var order = inOrder(subscriber, upstreamSubscriber);
        order.verify(subscriber, times(6)).onSubscribe(any());
        order.verify(subscriber).onSelectorSubscribe(any());
        order.verify(subscriber).onRuleSubscribe(any());
        order.verify(upstreamSubscriber).onSubscribe(any());
        loader.close();
        assertThat(loader.isReady()).isFalse();
        order.verify(upstreamSubscriber).unSubscribe(any());
        order.verify(subscriber).unRuleSubscribe(any());
        order.verify(subscriber).unSelectorSubscribe(any());
        order.verify(subscriber, times(6)).unSubscribe(any());
    }

    @Test
    void rejectsWholeDocumentBeforeApplyingAnything() throws IOException {
        Files.writeString(Path.of(environment.getProperty("shenyu.sync.file.path")), FileConfigurationParserTest.document().put("version", 99).toString());
        FileConfigurationLoader loader = loader();
        assertThatThrownBy(loader::afterSingletonsInstantiated).hasMessageContaining("version");
        assertThat(loader.isReady()).isFalse();
        verifyNoInteractions(subscriber, upstreamSubscriber);
    }

    @Test
    void synchronousFailurePreventsReadinessAndHidesPayload() {
        doThrow(new IllegalStateException("super-secret")).when(subscriber).onRuleSubscribe(any());
        FileConfigurationLoader loader = loader();
        assertThatThrownBy(loader::afterSingletonsInstantiated).hasMessageContaining("rules").hasMessageNotContaining("super-secret").hasNoCause();
        assertThat(loader.isReady()).isFalse();
        assertThat(InitialSyncApplication.isActive()).isFalse();
    }

    @Test
    void asynchronousFailurePreventsReadiness() {
        doAnswer(invocation -> {
            InitialSyncApplication.register(CompletableFuture.failedFuture(new IllegalStateException("super-secret")));
            return null;
        }).when(upstreamSubscriber).onSubscribe(any());
        FileConfigurationLoader loader = loader();
        assertThatThrownBy(loader::afterSingletonsInstantiated).hasMessageContaining("asynchronous handlers").hasMessageNotContaining("super-secret").hasNoCause();
        assertThat(loader.isReady()).isFalse();
    }

    @Test
    void incompleteApplicationTimesOut() {
        CompletableFuture<Void> application = new CompletableFuture<>();
        doAnswer(invocation -> {
            InitialSyncApplication.register(application);
            return null;
        }).when(upstreamSubscriber).onSubscribe(any());
        environment.withProperty("shenyu.sync.file.apply-timeout", "1ms");
        FileConfigurationLoader loader = loader();
        assertThatThrownBy(loader::afterSingletonsInstantiated).hasMessageContaining("TimeoutException");
        assertThat(loader.isReady()).isFalse();
        application.complete(null);
        assertThat(loader.isReady()).isFalse();
    }

    @Test
    void missingLocationFailsBeforePublication() {
        environment.setProperty("shenyu.sync.file.path", "");
        assertThatThrownBy(loader()::afterSingletonsInstantiated).hasMessageContaining("requires shenyu.sync.file.path");
        verifyNoInteractions(subscriber, upstreamSubscriber);
    }

    @Test
    void waitsForSuccessfulAsynchronousApplicationBeforeReadiness() throws Exception {
        CompletableFuture<Void> pending = new CompletableFuture<>();
        CountDownLatch registered = new CountDownLatch(1);
        doAnswer(invocation -> {
            InitialSyncApplication.register(pending);
            registered.countDown();
            return null;
        }).when(upstreamSubscriber).onSubscribe(any());
        FileConfigurationLoader loader = loader();
        CompletableFuture<Void> startup = CompletableFuture.runAsync(loader::afterSingletonsInstantiated);
        assertThat(registered.await(5, TimeUnit.SECONDS)).isTrue();
        assertThat(startup.isDone()).isFalse();
        assertThat(loader.isReady()).isFalse();
        pending.complete(null);
        startup.get(5, TimeUnit.SECONDS);
        assertThat(loader.isReady()).isTrue();
        loader.close();
    }

    @Test
    void readinessGateRefusesTrafficUntilApplicationCompletes() {
        FileConfigurationLoader loader = mock(FileConfigurationLoader.class);
        WebFilterChain chain = mock(WebFilterChain.class);
        when(chain.filter(any())).thenReturn(Mono.empty());
        var filter = new FileSyncDataConfiguration().fileConfigurationReadinessFilter(loader);
        var exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/api/hello"));
        filter.filter(exchange, chain).block();
        assertThat(exchange.getResponse().getStatusCode()).isEqualTo(HttpStatus.SERVICE_UNAVAILABLE);
        verifyNoInteractions(chain);
        when(loader.isReady()).thenReturn(true);
        filter.filter(MockServerWebExchange.from(MockServerHttpRequest.get("/api/hello")), chain).block();
        verify(chain).filter(any());
    }

    @ParameterizedTest
    @ValueSource(strings = {"0ms", "-1s", "1ns", "6m", "9223372036854775807s", "not-a-duration"})
    void invalidApplicationTimeoutFailsBeforePublication(final String timeout) {
        environment.withProperty("shenyu.sync.file.apply-timeout", timeout);
        assertThatThrownBy(loader()::afterSingletonsInstantiated).hasMessageContaining("shenyu.sync.file.apply-timeout");
        verifyNoInteractions(subscriber, upstreamSubscriber);
    }
}
