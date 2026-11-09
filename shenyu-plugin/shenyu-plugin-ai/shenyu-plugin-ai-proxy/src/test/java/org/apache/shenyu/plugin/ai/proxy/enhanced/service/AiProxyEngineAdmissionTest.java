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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmissionProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Orchestration tests for the opt-in concurrency admission: the tenant pool caps
 * concurrent invocations without touching the upstream, the streaming attempt
 * holds its target permit until the body ends or the request is cancelled, retry
 * re-acquires the target permit while the logical permit stays owned, and
 * rejections surface as 429.
 */
class AiProxyEngineAdmissionTest {

    private static final String BODY = "{\"model\":\"main-model\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}";

    private final AtomicInteger calls = new AtomicInteger();

    private AiConcurrencyAdmissionProperties admissionProperties() {
        AiConcurrencyAdmissionProperties properties = new AiConcurrencyAdmissionProperties();
        properties.setEnabled(true);
        properties.setTenantMaxInflight(1);
        properties.setTenantMaxWaiting(4);
        properties.setModelMaxInflight(1);
        properties.setModelMaxWaiting(4);
        properties.setMaxWaitMillis(50L);
        return properties;
    }

    private Mono<AiUpstreamResponse> okResponse(final String body) {
        return Mono.just(new AiUpstreamResponse(200, Map.of(), Flux.just(body.getBytes(StandardCharsets.UTF_8))));
    }

    private AiProxyTransport transport(final AtomicInteger upstreamCalls, final boolean holdStream) {
        return request -> {
            upstreamCalls.incrementAndGet();
            Flux<byte[]> payload = holdStream
                    ? Flux.just("{}".getBytes(StandardCharsets.UTF_8)).concatWith(Flux.never())
                    : Flux.just("{}".getBytes(StandardCharsets.UTF_8));
            return Mono.just(new AiUpstreamResponse(200, Map.of(), payload));
        };
    }

    private AiProxyEngine engine(final AiConcurrencyAdmission admission, final AiProxyTransport transport,
                                 final AiProxyRetryProperties retryProperties) {
        return new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("DeepSeek"))),
                protocolRouteResolver(), transport, retryProperties, null, event -> {
                }, admission);
    }

    private org.apache.shenyu.plugin.ai.common.config.AiCommonConfig config() {
        org.apache.shenyu.plugin.ai.common.config.AiCommonConfig config =
                new org.apache.shenyu.plugin.ai.common.config.AiCommonConfig();
        config.setProvider("OpenAI");
        config.setBaseUrl("https://main.example");
        config.setApiKey("main-key");
        config.setModel("main-model");
        config.setStream(false);
        return config;
    }

    private Function<AiUpstreamResponse, Mono<Void>> drainBody() {
        return response -> response.getBody().then();
    }

    private void awaitUpstreamCall(final AtomicInteger upstreamCalls) {
        await().atMost(5, TimeUnit.SECONDS).until(() -> upstreamCalls.get() == 1);
    }

    @Test
    void aFullTenantPoolRejectsWithoutAnyUpstreamCall() {
        AtomicInteger upstreamCalls = new AtomicInteger();
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(admissionProperties());
        AiProxyEngine engine = engine(admission, transport(upstreamCalls, true), new AiProxyRetryProperties(3, 1, Duration.ofSeconds(1)));

        // the first invocation holds the single tenant permit on an open stream
        final reactor.core.Disposable holder = engine.execute(BODY, config(), Optional.empty(), drainBody()).subscribe();
        awaitUpstreamCall(upstreamCalls);

        StepVerifier.create(engine.execute(BODY, config(), Optional.empty(), drainBody()))
                .verifyError(AiProxyException.class);
        assertEquals(1, upstreamCalls.get(), "an admission rejection must not open an upstream call");
        holder.dispose();
    }

    @Test
    void retryReacquiresTheTargetPermitAndTheTerminalReleaseRestoresBothPools() {
        AtomicInteger upstreamCalls = new AtomicInteger();
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(admissionProperties());
        AiProxyTransport transport = request -> {
            int call = upstreamCalls.incrementAndGet();
            if (call == 1) {
                return Mono.error(new org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException(
                        org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException.Kind.IO_FAILURE,
                        "transient", null));
            }
            return okResponse("{}");
        };
        AiProxyRetryProperties retryProperties = new AiProxyRetryProperties(3, 1, Duration.ofMillis(10));
        retryProperties.setJitterFraction(0d);
        AiProxyEngine engine = engine(admission, transport, retryProperties);

        StepVerifier.create(engine.execute(BODY, config(), Optional.empty(), drainBody())).verifyComplete();
        assertEquals(2, upstreamCalls.get());
        assertEquals(0, admission.inflightOf("model:main-model"), "the terminal release must restore the model pool");
        assertEquals(0, admission.inflightOf("tenant:anonymous"), "the logical release must restore the tenant pool");
    }

    @Test
    void theModelPoolCapsAttemptsIndependentlyOfTheTenantPools() {
        AtomicInteger upstreamCalls = new AtomicInteger();
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(admissionProperties());
        // only the holder's stream stays open; later invocations answer and complete
        AiProxyTransport transport = request -> {
            int call = upstreamCalls.incrementAndGet();
            Flux<byte[]> payload = call == 1
                    ? Flux.just("{}".getBytes(StandardCharsets.UTF_8)).concatWith(Flux.never())
                    : Flux.just("{}".getBytes(StandardCharsets.UTF_8));
            return Mono.just(new AiUpstreamResponse(200, Map.of(), payload));
        };
        AiProxyEngine engine = engine(admission, transport, new AiProxyRetryProperties(3, 1, Duration.ofSeconds(1)));

        // tenant t1 holds model:main-model on an open stream
        final reactor.core.Disposable holder = executeAs(engine, "t1", "main-model").subscribe();
        awaitUpstreamCall(upstreamCalls);

        // tenant t2 has a fresh tenant pool but shares the full model pool: rejected
        StepVerifier.create(executeAs(engine, "t2", "main-model"))
                .verifyError(AiProxyException.class);
        assertEquals(1, upstreamCalls.get());

        // the same t2 on a different model is an unaffected independent target: admitted
        StepVerifier.create(executeAs(engine, "t2", "other-model")).verifyComplete();
        assertEquals(2, upstreamCalls.get());
        holder.dispose();
    }

    private Mono<Void> executeAs(final AiProxyEngine engine, final String tenant, final String model) {
        org.apache.shenyu.plugin.ai.common.config.AiCommonConfig config = config();
        config.setModel(model);
        return engine.execute(BODY, config, Optional.empty(), drainBody())
                .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY,
                        new AgentRequestContext("req-" + tenant, "ai", "sel-admission", "rule-admission")
                                .withIdentity(tenant, "subject")));
    }

    @Test
    void cancellationReleasesTheHeldPermits() {
        AtomicInteger upstreamCalls = new AtomicInteger();
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(admissionProperties());
        AiProxyEngine engine = engine(admission, transport(upstreamCalls, true), new AiProxyRetryProperties(3, 1, Duration.ofSeconds(1)));

        reactor.core.Disposable disposable = engine.execute(BODY, config(), Optional.empty(), drainBody()).subscribe();
        awaitUpstreamCall(upstreamCalls);
        await().atMost(5, TimeUnit.SECONDS).until(() -> admission.inflightOf("tenant:anonymous") == 1);
        disposable.dispose();
        await().atMost(5, TimeUnit.SECONDS).until(() -> admission.inflightOf("tenant:anonymous") == 0);
        await().atMost(5, TimeUnit.SECONDS).until(() -> admission.inflightOf("model:main-model") == 0);
    }
}
