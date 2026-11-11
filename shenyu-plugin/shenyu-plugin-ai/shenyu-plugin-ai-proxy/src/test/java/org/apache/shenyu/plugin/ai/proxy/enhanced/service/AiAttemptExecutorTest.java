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
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiResponseAdapter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProtocolResponseInspector;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptEvent;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.MonoSink;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiAttemptExecutorTest {

    @Test
    void waitsForResponseHandlerBeforeFinishingObservation() {
        List<AiAttemptEvent> attempts = new ArrayList<>();
        AiExecutionState execution = execution(attempts);
        AiAttemptExecutor executor = executor();
        AtomicReference<MonoSink<Void>> completion = new AtomicReference<>();

        StepVerifier.create(executor.execute(execution, prepared(), 0, AiProxyAttemptEvent.Phase.PRIMARY,
                        (response, observation) -> Mono.create(completion::set)))
                .then(() -> {
                    assertNotNull(completion.get());
                    assertTrue(attempts.isEmpty());
                    completion.get().success();
                })
                .verifyComplete();

        assertEquals(1, attempts.size());
        assertEquals(AiCallOutcome.SUCCESS, attempts.get(0).outcome());
    }

    @Test
    void finishesObservationWhenResponseHandlerFails() {
        List<AiAttemptEvent> attempts = new ArrayList<>();
        RuntimeException failure = new RuntimeException("fixture");

        StepVerifier.create(executor().execute(execution(attempts), prepared(), 0,
                        AiProxyAttemptEvent.Phase.PRIMARY, (response, observation) -> Mono.error(failure)))
                .expectErrorMatches(error -> error == failure)
                .verify();

        assertEquals(1, attempts.size());
        assertEquals(AiCallOutcome.ERROR, attempts.get(0).outcome());
    }

    @Test
    void cancellationReachesResponseHandlerAndFinishesObservation() {
        List<AiAttemptEvent> attempts = new ArrayList<>();
        AtomicBoolean cancelled = new AtomicBoolean();

        StepVerifier.create(executor().execute(execution(attempts), prepared(), 0,
                        AiProxyAttemptEvent.Phase.PRIMARY,
                        (response, observation) -> Mono.<Void>never().doOnCancel(() -> cancelled.set(true))))
                .thenCancel()
                .verify();

        assertTrue(cancelled.get());
        assertEquals(1, attempts.size());
        assertEquals(AiCallOutcome.CANCELLED, attempts.get(0).outcome());
    }

    private static AiAttemptExecutor executor() {
        return new AiAttemptExecutor(request -> Mono.just(
                new AiUpstreamResponse(200, Map.of(), Flux.empty())));
    }

    private static AiExecutionState execution(final List<AiAttemptEvent> attempts) {
        AgentRequestContext context = new AgentRequestContext("request", "llm", "selector", "rule");
        AiCallObserver observer = new AiCallObserver() {
            @Override
            public void record(final AiTerminalEvent event) {
            }

            @Override
            public void recordAttempt(final AiAttemptEvent event) {
                attempts.add(event);
            }
        };
        return new AiExecutionState("{}", response -> Mono.empty(), new AtomicBoolean(),
                new AtomicInteger(), "invocation", System.nanoTime() + Duration.ofSeconds(5).toNanos(),
                new AiRequestObservation(observer, context));
    }

    private static AiPreparedExchange prepared() {
        AiUpstreamRequest request = new AiUpstreamRequest();
        request.setBody("{\"model\":\"test-model\"}".getBytes(StandardCharsets.UTF_8));
        return new AiPreparedExchange(request, AiResponseAdapter.identity(), AiProtocolResponseInspector.identity(),
                "openai-chat", "openai-chat", false, true, ShenyuAiRequest.Operation.CHAT_COMPLETION,
                true, "openai", false);
    }
}
