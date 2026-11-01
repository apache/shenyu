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

package org.apache.shenyu.web.handler;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.failure.GatewayFailure;
import org.apache.shenyu.plugin.api.failure.GatewayFailureMapper;
import org.apache.shenyu.plugin.api.failure.GatewayFailureMapping;
import org.apache.shenyu.plugin.api.failure.GatewayFailurePresentation;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.alert.AlarmService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.handler.ExceptionHandlingWebHandler;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CancellationException;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

class GatewayFailureMappingTest {

    private ApplicationContext previous;

    private GenericApplicationContext context;

    @BeforeEach
    void setUp() {
        previous = SpringBeanUtils.getInstance().getApplicationContext();
        context = new GenericApplicationContext();
        context.registerBean(ShenyuConfig.class, ShenyuConfig::new);
        context.registerBean("deploymentResult", ShenyuResult.class, DefaultShenyuResult::new);
        context.registerBean(AlarmService.class, () -> org.mockito.Mockito.mock(AlarmService.class));
    }

    @AfterEach
    void clean() {
        SpringBeanUtils.getInstance().setApplicationContext(previous);
        context.close();
    }

    @Test
    void legacyGlobalAndDirectMissResponsesKeepTheirStatusBodyAndContentType() {
        start(List.of());
        MockServerWebExchange global = exchange();
        new GlobalErrorHandler().handle(global, new IllegalArgumentException("legacy public detail")).block();
        assertEquals(HttpStatus.BAD_REQUEST, global.getResponse().getStatusCode());
        assertTrue(global.getResponse().getBodyAsString().block().contains("legacy public detail"));
        assertEquals(MediaType.APPLICATION_JSON, global.getResponse().getHeaders().getContentType());
        for (boolean selector : List.of(true, false)) {
            MockServerWebExchange direct = exchange();
            (selector ? WebFluxResultUtils.noSelectorResult("divide", direct) : WebFluxResultUtils.noRuleResult("divide", direct)).block();
            assertNull(direct.getResponse().getStatusCode(), "legacy misses do not assign a new HTTP status");
            assertTrue(direct.getResponse().getBodyAsString().block().contains("divide:"));
            assertEquals(MediaType.APPLICATION_JSON, direct.getResponse().getHeaders().getContentType());
        }
    }

    @Test
    void oneTypedPolicyServesActualGlobalHandlerAndBothDirectOwnersWithCustomResultBean() {
        List<GatewayFailure> facts = new ArrayList<>();
        context.removeBeanDefinition("deploymentResult");
        context.registerBean("deploymentResult", ShenyuResult.class, () -> new ShenyuResult<Object>() {
            @Override
            public Object error(final int code, final String message, final Object value) {
                return Map.of("deployment", true, "code", code, "message", message);
            }
        });
        start(List.of(failure -> {
            facts.add(failure);
            return Optional.of(new GatewayFailurePresentation(422, 8100, "Safe rejection", false));
        }));
        WebTestClient global = WebTestClient.bindToWebHandler(new ExceptionHandlingWebHandler(
                exchange -> Mono.error(new IllegalArgumentException("SECRET-CAUSE")), List.of(new GlobalErrorHandler()))).build();
        global.get().uri("/failure").exchange().expectStatus().isEqualTo(422)
                .expectBody().jsonPath("$.deployment").isEqualTo(true).jsonPath("$.message").isEqualTo("Safe rejection");
        for (boolean selector : List.of(true, false)) {
            WebTestClient direct = WebTestClient.bindToWebHandler(exchange -> selector
                    ? WebFluxResultUtils.noSelectorResult("divide", exchange) : WebFluxResultUtils.noRuleResult("divide", exchange)).build();
            direct.get().uri("/miss").exchange().expectStatus().isEqualTo(422).expectBody().jsonPath("$.code").isEqualTo(8100);
        }
        assertEquals(List.of(GatewayFailure.Stage.GLOBAL_HANDLER, GatewayFailure.Stage.SELECTOR_MATCH, GatewayFailure.Stage.RULE_MATCH),
                facts.stream().map(GatewayFailure::stage).toList());
        assertTrue(facts.stream().noneMatch(value -> value.toString().contains("SECRET")));
    }

    @Test
    void orderingNoMatchAndInvalidOrThrowingPoliciesAreDeterministicAndNonrecursive() {
        AtomicInteger calls = new AtomicInteger();
        GatewayFailure facts = WebFluxResultUtils.failure(exchange(), GatewayFailure.Reason.RULE_MISS, GatewayFailure.Stage.RULE_MATCH, "divide");
        GatewayFailureMapper last = value -> {
            calls.incrementAndGet();
            return Optional.of(new GatewayFailurePresentation(409, 9, "Last", false));
        };
        assertEquals(409, new GatewayFailureMapping(List.of(value -> Optional.empty(), last)).select(facts).orElseThrow().status());
        GatewayFailureMapper first = new GatewayFailureMapper() {
            @Override
            public Optional<GatewayFailurePresentation> map(final GatewayFailure value) {
                return Optional.of(new GatewayFailurePresentation(410, 10, "First", true));
            }

            @Override
            public int getOrder() {
                return -10;
            }
        };
        assertEquals(410, new GatewayFailureMapping(List.of(last, first)).select(facts).orElseThrow().status());
        for (GatewayFailureMapper invalid : List.<GatewayFailureMapper>of(value -> null,
                value -> Optional.of(new GatewayFailurePresentation(200, 0, "not an error", false)),
                value -> {
                    throw new IllegalStateException("SECRET-POLICY");
                })) {
            assertTrue(new GatewayFailureMapping(List.of(invalid, last)).select(facts).isEmpty());
        }
        assertEquals(1, calls.get());
        assertThrows(IllegalArgumentException.class, () -> new GatewayFailureMapping(java.util.Collections.nCopies(33, last)));
    }

    @Test
    void throwingPolicyFallsBackToActualLegacyWriteAndEmptyBodyIsAnExplicitDecision() {
        start(List.of(value -> {
            throw new IllegalStateException("SECRET-POLICY");
        }));
        MockServerWebExchange failed = exchange();
        new GlobalErrorHandler().handle(failed, new IllegalStateException("SECRET-CAUSE")).block();
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, failed.getResponse().getStatusCode());
        assertTrue(failed.getResponse().getBodyAsString().block().contains("Internal Server Error"));
        assertTrue(!failed.getResponse().getBodyAsString().block().contains("SECRET"));
        MockServerWebExchange empty = exchange();
        var facts = WebFluxResultUtils.failure(empty, GatewayFailure.Reason.AUTHORIZATION, GatewayFailure.Stage.GLOBAL_HANDLER, null);
        assertTrue(new GatewayFailureMapping(List.of(value -> Optional.of(new GatewayFailurePresentation(403, 403, "", true)))).select(facts).orElseThrow().emptyBody());
    }

    @Test
    void selectedEmptyBodyUsesTheExistingResponseOwnerOnce() {
        start(List.of(value -> Optional.of(new GatewayFailurePresentation(403, 403, "", true))));
        MockServerWebExchange exchange = exchange();
        WebFluxResultUtils.noRuleResult("divide", exchange).block();
        assertEquals(HttpStatus.FORBIDDEN, exchange.getResponse().getStatusCode());
        assertEquals("", exchange.getResponse().getBodyAsString().block());
    }

    @Test
    void deploymentResultCanIntentionallySuppressErrorBodies() {
        context.removeBeanDefinition("deploymentResult");
        context.registerBean("deploymentResult", ShenyuResult.class, () -> new ShenyuResult<Object>() { });
        start(List.of());
        MockServerWebExchange direct = exchange();
        WebFluxResultUtils.noRuleResult("divide", direct).block();
        assertNull(direct.getResponse().getStatusCode());
        assertNull(direct.getResponse().getHeaders().getContentType());
        assertEquals("", direct.getResponse().getBodyAsString().block());
        MockServerWebExchange global = exchange();
        new GlobalErrorHandler().handle(global, new IllegalArgumentException("legacy")).block();
        assertEquals(HttpStatus.BAD_REQUEST, global.getResponse().getStatusCode());
        assertNull(global.getResponse().getHeaders().getContentType());
        assertEquals("", global.getResponse().getBodyAsString().block());
    }

    @ParameterizedTest
    @MethodSource("classifiedFailures")
    void boundedReasonsDoNotDependOnRequestInput(final Throwable error, final GatewayFailure.Reason reason) {
        start(List.of());
        MockServerWebExchange exchange = exchange();
        Mono<Void> terminal = new GlobalErrorHandler().handle(exchange, error);
        if (reason == GatewayFailure.Reason.CANCELLATION) {
            StepVerifier.create(terminal).expectErrorMatches(value -> value == error).verify();
            assertNull(exchange.getResponse().getStatusCode());
        } else {
            terminal.block();
        }
        GatewayFailure facts = exchange.getAttribute(GatewayFailureMapping.FACTS_ATTRIBUTE);
        assertEquals(reason, facts.reason());
    }

    @Test
    void committedSseAndAlreadyOwnedOutputAreNeverMappedOrWrittenAgain() {
        AtomicInteger mappings = new AtomicInteger();
        start(List.of(value -> {
            mappings.incrementAndGet();
            return Optional.of(new GatewayFailurePresentation(500, 500, "must not append", false));
        }));
        MockServerWebExchange sse = exchange();
        byte[] event = "data: original\n\n".getBytes(StandardCharsets.UTF_8);
        sse.getResponse().getHeaders().setContentType(MediaType.TEXT_EVENT_STREAM);
        sse.getResponse().writeWith(Mono.just(sse.getResponse().bufferFactory().wrap(event))).block();
        IllegalStateException original = new IllegalStateException("stream failure");
        StepVerifier.create(new GlobalErrorHandler().handle(sse, original)).expectErrorMatches(value -> value == original).verify();
        assertEquals("data: original\n\n", sse.getResponse().getBodyAsString().block());
        MockServerWebExchange started = exchange();
        started.getAttributes().put(WebFluxResultUtils.OUTPUT_STARTED_ATTRIBUTE, Boolean.TRUE);
        StepVerifier.create(new GlobalErrorHandler().handle(started, original)).expectErrorMatches(value -> value == original).verify();
        assertNull(started.getResponse().getStatusCode());
        assertEquals(0, mappings.get());
    }

    private void start(final List<GatewayFailureMapper> mappers) {
        context.registerBean(GatewayFailureMapping.class, () -> new GatewayFailureMapping(mappers));
        context.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    private MockServerWebExchange exchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.get("/test").header("X-Reason", "untrusted").build());
    }

    private static Stream<org.junit.jupiter.params.provider.Arguments> classifiedFailures() {
        return Stream.of(
                org.junit.jupiter.params.provider.Arguments.of(new CancellationException(), GatewayFailure.Reason.CANCELLATION),
                org.junit.jupiter.params.provider.Arguments.of(new TimeoutException(), GatewayFailure.Reason.DEADLINE),
                org.junit.jupiter.params.provider.Arguments.of(new ResponseStatusException(HttpStatus.UNAUTHORIZED), GatewayFailure.Reason.AUTHENTICATION),
                org.junit.jupiter.params.provider.Arguments.of(new ResponseStatusException(HttpStatus.FORBIDDEN), GatewayFailure.Reason.AUTHORIZATION),
                org.junit.jupiter.params.provider.Arguments.of(new ResponseStatusException(HttpStatus.TOO_MANY_REQUESTS), GatewayFailure.Reason.RESOURCE_LIMIT),
                org.junit.jupiter.params.provider.Arguments.of(new ResponseStatusException(HttpStatus.BAD_GATEWAY), GatewayFailure.Reason.UPSTREAM),
                org.junit.jupiter.params.provider.Arguments.of(new IllegalArgumentException(), GatewayFailure.Reason.INVALID_REQUEST),
                org.junit.jupiter.params.provider.Arguments.of(new IllegalStateException(), GatewayFailure.Reason.UNKNOWN));
    }
}
