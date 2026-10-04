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


package org.apache.shenyu.plugin.agent.gateway.protocol;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.netty.buffer.PooledByteBufAllocator;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * HTTP transport limits, cancellation and trusted identity boundaries.
 */
class AgentMcpHttpHandlerTest {

    private final AgentTrafficContext traffic = new AgentTrafficContext("server-request", "mcp", "selector", "rule");

    @Test
    void shouldWriteOneJsonResultUsingTrustedIdentityAndContext() {
        AtomicReference<AgentMcpExecutionContext> captured = new AtomicReference<>();
        AtomicReference<String> subject = new AtomicReference<>();
        AtomicReference<AgentToolInvocation> invocation = new AtomicReference<>();
        final Instant started = Instant.now();
        AgentToolProvider tool = provider(input -> Mono.deferContextual(context -> {
            captured.set(context.get(AgentMcpExecutionContext.class));
            subject.set(input.getSubject());
            invocation.set(input);
            return Mono.just(input.getArguments());
        }));
        MockServerWebExchange exchange = exchange("tools/call", body("tools/call"));
        execute(handler(tool), exchange, "{}");
        assertEquals(200, exchange.getResponse().getStatusCode().value());
        assertEquals("trusted-owner", subject.get());
        assertEquals("server-request", captured.get().getRequestId());
        assertEquals(7, captured.get().getConfigurationVersion());
        assertEquals("rule", invocation.get().getRuleId());
        assertEquals(7, invocation.get().getConfigurationVersion());
        assertEquals(captured.get().getDeadline(), invocation.get().getDeadline());
        assertTrue(invocation.get().getDeadline().isAfter(started));
        assertTrue(invocation.get().getDeadline().isBefore(started.plusSeconds(31)));
        assertEquals("no-store", exchange.getResponse().getHeaders().getCacheControl());
        assertFalse(exchange.getResponse().getHeaders().containsKey("Mcp-Session-Id"));
        String result = exchange.getResponse().getBodyAsString().block();
        assertTrue(result.contains("\"id\":\"client-id\""));
        assertTrue(result.contains("\"isError\":false"));
    }

    @Test
    void shouldWriteExactlyOneCompleteSseFrame() {
        MockServerWebExchange exchange = exchange("tools/list", body("tools/list"));
        execute(handler(), exchange, "{\"responseMode\":\"sse\"}");
        String result = exchange.getResponse().getBodyAsString().block();
        assertTrue(result.startsWith("event: message\ndata: {"));
        assertTrue(result.endsWith("}\n\n"));
        assertEquals(1, result.split("event: message", -1).length - 1);
        assertEquals(MediaType.TEXT_EVENT_STREAM, exchange.getResponse().getHeaders().getContentType());
        assertEquals("no", exchange.getResponse().getHeaders().getFirst("X-Accel-Buffering"));
        assertEquals(-1, exchange.getResponse().getHeaders().getContentLength());
    }

    @ParameterizedTest
    @CsvSource({
        "GET,405", "DELETE,405", "POST,200"
    })
    void shouldSupportOnlyPost(final String method, final int status) {
        MockServerWebExchange exchange = MockServerWebExchange.from(builder(HttpMethod.valueOf(method), "server/discover").body(body("server/discover")));
        execute(handler(), exchange, "{}");
        assertEquals(status, exchange.getResponse().getStatusCode().value());
        if (status == 405) {
            assertEquals("POST", exchange.getResponse().getHeaders().getFirst(HttpHeaders.ALLOW));
        }
    }

    @Test
    void shouldRejectOriginBeforeReadingBodyOrResolvingIdentity() {
        AtomicBoolean read = new AtomicBoolean();
        AtomicBoolean resolved = new AtomicBoolean();
        MockServerWebExchange exchange = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/list")
                .header(HttpHeaders.ORIGIN, "https://untrusted.example")
                .body(Flux.defer(() -> {
                    read.set(true);
                    return Flux.empty();
                })));
        AgentMcpHttpHandler handler = handler(next -> {
            resolved.set(true);
            return Mono.just(new AgentMcpIdentity("owner", Set.of()));
        });
        execute(handler, exchange, "{}");
        assertEquals(403, exchange.getResponse().getStatusCode().value());
        assertFalse(read.get());
        assertFalse(resolved.get());
        assertFalse(exchange.getResponse().getBodyAsString().block().contains("jsonrpc"));
    }

    @Test
    void shouldAcceptExactOriginAndRejectDuplicateOrigins() {
        MockServerWebExchange accepted = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/list")
                .header(HttpHeaders.ORIGIN, "https://example.com").body(body("tools/list")));
        execute(handler(), accepted, "{\"allowedOrigins\":[\"https://example.com\"]}");
        assertEquals(200, accepted.getResponse().getStatusCode().value());
        MockServerWebExchange duplicate = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/list")
                .header(HttpHeaders.ORIGIN, "https://example.com", "https://example.com").body(body("tools/list")));
        execute(handler(), duplicate, "{\"allowedOrigins\":[\"https://example.com\"]}");
        assertEquals(403, duplicate.getResponse().getStatusCode().value());
    }

    @ParameterizedTest
    @CsvSource({
        "Content-Type,text/plain,415",
        "Content-Type,application/json;charset=ISO-8859-1,415",
        "Accept,application/json,406",
        "Accept,*/*,406",
        "Accept,text/event-stream;q=0,406",
        "Content-Length,broken,400",
        "Content-Length,-2,400",
        "Content-Length,1025,413"
    })
    void shouldRejectInvalidTransportHeaders(final String name, final String value, final int status) {
        MockServerWebExchange exchange = exchange("tools/list", body("tools/list"));
        ServerWebExchange changed = exchange.mutate().request(exchange.getRequest().mutate().headers(headers -> headers.set(name, value)).build()).build();
        execute(handler(), changed, "{\"maxRequestBytes\":1024}");
        assertEquals(status, changed.getResponse().getStatusCode().value());
    }

    @ParameterizedTest
    @CsvSource(value = {
        "json|application/json, text/event-stream|200",
        "sse|application/json, text/event-stream|200",
        "json|text/event-stream;q=0.5, application/json;q=0.8|200",
        "sse|text/event-stream;q=0.5, application/json;q=0.8|200",
        "json|APPLICATION/JSON, TEXT/EVENT-STREAM|200",
        "sse|APPLICATION/JSON, TEXT/EVENT-STREAM|200",
        "json|application/json|406",
        "sse|application/json|406",
        "json|text/event-stream|406",
        "sse|text/event-stream|406",
        "json|MISSING|406",
        "sse|MISSING|406",
        "json|*/*|406",
        "sse|*/*|406",
        "json|application/*, text/*|406",
        "sse|application/*, text/*|406",
        "json|application/json;q=0, text/event-stream|406",
        "sse|application/json;q=0, text/event-stream|406",
        "json|application/json, text/event-stream;q=0|406",
        "sse|application/json, text/event-stream;q=0|406",
        "json|application/json;q=0, text/event-stream;q=0|406",
        "sse|application/json;q=0, text/event-stream;q=0|406",
        "json|broken|400",
        "sse|broken|400"
    }, delimiter = '|')
    void shouldRequireBothAcceptTypesRegardlessOfResponseMode(final String mode, final String accept, final int status) {
        AtomicBoolean read = new AtomicBoolean();
        AtomicBoolean resolved = new AtomicBoolean();
        AtomicInteger invoked = new AtomicInteger();
        AgentToolProvider tool = provider(input -> {
            invoked.incrementAndGet();
            return Mono.just(input.getArguments());
        });
        AgentMcpHttpHandler handler = handler(next -> {
            resolved.set(true);
            return Mono.just(new AgentMcpIdentity("owner", Set.of("read")));
        }, tool);
        MockServerWebExchange original = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/call")
                .body(Flux.defer(() -> {
                    read.set(true);
                    return Flux.just(new DefaultDataBufferFactory().wrap(body("tools/call").getBytes(StandardCharsets.UTF_8)));
                })));
        ServerWebExchange exchange = original.mutate().request(original.getRequest().mutate().headers(headers -> {
            if ("MISSING".equals(accept)) {
                headers.remove(HttpHeaders.ACCEPT);
            } else {
                headers.set(HttpHeaders.ACCEPT, accept);
            }
        }).build()).build();
        execute(handler, exchange, "{\"responseMode\":\"" + mode + "\"}");
        assertEquals(status, exchange.getResponse().getStatusCode().value());
        assertEquals(status == 200, read.get());
        assertEquals(status == 200, resolved.get());
        assertEquals(status == 200 ? 1 : 0, invoked.get());
        if (status == 200) {
            assertEquals("sse".equals(mode) ? MediaType.TEXT_EVENT_STREAM : MediaType.APPLICATION_JSON,
                    exchange.getResponse().getHeaders().getContentType());
            assertTrue(original.getResponse().getBodyAsString().block().contains("\"isError\":false"));
        } else {
            verify(tool, never()).validate(org.mockito.ArgumentMatchers.any());
            assertEquals(MediaType.APPLICATION_JSON, exchange.getResponse().getHeaders().getContentType());
            if (status == 406) {
                assertTrue(original.getResponse().getBodyAsString().block().contains("Both JSON and SSE must be accepted"));
            }
        }
    }

    @ParameterizedTest
    @CsvSource({"json", "sse"})
    void shouldAcceptBothTypesAcrossMultipleHeaderValues(final String mode) {
        MockServerWebExchange original = exchange("tools/list", body("tools/list"));
        ServerWebExchange exchange = original.mutate().request(original.getRequest().mutate()
                .headers(headers -> headers.put(HttpHeaders.ACCEPT, List.of("application/json", "text/event-stream"))).build()).build();
        execute(handler(), exchange, "{\"responseMode\":\"" + mode + "\"}");
        assertEquals(200, exchange.getResponse().getStatusCode().value());
        assertEquals("sse".equals(mode) ? MediaType.TEXT_EVENT_STREAM : MediaType.APPLICATION_JSON,
                exchange.getResponse().getHeaders().getContentType());
    }

    @Test
    void shouldRejectDuplicateContentTypeAndMalformedAccept() {
        MockServerWebExchange duplicate = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/list")
                .header(HttpHeaders.CONTENT_TYPE, "application/json").body(body("tools/list")));
        execute(handler(), duplicate, "{}");
        assertEquals(400, duplicate.getResponse().getStatusCode().value());
        MockServerWebExchange original = exchange("tools/list", body("tools/list"));
        ServerWebExchange malformed = original.mutate().request(original.getRequest().mutate()
                .headers(headers -> headers.set(HttpHeaders.ACCEPT, "broken")).build()).build();
        execute(handler(), malformed, "{}");
        assertEquals(400, malformed.getResponse().getStatusCode().value());
    }

    @Test
    void shouldDenyMissingIdentityDespiteForgedHeadersAndMetadata() {
        AtomicBoolean read = new AtomicBoolean();
        MockServerWebExchange exchange = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/list")
                .header("X-Subject", "administrator").header("Mcp-Session-Id", "trusted")
                .body(Flux.defer(() -> {
                    read.set(true);
                    return Flux.empty();
                })));
        execute(handler(next -> Mono.empty()), exchange, "{}");
        assertEquals(401, exchange.getResponse().getStatusCode().value());
        assertFalse(read.get());
    }

    @Test
    void shouldIntersectRuleToolsWithSecurityGrants() {
        MockServerWebExchange exchange = exchange("tools/list", body("tools/list"));
        execute(handler(next -> Mono.just(new AgentMcpIdentity("owner", Set.of())), provider(input -> Mono.just(new JsonObject()))), exchange, "{}");
        assertEquals(200, exchange.getResponse().getStatusCode().value());
        assertTrue(exchange.getResponse().getBodyAsString().block().contains("\"tools\":[]"));
    }

    @Test
    void shouldPreserveProtocolErrorAndNeverInvokeInvalidRequest() {
        AtomicInteger calls = new AtomicInteger();
        AgentToolProvider tool = provider(input -> {
            calls.incrementAndGet();
            return Mono.just(new JsonObject());
        });
        MockServerWebExchange exchange = exchange("tools/call", "broken-json");
        execute(handler(tool), exchange, "{}");
        assertEquals(400, exchange.getResponse().getStatusCode().value());
        assertTrue(exchange.getResponse().getBodyAsString().block().contains("-32700"));
        assertEquals(0, calls.get());
    }

    @Test
    void shouldReleasePooledBodyOnNormalCompletionAndOverflow() {
        NettyDataBuffer normal = pooled(body("tools/list"));
        MockServerWebExchange exchange = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/list").body(Flux.just(normal)));
        execute(handler(), exchange, "{}");
        assertEquals(0, normal.getNativeBuffer().refCnt());
        NettyDataBuffer overflow = pooled("x".repeat(1025));
        MockServerWebExchange rejected = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/list").body(Flux.just(overflow)));
        execute(handler(), rejected, "{\"maxRequestBytes\":1024}");
        assertEquals(413, rejected.getResponse().getStatusCode().value());
        assertEquals(0, overflow.getNativeBuffer().refCnt());
    }

    @Test
    void shouldReleaseAccumulatedBodyOnCancellation() {
        AtomicBoolean read = new AtomicBoolean();
        NettyDataBuffer partial = pooled("part");
        MockServerWebExchange exchange = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/list")
                .body(Flux.concat(Flux.just(partial).doOnNext(buffer -> read.set(true)), Flux.never())));
        StepVerifier.create(handler().handle(exchange, config("{}"), traffic, 7)).then(() -> assertTrue(read.get())).thenCancel().verify();
        assertEquals(0, partial.getNativeBuffer().refCnt());
    }

    @Test
    void shouldAcceptExactBodyLimitAndRejectChunkedOverflow() {
        String text = body("tools/list");
        String padded = text + " ".repeat(1024 - text.getBytes(StandardCharsets.UTF_8).length);
        MockServerWebExchange exact = exchange("tools/list", padded);
        execute(handler(), exact, "{\"maxRequestBytes\":1024}");
        assertEquals(200, exact.getResponse().getStatusCode().value());
        NettyDataBuffer first = pooled(padded);
        NettyDataBuffer extra = pooled(" ");
        MockServerWebExchange chunked = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/list").body(Flux.just(first, extra)));
        execute(handler(), chunked, "{\"maxRequestBytes\":1024}");
        assertEquals(413, chunked.getResponse().getStatusCode().value());
        assertEquals(0, first.getNativeBuffer().refCnt());
        assertEquals(0, extra.getNativeBuffer().refCnt());
    }

    @Test
    void shouldLimitUtf8OutputBeforeCommittingAndNeverRetryTool() {
        AtomicInteger calls = new AtomicInteger();
        AgentToolProvider tool = provider(input -> {
            calls.incrementAndGet();
            JsonObject result = new JsonObject();
            result.addProperty("text", "汉".repeat(600));
            return Mono.just(result);
        });
        MockServerWebExchange exchange = exchange("tools/call", body("tools/call"));
        execute(handler(tool), exchange, "{\"maxResponseBytes\":1024,\"responseMode\":\"sse\"}");
        assertEquals(500, exchange.getResponse().getStatusCode().value());
        assertEquals(MediaType.APPLICATION_JSON, exchange.getResponse().getHeaders().getContentType());
        assertFalse(exchange.getResponse().getHeaders().containsKey("X-Accel-Buffering"));
        assertTrue(exchange.getResponse().getBodyAsString().block().getBytes(StandardCharsets.UTF_8).length <= 1024);
        assertEquals(1, calls.get());
    }

    @Test
    void shouldCancelProviderOnAbsoluteDeadline() {
        AtomicBoolean cancelled = new AtomicBoolean();
        AgentMcpHttpHandler handler = handler(provider(input -> Mono.<JsonObject>never().doOnCancel(() -> cancelled.set(true))));
        MockServerWebExchange exchange = exchange("tools/call", body("tools/call"));
        StepVerifier.withVirtualTime(() -> handler.handle(exchange, config("{\"timeoutMs\":100}"), traffic, 7))
                .expectSubscription().thenAwait(Duration.ofMillis(100)).verifyComplete();
        assertTrue(cancelled.get());
        assertEquals(504, exchange.getResponse().getStatusCode().value());
    }

    @Test
    void shouldApplyOneDeadlineAcrossIdentityAndProvider() {
        AtomicBoolean cancelled = new AtomicBoolean();
        AgentMcpHttpHandler handler = handler(next -> Mono.delay(Duration.ofMillis(60)).map(tick -> new AgentMcpIdentity("owner", Set.of("read"))),
                provider(input -> Mono.delay(Duration.ofMillis(60)).map(tick -> new JsonObject()).doOnCancel(() -> cancelled.set(true))));
        MockServerWebExchange exchange = exchange("tools/call", body("tools/call"));
        StepVerifier.withVirtualTime(() -> handler.handle(exchange, config("{\"timeoutMs\":100}"), traffic, 7))
                .expectSubscription().thenAwait(Duration.ofMillis(100)).verifyComplete();
        assertEquals(504, exchange.getResponse().getStatusCode().value());
        assertTrue(cancelled.get());
    }

    @Test
    void shouldDeadlineBodyReadingAndReleasePartialBuffer() {
        NettyDataBuffer partial = pooled("part");
        MockServerWebExchange exchange = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/list").body(Flux.concat(Flux.just(partial), Flux.never())));
        StepVerifier.withVirtualTime(() -> handler().handle(exchange, config("{\"timeoutMs\":100}"), traffic, 7))
                .expectSubscription().thenAwait(Duration.ofMillis(100)).verifyComplete();
        assertEquals(504, exchange.getResponse().getStatusCode().value());
        assertEquals(0, partial.getNativeBuffer().refCnt());
    }

    @Test
    void shouldBoundStalledWritingAndNeverWriteSecondResponseAfterCommit() {
        AtomicInteger writes = new AtomicInteger();
        AtomicBoolean cancelled = new AtomicBoolean();
        MockServerWebExchange exchange = exchange("tools/list", body("tools/list"));
        exchange.getResponse().setWriteHandler(buffers -> {
            writes.incrementAndGet();
            return buffers.doOnNext(DataBufferUtils::release).then(Mono.<Void>never()).doOnCancel(() -> cancelled.set(true));
        });
        StepVerifier.withVirtualTime(() -> handler().handle(exchange, config("{\"timeoutMs\":100}"), traffic, 7))
                .expectSubscription().thenAwait(Duration.ofMillis(100)).expectError(TimeoutException.class).verify();
        assertTrue(exchange.getResponse().isCommitted());
        assertTrue(cancelled.get());
        assertEquals(1, writes.get());
    }

    @Test
    void shouldCancelOnlyOneHttpCallWithTheSameClientId() {
        AtomicBoolean started = new AtomicBoolean();
        AtomicBoolean cancelled = new AtomicBoolean();
        Sinks.One<JsonObject> survivor = Sinks.one();
        AgentToolProvider tool = provider(input -> "A".equals(input.getSubject())
                ? Mono.<JsonObject>never().doOnSubscribe(subscription -> started.set(true)).doOnCancel(() -> cancelled.set(true)) : survivor.asMono());
        AgentMcpHttpHandler handler = handler(next -> Mono.just(new AgentMcpIdentity(next.getRequest().getHeaders().getFirst("Test-Verified-Identity"), Set.of("read"))), tool);
        MockServerWebExchange first = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/call").header("Test-Verified-Identity", "A").body(body("tools/call")));
        MockServerWebExchange second = MockServerWebExchange.from(builder(HttpMethod.POST, "tools/call").header("Test-Verified-Identity", "B").body(body("tools/call")));
        StepVerifier.create(handler.handle(second, config("{}"), traffic, 7)).then(() -> {
            StepVerifier.create(handler.handle(first, config("{}"), traffic, 7)).then(() -> assertTrue(started.get())).thenCancel().verify();
            assertTrue(cancelled.get());
            assertEquals(Sinks.EmitResult.OK, survivor.tryEmitValue(new JsonObject()));
        }).verifyComplete();
        assertEquals(200, second.getResponse().getStatusCode().value());
    }

    @ParameterizedTest
    @CsvSource({"json", "sse"})
    void shouldRejectMissingCapabilitiesWithHttp400BeforeToolWork(final String mode) {
        AgentToolProvider tool = provider(input -> Mono.just(input.getArguments()));
        when(tool.getRequiredClientCapabilities()).thenReturn(JsonParser.parseString("{\"sampling\":{}}").getAsJsonObject());
        MockServerWebExchange exchange = exchange("tools/call", body("tools/call"));
        execute(handler(tool), exchange, "{\"responseMode\":\"" + mode + "\"}");
        assertEquals(400, exchange.getResponse().getStatusCode().value());
        assertEquals(MediaType.APPLICATION_JSON, exchange.getResponse().getHeaders().getContentType());
        JsonObject error = JsonParser.parseString(exchange.getResponse().getBodyAsString().block()).getAsJsonObject();
        assertEquals("client-id", error.get("id").getAsString());
        assertEquals(-32021, error.getAsJsonObject("error").get("code").getAsInt());
        assertTrue(error.getAsJsonObject("error").getAsJsonObject("data").getAsJsonObject("requiredCapabilities").get("sampling").isJsonObject());
        verify(tool, never()).validate(org.mockito.ArgumentMatchers.any());
        verify(tool, never()).invoke(org.mockito.ArgumentMatchers.any());
    }

    private NettyDataBuffer pooled(final String text) {
        NettyDataBuffer buffer = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT).allocateBuffer();
        buffer.write(text.getBytes(StandardCharsets.UTF_8));
        return buffer;
    }

    private void execute(final AgentMcpHttpHandler handler, final ServerWebExchange exchange, final String settings) {
        StepVerifier.create(handler.handle(exchange, config(settings), traffic, 7)).verifyComplete();
    }

    private AgentGatewayMcpConfig config(final String settings) {
        JsonObject object = JsonParser.parseString(settings).getAsJsonObject();
        object.add("allowedTools", JsonParser.parseString("[\"read\"]"));
        return AgentGatewayMcpConfig.parse(object, true);
    }

    private AgentMcpHttpHandler handler(final AgentToolProvider... tools) {
        return handler(exchange -> Mono.just(new AgentMcpIdentity("trusted-owner", Set.of("read"))), tools);
    }

    private AgentMcpHttpHandler handler(final AgentMcpSecurityResolver security, final AgentToolProvider... tools) {
        List<AgentToolProvider> registered = tools.length == 0 ? List.of(provider(input -> Mono.just(new JsonObject()))) : List.of(tools);
        return new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(registered), "server", "1"), security);
    }

    private AgentToolProvider provider(final Function<AgentToolInvocation, Mono<JsonObject>> action) {
        AgentToolProvider tool = mock(AgentToolProvider.class);
        when(tool.getName()).thenReturn("read");
        when(tool.getDescription()).thenReturn("Read a resource");
        when(tool.getInputSchema()).thenReturn(JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject());
        when(tool.getRequiredClientCapabilities()).thenReturn(new JsonObject());
        when(tool.invoke(org.mockito.ArgumentMatchers.any())).thenAnswer(invocation -> action.apply(invocation.getArgument(0)));
        return tool;
    }

    private MockServerWebExchange exchange(final String method, final String body) {
        return MockServerWebExchange.from(builder(HttpMethod.POST, method).body(body));
    }

    private MockServerHttpRequest.BodyBuilder builder(final HttpMethod http, final String method) {
        MockServerHttpRequest.BodyBuilder builder = MockServerHttpRequest.method(http, "/agent");
        builder.contentType(MediaType.APPLICATION_JSON).header(HttpHeaders.ACCEPT, "application/json, text/event-stream")
                .header("MCP-Protocol-Version", AgentMcpRequestParser.VERSION).header("Mcp-Method", method);
        if ("tools/call".equals(method)) {
            builder.header("Mcp-Name", "read");
        }
        return builder;
    }

    private String body(final String method) {
        return "{\"jsonrpc\":\"2.0\",\"id\":\"client-id\",\"method\":\"" + method + "\",\"params\":{\"name\":\"read\",\"_meta\":{"
                + "\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}";
    }
}
