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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Bounded, single-response MCP transport used only by a matched gateway rule.
 */
public final class AgentMcpHttpHandler {

    private static final byte[] SSE_PREFIX = "event: message\ndata: ".getBytes(StandardCharsets.UTF_8);

    private static final byte[] SSE_SUFFIX = "\n\n".getBytes(StandardCharsets.UTF_8);

    private final AgentMcpDispatcher dispatcher;

    private final AgentMcpSecurityResolver securityResolver;

    private final AgentMcpRequestParser parser = new AgentMcpRequestParser();

    private final ObjectMapper mapper = new ObjectMapper();

    public AgentMcpHttpHandler(final AgentMcpDispatcher dispatcher, final AgentMcpSecurityResolver securityResolver) {
        this.dispatcher = Objects.requireNonNull(dispatcher, "dispatcher");
        this.securityResolver = Objects.requireNonNull(securityResolver, "securityResolver");
    }

    /**
     * Handle one matched rule with an immutable configuration and traffic context.
     *
     * @param exchange matched exchange
     * @param config configuration captured at subscription
     * @param traffic per-subscription server request identity
     * @param configurationVersion node-local rule snapshot generation, not an Admin revision
     * @return request execution and response writing linked to downstream cancellation
     */
    public Mono<Void> handle(final ServerWebExchange exchange, final AgentGatewayMcpConfig config,
                             final AgentTrafficContext traffic, final long configurationVersion) {
        return Mono.defer(() -> {
            final Instant deadline = Instant.now().plusMillis(config.getTimeoutMs());
            AtomicReference<JsonNode> rpcId = new AtomicReference<>();
            return Mono.fromRunnable(() -> preflight(exchange, config))
                    .then(Mono.defer(() -> securityResolver.resolve(exchange))
                            .switchIfEmpty(Mono.error(failure(401, "Trusted identity is required", null))))
                    .flatMap(identity -> read(exchange, config).map(body -> parser.parse(body, exchange.getRequest().getHeaders(), config.getMaxRequestBytes()))
                            .flatMap(request -> {
                                rpcId.set(request.getId());
                                return dispatcher.dispatch(request, () -> new AgentMcpExecutionContext(traffic.getRequestId(), identity.getSubject(),
                                        traffic.getRuleId(), configurationVersion, config.getAllowedTools(), identity.getToolGrants(), deadline));
                            }))
                    .flatMap(response -> write(exchange, response, config, "sse".equals(config.getResponseMode()), 200))
                    .timeout(Duration.ofMillis(config.getTimeoutMs()))
                    .onErrorResume(error -> handleFailure(exchange, config, rpcId.get(), error));
        });
    }

    private void preflight(final ServerWebExchange exchange, final AgentGatewayMcpConfig config) {
        HttpHeaders headers = exchange.getRequest().getHeaders();
        List<String> origins = headers.get(HttpHeaders.ORIGIN);
        if (Objects.nonNull(origins) && (origins.size() != 1 || !config.getAllowedOrigins().contains(origins.get(0)))) {
            throw failure(403, "Origin is not allowed", null);
        }
        if (exchange.getRequest().getMethod() != HttpMethod.POST) {
            exchange.getResponse().getHeaders().set(HttpHeaders.ALLOW, "POST");
            throw failure(405, "Only POST is supported", null);
        }
        try {
            List<String> contentTypes = headers.get(HttpHeaders.CONTENT_TYPE);
            if (Objects.nonNull(contentTypes) && contentTypes.size() != 1) {
                throw failure(400, "Ambiguous content type", null);
            }
            MediaType contentType = headers.getContentType();
            if (Objects.isNull(contentType) || !"application".equalsIgnoreCase(contentType.getType()) || !"json".equalsIgnoreCase(contentType.getSubtype())
                    || Objects.nonNull(contentType.getCharset()) && !StandardCharsets.UTF_8.equals(contentType.getCharset())) {
                throw failure(415, "UTF-8 application/json is required", null);
            }
            List<MediaType> accepted = headers.getAccept();
            if (!accepts(accepted, MediaType.APPLICATION_JSON) || !accepts(accepted, MediaType.TEXT_EVENT_STREAM)) {
                throw failure(406, "Both JSON and SSE must be accepted", null);
            }
        } catch (IllegalArgumentException error) {
            throw failure(400, "Invalid media headers", null);
        }
        try {
            List<String> lengths = headers.get(HttpHeaders.CONTENT_LENGTH);
            if (Objects.nonNull(lengths) && (lengths.size() != 1 || headers.getContentLength() < 0)) {
                throw failure(400, "Invalid content length", null);
            }
            if (headers.getContentLength() > config.getMaxRequestBytes()) {
                throw failure(413, "Request body exceeds the configured limit", null);
            }
        } catch (NumberFormatException error) {
            throw failure(400, "Invalid content length", null);
        }
    }

    private boolean accepts(final List<MediaType> accepted, final MediaType required) {
        return accepted.stream().anyMatch(type -> required.getType().equalsIgnoreCase(type.getType())
                && required.getSubtype().equalsIgnoreCase(type.getSubtype()) && type.getQualityValue() > 0);
    }

    private Mono<byte[]> read(final ServerWebExchange exchange, final AgentGatewayMcpConfig config) {
        return DataBufferUtils.join(exchange.getRequest().getBody(), config.getMaxRequestBytes())
                .map(buffer -> {
                    try {
                        byte[] bytes = new byte[buffer.readableByteCount()];
                        buffer.read(bytes);
                        return bytes;
                    } finally {
                        DataBufferUtils.release(buffer);
                    }
                }).defaultIfEmpty(new byte[0]);
    }

    private Mono<Void> handleFailure(final ServerWebExchange exchange, final AgentGatewayMcpConfig config, final JsonNode id, final Throwable error) {
        if (exchange.getResponse().isCommitted()) {
            return Mono.error(error);
        }
        ObjectNode response;
        int status;
        if (error instanceof AgentMcpProtocolException) {
            AgentMcpProtocolException protocol = (AgentMcpProtocolException) error;
            response = protocol.toResponse();
            status = protocol.getHttpStatus();
        } else if (error instanceof TransportFailure) {
            TransportFailure transport = (TransportFailure) error;
            response = mapper.createObjectNode().put("error", transport.getMessage());
            status = transport.status;
        } else if (error instanceof DataBufferLimitException) {
            response = mapper.createObjectNode().put("error", "Request body exceeds the configured limit");
            status = 413;
        } else if (error instanceof TimeoutException) {
            response = mapper.createObjectNode().put("error", "Request deadline exceeded");
            status = 504;
        } else {
            response = new AgentMcpProtocolException(500, -32603, "Internal error", id, null).toResponse();
            status = 500;
        }
        // Execution is already terminated. Bound the best-effort terminal error write too.
        return write(exchange, response, config, false, status)
                .onErrorResume(writeFailure -> {
                    if (exchange.getResponse().isCommitted()) {
                        return Mono.error(writeFailure);
                    }
                    exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                    return exchange.getResponse().setComplete();
                }).timeout(Duration.ofMillis(Math.min(1000, config.getTimeoutMs())));
    }

    private Mono<Void> write(final ServerWebExchange exchange, final ObjectNode response, final AgentGatewayMcpConfig config, final boolean sse, final int status) {
        return Mono.defer(() -> {
            byte[] bytes = encode(response, config.getMaxResponseBytes(), sse);
            exchange.getResponse().setStatusCode(HttpStatus.valueOf(status));
            HttpHeaders headers = exchange.getResponse().getHeaders();
            headers.remove(HttpHeaders.CONTENT_LENGTH);
            headers.remove("X-Accel-Buffering");
            headers.setContentType(sse ? MediaType.TEXT_EVENT_STREAM : MediaType.APPLICATION_JSON);
            headers.setCacheControl("no-store");
            if (sse) {
                headers.set("X-Accel-Buffering", "no");
            } else {
                headers.setContentLength(bytes.length);
            }
            return exchange.getResponse().writeWith(Mono.fromSupplier(() -> exchange.getResponse().bufferFactory().wrap(bytes))
                    .doOnDiscard(DataBuffer.class, DataBufferUtils::release));
        });
    }

    private byte[] encode(final ObjectNode response, final int limit, final boolean sse) {
        LimitedOutput output = new LimitedOutput(limit);
        try {
            if (sse) {
                output.write(SSE_PREFIX);
            }
            // Do not let ObjectMapper close this output before the SSE suffix is written.
            mapper.writer().without(com.fasterxml.jackson.core.JsonGenerator.Feature.AUTO_CLOSE_TARGET).writeValue(output, response);
            if (sse) {
                output.write(SSE_SUFFIX);
            }
            return output.bytes();
        } catch (IOException error) {
            throw new AgentMcpProtocolException(500, -32603, "Response exceeds the configured limit or cannot be encoded", response.get("id"), null);
        }
    }

    private TransportFailure failure(final int status, final String message, final JsonNode id) {
        return new TransportFailure(status, message);
    }

    private static final class TransportFailure extends RuntimeException {

        private final int status;

        private TransportFailure(final int status, final String message) {
            super(message);
            this.status = status;
        }
    }

    private static final class LimitedOutput extends OutputStream {

        private final int limit;

        private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();

        private LimitedOutput(final int limit) {
            this.limit = limit;
        }

        @Override
        public void write(final int value) throws IOException {
            check(1);
            bytes.write(value);
        }

        @Override
        public void write(final byte[] source, final int offset, final int length) throws IOException {
            check(length);
            bytes.write(source, offset, length);
        }

        private void check(final int length) throws IOException {
            if (length > limit - bytes.size()) {
                throw new IOException("Response limit exceeded");
            }
        }

        private byte[] bytes() {
            return bytes.toByteArray();
        }
    }
}
