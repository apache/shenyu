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

package org.apache.shenyu.plugin.agent.gateway.tool;

import com.fasterxml.jackson.core.StreamReadConstraints;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.StreamReadFeature;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig;
import org.apache.shenyu.plugin.agent.gateway.AgentGatewayConstants;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.AbstractServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.http.server.reactive.SslInfo;
import org.springframework.http.codec.multipart.Part;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.http.HttpCookie;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.ServerWebExchangeDecorator;
import org.springframework.web.server.WebSession;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.security.Principal;
import java.time.Instant;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;

/**
 * Dispatches fixed gateway routes through a fresh plugin cursor and captures a bounded private response.
 * It never opens a caller-selected socket or borrows the original MCP response, attributes or body.
 */
public final class AgentHttpToolExecutor implements AgentHttpToolInvoker {

    private final ServerWebExchange origin;

    private final ShenyuPluginChain chain;

    private final int maxRequestBytes;

    private final int maxResponseBytes;

    public AgentHttpToolExecutor(final ServerWebExchange origin, final ShenyuPluginChain chain,
                                 final int maxRequestBytes, final int maxResponseBytes) {
        this.origin = Objects.requireNonNull(origin, "origin");
        this.chain = Objects.requireNonNull(chain, "chain");
        this.maxRequestBytes = maxRequestBytes;
        this.maxResponseBytes = maxResponseBytes;
    }

    @Override
    public Mono<JsonObject> invoke(final AgentGatewayHttpToolConfig.Tool tool, final AgentToolInvocation invocation) {
        return Mono.defer(() -> {
            Duration remaining = Duration.between(Instant.now(), invocation.getDeadline());
            if (remaining.isNegative() || remaining.isZero()) {
                return Mono.error(new TimeoutException("HTTP tool deadline expired"));
            }
            JsonObject arguments = invocation.getArguments();
            byte[] body = "POST".equals(tool.getMethod()) ? tool.requestBody(arguments).toString().getBytes(StandardCharsets.UTF_8) : new byte[0];
            if (body.length > maxRequestBytes) {
                return Mono.error(new DataBufferLimitException("HTTP tool request exceeds its configured budget"));
            }
            String path = tool.requestPath(arguments);
            if (body.length + path.getBytes(StandardCharsets.UTF_8).length > maxRequestBytes) {
                return Mono.error(new DataBufferLimitException("HTTP tool request exceeds its configured budget"));
            }
            CapturedResponse response = new CapturedResponse(origin.getResponse().bufferFactory(), maxResponseBytes);
            // Spring's mutate builder can expose the original writable header map.
            // Start with private empty headers before invoking it, never clear the MCP request's map.
            ServerHttpRequest seed = new ServerHttpRequestDecorator(origin.getRequest()) {
                @Override
                public HttpHeaders getHeaders() {
                    return new HttpHeaders();
                }

                @Override
                public Flux<DataBuffer> getBody() {
                    return Flux.empty();
                }
            };
            ServerHttpRequest base = seed.mutate().uri(URI.create("http://localhost" + path)).contextPath("")
                    .method(HttpMethod.valueOf(tool.getMethod())).headers(headers -> {
                        headers.clear();
                        headers.setAccept(List.of(MediaType.APPLICATION_JSON));
                        headers.setContentLength(body.length);
                        if (body.length > 0) {
                            headers.setContentType(MediaType.APPLICATION_JSON);
                        }
                    }).build();
            final ChildExchange child = new ChildExchange(origin, new ToolRequest(base, body, response.bufferFactory()), response);
            ShenyuContext context = new ShenyuContext();
            context.setRpcType("http");
            context.setHttpMethod(tool.getMethod());
            context.setPath(URI.create(path).getPath());
            context.setRealUrl(path);
            context.setStartDateTime(LocalDateTime.now());
            child.getAttributes().put(Constants.CONTEXT, context);
            child.getAttributes().put(AgentGatewayConstants.HTTP_TOOL_CALL_ATTRIBUTE, true);
            // The existing MCP Server plugin recognizes this internal-only marker too.
            child.getAttributes().put("MCP_TOOL_CALL", true);
            return chain.fork().execute(child).then(Mono.fromCallable(response::result)).timeout(remaining);
        });
    }

    private static final class ToolRequest extends ServerHttpRequestDecorator {

        private final byte[] body;

        private final DataBufferFactory factory;

        ToolRequest(final ServerHttpRequest delegate, final byte[] body, final DataBufferFactory factory) {
            super(delegate);
            this.body = body.clone();
            this.factory = factory;
        }

        @Override
        public Flux<DataBuffer> getBody() {
            return body.length == 0 ? Flux.empty() : Flux.defer(() -> Flux.just(factory.wrap(body.clone())));
        }

        @Override
        public MultiValueMap<String, HttpCookie> getCookies() {
            return new LinkedMultiValueMap<>();
        }

        @Override
        public SslInfo getSslInfo() {
            return null;
        }
    }

    private static final class ChildExchange extends ServerWebExchangeDecorator {

        private final ServerHttpRequest request;

        private final CapturedResponse response;

        private final Map<String, Object> attributes = new ConcurrentHashMap<>();

        private Function<String, String> urlTransformer = Function.identity();

        ChildExchange(final ServerWebExchange origin, final ServerHttpRequest request, final CapturedResponse response) {
            super(origin);
            this.request = request;
            this.response = response;
        }

        @Override
        public ServerHttpRequest getRequest() {
            return request;
        }

        @Override
        public CapturedResponse getResponse() {
            return response;
        }

        @Override
        public Map<String, Object> getAttributes() {
            return attributes;
        }

        @Override
        public <T extends Principal> Mono<T> getPrincipal() {
            return Mono.empty();
        }

        @Override
        public Mono<WebSession> getSession() {
            return Mono.error(new UnsupportedOperationException("HTTP tool routes must not access the caller's web session"));
        }

        @Override
        public Mono<MultiValueMap<String, String>> getFormData() {
            return Mono.just(new LinkedMultiValueMap<>());
        }

        @Override
        public Mono<MultiValueMap<String, Part>> getMultipartData() {
            return Mono.just(new LinkedMultiValueMap<>());
        }

        @Override
        public Mono<Void> cleanupMultipart() {
            return Mono.empty();
        }

        @Override
        public boolean isNotModified() {
            return false;
        }

        @Override
        public boolean checkNotModified(final Instant lastModified) {
            return false;
        }

        @Override
        public boolean checkNotModified(final String etag) {
            return false;
        }

        @Override
        public boolean checkNotModified(final String etag, final Instant lastModified) {
            // Conditional caller headers are not delegated to this internal HTTP request.
            return false;
        }

        @Override
        public String transformUrl(final String url) {
            return urlTransformer.apply(url);
        }

        @Override
        public void addUrlTransformer(final Function<String, String> transformer) {
            urlTransformer = urlTransformer.andThen(Objects.requireNonNull(transformer, "transformer"));
        }
    }

    private static final class CapturedResponse extends AbstractServerHttpResponse {

        private final int maximum;

        private byte[] body = new byte[0];

        CapturedResponse(final DataBufferFactory factory, final int maximum) {
            super(factory);
            this.maximum = maximum;
        }

        @Override
        public <T> T getNativeResponse() {
            throw new UnsupportedOperationException("HTTP tools expose only an isolated response");
        }

        @Override
        protected void applyStatusCode() {
        }

        @Override
        protected void applyHeaders() {
        }

        @Override
        protected void applyCookies() {
        }

        @Override
        protected Mono<Void> writeWithInternal(final Publisher<? extends DataBuffer> publisher) {
            // Flux wrapping prevents the join helper's single-Mono fast path from bypassing its byte limit.
            return DataBufferUtils.join(Flux.from(publisher), maximum).doOnNext(buffer -> {
                try {
                    body = new byte[buffer.readableByteCount()];
                    buffer.read(body);
                } finally {
                    DataBufferUtils.release(buffer);
                }
            }).then();
        }

        @Override
        protected Mono<Void> writeAndFlushWithInternal(final Publisher<? extends Publisher<? extends DataBuffer>> publisher) {
            return writeWithInternal(Flux.from(publisher).concatMap(Flux::from));
        }

        JsonObject result() throws IOException {
            if (!isCommitted()) {
                throw new AgentToolExecutionException("HTTP tool route completed without a response");
            }
            int status = Objects.isNull(getStatusCode()) ? HttpStatus.OK.value() : getStatusCode().value();
            if (status < 200 || status >= 300) {
                throw new AgentToolExecutionException("HTTP tool route did not succeed");
            }
            if (!getCookies().isEmpty()) {
                throw new AgentToolExecutionException("HTTP tool routes must not create sessions or response cookies");
            }
            JsonObject result = new JsonObject();
            result.addProperty("httpStatus", status);
            if (body.length > 0) {
                MediaType type = getHeaders().getContentType();
                if (Objects.isNull(type) || !MediaType.APPLICATION_JSON.isCompatibleWith(type)
                        || Objects.nonNull(type.getCharset()) && !StandardCharsets.UTF_8.equals(type.getCharset())) {
                    throw new AgentToolExecutionException("HTTP tools require a UTF-8 JSON result");
                }
                String text = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
                        .onUnmappableCharacter(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(body)).toString();
                JsonFactory factory = JsonFactory.builder().enable(StreamReadFeature.STRICT_DUPLICATE_DETECTION)
                        .streamReadConstraints(StreamReadConstraints.builder().maxNestingDepth(32).maxStringLength(maximum).build()).build();
                ObjectMapper mapper = new ObjectMapper(factory).enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);
                result.add("body", JsonParser.parseString(mapper.readTree(text).toString()));
            }
            return result;
        }
    }
}
