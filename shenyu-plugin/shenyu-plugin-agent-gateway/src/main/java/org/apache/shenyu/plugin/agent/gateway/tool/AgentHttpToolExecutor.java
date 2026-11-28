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
import org.apache.shenyu.plugin.agent.gateway.delegation.AgentHttpIdentityDelegator;
import org.apache.shenyu.plugin.agent.gateway.delegation.AgentIdentityDelegationException;
import org.apache.shenyu.plugin.agent.gateway.delegation.AgentIdentityDelegationException.Reason;
import org.apache.shenyu.plugin.agent.gateway.delegation.DelegationRequest;
import org.apache.shenyu.plugin.agent.gateway.delegation.DelegatedCredential;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.HttpToolDispatchGuard;
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
import java.util.function.BooleanSupplier;
import java.util.function.Function;

/**
 * Dispatches fixed gateway routes through a fresh plugin cursor and captures a bounded private response.
 * It never opens a caller-selected socket or borrows the original MCP response, attributes or body.
 */
public final class AgentHttpToolExecutor implements AgentHttpToolInvoker {

    private static final Duration MAX_TIMER_DURATION = Duration.ofNanos(Long.MAX_VALUE);

    private final ServerWebExchange origin;

    private final ShenyuPluginChain chain;

    private final int maxRequestBytes;

    private final int maxResponseBytes;

    private final AgentHttpIdentityDelegator identityDelegator;

    public AgentHttpToolExecutor(final ServerWebExchange origin, final ShenyuPluginChain chain,
                                 final int maxRequestBytes, final int maxResponseBytes) {
        this(origin, chain, maxRequestBytes, maxResponseBytes, null);
    }

    public AgentHttpToolExecutor(final ServerWebExchange origin, final ShenyuPluginChain chain, final int maxRequestBytes,
                                 final int maxResponseBytes, final AgentHttpIdentityDelegator identityDelegator) {
        this.identityDelegator = identityDelegator;
        this.origin = Objects.requireNonNull(origin, "origin");
        this.chain = Objects.requireNonNull(chain, "chain");
        this.maxRequestBytes = maxRequestBytes;
        this.maxResponseBytes = maxResponseBytes;
    }

    @Override
    public Mono<JsonObject> invoke(final AgentGatewayHttpToolConfig.Tool tool, final AgentToolInvocation invocation) {
        return invoke(tool, invocation, () -> true);
    }

    /**
     * Invoke a tool while its administrative policy snapshot still authorizes delegation.
     * @param tool approved tool
     * @param invocation trusted input
     * @param currentPolicy checks the rule generation after asynchronous work
     * @return bounded tool result
     */
    public Mono<JsonObject> invoke(final AgentGatewayHttpToolConfig.Tool tool, final AgentToolInvocation invocation, final BooleanSupplier currentPolicy) {
        return Mono.defer(() -> {
            boolean delegated = tool.getIdentityDelegation().isPresent();
            if (delegated && Objects.isNull(identityDelegator)) {
                return Mono.error(new AgentIdentityDelegationException(Reason.UNCONFIGURED));
            }
            Duration remaining = Duration.between(Instant.now(), invocation.getDeadline());
            if (remaining.isNegative() || remaining.isZero()) {
                return Mono.error(delegated ? new AgentIdentityDelegationException(Reason.DEADLINE_EXCEEDED) : new TimeoutException("HTTP tool deadline expired"));
            }
            JsonObject arguments = invocation.getArguments();
            byte[] body = "POST".equals(tool.getMethod()) ? tool.requestBody(arguments).toString().getBytes(StandardCharsets.UTF_8) : new byte[0];
            String path = tool.requestPath(arguments);
            if (body.length + path.getBytes(StandardCharsets.UTF_8).length > maxRequestBytes) {
                return Mono.error(new DataBufferLimitException("HTTP tool request exceeds its configured budget"));
            }
            URI target = URI.create("http://localhost" + path);
            if (!delegated) {
                return dispatch(tool, target, body, null).timeout(remaining);
            }
            return resolveCredential(tool, invocation, currentPolicy)
                    .flatMap(credential -> {
                        DelegatedHttpRoute route = new DelegatedHttpRoute(tool, credential, invocation.getDeadline(), currentPolicy);
                        route.checkValidity();
                        return dispatch(tool, target, body, route);
                    }).timeout(remaining.compareTo(MAX_TIMER_DURATION) > 0 ? MAX_TIMER_DURATION : remaining,
                            Mono.error(() -> new AgentIdentityDelegationException(Reason.DEADLINE_EXCEEDED)));
        });
    }

    private Mono<DelegatedCredential> resolveCredential(final AgentGatewayHttpToolConfig.Tool tool, final AgentToolInvocation invocation, final BooleanSupplier currentPolicy) {
        if (!currentPolicy.getAsBoolean()) {
            return Mono.error(new AgentIdentityDelegationException(Reason.REVOKED_OR_EXPIRED));
        }
        if (tool.getIdentityDelegation().orElseThrow().isRequireTenant() && invocation.getTenant().isEmpty()) {
            return Mono.error(new AgentIdentityDelegationException(Reason.TENANT_MISSING));
        }
        return Mono.defer(() -> identityDelegator.delegate(new DelegationRequest(tool, invocation)))
                .switchIfEmpty(Mono.error(() -> new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE)))
                .onErrorMap(error -> !(error instanceof AgentIdentityDelegationException), error -> new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE));
    }

    private Mono<JsonObject> dispatch(final AgentGatewayHttpToolConfig.Tool tool, final URI target, final byte[] body, final DelegatedHttpRoute route) {
        CapturedResponse response = new CapturedResponse(origin.getResponse().bufferFactory(), maxResponseBytes);
        ShenyuContext context = new ShenyuContext();
        context.setRpcType("http");
        context.setHttpMethod(tool.getMethod());
        context.setPath(target.getRawPath());
        // RequestUrlUtils appends the encoded query separately.
        context.setRealUrl(target.getRawPath());
        context.setStartDateTime(LocalDateTime.now());
        ServerHttpRequest base = privateRequest(target, tool.getMethod(), body.length, route);
        ChildExchange child = new ChildExchange(origin, new ToolRequest(base, body, response.bufferFactory()), response);
        child.getAttributes().put(Constants.CONTEXT, context);
        child.getAttributes().put(AgentGatewayConstants.HTTP_TOOL_CALL_ATTRIBUTE, true);
        child.getAttributes().put("MCP_TOOL_CALL", true);
        if (Objects.isNull(route)) {
            return chain.fork().execute(child).then(Mono.fromCallable(response::result));
        }
        child.getAttributes().put(HttpToolDispatchGuard.ATTRIBUTE, route);
        return chain.fork(route).execute(route.protect(child)).then(Mono.fromCallable(response::result))
                .contextWrite(contextView -> contextView.put(HttpToolDispatchGuard.class, route));
    }

    private ServerHttpRequest privateRequest(final URI target, final String method, final int length, final DelegatedHttpRoute route) {
        // Mutate private headers and body only. Earlier Spring builders can reuse the seed's header map.
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
        return seed.mutate().uri(target).contextPath("").method(HttpMethod.valueOf(method)).headers(headers -> {
            headers.clear();
            headers.setAccept(List.of(MediaType.APPLICATION_JSON));
            headers.setContentLength(length);
            if (length > 0) {
                headers.setContentType(MediaType.APPLICATION_JSON);
            }
            if (Objects.nonNull(route)) {
                route.authorize(headers);
            }
        }).build();
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
