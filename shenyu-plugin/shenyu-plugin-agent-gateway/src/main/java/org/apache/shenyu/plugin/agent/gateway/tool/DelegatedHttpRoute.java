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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig;
import org.apache.shenyu.plugin.agent.gateway.delegation.AgentIdentityDelegationException;
import org.apache.shenyu.plugin.agent.gateway.delegation.AgentIdentityDelegationException.Reason;
import org.apache.shenyu.plugin.agent.gateway.delegation.DelegatedCredential;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.HttpToolDispatchGuard;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.ServerWebExchangeDecorator;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.security.Principal;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;

/**
 * Binds a single issued assertion to the approved HTTP route throughout the child plugin chain.
 */
final class DelegatedHttpRoute implements HttpToolDispatchGuard {

    private static final URI CHILD_ORIGIN = URI.create("http://localhost");

    private final AgentGatewayHttpToolConfig.Tool tool;

    private final DelegatedCredential credential;

    private final Instant deadline;

    private final BooleanSupplier currentPolicy;

    private final AtomicReference<URI> selectedUpstream = new AtomicReference<>();

    private final AtomicBoolean dispatched = new AtomicBoolean();

    DelegatedHttpRoute(final AgentGatewayHttpToolConfig.Tool tool, final DelegatedCredential credential, final Instant deadline, final BooleanSupplier currentPolicy) {
        this.tool = tool;
        this.credential = credential;
        this.deadline = deadline;
        this.currentPolicy = currentPolicy;
    }

    void authorize(final HttpHeaders headers) {
        headers.setBearerAuth(credential.getBearerToken());
    }

    void checkValidity() {
        Instant now = Instant.now();
        if (!deadline.isAfter(now)) {
            throw new AgentIdentityDelegationException(Reason.DEADLINE_EXCEEDED);
        }
        if (!credential.getExpiresAt().isAfter(now) || !currentPolicy.getAsBoolean()) {
            throw new AgentIdentityDelegationException(Reason.REVOKED_OR_EXPIRED);
        }
    }

    @Override
    public void checkRoute(final ServerWebExchange exchange) {
        verifyRoute(exchange, exchange.getRequest());
    }

    @Override
    public void bind(final String domain) {
        final URI upstream;
        try {
            upstream = URI.create(domain);
        } catch (IllegalArgumentException error) {
            throw mismatch();
        }
        if (!upstream.isAbsolute() || Objects.isNull(upstream.getHost()) || Objects.nonNull(upstream.getRawUserInfo())) {
            throw mismatch();
        }
        if (!selectedUpstream.compareAndSet(null, upstream) && !upstream.equals(selectedUpstream.get())) {
            throw mismatch();
        }
    }

    @Override
    public void verify(final ServerWebExchange exchange, final URI uri) {
        checkValidity();
        checkRoute(exchange);
        URI upstream = selectedUpstream.get();
        if (Objects.isNull(uri) || Objects.isNull(upstream) || !tool.getPath().equals(uri.getRawPath())) {
            throw mismatch();
        }
        if (!upstream.getScheme().equalsIgnoreCase(uri.getScheme()) || !upstream.getRawAuthority().equalsIgnoreCase(uri.getRawAuthority())) {
            throw mismatch();
        }
        if (Boolean.TRUE.equals(exchange.getAttribute(Constants.PRESERVE_HOST))) {
            List<String> hosts = exchange.getRequest().getHeaders().getOrEmpty(HttpHeaders.HOST);
            if (hosts.size() != 1 || !upstream.getRawAuthority().equalsIgnoreCase(hosts.get(0))) {
                throw mismatch();
            }
        }
        if (!dispatched.compareAndSet(false, true)) {
            throw mismatch();
        }
    }

    ServerWebExchange protect(final ServerWebExchange exchange) {
        final ServerHttpRequest protectedRequest = new ServerHttpRequestDecorator(exchange.getRequest()) {
            @Override
            public HttpHeaders getHeaders() {
                // A Spring header adapter can unwrap a read-only view; never hand it the backing map.
                HttpHeaders headers = new HttpHeaders();
                super.getHeaders().forEach((name, values) -> headers.put(name, new ArrayList<>(values)));
                return HttpHeaders.readOnlyHttpHeaders(headers);
            }
        };
        return new ServerWebExchangeDecorator(exchange) {
            @Override
            public ServerHttpRequest getRequest() {
                verifyRoute(exchange, protectedRequest);
                return protectedRequest;
            }

            @Override
            public Builder mutate() {
                return new CheckedBuilder(super.mutate());
            }
        };
    }

    private void verifyRoute(final ServerWebExchange exchange, final ServerHttpRequest request) {
        verifyRequest(request);
        verifyContext(exchange.getAttribute(Constants.CONTEXT));
        verifySelectorPath(exchange);
    }

    private void verifyRequest(final ServerHttpRequest request) {
        URI uri = request.getURI();
        if (!CHILD_ORIGIN.getScheme().equals(uri.getScheme()) || !CHILD_ORIGIN.getRawAuthority().equals(uri.getRawAuthority()) || !tool.getPath().equals(uri.getRawPath())) {
            throw mismatch();
        }
        List<String> authorization = request.getHeaders().getOrEmpty(HttpHeaders.AUTHORIZATION);
        if (!tool.getMethod().equals(request.getMethod().name()) || !authorization.equals(List.of("Bearer " + credential.getBearerToken()))) {
            throw mismatch();
        }
    }

    private void verifyContext(final ShenyuContext context) {
        if (Objects.isNull(context) || !tool.getPath().equals(context.getPath()) || !tool.getPath().equals(context.getRealUrl())) {
            throw mismatch();
        }
        if (!RpcTypeEnum.HTTP.getName().equals(context.getRpcType()) || !tool.getMethod().equals(context.getHttpMethod())) {
            throw mismatch();
        }
    }

    private void verifySelectorPath(final ServerWebExchange exchange) {
        // AbstractShenyuPlugin matches on the context prefix plus realUrl. With a fixed realUrl
        // any added prefix would select a different authentication or authorization rule.
        if (!exchange.getAttributeOrDefault(Constants.CONTEXT_PATH, "").isBlank() || !exchange.getAttributeOrDefault(Constants.REWRITE_CONTEXT_PATH, "").isBlank()) {
            throw mismatch();
        }
        String rewrite = exchange.getAttribute(Constants.REWRITE_URI);
        if (Objects.nonNull(rewrite) && !tool.getPath().equals(rewrite)) {
            throw mismatch();
        }
    }

    private AgentIdentityDelegationException mismatch() {
        return new AgentIdentityDelegationException(Reason.TARGET_MISMATCH);
    }

    private final class CheckedBuilder implements ServerWebExchange.Builder {

        private final ServerWebExchange.Builder delegate;

        CheckedBuilder(final ServerWebExchange.Builder delegate) {
            this.delegate = delegate;
        }

        @Override
        public ServerWebExchange.Builder request(final Consumer<ServerHttpRequest.Builder> consumer) {
            delegate.request(consumer);
            return this;
        }

        @Override
        public ServerWebExchange.Builder request(final ServerHttpRequest request) {
            delegate.request(request);
            return this;
        }

        @Override
        public ServerWebExchange.Builder response(final ServerHttpResponse response) {
            delegate.response(response);
            return this;
        }

        @Override
        public ServerWebExchange.Builder principal(final Mono<Principal> principal) {
            delegate.principal(principal);
            return this;
        }

        @Override
        public ServerWebExchange build() {
            ServerWebExchange updated = delegate.build();
            verifyRoute(updated, updated.getRequest());
            return protect(updated);
        }
    }
}
