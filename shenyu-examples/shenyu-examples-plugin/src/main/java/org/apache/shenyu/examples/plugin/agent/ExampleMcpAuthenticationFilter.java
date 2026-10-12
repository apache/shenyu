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

package org.apache.shenyu.examples.plugin.agent;

import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpPrincipal;
import org.apache.shenyu.plugin.jwt.strategy.DefaultJwtPayloadParseStrategy;
import org.springframework.core.Ordered;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.Base64;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import com.google.gson.JsonParser;
import java.nio.charset.StandardCharsets;

/**
 * Opt-in, loopback demo authentication using the existing JWT verifier.
 * This is not an OAuth authorization server or a production credential store.
 */
public final class ExampleMcpAuthenticationFilter implements WebFilter, Ordered {

    private static final String ISSUER = "shenyu-agent-example";

    private static final String AUDIENCE = "shenyu-mcp-example";

    private final String secret;

    public ExampleMcpAuthenticationFilter(final String secret) {
        if (Objects.requireNonNull(secret, "secret").length() < 32) {
            throw new IllegalArgumentException("Supply a random example JWT key of at least 32 characters");
        }
        this.secret = secret;
    }

    @Override
    public Mono<Void> filter(final ServerWebExchange exchange, final WebFilterChain chain) {
        String path = exchange.getRequest().getPath().value();
        if (!"/agent/mcp".equals(path) && !path.startsWith("/agent/mcp/")) {
            return chain.filter(exchange);
        }
        return Mono.defer(() -> {
            AgentMcpIdentity identity = authenticate(exchange);
            if (Objects.isNull(identity)) {
                exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
                exchange.getResponse().getHeaders().set(HttpHeaders.WWW_AUTHENTICATE, "Bearer realm=\"shenyu-mcp-example\"");
                return exchange.getResponse().setComplete();
            }
            return chain.filter(exchange.mutate().principal(Mono.just(new AgentMcpPrincipal(identity))).build());
        });
    }

    @Override
    public int getOrder() {
        return Ordered.HIGHEST_PRECEDENCE + 100;
    }

    private AgentMcpIdentity authenticate(final ServerWebExchange exchange) {
        List<String> headers = exchange.getRequest().getHeaders().get(HttpHeaders.AUTHORIZATION);
        if (Objects.isNull(headers) || headers.size() != 1 || !headers.get(0).regionMatches(true, 0, "Bearer ", 0, 7)) {
            return null;
        }
        String token = headers.get(0).substring(7);
        if (token.length() > 8192 || token.isBlank() || !token.equals(token.trim())) {
            return null;
        }
        try {
            String[] segments = token.split("\\.", -1);
            if (segments.length != 3 || !"HS256".equals(JsonParser.parseString(new String(Base64.getUrlDecoder().decode(segments[0]),
                    StandardCharsets.UTF_8)).getAsJsonObject().get("alg").getAsString())) {
                return null;
            }
            Map<String, Object> claims = new DefaultJwtPayloadParseStrategy().parse(secret, token);
            if (Objects.isNull(claims) || !ISSUER.equals(claims.get("iss")) || !(claims.get("exp") instanceof Number)
                    || !AUDIENCE.equals(claims.get("aud")) && !(claims.get("aud") instanceof Collection
                    && ((Collection<?>) claims.get("aud")).size() == 1 && ((Collection<?>) claims.get("aud")).contains(AUDIENCE))) {
                return null;
            }
            Object subject = claims.get("sub");
            if (!Set.of("agent-a", "agent-b", "agent-none").contains(Objects.toString(subject, ""))) {
                return null;
            }
            // Privileges come from server policy, not an arbitrary tool list in the JWT.
            Set<String> grants = "agent-none".equals(subject) ? Set.of() : Set.of("order_status");
            return new AgentMcpIdentity((String) subject, grants);
        } catch (RuntimeException error) {
            // Do not log raw tokens or include parser/claim details in responses.
            return null;
        }
    }
}
