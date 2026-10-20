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

package org.apache.shenyu.plugin.agent.gateway.security;

import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Trusted principal boundary, distinct from ordinary principals and headers.
 */
class AgentMcpPrincipalSecurityResolverTest {

    private final AgentMcpPrincipalSecurityResolver resolver = new AgentMcpPrincipalSecurityResolver();

    @Test
    void shouldRejectOrdinaryPrincipalAndForgedHeaders() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/agent/mcp").header("X-Subject", "agent-a"));
        StepVerifier.create(resolver.resolve(exchange)).verifyComplete();
        StepVerifier.create(resolver.resolve(exchange.mutate().principal(Mono.just(() -> "agent-a")).build())).verifyComplete();
    }

    @Test
    void shouldReadOnlyExplicitlyInstalledImmutableIdentity() {
        AgentMcpIdentity identity = new AgentMcpIdentity("agent-a", Set.of("order_status"));
        AgentMcpPrincipal principal = new AgentMcpPrincipal(identity);
        assertEquals("agent-a", principal.getName());
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/agent/mcp"));
        StepVerifier.create(resolver.resolve(exchange.mutate().principal(Mono.just(principal)).build()))
                .assertNext(result -> assertEquals(identity, result)).verifyComplete();
    }
}
