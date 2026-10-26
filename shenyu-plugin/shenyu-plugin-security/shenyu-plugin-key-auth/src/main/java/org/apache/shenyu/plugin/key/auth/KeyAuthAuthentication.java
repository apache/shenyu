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

package org.apache.shenyu.plugin.key.auth;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.util.context.Context;

import java.util.Map;
import java.util.Objects;

/**
 * Immutable, credential-free reference to a rule that authenticated this request.
 * Configuration namespace is not a tenant, and a rule reference is not a user identity or grant.
 * Downstream adapters must use an explicit trusted binding before deriving authorization.
 */
public final class KeyAuthAuthentication {

    private static final Object CONTEXT_KEY = new Object();

    private final String namespaceId;

    private final String selectorId;

    private final String ruleId;

    private KeyAuthAuthentication(final RuleData rule) {
        this.namespaceId = rule.getNamespaceId();
        this.selectorId = rule.getSelectorId();
        this.ruleId = rule.getId();
    }

    /**
     * Read authentication from this request's downstream subscription.
     * Exchange decorators sharing the original attributes retain the binding; another request does not.
     * No principal, header, query parameter or exchange attribute is accepted as authentication.
     *
     * @param exchange current request
     * @return verified rule reference, or empty when key-auth did not authenticate this request
     */
    public static Mono<KeyAuthAuthentication> from(final ServerWebExchange exchange) {
        Objects.requireNonNull(exchange, "exchange");
        return Mono.deferContextual(context -> {
            Binding binding = context.getOrDefault(CONTEXT_KEY, null);
            return Objects.nonNull(binding) && binding.attributes == exchange.getAttributes()
                    ? Mono.just(binding.authentication) : Mono.empty();
        });
    }

    static Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final RuleData rule) {
        Binding binding = new Binding(exchange.getAttributes(), new KeyAuthAuthentication(rule));
        return Mono.defer(() -> chain.execute(exchange)).contextWrite(context -> context.put(CONTEXT_KEY, binding));
    }

    static Context clear(final Context context) {
        return context.delete(CONTEXT_KEY);
    }

    /**
     * Return the configuration namespace, which may be absent in legacy configuration.
     *
     * @return configuration namespace, not an inferred tenant
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * Return the authenticated selector reference.
     *
     * @return selector id
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * Return the authenticated rule reference, never the credential.
     *
     * @return rule id
     */
    public String getRuleId() {
        return ruleId;
    }

    private static final class Binding {

        private final Map<String, Object> attributes;

        private final KeyAuthAuthentication authentication;

        private Binding(final Map<String, Object> attributes, final KeyAuthAuthentication authentication) {
            this.attributes = attributes;
            this.authentication = authentication;
        }
    }
}
