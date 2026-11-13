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

package org.apache.shenyu.web.contract;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * A configurable plugin used by the legacy contract fixture.
 *
 * <p>The role describes what the plugin does with the downstream chain. It is intentionally
 * explicit: a pass-through plugin, a transforming plugin and a short-circuit plugin must not be
 * judged by the same continuation count.</p>
 */
public final class RecordingPlugin implements ShenyuPlugin {

    /**
     * Exchange attribute written by the {@link Role#TRANSFORM} role.
     */
    public static final String TRANSFORMED_ATTRIBUTE = "contract.transformed";

    /**
     * Exchange attribute written by the {@link Role#WRAPPER} role after downstream completion.
     */
    public static final String WRAPPED_ATTRIBUTE = "contract.wrapped";

    private final String name;

    private final int order;

    private final Role role;

    private final boolean skip;

    private final AtomicInteger executeCount = new AtomicInteger();

    private final AtomicInteger subscribedCount = new AtomicInteger();

    private final AtomicInteger chainInvocations = new AtomicInteger();

    private final AtomicInteger beforeCount = new AtomicInteger();

    private final AtomicInteger afterCount = new AtomicInteger();

    private final AtomicInteger cancellationCount = new AtomicInteger();

    /**
     * Creates a plugin with the default skip behavior.
     *
     * @param name plugin name
     * @param order plugin order
     * @param role expected chain role
     */
    public RecordingPlugin(final String name, final int order, final Role role) {
        this(name, order, role, false);
    }

    /**
     * Creates a plugin.
     *
     * @param name plugin name
     * @param order plugin order
     * @param role expected chain role
     * @param skip whether the plugin should be skipped
     */
    public RecordingPlugin(final String name, final int order, final Role role, final boolean skip) {
        this.name = name;
        this.order = order;
        this.role = role;
        this.skip = skip;
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        executeCount.incrementAndGet();
        return Mono.defer(() -> {
            subscribedCount.incrementAndGet();
            switch (role) {
                case SHORT_CIRCUIT:
                    return Mono.empty();
                case CANCELLATION:
                    return Mono.<Void>never().doOnCancel(cancellationCount::incrementAndGet);
                case FAILURE:
                    return Mono.error(new IllegalStateException("plugin " + name + " failed"));
                case TRANSFORM:
                    exchange.getAttributes().put(TRANSFORMED_ATTRIBUTE, name);
                    chainInvocations.incrementAndGet();
                    return chain.execute(exchange);
                case WRAPPER:
                    chainInvocations.incrementAndGet();
                    return chain.execute(exchange)
                            .doOnSuccess(ignored -> exchange.getAttributes().put(WRAPPED_ATTRIBUTE, name));
                case PASS_THROUGH:
                default:
                    chainInvocations.incrementAndGet();
                    return chain.execute(exchange);
            }
        });
    }

    @Override
    public int getOrder() {
        return order;
    }

    @Override
    public String named() {
        return name;
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skip;
    }

    @Override
    public void before(final ServerWebExchange exchange) {
        beforeCount.incrementAndGet();
    }

    @Override
    public void after(final ServerWebExchange exchange) {
        afterCount.incrementAndGet();
    }

    /**
     * Returns the configured role.
     *
     * @return role
     */
    public Role getRole() {
        return role;
    }

    /**
     * Returns how many times the plugin was entered by the chain.
     *
     * @return execute count
     */
    public int getExecuteCount() {
        return executeCount.get();
    }

    /**
     * Returns how many times the returned publisher was subscribed.
     *
     * @return subscription count
     */
    public int getSubscribedCount() {
        return subscribedCount.get();
    }

    /**
     * Returns how many times the plugin invoked the downstream chain.
     *
     * @return downstream invocation count
     */
    public int getChainInvocations() {
        return chainInvocations.get();
    }

    /**
     * Returns how many times the chain invoked the plugin lifecycle before hook.
     *
     * @return before count
     */
    public int getBeforeCount() {
        return beforeCount.get();
    }

    /**
     * Returns how many times the chain invoked the plugin lifecycle after hook.
     *
     * @return after count
     */
    public int getAfterCount() {
        return afterCount.get();
    }

    /**
     * Returns how many times a cancellation signal was observed.
     *
     * @return cancellation count
     */
    public int getCancellationCount() {
        return cancellationCount.get();
    }

    /**
     * The role a recording plugin plays in the chain.
     */
    public enum Role {

        /**
         * Delegates once to the downstream chain.
         */
        PASS_THROUGH,

        /**
         * Mutates the exchange and delegates once to the downstream chain.
         */
        TRANSFORM,

        /**
         * Decorates the downstream publisher and records completion.
         */
        WRAPPER,

        /**
         * Completes without delegating to the downstream chain.
         */
        SHORT_CIRCUIT,

        /**
         * Never completes and records cancellation.
         */
        CANCELLATION,

        /**
         * Fails without delegating to the downstream chain.
         */
        FAILURE
    }
}
