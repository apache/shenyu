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

import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Wraps a plugin under test and records its observable interactions with the legacy chain.
 *
 * <p>The probe is inserted into the real chain. It does not decide whether the delegate should
 * continue; it records what the delegate actually does, so a third-party plugin can be checked
 * without adding counters to production code.</p>
 */
public final class ContractProbe implements ShenyuPlugin {

    private final ShenyuPlugin delegate;

    private final String name;

    private final int order;

    private final PluginContractProfile profile;

    private final AtomicInteger executeCount = new AtomicInteger();

    private final AtomicInteger subscribedCount = new AtomicInteger();

    private final AtomicInteger chainInvocations = new AtomicInteger();

    private final AtomicInteger downstreamSubscriptions = new AtomicInteger();

    private final AtomicInteger cancellationCount = new AtomicInteger();

    private final AtomicInteger completionCount = new AtomicInteger();

    private final AtomicInteger errorCount = new AtomicInteger();

    private ContractProbe(final ShenyuPlugin delegate, final String name, final int order,
                          final PluginContractProfile profile) {
        this.delegate = Objects.requireNonNull(delegate, "delegate");
        this.name = Objects.requireNonNull(name, "name");
        this.order = order;
        this.profile = Objects.requireNonNull(profile, "profile");
    }

    /**
     * Wraps a plugin with a probe.
     *
     * @param delegate plugin under test
     * @param name name reported in diagnostics
     * @param order plugin order in the fixture
     * @param profile expected chain role
     * @return probe
     */
    public static ContractProbe around(final ShenyuPlugin delegate, final String name, final int order,
                                       final PluginContractProfile profile) {
        return new ContractProbe(delegate, name, order, profile);
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        executeCount.incrementAndGet();
        // Keep execute() between the real handler's before/after construction hooks.
        return delegate.execute(exchange, recordingChain(chain))
                .doOnSubscribe(subscription -> subscribedCount.incrementAndGet())
                .doOnCancel(cancellationCount::incrementAndGet)
                .doOnSuccess(ignored -> completionCount.incrementAndGet())
                .doOnError(error -> errorCount.incrementAndGet());
    }

    private ShenyuPluginChain recordingChain(final ShenyuPluginChain chain) {
        return new ShenyuPluginChain() {

            @Override
            public Mono<Void> execute(final ServerWebExchange exchange) {
                chainInvocations.incrementAndGet();
                return chain.execute(exchange)
                        .doOnSubscribe(subscription -> downstreamSubscriptions.incrementAndGet());
            }

            @Override
            public ShenyuPluginChain fork() {
                return recordingChain(chain.fork());
            }
        };
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
        return delegate.skip(exchange);
    }

    @Override
    public void before(final ServerWebExchange exchange) {
        delegate.before(exchange);
    }

    @Override
    public void after(final ServerWebExchange exchange) {
        delegate.after(exchange);
    }

    /**
     * Returns the expected role of the wrapped plugin.
     *
     * @return profile
     */
    public PluginContractProfile getProfile() {
        return profile;
    }

    /**
     * Returns how many times the probe was entered by the chain.
     *
     * @return execute count
     */
    public int getExecuteCount() {
        return executeCount.get();
    }

    /**
     * Returns how many times the probe publisher was subscribed.
     *
     * @return subscription count
     */
    public int getSubscribedCount() {
        return subscribedCount.get();
    }

    /**
     * Returns how many times the delegate invoked the downstream chain.
     *
     * @return downstream invocation count
     */
    public int getChainInvocations() {
        return chainInvocations.get();
    }

    /**
     * Returns how many times the downstream publisher was subscribed, including resubscriptions.
     *
     * @return downstream subscription count
     */
    public int getDownstreamSubscriptions() {
        return downstreamSubscriptions.get();
    }

    /**
     * Returns how many cancellations reached the boundary of the delegate publisher.
     * This does not prove that a delegate-owned resource was released.
     *
     * @return cancellation count
     */
    public int getCancellationCount() {
        return cancellationCount.get();
    }

    /**
     * Returns how many completion signals the delegate publisher emitted.
     *
     * @return completion count
     */
    public int getCompletionCount() {
        return completionCount.get();
    }

    /**
     * Returns how many error signals the delegate publisher emitted.
     *
     * @return error count
     */
    public int getErrorCount() {
        return errorCount.get();
    }
}
