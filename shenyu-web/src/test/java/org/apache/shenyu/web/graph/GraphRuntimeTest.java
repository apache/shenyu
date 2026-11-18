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

package org.apache.shenyu.web.graph;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.GraphBranchData;
import org.apache.shenyu.common.dto.GraphData;
import org.apache.shenyu.common.dto.GraphNodeData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.graph.GraphValidationException;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Executable characterization of the initial graph runtime and its legacy boundary.
 */
class GraphRuntimeTest {

    @Test
    void linearPlanPreservesLegacyHooksAndWrapperNesting() {
        List<String> calls = new ArrayList<>();
        ShenyuPlugin wrapper = plugin("wrapper", (exchange, chain) -> {
            calls.add("assemble");
            return chain.execute(exchange).then(Mono.fromRunnable(() -> calls.add("unwind")));
        });
        doAnswer(invocation -> calls.add("before")).when(wrapper).before(any());
        doAnswer(invocation -> calls.add("after")).when(wrapper).after(any());
        ShenyuPlugin leaf = plugin("leaf", (exchange, chain) -> Mono.fromRunnable(() -> calls.add("leaf")).then(chain.execute(exchange)));
        List<ShenyuPlugin> plugins = List.of(wrapper, leaf);
        ShenyuWebHandler legacy = handler(plugins, new PluginExecutionDispatcher());
        Mono<Void> legacyResult = legacy.handle(exchange());
        assertTrue(calls.isEmpty());
        StepVerifier.create(legacyResult).verifyComplete();
        List<String> expected = List.copyOf(calls);
        assertEquals(List.of("before", "assemble", "after", "leaf", "unwind"), expected);
        calls.clear();
        BoundGraphPlan plan = bind(linear("wrapper", "leaf"), plugins);
        Mono<Void> graphResult = handler(plugins, graphDispatcher(() -> plan)).handle(exchange());
        assertTrue(calls.isEmpty());
        StepVerifier.create(graphResult).verifyComplete();
        assertEquals(expected, calls);
    }

    @Test
    void choiceUsesDecoratedExchangeAndCopiedConditionsAndConvergesOnce() {
        AtomicInteger yes = new AtomicInteger();
        AtomicInteger no = new AtomicInteger();
        AtomicInteger suffix = new AtomicInteger();
        ShenyuPlugin decorate = plugin("decorate", (exchange, chain) ->
                chain.execute(exchange.mutate().request(request -> request.headers(headers -> headers.set("route", "yes"))).build()));
        List<ShenyuPlugin> plugins = List.of(decorate, counting("yes", yes), counting("no", no), counting("suffix", suffix));
        GraphNodeData choice = choice("choice", "no");
        ConditionData condition = condition("yes");
        GraphBranchData first = new GraphBranchData(new ArrayList<>(List.of(condition)), 0, "yes");
        choice.setConditions(new ArrayList<>(List.of(first)));
        GraphData graph = graph(List.of(node("decorate", "choice"), choice, node("yes", "suffix"), node("no", "suffix"), node("suffix", "end"), end()));
        final BoundGraphPlan plan = bind(graph, plugins);
        // Mutations to the compiler DTOs must not change an already bound topology.
        condition.setParamValue("changed");
        first.setTarget("no");
        first.getConditions().clear();
        choice.setDefaultTarget("yes");
        graph.getNodes().get(0).setNext("end");
        graph.getNodes().clear();
        StepVerifier.create(new GraphExecutor().execute(plan, exchange())).verifyComplete();
        assertEquals(1, yes.get());
        assertEquals(0, no.get());
        assertEquals(1, suffix.get());
    }

    @Test
    void choiceIsOrderedAndHasDefault() {
        AtomicInteger yes = new AtomicInteger();
        AtomicInteger no = new AtomicInteger();
        GraphNodeData choice = choice("choice", "no");
        ConditionData second = condition("yes");
        second.setOperator("match");
        choice.setConditions(List.of(new GraphBranchData(List.of(condition("yes")), 0, "yes"),
                new GraphBranchData(List.of(second), 0, "no")));
        BoundGraphPlan plan = bind(graph(List.of(choice, node("yes", "end"), node("no", "end"), end())),
                List.of(counting("yes", yes), counting("no", no)));
        StepVerifier.create(new GraphExecutor().execute(plan,
                MockServerWebExchange.from(MockServerHttpRequest.get("/").header("route", "yes")))).verifyComplete();
        StepVerifier.create(new GraphExecutor().execute(plan, exchange())).verifyComplete();
        assertEquals(1, yes.get());
        assertEquals(1, no.get());
    }

    @Test
    void skipContinuesButShortCircuitDoesNot() {
        AtomicInteger last = new AtomicInteger();
        ShenyuPlugin skip = plugin("skip", (exchange, chain) -> Mono.error(new AssertionError("must skip")));
        when(skip.skip(any())).thenReturn(true);
        ShenyuPlugin stop = plugin("stop", (exchange, chain) -> Mono.empty());
        BoundGraphPlan plan = bind(linear("skip", "stop", "last"), List.of(skip, stop, counting("last", last)));
        StepVerifier.create(new GraphExecutor().execute(plan, exchange())).verifyComplete();
        assertEquals(0, last.get());
    }

    @Test
    void preservesErrorRecoveryAndCancellation() {
        AtomicInteger recovered = new AtomicInteger();
        ShenyuPlugin recover = plugin("recover", (exchange, chain) ->
                chain.execute(exchange).onErrorResume(error -> Mono.fromRunnable(recovered::incrementAndGet)));
        ShenyuPlugin fail = plugin("fail", (exchange, chain) -> Mono.error(new IllegalArgumentException("backend")));
        StepVerifier.create(new GraphExecutor().execute(bind(linear("recover", "fail"), List.of(recover, fail)), exchange())).verifyComplete();
        assertEquals(1, recovered.get());
        AtomicBoolean cancelled = new AtomicBoolean();
        ShenyuPlugin never = plugin("never", (exchange, chain) -> Mono.<Void>never().doOnCancel(() -> cancelled.set(true)));
        MockServerWebExchange exchange = exchange();
        ShenyuWebHandler handler = handler(List.of(never), graphDispatcher(() -> bind(linear("never"), List.of(never))));
        StepVerifier.create(handler.handle(exchange)).thenCancel().verify();
        assertTrue(cancelled.get());
        assertNull(exchange.getAttribute(Constants.CHAIN_START_TIME));
    }

    @Test
    void endLeavesResponseCompletionToItsWrapper() {
        AtomicInteger writes = new AtomicInteger();
        ShenyuPlugin writer = plugin("writer", (exchange, chain) -> chain.execute(exchange).then(Mono.defer(() -> {
            assertFalse(exchange.getResponse().isCommitted());
            writes.incrementAndGet();
            return exchange.getResponse().setComplete();
        })));
        MockServerWebExchange exchange = exchange();
        StepVerifier.create(new GraphExecutor().execute(bind(linear("writer"), List.of(writer)), exchange)).verifyComplete();
        assertEquals(1, writes.get());
        assertTrue(exchange.getResponse().isCommitted());
    }

    @Test
    void transportRetryDoesNotResubscribeSuccessor() {
        AtomicInteger attempts = new AtomicInteger();
        AtomicInteger downstream = new AtomicInteger();
        ShenyuPlugin transport = plugin("transport", (exchange, chain) ->
                Mono.defer(() -> attempts.incrementAndGet() == 1 ? Mono.error(new IllegalStateException("connection")) : Mono.just("response"))
                        .retry(1).flatMap(response -> chain.execute(exchange)));
        BoundGraphPlan plan = bind(linear("transport", "downstream"), List.of(transport, counting("downstream", downstream)));
        StepVerifier.create(new GraphExecutor().execute(plan, exchange())).verifyComplete();
        assertEquals(2, attempts.get());
        assertEquals(1, downstream.get());
    }

    @Test
    void repeatedInvocationIsExplicitlyRejectedByProvisionalPolicy() {
        assertRepeatedSuccessorRejected(true);
    }

    @Test
    void repeatedSubscriptionIsExplicitlyRejectedByProvisionalPolicy() {
        assertRepeatedSuccessorRejected(false);
    }

    private void assertRepeatedSuccessorRejected(final boolean invokeTwice) {
        AtomicInteger effects = new AtomicInteger();
        ShenyuPlugin repeat = plugin("repeat", (exchange, chain) -> {
            Mono<Void> next = chain.execute(exchange);
            return next.then(invokeTwice ? chain.execute(exchange) : next);
        });
        BoundGraphPlan plan = bind(linear("repeat", "effect"), List.of(repeat, counting("effect", effects)));
        StepVerifier.create(new GraphExecutor().execute(plan, exchange()))
                .expectErrorMatches(error -> error instanceof IllegalStateException && error.getMessage().contains("already consumed")).verify();
        assertEquals(1, effects.get());
    }

    @Test
    void entryResubscriptionHasFreshState() {
        AtomicInteger calls = new AtomicInteger();
        BoundGraphPlan plan = bind(linear("leaf"), List.of(counting("leaf", calls)));
        Mono<Void> entry = new GraphExecutor().execute(plan, exchange());
        assertEquals(0, calls.get());
        StepVerifier.create(entry.then(entry)).verifyComplete();
        assertEquals(2, calls.get());
    }

    @Test
    void forkPinsPlanAcrossPublicationAndStartsAtEntry() {
        AtomicInteger oldCalls = new AtomicInteger();
        AtomicInteger newCalls = new AtomicInteger();
        Sinks.One<Void> release = Sinks.one();
        AtomicReference<BoundGraphPlan> active = new AtomicReference<>();
        ShenyuPlugin parent = plugin("parent", (exchange, chain) -> {
            if (Boolean.TRUE.equals(exchange.getAttribute("child"))) {
                return chain.execute(exchange);
            }
            return release.asMono().then(Mono.defer(() -> {
                MockServerWebExchange child = exchange();
                child.getAttributes().put("child", true);
                return chain.fork().execute(child).then(chain.execute(exchange));
            }));
        });
        active.set(bind(linear("parent", "old"), List.of(parent, counting("old", oldCalls))));
        ShenyuWebHandler handler = handler(List.of(), graphDispatcher(active::get));
        Mono<Void> request = handler.handle(exchange());
        StepVerifier.create(request).then(() -> {
            active.set(bind(linear("new"), List.of(counting("new", newCalls))));
            release.tryEmitEmpty();
        }).verifyComplete();
        assertEquals(2, oldCalls.get());
        assertEquals(0, newCalls.get());
        StepVerifier.create(request).verifyComplete();
        assertEquals(1, newCalls.get());
    }

    @Test
    void forkDepthIsBounded() {
        ShenyuPlugin recursive = plugin("recursive", (exchange, chain) -> chain.fork().execute(exchange));
        BoundGraphPlan plan = bind(linear("recursive"), List.of(recursive));
        StepVerifier.create(new GraphExecutor(2).execute(plan, exchange()))
                .expectErrorMatches(error -> error.getMessage().contains("fork depth")).verify();
    }

    @Test
    void legacyDoesNotConsultGraphAndGraphNeverUsesLegacyOnError() {
        AtomicInteger legacyCalls = new AtomicInteger();
        ShenyuPlugin legacy = counting("legacy", legacyCalls);
        StepVerifier.create(handler(List.of(legacy), new PluginExecutionDispatcher()).handle(exchange())).verifyComplete();
        assertEquals(1, legacyCalls.get());
        ShenyuWebHandler missing = handler(List.of(legacy), graphDispatcher(() -> null));
        StepVerifier.create(missing.handle(exchange())).expectError(ResponseStatusException.class).verify();
        ShenyuPlugin fail = plugin("fail", (exchange, chain) -> Mono.error(new IllegalStateException("failure")));
        BoundGraphPlan broken = bind(linear("fail"), List.of(fail));
        StepVerifier.create(handler(List.of(legacy), graphDispatcher(() -> broken)).handle(exchange()))
                .expectErrorMessage("failure").verify();
        assertEquals(1, legacyCalls.get());
    }

    @Test
    void rejectsMismatchedDeploymentAndInvalidConditionConfiguration() {
        GraphData graph = linear("leaf");
        graph.setDeploymentBinding("other");
        BoundGraphPlan plan = bind(graph, List.of(counting("leaf", new AtomicInteger())));
        StepVerifier.create(handler(List.of(), graphDispatcher(() -> plan)).handle(exchange())).expectError(ResponseStatusException.class).verify();
        graph.setDeploymentBinding(null);
        graph.setNamespaceId("another-namespace");
        BoundGraphPlan wrongNamespace = bind(graph, List.of(counting("leaf", new AtomicInteger())));
        StepVerifier.create(handler(List.of(), graphDispatcher(() -> wrongNamespace)).handle(exchange())).expectError(ResponseStatusException.class).verify();
        GraphNodeData choice = choice("choice", "end");
        GraphBranchData branch = new GraphBranchData(List.of(condition("yes")), 99, "end");
        choice.setConditions(List.of(branch));
        GraphData invalid = graph(List.of(choice, end()));
        assertThrows(RuntimeException.class, () -> bind(invalid, List.of()));
        branch.setMatchMode(0);
        branch.getConditions().get(0).setParamType("unknown");
        assertThrows(IllegalArgumentException.class, () -> bind(invalid, List.of()));
        assertThrows(GraphValidationException.class, () -> bind(linear("missing"), List.of()));
    }

    private PluginExecutionDispatcher graphDispatcher(final java.util.function.Supplier<BoundGraphPlan> provider) {
        return new PluginExecutionDispatcher(provider, "ns", "graph", "deployment", new GraphExecutor());
    }

    @Test
    void realPluginKeepsSelectorAndRuleMatching() {
        AtomicInteger processed = new AtomicInteger();
        final AtomicInteger downstream = new AtomicInteger();
        AbstractShenyuPlugin matching = new AbstractShenyuPlugin() {
            @Override
            protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                           final SelectorData selector, final RuleData rule) {
                processed.incrementAndGet();
                return chain.execute(exchange);
            }

            @Override
            public String named() {
                return "graphMatchingTest";
            }

            @Override
            public int getOrder() {
                return 0;
            }
        };
        ShenyuConfig config = new ShenyuConfig();
        config.getSelectorMatchCache().getCache().setEnabled(false);
        config.getRuleMatchCache().getCache().setEnabled(false);
        ReflectionTestUtils.setField(matching, "selectorMatchConfig", config.getSelectorMatchCache());
        ReflectionTestUtils.setField(matching, "ruleMatchConfig", config.getRuleMatchCache());
        final BoundGraphPlan plan = bind(linear(matching.named(), "leaf"), List.of(matching, counting("leaf", downstream)));
        BaseDataCache cache = BaseDataCache.getInstance();
        PluginData pluginData = new PluginData();
        pluginData.setName(matching.named());
        pluginData.setEnabled(true);
        cache.cachePluginData(pluginData);
        SelectorData selector = new SelectorData();
        selector.setId("graph-matching-selector");
        selector.setPluginName(matching.named());
        selector.setType(0);
        selector.setMatchMode(0);
        selector.setEnabled(true);
        selector.setContinued(true);
        selector.setLogged(false);
        selector.setSort(0);
        try {
            // No selector, then a selector with no rules: both must continue.
            StepVerifier.create(new GraphExecutor().execute(plan, exchange())).verifyComplete();
            cache.cacheSelectData(selector);
            StepVerifier.create(new GraphExecutor().execute(plan, exchange())).verifyComplete();
            assertEquals(0, processed.get());
            RuleData rule = new RuleData();
            rule.setId("graph-matching-rule");
            rule.setPluginName(matching.named());
            rule.setSelectorId(selector.getId());
            rule.setEnabled(true);
            rule.setSort(0);
            cache.cacheRuleData(rule);
            StepVerifier.create(new GraphExecutor().execute(plan, exchange())).verifyComplete();
            assertEquals(1, processed.get());
            assertEquals(3, downstream.get());
        } finally {
            cache.removeRuleDataBySelectorId(selector.getId());
            cache.removeSelectData(selector);
            cache.removePluginData(pluginData);
        }
    }

    private ShenyuWebHandler handler(final List<ShenyuPlugin> plugins, final PluginExecutionDispatcher dispatcher) {
        return new ShenyuWebHandler(plugins, mock(ShenyuLoaderService.class), new ShenyuConfig(), dispatcher);
    }

    private BoundGraphPlan bind(final GraphData graph, final List<ShenyuPlugin> plugins) {
        return new GraphPlanBinder().bind(graph, plugins);
    }

    private MockServerWebExchange exchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.get("/test"));
    }

    private ShenyuPlugin counting(final String name, final AtomicInteger counter) {
        return plugin(name, (exchange, chain) -> Mono.fromRunnable(counter::incrementAndGet).then(chain.execute(exchange)));
    }

    private ShenyuPlugin plugin(final String name, final BiFunction<ServerWebExchange, ShenyuPluginChain, Mono<Void>> action) {
        ShenyuPlugin plugin = mock(ShenyuPlugin.class);
        when(plugin.named()).thenReturn(name);
        when(plugin.execute(any(), any())).thenAnswer(invocation -> action.apply(invocation.getArgument(0), invocation.getArgument(1)));
        return plugin;
    }

    private GraphData linear(final String... names) {
        List<GraphNodeData> nodes = new ArrayList<>();
        for (int i = 0; i < names.length; i++) {
            nodes.add(node(names[i], i + 1 < names.length ? names[i + 1] : "end"));
        }
        nodes.add(end());
        return graph(nodes);
    }

    private GraphData graph(final List<GraphNodeData> nodes) {
        GraphData graph = new GraphData();
        graph.setSchemaVersion(1);
        graph.setGraphId("graph");
        graph.setNamespaceId("ns");
        graph.setRevision(1);
        graph.setContentHash("test");
        graph.setEntryNodeId(nodes.get(0).getNodeId());
        graph.setNodes(new ArrayList<>(nodes));
        return graph;
    }

    private GraphNodeData node(final String name, final String next) {
        GraphNodeData node = new GraphNodeData();
        node.setNodeId(name);
        node.setType("PLUGIN");
        node.setPluginName(name);
        node.setNext(next);
        return node;
    }

    private GraphNodeData choice(final String name, final String fallback) {
        GraphNodeData node = new GraphNodeData();
        node.setNodeId(name);
        node.setType("CHOICE");
        node.setDefaultTarget(fallback);
        return node;
    }

    private GraphNodeData end() {
        GraphNodeData node = new GraphNodeData();
        node.setNodeId("end");
        node.setType("END");
        return node;
    }

    private ConditionData condition(final String value) {
        ConditionData condition = new ConditionData();
        condition.setParamType("header");
        condition.setParamName("route");
        condition.setOperator("=");
        condition.setParamValue(value);
        return condition;
    }
}
