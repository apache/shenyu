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

package org.apache.shenyu.plugin.base;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategyFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

class PublishedMatchCacheRefillTest {

    private static final String PLUGIN = "published-refill";

    private static final String PATH = "/refill";

    private static final String OLD = "old";

    private static final String NEW = "new";

    private final BaseDataCache base = BaseDataCache.getInstance();

    private final MatchDataCache matches = MatchDataCache.getInstance();

    private final RecordingPlugin plugin = new RecordingPlugin();

    @BeforeEach
    void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuConfig.class)).thenReturn(new ShenyuConfig());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        clear();
        base.cachePluginData(PluginData.builder().name(PLUGIN).enabled(true).build());
    }

    @AfterEach
    void clear() {
        base.removePluginDataByPluginName(PLUGIN);
        base.removeSelectDataByPluginName(PLUGIN);
        base.removeRuleDataBySelectorId(OLD);
        matches.removeSelectorData(PLUGIN);
        matches.removeRuleData(PLUGIN);
    }

    @Test
    void doesNotRefillPositiveSelectorAfterBetterCandidateIsPublished() throws Exception {
        verifySelectorRefill(true);
    }

    @Test
    void doesNotRefillNegativeSelectorAfterMatchingCandidateIsPublished() throws Exception {
        verifySelectorRefill(false);
    }

    @Test
    void doesNotRefillPositiveRuleAfterBetterCandidateIsPublished() throws Exception {
        verifyRuleRefill(true);
    }

    @Test
    void doesNotRefillNegativeRuleAfterMatchingCandidateIsPublished() throws Exception {
        verifyRuleRefill(false);
    }

    @Test
    void cachesUnchangedCandidatesAndInvalidatesEarlierWrites() {
        base.cacheSelectData(selector(OLD, true, List.of(uri(PATH))));
        base.cacheRuleData(rule(OLD, List.of(uri(PATH))));
        assertEquals(OLD, request().getAttribute("rule"));
        assertSame(base.readOnly().obtainSelectorData(PLUGIN).get(0), matches.readOnly().obtainSelectorData(PLUGIN, PATH));
        assertSame(base.readOnly().obtainRuleData(OLD).get(0), matches.readOnly().obtainRuleData(PLUGIN, PATH));
        base.cacheRuleData(rule(OLD, List.of(uri("/other"))));
        matches.removeRuleData(PLUGIN, OLD);
        assertNull(matches.readOnly().obtainRuleData(PLUGIN, PATH));
        assertNull(request().getAttribute("rule"));
        assertNotNull(matches.readOnly().obtainRuleData(PLUGIN, PATH));
        assertNull(matches.readOnly().obtainRuleData(PLUGIN, PATH).getId());
        base.cacheSelectData(selector(OLD, true, List.of(uri("/other"))));
        matches.cleanSelectorData();
        assertNull(matches.readOnly().obtainSelectorData(PLUGIN, PATH));
        assertNull(request().getAttribute("selector"));
        assertNotNull(matches.readOnly().obtainSelectorData(PLUGIN, PATH));
        assertNull(matches.readOnly().obtainSelectorData(PLUGIN, PATH).getId());
    }

    @Test
    void ruleRefillAlsoRejectsAChangedSelectorCandidateSet() throws Exception {
        base.cacheSelectData(selector(OLD, true, List.of(uri(PATH))));
        ConditionData original = uri("/*");
        base.cacheRuleData(rule(OLD, List.of(original)));
        List<RuleData> unchangedRules = base.readOnly().obtainRuleData(OLD);
        duringMatch(original, () -> {
            base.cacheSelectData(selector(NEW, false, List.of(uri(PATH), uri(PATH))));
            assertSame(unchangedRules, base.readOnly().obtainRuleData(OLD));
            matches.removeSelectorData(PLUGIN);
            matches.removeRuleData(PLUGIN);
        });
        assertNull(matches.readOnly().obtainRuleData(PLUGIN, PATH));
        assertEquals(NEW, request().getAttribute("selector"));
    }

    @Test
    void invalidationCannotSplitSelectorValidationAndStore() throws Exception {
        base.cacheSelectData(selector(OLD, false, List.of(uri(PATH))));
        verifyAtomicStore(true);
    }

    @Test
    void cleanCannotSplitRuleValidationAndStore() throws Exception {
        base.cacheSelectData(selector(OLD, true, List.of(uri(PATH))));
        base.cacheRuleData(rule(OLD, List.of(uri("/*"))));
        verifyAtomicStore(false);
    }

    private void verifyAtomicStore(final boolean selector) throws Exception {
        final MatchDataCache published = (MatchDataCache) ReflectionTestUtils.getField(MatchDataCache.class, "PUBLISHED");
        CountDownLatch storing = new CountDownLatch(1);
        CountDownLatch resume = new CountDownLatch(1);
        Map<String, Object> bucket = new ConcurrentHashMap<>() {
            @Override
            public Object put(final String key, final Object value) {
                storing.countDown();
                try {
                    assertTrue(resume.await(10, TimeUnit.SECONDS));
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
                    throw new IllegalStateException(ex);
                }
                return super.put(key, value);
            }
        };
        Map<String, Map<String, Object>> buckets = (Map<String, Map<String, Object>>) ReflectionTestUtils
                .getField(published, selector ? "selectorDataMap" : "ruleDataMap");
        buckets.put(PLUGIN, bucket);
        ExecutorService workers = Executors.newFixedThreadPool(2);
        AtomicReference<Thread> invalidator = new AtomicReference<>();
        CountDownLatch invalidating = new CountDownLatch(1);
        try {
            final Future<ServerWebExchange> pending = workers.submit(this::request);
            assertTrue(storing.await(10, TimeUnit.SECONDS), "request did not reach cache put");
            final Future<?> invalidated = workers.submit(() -> {
                invalidator.set(Thread.currentThread());
                invalidating.countDown();
                if (selector) {
                    matches.removeSelectorData(PLUGIN, OLD);
                } else {
                    matches.cleanRuleDataData();
                }
            });
            assertTrue(invalidating.await(10, TimeUnit.SECONDS));
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
            while (invalidator.get().getState() != Thread.State.BLOCKED && !invalidated.isDone() && System.nanoTime() < deadline) {
                Thread.onSpinWait();
            }
            assertFalse(invalidated.isDone(), "invalidation overtook a validated store");
            assertEquals(Thread.State.BLOCKED, invalidator.get().getState());
            resume.countDown();
            pending.get(10, TimeUnit.SECONDS);
            invalidated.get(10, TimeUnit.SECONDS);
            if (selector) {
                assertNull(matches.readOnly().obtainSelectorData(PLUGIN, PATH));
            } else {
                assertNull(matches.readOnly().obtainRuleData(PLUGIN, PATH));
            }
        } finally {
            resume.countDown();
            workers.shutdownNow();
            assertTrue(workers.awaitTermination(10, TimeUnit.SECONDS));
        }
    }

    private void verifySelectorRefill(final boolean positive) throws Exception {
        ConditionData original = uri(positive ? PATH : "/other");
        base.cacheSelectData(selector(OLD, false, List.of(original)));
        SelectorData unchanged = base.readOnly().obtainSelectorData(PLUGIN).get(0);
        ServerWebExchange oldRequest = duringMatch(original, () -> {
            base.cacheSelectData(selector(NEW, false, List.of(uri(PATH), uri(PATH))));
            assertSame(unchanged, base.readOnly().obtainSelectorData(PLUGIN).get(0));
            matches.removeSelectorData(PLUGIN);
        });
        assertEquals(positive ? OLD : null, oldRequest.getAttribute("selector"));
        assertNull(matches.readOnly().obtainSelectorData(PLUGIN, PATH), "obsolete selector result refilled the cache");
        assertEquals(NEW, request().getAttribute("selector"));
    }

    private void verifyRuleRefill(final boolean positive) throws Exception {
        base.cacheSelectData(selector(OLD, true, List.of(uri(PATH))));
        ConditionData original = uri(positive ? "/*" : "/other");
        base.cacheRuleData(rule(OLD, List.of(original)));
        RuleData unchanged = base.readOnly().obtainRuleData(OLD).get(0);
        ServerWebExchange oldRequest = duringMatch(original, () -> {
            base.cacheRuleData(rule(NEW, List.of(uri(PATH), uri(PATH))));
            assertSame(unchanged, base.readOnly().obtainRuleData(OLD).get(0));
            matches.removeRuleData(PLUGIN);
        });
        assertEquals(positive ? OLD : null, oldRequest.getAttribute("rule"));
        assertNull(matches.readOnly().obtainRuleData(PLUGIN, PATH), "obsolete rule result refilled the cache");
        assertEquals(NEW, request().getAttribute("rule"));
    }

    private ServerWebExchange duringMatch(final ConditionData original, final Runnable publishAndInvalidate) throws Exception {
        CountDownLatch matched = new CountDownLatch(1);
        CountDownLatch resume = new CountDownLatch(1);
        ExecutorService worker = Executors.newSingleThreadExecutor();
        try {
            final Future<ServerWebExchange> pending = worker.submit(() -> {
                // Static interception is local to the worker; evaluate the real predicate before pausing.
                try (MockedStatic<MatchStrategyFactory> ignored = mockStatic(MatchStrategyFactory.class, invocation -> {
                    Object result = invocation.callRealMethod();
                    if (invocation.getMethod().getName().equals("match")
                            && ((List<ConditionData>) invocation.getArgument(1)).get(0).getParamValue().equals(original.getParamValue())) {
                        matched.countDown();
                        assertTrue(resume.await(10, TimeUnit.SECONDS), "publication did not complete");
                    }
                    return result;
                })) {
                    return request();
                }
            });
            assertTrue(matched.await(10, TimeUnit.SECONDS), "request did not evaluate the old candidate");
            publishAndInvalidate.run();
            resume.countDown();
            return pending.get(10, TimeUnit.SECONDS);
        } finally {
            resume.countDown();
            worker.shutdownNow();
            assertTrue(worker.awaitTermination(10, TimeUnit.SECONDS));
        }
    }

    private ServerWebExchange request() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get(PATH));
        plugin.execute(exchange, ignored -> Mono.empty()).block();
        return exchange;
    }

    private SelectorData selector(final String id, final boolean continued, final List<ConditionData> conditions) {
        return SelectorData.builder().id(id).pluginName(PLUGIN).enabled(true).logged(false).continued(continued)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).matchMode(0).sort(OLD.equals(id) ? 0 : 1).conditionList(conditions).build();
    }

    private RuleData rule(final String id, final List<ConditionData> conditions) {
        return RuleData.builder().id(id).pluginName(PLUGIN).selectorId(OLD).enabled(true).logged(false)
                .matchMode(0).sort(OLD.equals(id) ? 0 : 1).conditionDataList(conditions).build();
    }

    private ConditionData uri(final String pattern) {
        ConditionData condition = new ConditionData();
        condition.setParamType("uri");
        condition.setOperator("match");
        condition.setParamName("/");
        condition.setParamValue(pattern);
        return condition;
    }

    private static final class RecordingPlugin extends AbstractShenyuPlugin {

        @Override
        protected boolean useReadOnlyConfiguration() {
            return true;
        }

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
            exchange.getAttributes().put("selector", selector.getId());
            exchange.getAttributes().put("rule", rule.getId());
            return Mono.empty();
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return PLUGIN;
        }
    }
}
