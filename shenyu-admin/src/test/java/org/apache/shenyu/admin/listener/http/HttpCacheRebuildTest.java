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

package org.apache.shenyu.admin.listener.http;

import jakarta.servlet.AsyncContext;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.config.properties.HttpSyncProperties;
import org.apache.shenyu.admin.listener.ConfigDataCache;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.listener.DataChangedEventDispatcher;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.h2.jdbcx.JdbcDataSource;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.support.TransactionTemplate;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Exercise committed independent events against H2, the real dispatcher and the HTTP cache.
 */
class HttpCacheRebuildTest {

    private TestListener listener;

    private JdbcTemplate jdbc;

    private TransactionTemplate transactions;

    private DataChangedEventDispatcher dispatcher;

    private DataChangedListener otherListener;

    private Queue<Runnable> tasks;

    private final AtomicInteger queries = new AtomicInteger();

    @BeforeEach
    void setUp() {
        JdbcDataSource source = new JdbcDataSource();
        source.setURL("jdbc:h2:mem:" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1");
        jdbc = new JdbcTemplate(source);
        jdbc.execute("CREATE TABLE test_rule (id VARCHAR PRIMARY KEY, namespace_id VARCHAR, name VARCHAR)");
        transactions = new TransactionTemplate(new DataSourceTransactionManager(source));
        HttpSyncProperties properties = new HttpSyncProperties();
        properties.setCacheRebuildWindow(Duration.ofMillis(10));
        listener = new TestListener(properties);
        listener.clearCache();
        HttpCacheRebuildCoordinator original = (HttpCacheRebuildCoordinator) ReflectionTestUtils.getField(listener, "cacheRebuildCoordinator");
        original.close();
        ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
        tasks = new ConcurrentLinkedQueue<>();
        doAnswer(invocation -> {
            tasks.add(invocation.getArgument(0));
            return null;
        }).when(executor).schedule(any(Runnable.class), eq(10L), eq(TimeUnit.MILLISECONDS));
        ReflectionTestUtils.setField(listener, "cacheRebuildCoordinator", new HttpCacheRebuildCoordinator(executor, 10, 2));
        RuleService rules = mock(RuleService.class);
        when(rules.listAllByNamespaceId(anyString())).thenAnswer(invocation -> {
            queries.incrementAndGet();
            String namespaceId = invocation.getArgument(0);
            return jdbc.query("SELECT id, namespace_id, name FROM test_rule WHERE namespace_id = ? ORDER BY id",
                    (row, index) -> rule(row.getString("id"), row.getString("namespace_id"), row.getString("name")), namespaceId);
        });
        ReflectionTestUtils.setField(listener, "ruleService", rules);
        SelectorService selectors = mock(SelectorService.class);
        when(selectors.listAllByNamespaceId(anyString())).thenReturn(Collections.emptyList());
        ReflectionTestUtils.setField(listener, "selectorService", selectors);
        otherListener = mock(DataChangedListener.class);
        dispatcher = new DataChangedEventDispatcher(mock(ApplicationContext.class));
        ReflectionTestUtils.setField(dispatcher, "clusterProperties", new ClusterProperties());
        ReflectionTestUtils.setField(dispatcher, "listeners", List.of(listener, otherListener));
    }

    @AfterEach
    void tearDown() {
        listener.destroy();
        listener.clearCache();
        jdbc.execute("SHUTDOWN");
    }

    @Test
    void independentConcurrentCommitsCoalesceWithoutDroppingEvents() throws Exception {
        ExecutorService writers = Executors.newFixedThreadPool(4);
        try {
            List<Future<?>> commits = new ArrayList<>();
            for (int index = 0; index < 40; index++) {
                String id = String.valueOf(index);
                commits.add(writers.submit(() -> commit(id, "a", "committed")));
            }
            for (Future<?> commit : commits) {
                commit.get(10, TimeUnit.SECONDS);
            }
        } finally {
            writers.shutdownNow();
        }
        assertEquals(0, queries.get());
        assertEquals(1, tasks.size());
        assertEquals(40, listener.callbacks.size());
        tasks.remove().run();
        assertEquals(1, queries.get());
        assertEquals(1, listener.serializations.get());
        assertEquals(40, listener.fetchConfig(ConfigGroupEnum.RULE, "a").getData().size());
        verify(otherListener, org.mockito.Mockito.times(40)).onRuleChanged(any(), eq(DataEventTypeEnum.UPDATE), any());
    }

    @Test
    void rollbackNeverSchedulesOrPublishesUncommittedData() {
        transactions.executeWithoutResult(status -> {
            jdbc.update("INSERT INTO test_rule VALUES (?, ?, ?)", "1", "a", "rolled-back");
            event(rule("1", "a", "rolled-back"), DataEventTypeEnum.UPDATE);
            assertTrue(tasks.isEmpty());
            assertEquals(0, queries.get());
            status.setRollbackOnly();
        });
        assertTrue(tasks.isEmpty());
        assertTrue(listener.callbacks.isEmpty());
        verify(otherListener, never()).onRuleChanged(any(), any(), any());
        commit("2", "a", "committed");
        tasks.remove().run();
        assertEquals("committed", ((RuleData) listener.fetchConfig(ConfigGroupEnum.RULE, "a").getData().get(0)).getName());
    }

    @Test
    void deleteThenUpdateUsesAuthoritativeSnapshotAndEmptyGroup() {
        commit("1", "a", "first");
        tasks.remove().run();
        transactions.executeWithoutResult(status -> {
            jdbc.update("DELETE FROM test_rule WHERE id = '1'");
            event(rule("1", "a", "first"), DataEventTypeEnum.DELETE);
        });
        commit("1", "a", "replacement");
        tasks.remove().run();
        assertEquals("replacement", ((RuleData) listener.fetchConfig(ConfigGroupEnum.RULE, "a").getData().get(0)).getName());
        transactions.executeWithoutResult(status -> {
            jdbc.update("DELETE FROM test_rule");
            event(rule("1", "a", "replacement"), DataEventTypeEnum.DELETE);
        });
        tasks.remove().run();
        assertTrue(listener.fetchConfig(ConfigGroupEnum.RULE, "a").getData().isEmpty());
        assertEquals(List.of(DataEventTypeEnum.UPDATE, DataEventTypeEnum.DELETE, DataEventTypeEnum.UPDATE, DataEventTypeEnum.DELETE), listener.callbacks);
    }

    @Test
    void namespacesGroupsAndCapacityFallbackRemainIndependent() {
        commit("1", "a", "a");
        commit("2", "b", "b");
        SelectorData selector = new SelectorData();
        selector.setNamespaceId("a");
        listener.onSelectorChanged(List.of(selector), DataEventTypeEnum.UPDATE);
        // Both pending slots are occupied. The new selector key rebuilds on the caller.
        assertTrue(listener.fetchConfig(ConfigGroupEnum.SELECTOR, "a").getData().isEmpty());
        assertEquals(2, tasks.size());
        tasks.remove().run();
        tasks.remove().run();
        assertEquals("a", ((RuleData) listener.fetchConfig(ConfigGroupEnum.RULE, "a").getData().get(0)).getName());
        assertEquals("b", ((RuleData) listener.fetchConfig(ConfigGroupEnum.RULE, "b").getData().get(0)).getName());
    }

    @Test
    void unchangedDigestPreservesVersion() {
        commit("1", "a", "same");
        tasks.remove().run();
        ConfigDataCache before = listener.cached("a");
        event(rule("1", "a", "same"), DataEventTypeEnum.UPDATE);
        tasks.remove().run();
        assertSame(before, listener.cached("a"));
    }

    @Test
    void disabledModeStillRebuildsSynchronouslyPerEvent() {
        ((HttpCacheRebuildCoordinator) ReflectionTestUtils.getField(listener, "cacheRebuildCoordinator")).close();
        ReflectionTestUtils.setField(listener, "cacheRebuildCoordinator", null);
        commit("1", "a", "first");
        commit("2", "a", "second");
        assertEquals(2, queries.get());
        assertEquals(2, listener.serializations.get());
        assertEquals(2, listener.fetchConfig(ConfigGroupEnum.RULE, "a").getData().size());
        assertTrue(tasks.isEmpty());
    }

    @Test
    void commitRacingShutdownRebuildsSynchronouslyWithoutFailingTheCommit() {
        listener.destroy();
        commit("1", "a", "after-shutdown");
        assertTrue(tasks.isEmpty());
        assertEquals(1, queries.get());
        assertEquals("after-shutdown", ((RuleData) listener.fetchConfig(ConfigGroupEnum.RULE, "a").getData().get(0)).getName());
        assertEquals(List.of(DataEventTypeEnum.UPDATE), listener.callbacks);
    }

    @Test
    void configuredWorkerThreadsSizeTheRebuildExecutor() {
        HttpSyncProperties properties = new HttpSyncProperties();
        properties.setCacheRebuildWindow(Duration.ofMillis(10));
        properties.setCacheRebuildThreads(3);
        TestListener sized = new TestListener(properties);
        try {
            HttpCacheRebuildCoordinator coordinator = (HttpCacheRebuildCoordinator) ReflectionTestUtils.getField(sized, "cacheRebuildCoordinator");
            assertEquals(3, ((java.util.concurrent.ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(coordinator, "executor")).getCorePoolSize());
        } finally {
            sized.destroy();
        }
    }

    @Test
    void longPollIsWokenOnlyAfterPublicationAndOnlyForItsNamespace() throws Exception {
        AsyncContext client = client("a");
        final AsyncContext other = client("b");
        scheduler().submit(() -> { }).get(5, TimeUnit.SECONDS);
        commit("1", "a", "visible-before-notification");
        scheduler().submit(() -> { }).get(5, TimeUnit.SECONDS);
        verify(client, never()).complete();
        tasks.remove().run();
        scheduler().submit(() -> { }).get(5, TimeUnit.SECONDS);
        verify(client).complete();
        verify(other, never()).complete();
        assertTrue(((MockHttpServletResponse) client.getResponse()).getContentAsString().contains("RULE"));
        assertEquals(1, listener.fetchConfig(ConfigGroupEnum.RULE, "a").getData().size());
    }

    private AsyncContext client(final String namespace) {
        AsyncContext context = mock(AsyncContext.class);
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.setParameter("namespaceId", namespace);
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            listener.seedCache(group, namespace);
            request.setParameter(group.name(), "seed,0");
        }
        when(context.getRequest()).thenReturn(request);
        when(context.getResponse()).thenReturn(new MockHttpServletResponse());
        scheduler().execute(listener.new LongPollingClient(context, "127.0.0.1", 30000, namespace));
        return context;
    }

    @Test
    void registrationRechecksChangesPublishedAfterTheInitialComparison() throws Exception {
        commit("1", "a", "changed-before-registration");
        tasks.remove().run();
        scheduler().submit(() -> { }).get(5, TimeUnit.SECONDS);
        AsyncContext lateClient = client("a");
        scheduler().submit(() -> { }).get(5, TimeUnit.SECONDS);
        verify(lateClient).complete();
        assertTrue(((MockHttpServletResponse) lateClient.getResponse()).getContentAsString().contains("RULE"));
    }

    private ScheduledExecutorService scheduler() {
        return (ScheduledExecutorService) ReflectionTestUtils.getField(listener, "scheduler");
    }

    private void commit(final String id, final String namespace, final String name) {
        transactions.executeWithoutResult(status -> {
            jdbc.update("INSERT INTO test_rule VALUES (?, ?, ?)", id, namespace, name);
            event(rule(id, namespace, name), DataEventTypeEnum.UPDATE);
        });
    }

    private void event(final RuleData rule, final DataEventTypeEnum type) {
        dispatcher.onApplicationEvent(new DataChangedEvent(ConfigGroupEnum.RULE, type, List.of(rule)));
    }

    private static RuleData rule(final String id, final String namespace, final String name) {
        RuleData rule = new RuleData();
        rule.setId(id);
        rule.setNamespaceId(namespace);
        rule.setName(name);
        return rule;
    }

    private static final class TestListener extends HttpLongPollingDataChangedListener {

        private final AtomicInteger serializations = new AtomicInteger();

        private final List<DataEventTypeEnum> callbacks = Collections.synchronizedList(new ArrayList<>());

        private TestListener(final HttpSyncProperties properties) {
            super(properties);
        }

        @Override
        protected <T> void updateCache(final ConfigGroupEnum group, final List<T> data, final String namespaceId) {
            serializations.incrementAndGet();
            super.updateCache(group, data, namespaceId);
        }

        @Override
        protected void afterRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
            callbacks.add(eventType);
            super.afterRuleChanged(changed, eventType, namespaceId);
        }

        private ConfigDataCache cached(final String namespaceId) {
            return CACHE.get(buildCacheKey(namespaceId, ConfigGroupEnum.RULE.name()));
        }

        private void seedCache(final ConfigGroupEnum group, final String namespaceId) {
            String key = buildCacheKey(namespaceId, group.name());
            CACHE.putIfAbsent(key, new ConfigDataCache(key, "[]", "seed", 0, namespaceId));
        }

        private void clearCache() {
            CACHE.clear();
        }
    }
}
