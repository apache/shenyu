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

package org.apache.shenyu.admin.service.fencing;

import org.apache.shenyu.admin.config.properties.RegistrationFencingProperties;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.RegistrationFencingTargetMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RegistrationFencingTargetDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.service.DataPermissionChecker;
import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.impl.DiscoveryUpstreamServiceImpl;
import org.apache.shenyu.admin.service.impl.SelectorServiceImpl;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.service.publish.SelectorEventPublisher;
import org.apache.shenyu.admin.service.register.FencedRegisterService;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterDivideServiceImpl;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.junit.jupiter.api.Test;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.transaction.annotation.AnnotationTransactionAttributeSource;
import org.springframework.transaction.interceptor.TransactionInterceptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol.HEARTBEAT;
import static org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol.OFFLINE;
import static org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol.REGISTER;
import static org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol.UPDATE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Verifies fencing state and row locking through independent services and a shared H2 database.
 */
final class RegistrationFencingPersistenceTest {

    private static final String DOMAIN = "domain-a";

    private static final String PRINCIPAL = "principal-a";

    private static final String SELECTOR_ID = "fencing-selector";

    private static final String TARGET_ID = "fencing-target";

    private static final String STARTUP_A = "6f9619ff-8b86-d011-b42d-00cf4fc964ff";

    private static final String STARTUP_B = "16fd2706-8baf-433b-82eb-8c7fada847da";

    @Test
    void readinessPersistsThroughFencedUpdatesAndRejectsTheReplacedSession() throws Exception {
        TestDatabase database = new TestDatabase();
        try {
            database.seedTargetSource();
            database.seedOwnership();
            database.jdbc.update("UPDATE discovery_upstream SET props = ? WHERE id = 'fencing-upstream'", "{\"warmup\":\"20\"}");
            RegistrationFencingService admin = database.service(database.readinessWriter());
            RegistrationFencingProtocol.SessionData sessionA = admin.acquire(acquire(STARTUP_A, "0"), PRINCIPAL).data();
            assertMutation(admin, sessionA, "1", REGISTER, EventType.REGISTER);
            assertReadiness(database, false, false);

            assertMutation(admin, sessionA, "2", UPDATE, EventType.NOT_READY);
            assertReadiness(database, true, false);
            assertMutation(admin, sessionA, "2", UPDATE, EventType.NOT_READY);
            assertEquals(2L, database.targets().selectById(TARGET_ID).getLastSequence());
            assertMutation(admin, sessionA, "3", HEARTBEAT, EventType.HEARTBEAT);
            assertReadiness(database, true, false);
            assertMutation(admin, sessionA, "4", UPDATE, EventType.DRAINING);
            assertReadiness(database, true, true);
            assertMutation(admin, sessionA, "5", UPDATE, EventType.READY);
            assertReadiness(database, false, false);

            RegistrationFencingProtocol.SessionData sessionB = admin.replace(TARGET_ID,
                    new RegistrationFencingProtocol.ReplaceRequest(UUID.randomUUID().toString(), STARTUP_A,
                            sessionA.generation(), STARTUP_B, PRINCIPAL), true).data();
            assertNotNull(sessionB);
            assertMutation(admin, sessionB, "1", UPDATE, EventType.NOT_READY);
            assertReadiness(database, true, false);
            for (EventType eventType : List.of(EventType.READY, EventType.NOT_READY)) {
                URIRegisterDTO payload = uri();
                payload.setEventType(eventType);
                assertStale(admin, new RegistrationFencingProtocol.MutationRequest(DOMAIN, sessionA.sessionId(),
                        SELECTOR_ID, sessionA.generation(), "6", UPDATE, payload));
                assertReadiness(database, true, false);
                assertEquals(1L, database.targets().selectById(TARGET_ID).getLastSequence());
            }
            assertMutation(admin, sessionB, "2", UPDATE, EventType.READY);
            assertReadiness(database, false, false);
        } finally {
            database.close();
        }
    }

    private void assertMutation(final RegistrationFencingService admin, final RegistrationFencingProtocol.SessionData session,
                                final String sequence, final String operation, final EventType eventType) {
        URIRegisterDTO payload = uri();
        payload.setEventType(eventType);
        assertEquals(RegistrationFencingProtocol.OK, admin.mutate(new RegistrationFencingProtocol.MutationRequest(
                DOMAIN, session.sessionId(), SELECTOR_ID, session.generation(), sequence, operation, payload), PRINCIPAL).code());
    }

    private void assertReadiness(final TestDatabase database, final boolean notReady, final boolean draining) {
        String serialized = database.jdbc.queryForObject("SELECT props FROM discovery_upstream WHERE id = 'fencing-upstream'", String.class);
        Map<String, Object> props = GsonUtils.getInstance().convertToMap(serialized);
        assertEquals(notReady, Boolean.TRUE.equals(props.get("notReady")));
        assertEquals(draining, Boolean.TRUE.equals(props.get("draining")));
        assertEquals("20", props.get("warmup"));
    }

    @Test
    void staleSessionCannotMutateReplacementAndOwnershipSurvivesServiceRestart() throws Exception {
        TestDatabase database = new TestDatabase();
        try {
            database.seedTarget();
            FencedRegisterService writer = registerService();
            when(writer.offlineWithoutFallback(anyString(), anyList(), anyString())).thenReturn("SUCCESS");
            RegistrationFencingService firstAdmin = database.service(writer);
            RegistrationFencingService secondAdmin = database.service(writer);

            RegistrationFencingProtocol.SessionData sessionA = firstAdmin.acquire(acquire(STARTUP_A, "0"), PRINCIPAL).data();
            RegistrationFencingProtocol.Response<RegistrationFencingProtocol.MutationData> offlineA = firstAdmin.mutate(
                    mutation(sessionA, "1", OFFLINE), PRINCIPAL);
            assertEquals(RegistrationFencingProtocol.OK, offlineA.code());

            RegistrationFencingProtocol.SessionData sessionB = secondAdmin.acquire(acquire(STARTUP_B, "2"), PRINCIPAL).data();
            assertNotEquals(sessionA.sessionId(), sessionB.sessionId());
            assertEquals("3", sessionB.generation());

            assertStale(firstAdmin, mutation(sessionA, "2", HEARTBEAT));
            assertStale(firstAdmin, mutation(sessionA, "2", OFFLINE));
            assertStale(firstAdmin, mutation(sessionA, "2", UPDATE));

            RegistrationFencingTargetDO persisted = database.targets().selectById(TARGET_ID);
            assertEquals("ACTIVE", persisted.getState());
            assertEquals(sessionB.sessionId(), persisted.getSessionId());
            assertEquals(STARTUP_B, persisted.getOwnerStartupId());
            assertEquals(3L, persisted.getGeneration());

            RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData> restartedOwner =
                    database.service(writer).acquire(acquire(STARTUP_B, "0"), PRINCIPAL);
            assertEquals(RegistrationFencingProtocol.OK, restartedOwner.code());
            assertEquals(sessionB.sessionId(), restartedOwner.data().sessionId());
            assertEquals(RegistrationFencingProtocol.OWNER_CONFLICT,
                    database.service(writer).acquire(acquire(STARTUP_A, "3"), PRINCIPAL).code());
            verify(writer, never()).heartbeatWithoutFallback(anyString(), anyList(), anyString());
            verify(writer, times(1)).offlineWithoutFallback(eq("/fenced"), anyList(), anyString());
            verify(writer, never()).drainingWithoutFallback(anyString(), anyList(), anyString());
            verify(writer, never()).registerURIWithoutFallback(anyString(), anyList(), anyString());
        } finally {
            database.close();
        }
    }

    @Test
    void concurrentAdminsSerializeCompetingStartupAcquires() throws Exception {
        TestDatabase database = new TestDatabase();
        ExecutorService workers = Executors.newFixedThreadPool(2);
        try {
            database.seedTarget();
            FencedRegisterService writer = registerService();
            RegistrationFencingService firstAdmin = database.service(writer);
            RegistrationFencingService secondAdmin = database.service(writer);
            CyclicBarrier start = new CyclicBarrier(2);
            Future<RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData>> first = workers.submit(
                    () -> acquireAtBarrier(firstAdmin, STARTUP_A, start));
            Future<RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData>> second = workers.submit(
                    () -> acquireAtBarrier(secondAdmin, STARTUP_B, start));

            RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData> firstResult = first.get(10, TimeUnit.SECONDS);
            RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData> secondResult = second.get(10, TimeUnit.SECONDS);
            assertNotEquals(firstResult.code(), secondResult.code());
            assertEquals(RegistrationFencingProtocol.OK,
                    RegistrationFencingProtocol.OK.equals(firstResult.code()) ? firstResult.code() : secondResult.code());
            assertEquals(RegistrationFencingProtocol.OWNER_CONFLICT,
                    RegistrationFencingProtocol.OK.equals(firstResult.code()) ? secondResult.code() : firstResult.code());
            RegistrationFencingTargetDO persisted = database.targets().selectById(TARGET_ID);
            assertEquals("ACTIVE", persisted.getState());
            assertNotNull(persisted.getSessionId());
            assertEquals(1L, persisted.getGeneration());
        } finally {
            workers.shutdownNow();
            database.close();
        }
    }

    @Test
    void managementWriteCannotCommitAfterConcurrentTargetCreation() throws Exception {
        TestDatabase database = new TestDatabase();
        ExecutorService workers = Executors.newFixedThreadPool(2);
        CountDownLatch managementGuardPassed = new CountDownLatch(1);
        CountDownLatch continueManagementWrite = new CountDownLatch(1);
        CountDownLatch targetCreationStarted = new CountDownLatch(1);
        try {
            database.seedTargetSource();
            RegistrationFencingGuard pausingGuard = new RegistrationFencingGuard(database.targets(), database.selectors(), database.plugins()) {
                @Override
                public void assertSelectorWriteAllowed(final String selectorId) {
                    super.assertSelectorWriteAllowed(selectorId);
                    managementGuardPassed.countDown();
                    await(continueManagementWrite);
                }
            };
            SelectorService management = database.selectorService(pausingGuard);
            final RegistrationFencingService registration = database.service(registerService());
            SelectorDO update = new SelectorDO();
            update.setId(SELECTOR_ID);
            update.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
            update.setHandle("[\"management\"]");

            final Future<Integer> managementWrite = workers.submit(() -> management.updateSelective(update));
            assertTrue(managementGuardPassed.await(5, TimeUnit.SECONDS));
            Future<RegistrationFencingProtocol.Response<RegistrationFencingProtocol.TargetData>> targetCreation = workers.submit(() -> {
                targetCreationStarted.countDown();
                return registration.createTarget(targetRequest(), true);
            });
            assertTrue(targetCreationStarted.await(5, TimeUnit.SECONDS));
            Thread.sleep(200);
            assertFalse(targetCreation.isDone());

            continueManagementWrite.countDown();
            assertEquals(1, managementWrite.get(10, TimeUnit.SECONDS));
            assertEquals(RegistrationFencingProtocol.OK, targetCreation.get(10, TimeUnit.SECONDS).code());
            assertEquals("[\"management\"]", database.selectors().selectById(SELECTOR_ID).getHandle());

            SelectorDO lateUpdate = new SelectorDO();
            lateUpdate.setId(SELECTOR_ID);
            lateUpdate.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
            lateUpdate.setHandle("[\"late\"]");
            assertThrows(RegistrationFencingException.class, () -> management.updateSelective(lateUpdate));
            assertEquals("[\"management\"]", database.selectors().selectById(SELECTOR_ID).getHandle());
        } finally {
            continueManagementWrite.countDown();
            workers.shutdownNow();
            workers.awaitTermination(5, TimeUnit.SECONDS);
            database.close();
        }
    }

    private void await(final CountDownLatch latch) {
        try {
            if (!latch.await(5, TimeUnit.SECONDS)) {
                throw new IllegalStateException("timed out waiting to continue selector management write");
            }
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("interrupted while waiting to continue selector management write", exception);
        }
    }

    private RegistrationFencingProtocol.TargetRequest targetRequest() {
        return new RegistrationFencingProtocol.TargetRequest(DOMAIN, SYS_DEFAULT_NAMESPACE_ID, SELECTOR_ID,
                "http", "127.0.0.1", 18080, PRINCIPAL);
    }

    private RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData> acquireAtBarrier(
            final RegistrationFencingService service, final String startupId, final CyclicBarrier start) throws Exception {
        start.await(5, TimeUnit.SECONDS);
        return service.acquire(acquire(startupId, "0"), PRINCIPAL);
    }

    private void assertStale(final RegistrationFencingService service,
                             final RegistrationFencingProtocol.MutationRequest request) {
        assertEquals(RegistrationFencingProtocol.STALE_SESSION, service.mutate(request, PRINCIPAL).code());
    }

    private RegistrationFencingProtocol.AcquireRequest acquire(final String startupId, final String generation) {
        return new RegistrationFencingProtocol.AcquireRequest(DOMAIN, TARGET_ID, startupId, generation);
    }

    private RegistrationFencingProtocol.MutationRequest mutation(final RegistrationFencingProtocol.SessionData session,
                                                                  final String sequence, final String operation) {
        return new RegistrationFencingProtocol.MutationRequest(DOMAIN, session.sessionId(), SELECTOR_ID,
                session.generation(), sequence, operation, uri());
    }

    private URIRegisterDTO uri() {
        return URIRegisterDTO.builder().appName("fenced").rpcType("http").protocol("http")
                .host("127.0.0.1").port(18080).namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
    }

    private FencedRegisterService registerService() {
        FencedRegisterService service = mock(FencedRegisterService.class);
        when(service.rpcType()).thenReturn("http");
        return service;
    }

    private static final class TestDatabase {

        private final DriverManagerDataSource dataSource;

        private final JdbcTemplate jdbc;

        private final SqlSessionTemplate sessions;

        private final RegistrationFencingTargetMapper targets;

        private final SelectorMapper selectors;

        private final PluginMapper plugins;

        private TestDatabase() throws Exception {
            dataSource = new DriverManagerDataSource("jdbc:h2:mem:fencing-" + UUID.randomUUID()
                    + ";MODE=MySQL;DB_CLOSE_DELAY=-1;LOCK_TIMEOUT=5000", "sa", "");
            new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
            jdbc = new JdbcTemplate(dataSource);
            SqlSessionFactoryBean factory = new SqlSessionFactoryBean();
            factory.setDataSource(dataSource);
            factory.setMapperLocations(new ClassPathResource("mappers/selector-sqlmap.xml"),
                    new ClassPathResource("mappers/registration-fencing-target-sqlmap.xml"),
                    new ClassPathResource("mappers/discovery-upstream-sqlmap.xml"),
                    new ClassPathResource("mappers/discovery-handler-sqlmap.xml"),
                    new ClassPathResource("mappers/discovery-rel-sqlmap.xml"));
            sessions = new SqlSessionTemplate(factory.getObject());
            targets = sessions.getMapper(RegistrationFencingTargetMapper.class);
            selectors = sessions.getMapper(SelectorMapper.class);
            plugins = mock(PluginMapper.class);
            PluginDO plugin = PluginDO.builder().id("fencing-plugin").name("divide").build();
            when(plugins.selectById("fencing-plugin")).thenReturn(plugin);
            when(plugins.selectByName("divide")).thenReturn(plugin);
            when(plugins.selectByNameForUpdate("divide")).thenReturn(plugin);
        }

        private RegistrationFencingTargetMapper targets() {
            return targets;
        }

        private SelectorMapper selectors() {
            return selectors;
        }

        private PluginMapper plugins() {
            return plugins;
        }

        private SelectorService selectorService(final RegistrationFencingGuard guard) {
            SelectorServiceImpl target = new SelectorServiceImpl(selectors, mock(SelectorConditionMapper.class), plugins,
                    mock(ApplicationEventPublisher.class), mock(DiscoveryMapper.class), mock(DiscoveryHandlerMapper.class),
                    mock(DiscoveryRelMapper.class), mock(DiscoveryUpstreamMapper.class), mock(DiscoveryProcessorHolder.class),
                    mock(SelectorEventPublisher.class), mock(DataPermissionChecker.class), guard);
            ProxyFactory proxy = new ProxyFactory(target);
            proxy.addAdvice(new TransactionInterceptor(new DataSourceTransactionManager(dataSource),
                    new AnnotationTransactionAttributeSource()));
            return (SelectorService) proxy.getProxy();
        }

        private void seedTargetSource() {
            jdbc.update("INSERT INTO selector (id, plugin_id, selector_name, match_mode, selector_type, sort_code, handle, enabled, logged, continued, match_restful, namespace_id)"
                    + " VALUES (?, ?, '/fenced', 0, 0, 1, '[]', 1, 0, 0, 0, ?)", SELECTOR_ID, "fencing-plugin", SYS_DEFAULT_NAMESPACE_ID);
            jdbc.update("INSERT INTO discovery_handler (id, discovery_id, handler) VALUES (?, ?, ?)", "fencing-handler", "fencing-discovery", "[]");
            jdbc.update("INSERT INTO discovery_upstream (id, discovery_handler_id, namespace_id, protocol, upstream_url, upstream_status, weight)"
                    + " VALUES (?, ?, ?, 'http', '127.0.0.1:18080', 0, 100)", "fencing-upstream", "fencing-handler", SYS_DEFAULT_NAMESPACE_ID);
            jdbc.update("INSERT INTO discovery_rel (id, plugin_name, discovery_handler_id, selector_id) VALUES (?, ?, ?, ?)",
                    "fencing-rel", "divide", "fencing-handler", SELECTOR_ID);
        }

        private void seedTarget() {
            jdbc.update("INSERT INTO selector (id, plugin_id, selector_name, match_mode, selector_type, sort_code, handle, enabled, logged, continued, match_restful, namespace_id)"
                    + " VALUES (?, ?, ?, 0, 0, 1, '[]', 1, 0, 0, 0, ?)",
                    SELECTOR_ID, "fencing-plugin", "/fenced", SYS_DEFAULT_NAMESPACE_ID);
            seedOwnership();
        }

        private void seedOwnership() {
            Timestamp now = new Timestamp(System.currentTimeMillis());
            RegistrationFencingTargetDO target = new RegistrationFencingTargetDO();
            target.setId(TARGET_ID);
            target.setDateCreated(now);
            target.setDateUpdated(now);
            target.setDomainId(DOMAIN);
            target.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
            target.setSelectorId(SELECTOR_ID);
            target.setPluginId("fencing-plugin");
            target.setProtocol("http");
            target.setHost("127.0.0.1");
            target.setPort(18080);
            target.setPrincipalId(PRINCIPAL);
            target.setEnabled(true);
            target.setState("NEVER");
            target.setGeneration(0L);
            target.setLastSequence(0L);
            targets.insert(target);
        }

        private FencedRegisterService readinessWriter() {
            RegistrationFencingGuard guard = new RegistrationFencingGuard(targets, selectors, plugins);
            final DiscoveryUpstreamServiceImpl upstreamService = new DiscoveryUpstreamServiceImpl(
                    sessions.getMapper(DiscoveryUpstreamMapper.class), sessions.getMapper(DiscoveryHandlerMapper.class),
                    mock(ProxySelectorMapper.class), mock(DiscoveryMapper.class), sessions.getMapper(DiscoveryRelMapper.class),
                    selectors, plugins, mock(DiscoveryProcessorHolder.class), guard);
            DiscoveryService discoveryService = mock(DiscoveryService.class);
            when(discoveryService.registerDefaultDiscovery(SELECTOR_ID, "divide", SYS_DEFAULT_NAMESPACE_ID)).thenReturn("fencing-handler");
            NamespacePluginRelMapper namespacePlugins = mock(NamespacePluginRelMapper.class);
            when(namespacePlugins.selectByPluginIdAndNamespaceId("fencing-plugin", SYS_DEFAULT_NAMESPACE_ID)).thenReturn(new NamespacePluginVO());
            ShenyuClientRegisterDivideServiceImpl writer = new ShenyuClientRegisterDivideServiceImpl();
            ReflectionTestUtils.setField(writer, "selectorService", selectorService(guard));
            ReflectionTestUtils.setField(writer, "discoveryUpstreamService", upstreamService);
            ReflectionTestUtils.setField(writer, "discoveryService", discoveryService);
            ReflectionTestUtils.setField(writer, "registrationFencingGuard", guard);
            ReflectionTestUtils.setField(writer, "pluginMapper", plugins);
            ReflectionTestUtils.setField(writer, "namespacePluginRelMapper", namespacePlugins);
            ReflectionTestUtils.setField(writer, "upstreamCheckService", mock(UpstreamCheckService.class));
            ReflectionTestUtils.setField(writer, "eventPublisher", mock(ApplicationEventPublisher.class));
            ReflectionTestUtils.setField(writer, "transactionManager", new DataSourceTransactionManager(dataSource));
            return writer;
        }

        private RegistrationFencingService service(final FencedRegisterService writer) {
            SqlSessionTemplate adminSessions = new SqlSessionTemplate(sessions.getSqlSessionFactory());
            final RegistrationFencingTargetMapper targetMapper = adminSessions.getMapper(RegistrationFencingTargetMapper.class);
            final SelectorMapper selectorMapper = adminSessions.getMapper(SelectorMapper.class);
            final DiscoveryUpstreamMapper upstreamMapper = mock(DiscoveryUpstreamMapper.class);
            DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
            upstream.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
            upstream.setUpstreamUrl("127.0.0.1:18080");
            upstream.setProtocol("http");
            when(upstreamMapper.selectBySelectorId(SELECTOR_ID)).thenReturn(List.of(upstream));
            RegistrationFencingProperties properties = new RegistrationFencingProperties();
            properties.setEnabled(true);
            RegistrationFencingServiceImpl target = new RegistrationFencingServiceImpl(targetMapper, selectorMapper, plugins,
                    upstreamMapper, mock(DiscoveryHandlerMapper.class), mock(DiscoveryMapper.class),
                    properties, List.of(writer));
            ProxyFactory proxy = new ProxyFactory(target);
            proxy.addAdvice(new TransactionInterceptor(new DataSourceTransactionManager(dataSource),
                    new AnnotationTransactionAttributeSource()));
            return (RegistrationFencingService) proxy.getProxy();
        }

        private void close() {
            jdbc.execute("SHUTDOWN");
        }
    }
}
