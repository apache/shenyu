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
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.RegistrationFencingTargetMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.DataPermissionChecker;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.impl.DiscoveryUpstreamServiceImpl;
import org.apache.shenyu.admin.service.impl.SelectorServiceImpl;
import org.apache.shenyu.admin.service.publish.SelectorEventPublisher;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.annotation.AnnotationTransactionAttributeSource;
import org.springframework.transaction.interceptor.TransactionInterceptor;
import org.springframework.transaction.support.TransactionTemplate;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Native MySQL regression: a consistent-read snapshot predates committed target creation.
 * Uses an explicitly configured disposable database, as with the AppAuth MySQL tests.
 */
@EnabledIfSystemProperty(named = "shenyu.fencing.test.jdbcUrl", matches = ".+")
final class RegistrationFencingSnapshotTest {

    private static final String SELECTOR_ID = "snapshot-selector";

    private static final String ENDPOINT = "127.0.0.1:18080";

    private JdbcTemplate jdbc;

    private DataSourceTransactionManager transactions;

    private SelectorMapper selectors;

    private RegistrationFencingTargetMapper targets;

    private RegistrationFencingGuard guard;

    private SelectorService management;

    private DiscoveryUpstreamService upstreams;

    private RegistrationFencingService registration;

    @BeforeEach
    void setUp() throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource(System.getProperty("shenyu.fencing.test.jdbcUrl"),
                System.getProperty("shenyu.fencing.test.username"), System.getProperty("shenyu.fencing.test.password", ""));
        try (Connection connection = dataSource.getConnection()) {
            assertTrue(connection.getCatalog().matches("shenyu_fencing_test[\\w]*"), "a disposable fencing test database is required");
        }
        jdbc = new JdbcTemplate(dataSource);
        recreateTables();
        seedSource();
        SqlSessionFactoryBean factory = new SqlSessionFactoryBean();
        factory.setDataSource(dataSource);
        factory.setMapperLocations(new ClassPathResource("mappers/selector-sqlmap.xml"),
                new ClassPathResource("mappers/registration-fencing-target-sqlmap.xml"),
                new ClassPathResource("mappers/discovery-upstream-sqlmap.xml"),
                new ClassPathResource("mappers/discovery-handler-sqlmap.xml"));
        SqlSessionTemplate sessions = new SqlSessionTemplate(factory.getObject());
        selectors = sessions.getMapper(SelectorMapper.class);
        targets = sessions.getMapper(RegistrationFencingTargetMapper.class);
        final DiscoveryUpstreamMapper upstreamMapper = sessions.getMapper(DiscoveryUpstreamMapper.class);
        assertEquals(SYS_DEFAULT_NAMESPACE_ID, upstreamMapper.selectBySelectorId(SELECTOR_ID).get(0).getNamespaceId());
        final DiscoveryHandlerMapper handlers = sessions.getMapper(DiscoveryHandlerMapper.class);
        PluginMapper plugins = mock(PluginMapper.class);
        when(plugins.selectById("snapshot-plugin")).thenReturn(PluginDO.builder().id("snapshot-plugin").name("divide").build());
        transactions = new DataSourceTransactionManager(dataSource);
        guard = new RegistrationFencingGuard(targets, selectors, plugins);
        management = (SelectorService) transactional(new SelectorServiceImpl(selectors, mock(SelectorConditionMapper.class), plugins,
                mock(ApplicationEventPublisher.class), mock(DiscoveryMapper.class), handlers, mock(DiscoveryRelMapper.class),
                upstreamMapper, mock(DiscoveryProcessorHolder.class), mock(SelectorEventPublisher.class), mock(DataPermissionChecker.class), guard));
        upstreams = (DiscoveryUpstreamService) transactional(new DiscoveryUpstreamServiceImpl(upstreamMapper, handlers,
                mock(ProxySelectorMapper.class), mock(DiscoveryMapper.class), mock(DiscoveryRelMapper.class), selectors, plugins,
                mock(DiscoveryProcessorHolder.class), guard));
        RegistrationFencingProperties properties = new RegistrationFencingProperties();
        properties.setEnabled(true);
        registration = (RegistrationFencingService) transactional(new RegistrationFencingServiceImpl(targets, selectors, plugins,
                upstreamMapper, handlers, mock(DiscoveryMapper.class), properties, List.of()));
    }

    @Test
    void selectorManagementCannotCommitAfterSnapshotPredatesTargetCreation() throws Exception {
        SelectorDO update = new SelectorDO();
        update.setId(SELECTOR_ID);
        update.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        update.setHandle("[\"stale-management-write\"]");

        RegistrationFencingException rejected = writeAfterConcurrentTargetCreation(() -> management.updateSelective(update));

        assertEquals("[]", selectors.selectById(SELECTOR_ID).getHandle(), "a stale management transaction must not commit its handle");
        assertRejected(rejected);
    }

    @Test
    void addressManagementCannotChangeReadinessAfterSnapshotPredatesTargetCreation() throws Exception {
        RegistrationFencingException rejected = writeAfterConcurrentTargetCreation(
                () -> upstreams.markNotReadyBySelectorIdAndUrl(SELECTOR_ID, ENDPOINT, true));

        assertEquals("{}", jdbc.queryForObject("SELECT props FROM discovery_upstream WHERE id = 'snapshot-upstream'", String.class),
                "a stale address writer must not commit readiness changes");
        assertRejected(rejected);
    }

    @Test
    void legacyUriGuardCannotMissTargetCreatedAfterSnapshot() throws Exception {
        RegistrationFencingException rejected = writeAfterConcurrentTargetCreation(() -> {
            guard.assertLegacyUriWrite(SELECTOR_ID, "127.0.0.1", 18080);
            jdbc.update("UPDATE discovery_upstream SET upstream_status = 1 WHERE id = 'snapshot-upstream'");
        });

        assertEquals(0, jdbc.queryForObject("SELECT upstream_status FROM discovery_upstream WHERE id = 'snapshot-upstream'", Integer.class),
                "a legacy URI writer must not commit after target creation");
        assertRejected(rejected);
    }

    private RegistrationFencingException writeAfterConcurrentTargetCreation(final Runnable writer) throws Exception {
        ExecutorService independentAdmin = Executors.newSingleThreadExecutor();
        try {
            TransactionTemplate snapshot = new TransactionTemplate(transactions);
            snapshot.setIsolationLevel(TransactionDefinition.ISOLATION_REPEATABLE_READ);
            snapshot.executeWithoutResult(status -> {
                assertEquals("[]", selectors.selectById(SELECTOR_ID).getHandle());
                assertTrue(targets.selectBySelectorId(SELECTOR_ID).isEmpty());
                createTargetOnIndependentAdmin(independentAdmin);
                // A fresh JDBC SELECT avoids MyBatis cache and confirms the database snapshot is still old.
                assertEquals(0, jdbc.queryForObject("SELECT COUNT(*) FROM registration_fencing_target WHERE selector_id = ?", Integer.class, SELECTOR_ID));
                writer.run();
            });
            return null;
        } catch (RegistrationFencingException rejected) {
            return rejected;
        } finally {
            independentAdmin.shutdownNow();
            assertTrue(independentAdmin.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    private void createTargetOnIndependentAdmin(final ExecutorService independentAdmin) {
        try {
            RegistrationFencingProtocol.Response<RegistrationFencingProtocol.TargetData> created = independentAdmin.submit(
                    () -> registration.createTarget(new RegistrationFencingProtocol.TargetRequest("domain-a", SYS_DEFAULT_NAMESPACE_ID,
                            SELECTOR_ID, "http", "127.0.0.1", 18080, "principal-a"), true)).get(10, TimeUnit.SECONDS);
            assertEquals(RegistrationFencingProtocol.OK, created.code(), created.message());
        } catch (Exception exception) {
            throw new IllegalStateException("independent target creation failed", exception);
        }
    }

    private void assertRejected(final RegistrationFencingException rejected) {
        assertNotNull(rejected, "the current protection state must reject the stale writer");
        assertEquals(RegistrationFencingProtocol.FENCING_REQUIRED, rejected.getCode());
        assertEquals(1, jdbc.queryForObject("SELECT COUNT(*) FROM registration_fencing_target WHERE selector_id = ? AND enabled = 1",
                Integer.class, SELECTOR_ID));
    }

    private Object transactional(final Object target) {
        ProxyFactory proxy = new ProxyFactory(target);
        proxy.addAdvice(new TransactionInterceptor(transactions, new AnnotationTransactionAttributeSource()));
        return proxy.getProxy();
    }

    private void recreateTables() throws Exception {
        String schema = Files.readString(Path.of("../db/init/mysql/schema.sql"), StandardCharsets.UTF_8);
        for (String table : List.of("registration_fencing_target", "selector", "discovery_handler", "discovery_upstream", "discovery_rel")) {
            int start = schema.indexOf("CREATE TABLE `" + table + "`");
            if (start < 0) {
                start = schema.indexOf("CREATE TABLE IF NOT EXISTS `" + table + "`");
            }
            assertTrue(start >= 0, "production MySQL schema must define " + table);
            int end = schema.indexOf(';', start);
            assertTrue(end > start, "production DDL must terminate for " + table);
            jdbc.execute("DROP TABLE IF EXISTS " + table);
            jdbc.execute(schema.substring(start, end));
        }
    }

    private void seedSource() {
        jdbc.update("INSERT INTO selector (id, plugin_id, selector_name, match_mode, selector_type, sort_code, handle, enabled, logged, continued, match_restful, namespace_id)"
                + " VALUES (?, 'snapshot-plugin', '/fenced', 0, 0, 1, '[]', 1, 0, 0, 0, ?)", SELECTOR_ID, SYS_DEFAULT_NAMESPACE_ID);
        jdbc.update("INSERT INTO discovery_handler (id, discovery_id, handler) VALUES ('snapshot-handler', 'snapshot-discovery', '[]')");
        jdbc.update("INSERT INTO discovery_upstream (id, discovery_handler_id, namespace_id, protocol, upstream_url, upstream_status, weight, props)"
                + " VALUES ('snapshot-upstream', 'snapshot-handler', ?, 'http', ?, 0, 100, '{}')", SYS_DEFAULT_NAMESPACE_ID, ENDPOINT);
        jdbc.update("INSERT INTO discovery_rel (id, plugin_name, discovery_handler_id, selector_id)"
                + " VALUES ('snapshot-rel', 'divide', 'snapshot-handler', ?)", SELECTOR_ID);
    }
}
