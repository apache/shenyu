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

package org.apache.shenyu.admin.mapper;

import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.mapping.Environment;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.apache.shenyu.admin.model.entity.InstanceInfoDO;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.admin.service.impl.InstanceInfoServiceImpl;
import org.h2.jdbcx.JdbcDataSource;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mybatis.spring.SqlSessionTemplate;
import org.mybatis.spring.transaction.SpringManagedTransactionFactory;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.core.io.FileSystemResource;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.annotation.AnnotationTransactionAttributeSource;
import org.springframework.transaction.interceptor.TransactionInterceptor;
import org.springframework.transaction.support.TransactionTemplate;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.AdditionalAnswers.delegatesTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

/**
 * Exercises concurrent writers with real SQL sessions and the production H2 schema.
 */
public final class InstanceInfoUpsertTest {

    private JdbcTemplate jdbc;

    private InstanceInfoMapper mapper;

    private DataSourceTransactionManager transactionManager;

    @BeforeEach
    void setUp() throws Exception {
        JdbcDataSource dataSource = new JdbcDataSource();
        dataSource.setURL("jdbc:h2:mem:" + UUID.randomUUID() + ";MODE=MySQL;DB_CLOSE_DELAY=-1");
        jdbc = new JdbcTemplate(dataSource);
        transactionManager = new DataSourceTransactionManager(dataSource);
        try (InputStream schema = Resources.getResourceAsStream("sql-script/h2/schema.sql")) {
            String sql = new String(schema.readAllBytes(), StandardCharsets.UTF_8);
            Matcher table = Pattern.compile("(?is)CREATE TABLE IF NOT EXISTS instance_info\\s*\\(.*?\\);").matcher(sql);
            assertTrue(table.find());
            jdbc.execute(table.group());
        }
        Configuration configuration = new Configuration(new Environment("test", new SpringManagedTransactionFactory(), dataSource));
        configuration.setMapUnderscoreToCamelCase(true);
        try (InputStream in = Resources.getResourceAsStream("mappers/instance-info-sqlmap.xml")) {
            new XMLMapperBuilder(in, configuration, "mappers/instance-info-sqlmap.xml", configuration.getSqlFragments()).parse();
        }
        mapper = new SqlSessionTemplate(new SqlSessionFactoryBuilder().build(configuration)).getMapper(InstanceInfoMapper.class);
    }

    @AfterEach
    void tearDown() {
        jdbc.execute("SHUTDOWN");
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void concurrentRegistrationRetainsOneLatestObservation(final boolean newerFirst) throws Exception {
        CyclicBarrier absentLookups = new CyclicBarrier(2);
        AtomicInteger lookups = new AtomicInteger();
        CountDownLatch firstInsert = new CountDownLatch(1);
        InstanceInfoMapper concurrentMapper = mock(InstanceInfoMapper.class, delegatesTo(mapper));
        doAnswer(invocation -> {
            InstanceInfoDO found = mapper.selectOneByQuery(invocation.getArgument(0));
            if (lookups.incrementAndGet() <= 2) {
                assertNull(found);
                absentLookups.await(10, TimeUnit.SECONDS);
            }
            return found;
        }).when(concurrentMapper).selectOneByQuery(any(InstanceQuery.class));
        doAnswer(invocation -> {
            InstanceInfoDO observation = invocation.getArgument(0);
            boolean first = (observation.getLastHeartBeatTime() == 200) == newerFirst;
            if (!first) {
                assertTrue(firstInsert.await(10, TimeUnit.SECONDS));
            }
            try {
                return mapper.insert(observation);
            } finally {
                if (first) {
                    firstInsert.countDown();
                }
            }
        }).when(concurrentMapper).insert(any(InstanceInfoDO.class));
        InstanceInfoServiceImpl service = new InstanceInfoServiceImpl(concurrentMapper);
        ExecutorService writers = Executors.newFixedThreadPool(2);
        try {
            Future<?> older = writers.submit(() -> service.createOrUpdate(report("ns", 100, 2)));
            Future<?> newer = writers.submit(() -> service.createOrUpdate(report("ns", 200, 1)));
            older.get(15, TimeUnit.SECONDS);
            newer.get(15, TimeUnit.SECONDS);
            List<InstanceInfoDO> rows = mapper.findAllByNamespaceId("ns");
            assertEquals(1, rows.size(), "The database must enforce logical identity across writers");
            assertEquals(200, rows.get(0).getLastHeartBeatTime());
            assertEquals(1, rows.get(0).getInstanceState());
            assertEquals("info-200", rows.get(0).getInstanceInfo());
        } finally {
            writers.shutdownNow();
            assertTrue(writers.awaitTermination(10, TimeUnit.SECONDS));
        }
    }

    @Test
    void staleUpdateDoesNotOverwriteNewerReportAndNamespacesRemainIndependent() {
        InstanceInfoServiceImpl service = new InstanceInfoServiceImpl(mapper);
        service.createOrUpdate(report("ns", 200, 1));
        final InstanceInfoDO initial = mapper.findAllByNamespaceId("ns").get(0);
        service.createOrUpdate(report("other", 50, 2));
        service.createOrUpdate(report("ns", 100, 2));
        service.createOrUpdate(report("ns", 0, 0));
        InstanceInfoDO latest = mapper.findAllByNamespaceId("ns").get(0);
        assertEquals(initial.getId(), latest.getId());
        assertEquals(initial.getDateCreated(), latest.getDateCreated());
        assertEquals(200, latest.getLastHeartBeatTime());
        assertEquals(1, latest.getInstanceState());
        assertEquals("info-200", latest.getInstanceInfo());
        assertEquals(50, mapper.findAllByNamespaceId("other").get(0).getLastHeartBeatTime());

        // Liveness can transition to offline without a new heartbeat.
        service.createOrUpdate(report("ns", 200, 2));
        assertEquals(2, mapper.findAllByNamespaceId("ns").get(0).getInstanceState());
        service.createOrUpdate(report("ns", 300, 1));
        assertEquals(300, mapper.findAllByNamespaceId("ns").get(0).getLastHeartBeatTime());
        assertEquals(2, mapper.selectAll().size());
    }

    @Test
    void conflictRecoverySuspendsTheCallersTransaction() {
        new InstanceInfoServiceImpl(mapper).createOrUpdate(report("ns", 100, 2));
        AtomicInteger lookups = new AtomicInteger();
        InstanceInfoMapper conflictingMapper = mock(InstanceInfoMapper.class, delegatesTo(mapper));
        doAnswer(invocation -> lookups.incrementAndGet() == 1 ? null : mapper.selectOneByQuery(invocation.getArgument(0)))
                .when(conflictingMapper).selectOneByQuery(any(InstanceQuery.class));
        ProxyFactory factory = new ProxyFactory(new InstanceInfoServiceImpl(conflictingMapper));
        factory.addAdvice(new TransactionInterceptor(transactionManager, new AnnotationTransactionAttributeSource()));
        InstanceInfoService service = (InstanceInfoService) factory.getProxy();
        new TransactionTemplate(transactionManager).executeWithoutResult(status -> {
            service.createOrUpdate(report("ns", 200, 1));
            assertEquals(1, jdbc.queryForObject("SELECT COUNT(*) FROM instance_info", Integer.class));
            status.setRollbackOnly();
        });
        assertEquals(200, mapper.findAllByNamespaceId("ns").get(0).getLastHeartBeatTime());
    }

    @Test
    void migrationRetainsLatestCompleteRowsAndRejectsDuplicates() {
        jdbc.execute("ALTER TABLE instance_info DROP CONSTRAINT uk_instance_info_identity");
        jdbc.execute("CREATE TABLE selector (loged INT)");
        jdbc.execute("CREATE TABLE rule (loged INT)");
        jdbc.execute("CREATE TABLE dashboard_user (id VARCHAR(128))");
        jdbc.execute("CREATE TABLE mock_request_record (header VARCHAR(1024))");
        jdbc.execute("CREATE TABLE plugin (id VARCHAR(128), name VARCHAR(128), role VARCHAR(64), sort INT, enabled INT)");
        jdbc.execute("CREATE TABLE namespace_plugin_rel (id VARCHAR(128), namespace_id VARCHAR(50), plugin_id VARCHAR(128),"
                + " config VARCHAR(2048), sort INT, enabled INT, date_created TIMESTAMP, date_updated TIMESTAMP)");
        jdbc.execute("CREATE TABLE `resource` (id VARCHAR(128), parent_id VARCHAR(128), title VARCHAR(128),"
                + " name VARCHAR(128), url VARCHAR(256), component VARCHAR(128), resource_type INT, sort INT,"
                + " icon VARCHAR(64), is_leaf INT, is_route INT, perms VARCHAR(128), status INT,"
                + " date_created TIMESTAMP, date_updated TIMESTAMP)");
        jdbc.execute("CREATE TABLE permission (id VARCHAR(128), object_id VARCHAR(128), resource_id VARCHAR(128),"
                + " date_created TIMESTAMP, date_updated TIMESTAMP)");
        jdbc.execute("CREATE TABLE plugin_handle (id VARCHAR(128) PRIMARY KEY, plugin_id VARCHAR(128), field VARCHAR(100),"
                + " label VARCHAR(100), data_type INT, type INT, sort INT, ext_obj VARCHAR(2048),"
                + " date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP, date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP)");
        jdbc.execute("CREATE TABLE shenyu_dict (id VARCHAR(128) PRIMARY KEY, type VARCHAR(100), dict_code VARCHAR(100),"
                + " dict_name VARCHAR(100), dict_value VARCHAR(2048), `desc` VARCHAR(255), sort INT, enabled INT,"
                + " date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP, date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP)");
        insertLegacy("old-heartbeat", "ns", 100, "2026-01-03 00:00:00");
        insertLegacy("latest-heartbeat", "ns", 200, "2026-01-01 00:00:00");
        insertLegacy("tie-a", "ns-tie", 0, "2026-01-01 00:00:00");
        insertLegacy("tie-z", "ns-tie", 0, "2026-01-01 00:00:00");
        insertLegacy("other", "ns-other", 100, "2026-01-01 00:00:00");
        Path root = Path.of(System.getProperty("user.dir")).toAbsolutePath();
        while (Objects.nonNull(root) && !Files.isDirectory(root.resolve("db/upgrade"))) {
            root = root.getParent();
        }
        assertNotNull(root);
        new ResourceDatabasePopulator(new FileSystemResource(root.resolve("db/upgrade/2.7.2-upgrade-2.7.3-h2.sql")))
                .execute(Objects.requireNonNull(jdbc.getDataSource()));
        assertEquals(Integer.valueOf(1), jdbc.queryForObject("SELECT COUNT(*) FROM plugin WHERE name = 'tracing'", Integer.class));
        assertEquals(Integer.valueOf(10), jdbc.queryForObject(
                "SELECT COUNT(*) FROM `resource` WHERE id BETWEEN '1890000000000000020' AND '1890000000000000029'", Integer.class));
        assertEquals(Integer.valueOf(10), jdbc.queryForObject(
                "SELECT COUNT(*) FROM permission WHERE id BETWEEN '1890000000000000040' AND '1890000000000000049'", Integer.class));
        assertEquals(Set.of("latest-heartbeat", "tie-z", "other"), mapper.selectAll().stream().map(InstanceInfoDO::getId).collect(Collectors.toSet()));
        InstanceInfoDO retained = mapper.selectById("latest-heartbeat");
        assertEquals(200, retained.getLastHeartBeatTime());
        assertEquals("info-200", retained.getInstanceInfo());
        assertEquals(Timestamp.valueOf("2026-01-01 00:00:00"), retained.getDateUpdated());
        assertThrows(DuplicateKeyException.class, () -> insertLegacy("conflict", "ns", 300, "2026-01-04 00:00:00"));
    }

    private void insertLegacy(final String id, final String namespace, final long heartbeat, final String updated) {
        InstanceInfoDO row = InstanceInfoDO.buildInstanceInfoDO(report(namespace, heartbeat, 1));
        row.setId(id);
        row.setDateCreated(Timestamp.valueOf("2026-01-01 00:00:00"));
        row.setDateUpdated(Timestamp.valueOf(updated));
        mapper.insert(row);
    }

    private InstanceInfoVO report(final String namespace, final long heartbeat, final int state) {
        InstanceInfoVO report = new InstanceInfoVO();
        report.setNamespaceId(namespace);
        report.setInstanceIp("127.0.0.1");
        report.setInstancePort("9195");
        report.setInstanceType("bootstrap");
        report.setInstanceInfo("info-" + heartbeat);
        report.setLastHeartBeatTime(heartbeat);
        report.setInstanceState(state);
        return report;
    }
}
