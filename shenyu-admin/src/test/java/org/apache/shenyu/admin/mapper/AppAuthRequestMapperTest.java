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
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.apache.ibatis.transaction.jdbc.JdbcTransactionFactory;
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.apache.shenyu.admin.service.impl.AppAuthMySqlFixture;
import org.apache.ibatis.datasource.unpooled.UnpooledDataSource;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * AppAuthRequestMapper verification on real MySQL (isolated database):
 * reserve replay never mutates the original row, completeCreated is a real CAS, caller/namespace isolation holds, empty-set queries are safe,
 * markRevoked the historical authId is kept.
 */
@org.junit.jupiter.api.condition.EnabledIfSystemProperty(named = "shenyu.auth.test.jdbcUrl", matches = ".+")
class AppAuthRequestMapperTest {

    private static SqlSessionFactory sqlSessionFactory;

    private SqlSession session;

    private AppAuthRequestMapper mapper;

    @BeforeAll
    static void initMyBatis() throws Exception {
        AppAuthMySqlFixture.requireExplicitTestConfig();
        final UnpooledDataSource dataSource = new UnpooledDataSource(
                "com.mysql.cj.jdbc.Driver",
                System.getProperty("shenyu.auth.test.jdbcUrl"),
                System.getProperty("shenyu.auth.test.username"),
                System.getProperty("shenyu.auth.test.password"));
        final Environment environment = new Environment("app-auth-test", new JdbcTransactionFactory(), dataSource);
        final Configuration configuration = new Configuration(environment);
        configuration.setMapUnderscoreToCamelCase(true);
        new XMLMapperBuilder(Resources.getResourceAsStream("mappers/app-auth-request-sqlmap.xml"),
                configuration, "mappers/app-auth-request-sqlmap.xml", configuration.getSqlFragments()).parse();
        sqlSessionFactory = new SqlSessionFactoryBuilder().build(configuration);

        try (Connection connection = AppAuthMySqlFixture.openConnection();
             Statement statement = connection.createStatement()) {
            AppAuthMySqlFixture.verifyTestDatabaseIdentity(connection);
            AppAuthMySqlFixture.initProbeTable(connection);
            statement.execute("DROP TABLE IF EXISTS app_auth_request");
            // the migration script owns only app_auth_request; this test creates the minimal table directly
            statement.execute("CREATE TABLE app_auth_request ("
                    + "id VARCHAR(128) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,"
                    + "caller_key VARCHAR(128) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,"
                    + "namespace_id VARCHAR(64) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,"
                    + "request_id VARCHAR(64) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,"
                    + "request_hash VARCHAR(64) CHARACTER SET ascii COLLATE ascii_bin NULL,"
                    + "auth_id VARCHAR(255) CHARACTER SET ascii COLLATE ascii_bin NULL,"
                    + "state VARCHAR(16) CHARACTER SET ascii NOT NULL,"
                    + "revoked_reason VARCHAR(500) NULL,"
                    + "date_created DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),"
                    + "date_updated DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3),"
                    + "PRIMARY KEY (id), UNIQUE KEY uk_request (caller_key, namespace_id, request_id),"
                    + "KEY idx_auth (auth_id)) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci");
        }
    }

    @BeforeEach
    void openSession() {
        session = sqlSessionFactory.openSession(false);
        mapper = session.getMapper(AppAuthRequestMapper.class);
    }

    @AfterEach
    void closeSession() {
        if (Objects.nonNull(session)) {
            session.rollback();
            session.close();
        }
    }

    @AfterAll
    static void tearDownAll() throws SQLException {
        try (Connection connection = AppAuthMySqlFixture.openConnection();
             Statement statement = connection.createStatement()) {
            statement.execute("DROP TABLE IF EXISTS app_auth_request");
        }
    }

    private AppAuthRequestDO candidate(final String id, final String caller, final String ns,
                                          final String requestId, final String hash, final String state) {
        final AppAuthRequestDO candidate = new AppAuthRequestDO();
        candidate.setId(id);
        candidate.setCallerKey(caller);
        candidate.setNamespaceId(ns);
        candidate.setRequestId(requestId);
        candidate.setRequestHash(hash);
        candidate.setState(state);
        return candidate;
    }

    @Test
    void reserveReplayDoesNotClobberOriginal() {
        mapper.reserveIfAbsent(candidate("id-1", "svc-a", "ns-1", "req-1", "hash-1", "CREATED"));
        assertEquals(1, mapper.completeCreated("id-1", "auth-A"));
        session.commit();

        // another transaction replays the same key with a different candidate: id/hash/authId of the original row must stay untouched
        final SqlSession other = sqlSessionFactory.openSession(false);
        final AppAuthRequestMapper otherMapper = other.getMapper(AppAuthRequestMapper.class);
        try {
            otherMapper.reserveIfAbsent(candidate("id-2", "svc-a", "ns-1", "req-1", "hash-2", "REVOKED"));
            final AppAuthRequestDO locked = otherMapper.selectByKeyForUpdate("svc-a", "ns-1", "req-1");
            assertEquals("id-1", locked.getId());
            assertEquals("hash-1", locked.getRequestHash());
            assertEquals("auth-A", locked.getAuthId());
            assertEquals("CREATED", locked.getState());
        } finally {
            other.rollback();
            other.close();
        }
    }

    @Test
    void completeCreatedIsCasOnStateAndNullAuthId() {
        mapper.reserveIfAbsent(candidate("id-c", "svc-c", "ns-1", "req-c", "hash-c", "CREATED"));
        assertEquals(1, mapper.completeCreated("id-c", "auth-C1"));
        // backfill again after a successful backfill (CAS fails)
        assertEquals(0, mapper.completeCreated("id-c", "auth-C2"));
        session.commit();
        final AppAuthRequestDO record = mapper.selectByKey("svc-c", "ns-1", "req-c");
        assertEquals("auth-C1", record.getAuthId());
    }

    @Test
    void crossCallerAndNamespaceAreIsolated() {
        mapper.reserveIfAbsent(candidate("id-x", "svc-a", "ns-1", "req-same", "h1", "CREATED"));
        mapper.reserveIfAbsent(candidate("id-y", "svc-b", "ns-1", "req-same", "h1", "CREATED"));
        mapper.reserveIfAbsent(candidate("id-z", "svc-a", "ns-2", "req-same", "h1", "CREATED"));
        session.commit();
        assertEquals("id-x", mapper.selectByKey("svc-a", "ns-1", "req-same").getId());
        assertEquals("id-y", mapper.selectByKey("svc-b", "ns-1", "req-same").getId());
        assertEquals("id-z", mapper.selectByKey("svc-a", "ns-2", "req-same").getId());
    }

    @Test
    void emptyAuthIdsQueryIsSafeAndReturnsNothing() {
        final List<AppAuthRequestDO> records = mapper.selectKeysByAuthIds(Collections.emptyList());
        assertTrue(records.isEmpty());
        assertNull(mapper.selectByAuthId("no-such-auth"));
    }

    @Test
    void markRevokedKeepsHistoricalAuthId() {
        mapper.reserveIfAbsent(candidate("id-r", "svc-r", "ns-1", "req-r", "hash-r", "CREATED"));
        assertEquals(1, mapper.completeCreated("id-r", "auth-R"));
        assertEquals(1, mapper.markRevoked("id-r", "manual-drill"));
        session.commit();

        final AppAuthRequestDO record = mapper.selectByAuthId("auth-R");
        assertEquals("REVOKED", record.getState());
        assertEquals("auth-R", record.getAuthId(), "revocation keeps the historical authId");
        assertEquals("hash-r", record.getRequestHash());
    }
}
