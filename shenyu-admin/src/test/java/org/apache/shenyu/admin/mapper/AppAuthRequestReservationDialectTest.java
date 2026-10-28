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
import org.apache.ibatis.datasource.unpooled.UnpooledDataSource;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.mapping.Environment;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.apache.ibatis.transaction.jdbc.JdbcTransactionFactory;
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.junit.jupiter.api.Test;

import java.sql.Connection;
import java.sql.Statement;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * reserveIfAbsent keeps the original row on every Admin databaseId.
 * H2 compatibility modes stand in for MySQL, PostgreSQL and Oracle syntax; they are not those servers.
 */
class AppAuthRequestReservationDialectTest {

    private static final String CREATE_TABLE = "CREATE TABLE app_auth_request ("
            + "id varchar(128) NOT NULL, caller_key varchar(128) NOT NULL, namespace_id varchar(64) NOT NULL, "
            + "request_id varchar(64) NOT NULL, request_hash varchar(64), auth_id varchar(255), state varchar(16) NOT NULL, "
            + "revoked_reason varchar(500), date_created timestamp default current_timestamp, "
            + "date_updated timestamp default current_timestamp, "
            + "PRIMARY KEY (id), UNIQUE (caller_key, namespace_id, request_id))";

    @Test
    void reserveIfAbsentKeepsTheOriginalRowOnEachDialect() throws Exception {
        assertReplay("mysql", "MySQL");
        assertReplay("h2", "MySQL");
        assertReplay("oceanbase", "MySQL");
        assertReplay("postgresql", "PostgreSQL");
        assertReplay("opengauss", "PostgreSQL");
        assertReplay("oracle", "Oracle");
    }

    private void assertReplay(final String databaseId, final String h2Mode) throws Exception {
        final UnpooledDataSource dataSource = new UnpooledDataSource("org.h2.Driver",
                "jdbc:h2:mem:app-auth-" + databaseId + ";MODE=" + h2Mode + ";DB_CLOSE_DELAY=-1", "sa", "");
        try (Connection connection = dataSource.getConnection();
             Statement statement = connection.createStatement()) {
            statement.execute(CREATE_TABLE);
        }
        final Environment environment = new Environment("app-auth-" + databaseId, new JdbcTransactionFactory(), dataSource);
        final Configuration configuration = new Configuration(environment);
        configuration.setMapUnderscoreToCamelCase(true);
        configuration.setDatabaseId(databaseId);
        new XMLMapperBuilder(Resources.getResourceAsStream("mappers/app-auth-request-sqlmap.xml"),
                configuration, "mappers/app-auth-request-sqlmap.xml", configuration.getSqlFragments()).parse();
        final SqlSessionFactory factory = new SqlSessionFactoryBuilder().build(configuration);
        try (SqlSession session = factory.openSession(true)) {
            final AppAuthRequestMapper mapper = session.getMapper(AppAuthRequestMapper.class);
            mapper.reserveIfAbsent(candidate("id-1", "hash-1", "CREATED"));
            mapper.reserveIfAbsent(candidate("id-2", "hash-2", "REVOKED"));
            final AppAuthRequestDO locked = mapper.selectByKeyForUpdate("caller", "ns", "req");
            assertEquals("id-1", locked.getId(), databaseId);
            assertEquals("hash-1", locked.getRequestHash(), databaseId);
            assertEquals("CREATED", locked.getState(), databaseId);
            assertNull(locked.getAuthId(), databaseId);

            mapper.reserveIfAbsent(candidate("id-new", null, "REVOKED", "req-new"));
            final AppAuthRequestDO tombstone = mapper.selectByKey("caller", "ns", "req-new");
            assertEquals("id-new", tombstone.getId(), databaseId);
            assertNull(tombstone.getRequestHash(), databaseId);
            assertEquals("REVOKED", tombstone.getState(), databaseId);
        } finally {
            try (Connection connection = dataSource.getConnection();
                 Statement statement = connection.createStatement()) {
                statement.execute("DROP TABLE IF EXISTS app_auth_request");
            }
        }
    }

    private AppAuthRequestDO candidate(final String id, final String hash, final String state) {
        return candidate(id, hash, state, "req");
    }

    private AppAuthRequestDO candidate(final String id, final String hash, final String state, final String requestId) {
        final AppAuthRequestDO candidate = new AppAuthRequestDO();
        candidate.setId(id);
        candidate.setCallerKey("caller");
        candidate.setNamespaceId("ns");
        candidate.setRequestId(requestId);
        candidate.setRequestHash(hash);
        candidate.setState(state);
        return candidate;
    }
}
