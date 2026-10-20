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

package org.apache.shenyu.admin.service.impl;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Objects;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Isolated-database MySQL fixture: connection settings must be passed explicitly via system properties;
 * missing properties fail fast and never silently connect to a default or dev database. Isolated environment only.
 */
public final class AppAuthMySqlFixture {

    /** Isolated-database naming convention: the catalog must match this prefix to avoid touching other databases. */
    public static final String TEST_DB_NAME_PATTERN = "flm_auth_test.*";

    private static final String PROP_JDBC_URL = "shenyu.auth.test.jdbcUrl";

    private static final String PROP_USERNAME = "shenyu.auth.test.username";

    private static final String PROP_PASSWORD = "shenyu.auth.test.password";

    /** Probe table: verifies transaction visibility and runId isolation; created and cleaned by the fixture. */
    private static final String PROBE_TABLE = "flm_auth_env_probe";

    private static final DateTimeFormatter TS = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

    private AppAuthMySqlFixture() {
    }

    /**
     * Requires explicit test connection settings; throws a clear error when they are missing.
     */
    public static void requireExplicitTestConfig() {
        if (Objects.isNull(System.getProperty(PROP_JDBC_URL)) || Objects.isNull(System.getProperty(PROP_USERNAME))) {
            throw new IllegalStateException("Missing explicit test connection settings: "
                    + "pass the isolated-database connection via -Dshenyu.auth.test.jdbcUrl / username / password (default or dev databases are forbidden)");
        }
    }

    /**
     * Open a new dedicated connection (the caller must close it).
     *
     * @return the JDBC connection
     * @throws SQLException when connecting fails
     */
    public static Connection openConnection() throws SQLException {
        requireExplicitTestConfig();
        return DriverManager.getConnection(
                System.getProperty(PROP_JDBC_URL),
                System.getProperty(PROP_USERNAME),
                System.getProperty(PROP_PASSWORD));
    }

    /**
     * Verify the test database identity: the catalog must match the isolated-database naming convention.
     *
     * @param connection the open connection
     * @throws SQLException when verification fails
     */
    public static void verifyTestDatabaseIdentity(final Connection connection) throws SQLException {
        final String catalog = connection.getCatalog();
        if (Objects.isNull(catalog) || !catalog.matches(TEST_DB_NAME_PATTERN)) {
            throw new IllegalStateException("Test database identity check failed: catalog '" + catalog
                    + "' does not match the isolated-database convention " + TEST_DB_NAME_PATTERN + "; running tests on it is forbidden");
        }
    }

    /**
     * Initialize the probe table (idempotent).
     *
     * @param connection the open connection
     * @throws SQLException when creating the table fails
     */
    public static void initProbeTable(final Connection connection) throws SQLException {
        try (Statement statement = connection.createStatement()) {
            statement.execute("CREATE TABLE IF NOT EXISTS " + PROBE_TABLE + " ("
                    + "run_id VARCHAR(64) NOT NULL, marker VARCHAR(128) NOT NULL, "
                    + "created_at DATETIME NOT NULL, PRIMARY KEY (run_id, marker)) "
                    + "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin");
        }
    }

    /**
     * Verify transaction visibility: uncommitted rows are invisible to other connections, disappear after rollback and appear after commit.
     *
     * @param writer the writing connection
     * @param reader an independent reading connection
     * @throws SQLException when verification fails
     */
    public static void verifyTransactionVisibility(final Connection writer, final Connection reader) throws SQLException {
        final String marker = "txn-probe-" + newRunId();
        writer.setAutoCommit(false);
        insertProbe(writer, marker);
        if (countMarker(reader, marker) != 0) {
            writer.rollback();
            throw new IllegalStateException("Transaction visibility check failed: uncommitted rows must be invisible to other connections (isolation level anomaly)");
        }
        writer.rollback();
        if (countMarker(reader, marker) != 0) {
            throw new IllegalStateException("Transaction visibility check failed: rows remain visible after rollback");
        }
        insertProbe(writer, marker);
        writer.commit();
        writer.setAutoCommit(true);
        if (countMarker(reader, marker) != 1) {
            throw new IllegalStateException("Transaction visibility check failed: other connections must see exactly one row after commit");
        }
        deleteMarker(marker);
    }

    /**
     * Generate a runId with a timestamp and random suffix.
     *
     * @return the new runId
     */
    public static String newRunId() {
        return "run" + TS.format(LocalDateTime.now()) + "-" + Integer.toHexString(ThreadLocalRandom.current().nextInt());
    }

    /**
     * Write a set of probe rows for the runId.
     *
     * @param runId the run identifier
     * @param count row count
     * @throws SQLException when writing fails
     */
    public static void insertRunRows(final String runId, final int count) throws SQLException {
        try (Connection connection = openConnection()) {
            connection.setAutoCommit(true);
            for (int i = 0; i < count; i++) {
                insertProbe(connection, runId + "-" + i);
            }
        }
    }

    /**
     * Clean the probe rows of one runId.
     *
     * @param runId the run identifier
     * @return the deleted row count
     * @throws SQLException when deleting fails
     */
    public static int cleanupRun(final String runId) throws SQLException {
        try (Connection connection = openConnection();
             PreparedStatement ps = connection.prepareStatement(
                     "DELETE FROM " + PROBE_TABLE + " WHERE run_id LIKE ? OR marker LIKE ?")) {
            ps.setString(1, runId + "%");
            ps.setString(2, runId + "%");
            return ps.executeUpdate();
        }
    }

    /**
     * Count probe rows with the given runId prefix.
     *
     * @param runId the run identifier
     * @return the row count
     * @throws SQLException when the query fails
     */
    public static int countRun(final String runId) throws SQLException {
        try (Connection connection = openConnection();
             PreparedStatement ps = connection.prepareStatement(
                     "SELECT COUNT(*) FROM " + PROBE_TABLE + " WHERE marker LIKE ?")) {
            ps.setString(1, runId + "%");
            try (ResultSet rs = ps.executeQuery()) {
                rs.next();
                return rs.getInt(1);
            }
        }
    }

    private static void insertProbe(final Connection connection, final String marker) throws SQLException {
        try (PreparedStatement ps = connection.prepareStatement(
                "INSERT INTO " + PROBE_TABLE + " (run_id, marker, created_at) VALUES (?, ?, NOW())")) {
            final int dash = marker.indexOf('-');
            ps.setString(1, dash > 0 ? marker.substring(0, dash) : marker);
            ps.setString(2, marker);
            ps.executeUpdate();
        }
    }

    private static int countMarker(final Connection connection, final String marker) throws SQLException {
        try (PreparedStatement ps = connection.prepareStatement(
                "SELECT COUNT(*) FROM " + PROBE_TABLE + " WHERE marker = ?")) {
            ps.setString(1, marker);
            try (ResultSet rs = ps.executeQuery()) {
                rs.next();
                return rs.getInt(1);
            }
        }
    }

    private static void deleteMarker(final String marker) throws SQLException {
        try (Connection connection = openConnection();
             PreparedStatement ps = connection.prepareStatement(
                     "DELETE FROM " + PROBE_TABLE + " WHERE marker = ?")) {
            ps.setString(1, marker);
            ps.executeUpdate();
        }
    }
}
