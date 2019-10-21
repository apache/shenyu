/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.store.mysql.impl;

import com.google.common.collect.Maps;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.jdbc.ScriptRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.sql.DataSource;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * The type Abstract jdbc repository service.
 *
 * @author xiaoyu
 */
abstract class AbstractJdbcRepositoryService {

    private static final Logger LOGGER = LoggerFactory.getLogger(JdbcRepositoryService.class);

    private static final String SCHEMA_SQL_FILE = "META-INF/schema.sql";

    private DataSource dataSource;

    /**
     * Sets data source.
     *
     * @param dataSource the data source
     */
    void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    /**
     * Execute script.
     */
    void executeScript() {
        try (Connection connection = dataSource.getConnection()) {
            ScriptRunner runner = new ScriptRunner(connection);
            // doesn't print logger
            runner.setLogWriter(null);
            Resources.setCharset(StandardCharsets.UTF_8);
            Reader read = Resources.getResourceAsReader(SCHEMA_SQL_FILE);
            LOGGER.info("execute soul schema sql: {}", SCHEMA_SQL_FILE);
            runner.runScript(read);
            runner.closeConnection();
        } catch (Exception e) {
            LOGGER.error("execute sql script exception:", e);
        }

    }

    /**
     * Execute update int.
     *
     * @param sql    the sql
     * @param params the params
     * @return the int
     */
    int executeUpdate(final String sql, final Object... params) {
        try (Connection connection = dataSource.getConnection();
             PreparedStatement ps = connection.prepareStatement(sql)) {
            if (params != null) {
                for (int i = 0; i < params.length; i++) {
                    ps.setObject(i + 1, params[i]);
                }
            }
            return ps.executeUpdate();
        } catch (SQLException e) {
            LOGGER.error("executeUpdate-> " + e.getMessage());
            return -1;
        }

    }

    /**
     * Execute query list.
     *
     * @param sql    the sql
     * @param params the params
     * @return the list
     */
    List<Map<String, Object>> executeQuery(final String sql, final Object... params) {
        List<Map<String, Object>> list = null;
        try (Connection connection = dataSource.getConnection();
             PreparedStatement ps = connection.prepareStatement(sql);
             ResultSet rs = ps.executeQuery();) {
            if (params != null) {
                for (int i = 0; i < params.length; i++) {
                    ps.setObject(i + 1, params[i]);
                }
            }
            ResultSetMetaData md = rs.getMetaData();
            int columnCount = md.getColumnCount();
            list = new ArrayList<>();
            while (rs.next()) {
                Map<String, Object> rowData = Maps.newHashMap();
                for (int i = 1; i <= columnCount; i++) {
                    rowData.put(md.getColumnName(i), rs.getObject(i));
                }
                list.add(rowData);
            }
        } catch (SQLException e) {
            LOGGER.error("executeQuery-> " + e.getMessage());
        }
        return list;
    }
}
