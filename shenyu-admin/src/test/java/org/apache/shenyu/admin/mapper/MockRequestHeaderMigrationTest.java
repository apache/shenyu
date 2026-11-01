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
import org.apache.shenyu.admin.model.entity.MockRequestRecordDO;
import org.h2.jdbcx.JdbcDataSource;
import org.junit.jupiter.api.Test;
import org.mybatis.spring.SqlSessionTemplate;
import org.mybatis.spring.transaction.SpringManagedTransactionFactory;
import org.springframework.core.io.FileSystemResource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Files;
import java.util.Objects;
import java.sql.Connection;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises the documented persistent-H2 migration without resetting existing rows.
 */
public final class MockRequestHeaderMigrationTest {

    @Test
    public void testMigrationPreservesLegacyDataAndAcceptsLongHeaders() throws Exception {
        JdbcDataSource dataSource = new JdbcDataSource();
        dataSource.setURL("jdbc:h2:mem:header-migration;MODE=MySQL");
        try (Connection connection = dataSource.getConnection(); Statement statement = connection.createStatement()) {
            String schema;
            try (InputStream in = Resources.getResourceAsStream("sql-script/h2/schema.sql")) {
                schema = new String(in.readAllBytes(), StandardCharsets.UTF_8);
            }
            Matcher table = Pattern.compile("(?is)CREATE TABLE IF NOT EXISTS `mock_request_record`.*?;").matcher(schema);
            assertTrue(table.find());
            String legacy = table.group().replace("`header` clob", "`header` varchar(1024)");
            assertTrue(legacy.contains("`header` varchar(1024)"));
            statement.execute(legacy);
            MockRequestRecordMapper mapper = mapper(dataSource);
            Timestamp time = Timestamp.valueOf("2026-01-01 00:00:00");
            MockRequestRecordDO record = MockRequestRecordDO.builder().id("legacy").apiId("legacy-api")
                    .host("localhost").port(9195).url("http://localhost/test").pathVariable("path").query("query")
                    .header("{\"X-Legacy\":\"汉字\"}").body("legacy-body").dateCreated(time).dateUpdated(time).build();
            mapper.insert(record);

            statement.execute("CREATE TABLE selector (loged INT)");
            statement.execute("CREATE TABLE rule (loged INT)");
            statement.execute("CREATE TABLE dashboard_user (id VARCHAR(128))");
            statement.execute("CREATE TABLE instance_info (id VARCHAR(128), namespace_id VARCHAR(50), instance_ip VARCHAR(128),"
                    + "instance_port VARCHAR(128), instance_type VARCHAR(128), last_heart_beat_time BIGINT, date_updated TIMESTAMP, date_created TIMESTAMP)");
            statement.execute("CREATE TABLE plugin (id VARCHAR(128), name VARCHAR(128), sort INT)");
            statement.execute("CREATE TABLE namespace_plugin_rel (plugin_id VARCHAR(128), sort INT)");
            statement.execute("CREATE TABLE plugin_handle (id VARCHAR(128) PRIMARY KEY, plugin_id VARCHAR(128), field VARCHAR(100),"
                    + " label VARCHAR(100), data_type INT, type INT, sort INT, ext_obj VARCHAR(2048),"
                    + " date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP, date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP)");
            statement.execute("CREATE TABLE shenyu_dict (id VARCHAR(128) PRIMARY KEY, type VARCHAR(100), dict_code VARCHAR(100),"
                    + " dict_name VARCHAR(100), dict_value VARCHAR(2048), `desc` VARCHAR(255), sort INT, enabled INT,"
                    + " date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP, date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP)");
            new ResourceDatabasePopulator(new FileSystemResource(upgradeScript())).execute(dataSource);

            MockRequestRecordDO restored = mapper.queryById("legacy");
            assertEquals(record.getHeader(), restored.getHeader());
            assertEquals(record.getBody(), restored.getBody());
            assertEquals(time, restored.getDateCreated());
            record.setHeader("{\"Authorization\":\"Bearer " + "t".repeat(16384) + "\"}");
            assertEquals(1, mapper.update(record));
            assertEquals(record.getHeader(), mapper.queryById("legacy").getHeader());
            assertEquals("legacy-body", mapper.queryById("legacy").getBody());
        }
    }

    private Path upgradeScript() {
        Path root = Path.of(System.getProperty("user.dir")).toAbsolutePath();
        while (Objects.nonNull(root) && !Files.isDirectory(root.resolve("db/upgrade"))) {
            root = root.getParent();
        }
        org.junit.jupiter.api.Assertions.assertNotNull(root);
        return root.resolve("db/upgrade/2.7.2-upgrade-2.7.3-h2.sql");
    }

    private MockRequestRecordMapper mapper(final JdbcDataSource dataSource) throws Exception {
        Configuration configuration = new Configuration(new Environment("header-migration", new SpringManagedTransactionFactory(), dataSource));
        configuration.setDatabaseId("h2");
        configuration.setMapUnderscoreToCamelCase(true);
        try (InputStream in = Resources.getResourceAsStream("mappers/mock-request-record-sqlmap.xml")) {
            new XMLMapperBuilder(in, configuration, "mappers/mock-request-record-sqlmap.xml", configuration.getSqlFragments()).parse();
        }
        return new SqlSessionTemplate(new SqlSessionFactoryBuilder().build(configuration)).getMapper(MockRequestRecordMapper.class);
    }
}
